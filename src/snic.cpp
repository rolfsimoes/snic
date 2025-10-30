#ifdef _FORTIFY_SOURCE
#undef _FORTIFY_SOURCE
#endif

#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <numeric>
#include <queue>
#include <stdexcept>
#include <vector>

namespace snic {

// =====================
// Data containers (declarations)
// =====================

struct Img {
    const double* data;
    int w;
    int h;
    int b;
    int n;
    std::vector<std::uint8_t> mask_;
    int n_valid;

    Img(const double* data, int width, int height, int bands);

    int idx(int r, int c) const;
    int row(int pid) const;
    int col(int pid) const;
    void vals(int pid, std::vector<double>& out) const;
    void neighbors4(int pid, std::vector<int>& out) const;
    bool has_nan(int pid) const;
    int valid_pixel_count() const;
    const std::vector<std::uint8_t>& mask() const;
};

static double dist_sq_eu_buf(const double* a, const double* b, int len);

struct Clu {
    std::vector<double> val;
    double r;
    double c;
    int n;

    Clu(int row, int col, const std::vector<double>& seeds_val);

    void update_with_vals(const std::vector<double>& px_val, int row, int col);

    double dist_sq_to_vals(const std::vector<double>& px_val,
                           int row, int col,
                           double spat_scale,
                           double compactness) const;
};

struct Node {
    double dst;
    int pid;
    int cid;

    Node();
    Node(double d, int pixel, int cluster);
};

struct NodeGreater {
    bool operator()(const Node& a, const Node& b) const;
};

// =====================
// Img implementation
// =====================

/*
 * Construct an Img object.
 * @param data_ Pointer to image data in band-interleaved format.
 *   pixels are stored column-wise, with bands stacked. A pixel at (r, c)
 *   and band b is located at data_[r + c * height + b * (width * height)].
 *   data_ comes from R as 3D numerical array (height x width x bands).
 *   Column-major order is used to match R's internal storage. No rearrangement
 *   is needed.
 * @param height Image height (number of rows)
 * @param width Image width (number of columns)
 * @param bands Number of bands (channels) in the image
 *
 */
Img::Img(const double* data_, int height, int width, int bands)
    : data(data_), w(width), h(height), b(bands), n(width * height),
      mask_(), n_valid(0)
{
    if (data == nullptr) {
        throw std::invalid_argument("Image data pointer must not be null");
    }
    if (w <= 0 || h <= 0) {
        throw std::invalid_argument("Image width and height must be positive");
    }
    if (b <= 0) {
        throw std::invalid_argument("Number of bands must be positive");
    }

    mask_.resize(static_cast<std::size_t>(n));
    int valid = 0;
    for (int pid = 0; pid < n; ++pid) {
        const bool has_na = has_nan(pid);
        mask_[pid] = has_na ? static_cast<std::uint8_t>(0) :
            static_cast<std::uint8_t>(1);
        if (!has_na) {
            ++valid;
        }
    }
    n_valid = valid;
}

int Img::idx(int r, int c) const
{
    return r + c * h;
}

int Img::row(int pid) const
{
    return pid % h;
}

int Img::col(int pid) const
{
    return pid / h;
}

void Img::vals(int pid, std::vector<double>& out) const
{
    out.resize(b);
    const std::size_t base = static_cast<std::size_t>(pid);
    const std::size_t stride = static_cast<std::size_t>(n);
    for (int bi = 0; bi < b; ++bi) {
        out[bi] = data[base + stride * static_cast<std::size_t>(bi)];
    }
}

void Img::neighbors4(int pid, std::vector<int>& out) const
{
    out.clear();
    out.reserve(4);

    const int r = row(pid);
    const int c = col(pid);
    if (r > 0) {
        out.push_back(idx(r - 1, c));
    }
    if (r + 1 < h) {
        out.push_back(idx(r + 1, c));
    }
    if (c > 0) {
        out.push_back(idx(r, c - 1));
    }
    if (c + 1 < w) {
        out.push_back(idx(r, c + 1));
    }
}

bool Img::has_nan(int pid) const
{
    const std::size_t base = static_cast<std::size_t>(pid);
    const std::size_t stride = static_cast<std::size_t>(n);
    for (int bi = 0; bi < b; ++bi) {
        if (ISNAN(data[base + stride * static_cast<std::size_t>(bi)])) {
            return true;
        }
    }
    return false;
}

int Img::valid_pixel_count() const
{
    return n_valid;
}

const std::vector<std::uint8_t>& Img::mask() const
{
    return mask_;
}

// =====================
// Clu implementation
// =====================

Clu::Clu(int row, int col, const std::vector<double>& seeds_val)
    : val(seeds_val),
      r(static_cast<double>(row)),
      c(static_cast<double>(col)),
      n(0)
{
}

void Clu::update_with_vals(const std::vector<double>& px_val, int row, int col)
{
    n += 1;
    const double inv_n = 1.0 / static_cast<double>(n);
    const double wt = 1.0 - inv_n;

    const int k = std::min(static_cast<int>(val.size()),
        static_cast<int>(px_val.size()));
    for (int i = 0; i < k; ++i) {
        val[i] = val[i] * wt + px_val[i] * inv_n;
    }

    r = r * wt + static_cast<double>(row) * inv_n;
    c = c * wt + static_cast<double>(col) * inv_n;
}

double Clu::dist_sq_to_vals(const std::vector<double>& px_val,
                            int row, int col,
                            double spat_scale,
                            double compactness) const
{
    const int k = std::min(static_cast<int>(val.size()),
        static_cast<int>(px_val.size()));
    const double dval_sq = dist_sq_eu_buf(px_val.data(), val.data(), k);

    const double dr = static_cast<double>(row) - r;
    const double dc = static_cast<double>(col) - c;
    double ratio = 0.0;
    if (spat_scale > 0.0) {
        ratio = compactness / spat_scale;
    }
    return dval_sq + ratio * ratio * (dr * dr + dc * dc);
}

// =====================
// Node and comparator
// =====================

Node::Node()
    : dst(0.0), pid(-1), cid(-1)
{
}

Node::Node(double d, int pixel, int cluster)
    : dst(d), pid(pixel), cid(cluster)
{
}

bool NodeGreater::operator()(const Node& a, const Node& b) const
{
    return a.dst > b.dst;
}

// =====================
// Free functions
// =====================


static double dist_sq_eu_buf(const double* a, const double* b, int len)
{
  double sum = 0.0;
  for (int i = 0; i < len; ++i) {
    const double d = a[i] - b[i];
    sum += d * d;
  }
  return sum;
}

/*
 * Implement a grid seeding function that places seeds at regular intervals
 *   across the image, respecting the image mask.
*/
static std::vector<int> grid_seeds(const Img& img,
                                   int grid_step)
{
    std::vector<int> out;
    if (grid_step <= 0) {
        return out;
    }

    const std::vector<std::uint8_t>& mask = img.mask();
    if (mask.size() != static_cast<std::size_t>(img.n)) {
        throw std::runtime_error("Mask size mismatch for image size.");
    }
    out.reserve(mask.size() / (grid_step * grid_step));
    const int offset = grid_step / 2;
    for (int r = offset; r < img.h; r += grid_step) {
        for (int c = offset; c < img.w; c += grid_step) {
            const int pid = img.idx(r, c);
            if (mask[pid]) {
                out.push_back(pid);
            }
        }
    }

    return out;
}

/*
 * SNIC segmentation function
 * Returns segmentation vector (size img.n). Pixels masked out remain 0.
 * Segments are labeled from 1 to number of seeds.
 */
static std::vector<int> snic_segment(const Img& img,
                                     const std::vector<int>& seeds,
                                     double compactness)
{
    if (img.n <= 0) {
        throw std::runtime_error("Image must contain at least one pixel");
    }

    const std::vector<std::uint8_t>& mask = img.mask();
    if (mask.size() != static_cast<std::size_t>(img.n)) {
        throw std::runtime_error("Mask size mismatch for image size");
    }
    if (seeds.empty()) {
        throw std::runtime_error("No seeds provided for SNIC segmentation");
    }

    if (compactness < 0.0) {
        compactness = 0.0;
    }

    const int valid = img.valid_pixel_count();
    if (valid == 0) {
        throw std::runtime_error("All pixels contain NA values; SNIC cannot segment");
    }

    for (int pid : seeds) {
        if (pid < 0 || pid >= img.n) {
            throw std::runtime_error("Seed index out of bounds for provided image dimensions");
        }
        if (!mask[pid]) {
            throw std::runtime_error("Seed placed on pixel containing NA values");
        }
    }

    const double spat_scale = std::sqrt(
        static_cast<double>(valid) / static_cast<double>(seeds.size())
    );

    std::vector<Clu> clus;
    clus.reserve(seeds.size());
    std::vector<double> px_val;
    px_val.reserve(static_cast<std::size_t>(img.b));

    for (int pid : seeds) {
        img.vals(pid, px_val);
        clus.emplace_back(img.row(pid), img.col(pid), px_val);
    }

    std::vector<int> seg(static_cast<std::size_t>(img.n), 0);

    std::priority_queue<Node, std::vector<Node>, NodeGreater> pq;
    for (int cid = 0; cid < static_cast<int>(seeds.size()); ++cid) {
        const int pid = seeds[cid];
        pq.emplace(0.0, pid, cid);
    }

    std::vector<int> neighbors;
    neighbors.reserve(4);

    while (!pq.empty()) {
        const Node node = pq.top();
        pq.pop();

        const int pid = node.pid;
        const int cid = node.cid;

        if (pid < 0 || pid >= img.n) {
            continue;
        }
        if (seg[pid] != 0) {
            continue;
        }
        if (!mask[pid]) {
            continue;
        }

        img.vals(pid, px_val);
        clus[cid].update_with_vals(px_val, img.row(pid), img.col(pid));
        seg[pid] = cid + 1;

        neighbors.clear();
        img.neighbors4(pid, neighbors);
        for (int nid : neighbors) {
            if (!mask[nid]) {
                continue;
            }
            if (seg[nid] != 0) {
                continue;
            }

            img.vals(nid, px_val);
            const double d2 = clus[cid].dist_sq_to_vals(
                px_val, img.row(nid), img.col(nid),
                spat_scale, compactness
            );

            pq.emplace(d2, nid, cid);
        }
    }

    return seg;
}

} // namespace snic


/*
 * This is the R interface to the SNIC segmentation function.
 *
 * @param imgSEXP Numeric array (height x width x bands) representing the image.
 *   Pixels are stored column-wise, with bands stacked. No rearrangement is
 *   needed. A pixel at (r, c) and band b is located at
 *   img[r + c * height + b * (width * height)].
 *   The array must have exactly three dimensions and strictly positive sizes.
 * @param seedsSEXP Integer matrix (m x 2) of seed coordinates (row, column).
 *   Coordinates are 1-based (R style). Values must be within image bounds.
 * @param compactSEXP Single numeric value (scalar) representing the
 *   compactness parameter; must be finite and non-negative.
 * @param grid_stepSEXP Used just when seeds are not provided. Integer scalar
 *   representing the grid step size; must be positive and not exceed the
 *   image width or height.
 * @return Integer vector (length width * height) representing the
 *  segmentation. Pixels masked out remain NA. Segments are labeled from
 *  1 to number of seeds.
 */
extern "C" SEXP _snic(SEXP imgSEXP,
                      SEXP seedsSEXP,
                      SEXP compactSEXP,
                      SEXP grid_stepSEXP)
{
    if (!Rf_isReal(imgSEXP)) {
        Rf_error("Argument `img` must be a numeric array");
    }
    SEXP dim = getAttrib(imgSEXP, R_DimSymbol);
    if (dim == R_NilValue) {
        Rf_error("Argument `img` must have dimensions");
    }
    if (!Rf_isInteger(dim) || LENGTH(dim) != 3) {
        Rf_error("Argument `img` must be a numeric array with three dimensions");
    }

    const int h = INTEGER(dim)[0];
    const int w = INTEGER(dim)[1];
    const int b = INTEGER(dim)[2];
    if (h == NA_INTEGER || h <= 0 ||
        w == NA_INTEGER || w <= 0 ||
        b == NA_INTEGER || b <= 0) {
        Rf_error("Argument `img` dimensions must be positive integers");
    }

    const double* data = REAL(imgSEXP);
   snic::Img img(data, h, w, b);

    if (compactSEXP == R_NilValue || !Rf_isReal(compactSEXP) ||
        LENGTH(compactSEXP) != 1) {
        Rf_error("Argument `compactness` must be a numeric scalar");
    }

    const double compact = REAL(compactSEXP)[0];
    if (!R_finite(compact) || compact < 0.0) {
        Rf_error("Argument `compactness` must be a non-negative finite number");
    }

    std::vector<int> seeds;
    if (seedsSEXP == R_NilValue || !Rf_isMatrix(seedsSEXP) ||
        !Rf_isInteger(seedsSEXP)) {
        if (!Rf_isInteger(grid_stepSEXP) || LENGTH(grid_stepSEXP) != 1) {
            // no seeds provided, grid_step also not provided
            Rf_error("Either argument `seeds` or `grid_step` must be provided");
        } else {
            // no seeds provided, grid_step must be valid
            const int step = INTEGER(grid_stepSEXP)[0];
            if (step == NA_INTEGER || step <= 0) {
                Rf_error("Argument `grid_step` must be a positive integer");
            }
            if (step > w || step > h) {
                Rf_error("Argument `grid_step` must not exceed the image width or height");
            }
            // no seeds provided, create a new one
            try {
              seeds = snic::grid_seeds(img, step);
            } catch (const std::runtime_error& err) {
              Rf_error("%s", err.what());
            }

            if (seeds.empty()) {
              Rf_error("Grid seeding produced no valid seeds; adjust `grid_step` or check image nodata.");
            }
        }
    } else {
        // seeds provided
        SEXP seedDim = getAttrib(seedsSEXP, R_DimSymbol);
        if (seedDim == R_NilValue) {
            Rf_error("Argument `seeds` must have dimensions");
        }
        const int seed_rows = INTEGER(seedDim)[0];
        const int seed_cols = INTEGER(seedDim)[1];
        if (seed_cols != 2) {
            Rf_error("Argument `seeds` must have two columns (row, column)");
        }
        if (seed_rows <= 0) {
            Rf_error("Argument `seeds` must contain at least one coordinate");
        }
        if (LENGTH(seedsSEXP) != seed_rows * seed_cols) {
            Rf_error("Argument `seeds` length mismatch");
        }

        const int* row_ptr = INTEGER(seedsSEXP);
        const int* col_ptr = row_ptr + seed_rows;
        seeds.reserve(seed_rows);

        for (int i = 0; i < seed_rows; ++i) {
            const int r = row_ptr[i];
            const int c = col_ptr[i];
            if (r == NA_INTEGER || c == NA_INTEGER) {
                Rf_error("Argument `seeds` cannot contain NA coordinates");
            }
            if (r < 1 || r > h || c < 1 || c > w) {
                Rf_error("Argument `seeds` coordinates must lie within image bounds");
            }
            seeds.push_back(img.idx(r - 1, c - 1));
        }
    }

    const int n_valid = img.valid_pixel_count();
    if (n_valid == 0) {
      Rf_error("All pixels contain NA values; SNIC cannot segment.");
    }
    std::vector<int> seg;
    try {
        seg = snic::snic_segment(img, seeds, compact);
    } catch (const std::runtime_error& err) {
        Rf_error("%s", err.what());
    }

    // prepare and fill output segmentation
    const int n = w * h;
    SEXP outSEXP = PROTECT(Rf_allocVector(INTSXP, n));
    int* out_ptr = INTEGER(outSEXP);
    for (int pid = 0; pid < n; ++pid) {
        if (seg[pid] == 0) {
            out_ptr[pid] = NA_INTEGER;
        } else {
            out_ptr[pid] = seg[pid];
        }
    }

    // Update dimensions attribute (height, width, 1)
    SEXP newdim = PROTECT(Rf_allocVector(INTSXP, 3));
    INTEGER(newdim)[0] = h;
    INTEGER(newdim)[1] = w;
    INTEGER(newdim)[2] = 1;
    setAttrib(outSEXP, R_DimSymbol, newdim);

    UNPROTECT(2);
    return outSEXP;
}

/*
 * This is the R interface to the SNIC grid seeding function.
 *
 * @param imgSEXP Numeric array (height x width x bands) representing the image.
 *   Pixels are stored column-wise, with bands stacked. No rearrangement is
 *   needed. A pixel at (r, c) and band b is located at
 *   img[r + c * height + b * (width * height)]. The array must have exactly
 *   three dimensions and strictly positive sizes.
 * @param grid_stepSEXP Integer scalar representing the grid step size; must be
 *   positive and not exceed the image width or height.
 * @return Integer matrix (m x 2) of seed coordinates (row, column).
 *   Coordinates are 1-based (R style).
 */
extern "C" SEXP _seeds_grid(SEXP imgSEXP,
                            SEXP grid_stepSEXP)
{
    if (!Rf_isReal(imgSEXP)) {
        Rf_error("Argument `img` must be a numeric array");
    }
    SEXP dim = getAttrib(imgSEXP, R_DimSymbol);
    if (dim == R_NilValue) {
        Rf_error("Argument `img` must have dimensions");
    }
    if (!Rf_isInteger(dim) || LENGTH(dim) != 3) {
        Rf_error("Argument `img` must be a numeric array with three dimensions");
    }

    const int h = INTEGER(dim)[0];
    const int w = INTEGER(dim)[1];
    const int b = INTEGER(dim)[2];
    if (h == NA_INTEGER || h <= 0 ||
        w == NA_INTEGER || w <= 0 ||
        b == NA_INTEGER || b <= 0) {
        Rf_error("Argument `img` dimensions must be positive integers");
    }

    if (!Rf_isInteger(grid_stepSEXP) || LENGTH(grid_stepSEXP) != 1) {
        Rf_error("Argument `grid_step` must be an integer scalar");
    }

    const int step = INTEGER(grid_stepSEXP)[0];
    if (step == NA_INTEGER || step <= 0) {
        Rf_error("Argument `grid_step` must be a positive integer");
    }
    if (step > w || step > h) {
        Rf_error("Argument `grid_step` must not exceed the image width or height");
    }

    const double* data = REAL(imgSEXP);
    snic::Img img(data, h, w, b);

    const int n_valid = img.valid_pixel_count();
    if (n_valid == 0) {
        Rf_error("All pixels contain NA values; no seeds can be placed.");
    }

    std::vector<int> seeds;
    try {
        seeds = snic::grid_seeds(img, step);
    } catch (const std::runtime_error& err) {
        Rf_error("%s", err.what());
    }

    if (seeds.empty()) {
        Rf_error("Grid seeding produced no valid seeds; adjust `step` or mask.");
    }

    // prepare and fill output segmentation
    const int m = static_cast<int>(seeds.size());
    SEXP out = PROTECT(Rf_allocMatrix(INTSXP, m, 2));
    int* out_ptr = INTEGER(out);
    for (int i = 0; i < m; ++i) {
        const int pid = seeds[i];
        const int r = img.row(pid);
        const int c = img.col(pid);
        // column-major
        out_ptr[i] = r + 1;
        out_ptr[i + m] = c + 1;
    }
    UNPROTECT(1);
    return out;
}

/*
 * Implements a conversion of row-major (terra) to
 * column-major (R) layout.
 *
 * @param imgSEXP A numeric array (2D or 3D)
 * @param hSEXP, wSEXP, bSEXP Integer scalars for dimensions
 * @return The same numeric array (transposed in place)
 */
extern "C" SEXP _colmaj(SEXP imgSEXP,
                        SEXP hSEXP,
                        SEXP wSEXP,
                        SEXP bSEXP)
{
    if (!Rf_isReal(imgSEXP))
        Rf_error("`img` must be a numeric array");

    SEXP dimSEXP = getAttrib(imgSEXP, R_DimSymbol);
    if (dimSEXP == R_NilValue)
        Rf_error("`img` must have 2 or 3 dimensions");

    if (!Rf_isInteger(hSEXP) || !Rf_isInteger(wSEXP) || !Rf_isInteger(bSEXP) ||
        LENGTH(hSEXP) != 1 || LENGTH(wSEXP) != 1 || LENGTH(bSEXP) != 1)
        Rf_error("`height`, `width`, and `bands` must be integer scalars");

    const int h = INTEGER(hSEXP)[0];
    const int w = INTEGER(wSEXP)[0];
    const int b = INTEGER(bSEXP)[0];

    if (h <= 0 || w <= 0 || b <= 0)
        Rf_error("`height`, `width`, and `bands` must be positive");

    const int *dim = INTEGER(dimSEXP);
    const int n_dim = LENGTH(dimSEXP);

    // Verify dimension agreement
    if (n_dim == 2) {
        if (dim[0] != h * w || dim[1] != b) {
            Rf_error("`img` dimensions (%d,%d) do not match (h*w=%d, b=%d)",
                     dim[0], dim[1], h * w, b);
        }
    } else if (n_dim == 3) {
        if (dim[0] != h || dim[1] != w || dim[2] != b) {
            Rf_error("`img` dimensions (%d,%d,%d) do not match (h=%d, w=%d, b=%d)",
                     dim[0], dim[1], dim[2], h, w, b);
        }
    } else {
        Rf_error("`img` must have 2 or 3 dimensions");
    }

    // Transpose data
    const int n = h * w;
    double *data = REAL(imgSEXP);
    double *tmp  = (double *) R_alloc(n, sizeof(double));

    for (int band = 0; band < b; ++band) {
        double *src = data + band * n;
        for (int r = 0; r < h; ++r) {
            for (int c = 0; c < w; ++c) {
                tmp[c * h + r] = src[r * w + c];
            }
        }
        for (int i = 0; i < n; ++i)
            src[i] = tmp[i];
    }

    // Update dimensions in-place
    SEXP newdim = PROTECT(Rf_allocVector(INTSXP, 3));
    INTEGER(newdim)[0] = h;
    INTEGER(newdim)[1] = w;
    INTEGER(newdim)[2] = b;
    setAttrib(imgSEXP, R_DimSymbol, newdim);
    UNPROTECT(1);

    return imgSEXP;
}

/*
 * _dim: Set a new dimension on an existing atomic R object in place.
 *
 * @param imgSEXP Any atomic vector or array (numeric, integer, logical, complex, raw, character)
 * @param newdimSEXP Integer vector specifying new dimensions
 * @return The same object, with updated dimension attribute.
 */
extern "C" SEXP _dim(SEXP imgSEXP, SEXP newdimSEXP)
{
    if (!isVectorAtomic(imgSEXP)) {
        Rf_error("`img` must be an atomic R object");
    }

    if (!Rf_isInteger(newdimSEXP)) {
        Rf_error("`newdim` must be an integer vector");
    }

    const R_xlen_t len_img = XLENGTH(imgSEXP);
    const R_xlen_t len_dim = XLENGTH(newdimSEXP);

    if (len_dim < 1) {
        Rf_error("`newdim` must contain at least one dimension");
    }

    // Validate new dimensions
    const int *dims = INTEGER(newdimSEXP);
    R_xlen_t prod_dim = 1;
    for (R_xlen_t i = 0; i < len_dim; i++) {
        if (dims[i] <= 0) {
            Rf_error("`newdim` contains non-positive value at position %lld", (long long)(i + 1));
        }
        prod_dim *= (R_xlen_t)dims[i];
    }

    if (prod_dim != len_img) {
        Rf_error("Product of `newdim` (%lld) does not match object length (%lld)",
                 (long long)prod_dim, (long long)len_img);
    }

    setAttrib(imgSEXP, R_DimSymbol, newdimSEXP);

    return imgSEXP;
}
