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

#include "snic.h"

namespace snic {

template <typename T>
struct RNaTraits {
    static bool is_na(T)
    {
        return false;
    }
};

template <>
struct RNaTraits<double> {
    static bool is_na(double value)
    {
        return ISNAN(value);
    }
};

template <>
struct RNaTraits<const double> {
    static bool is_na(const double value)
    {
        return ISNAN(value);
    }
};

template <>
struct RNaTraits<int> {
    static bool is_na(int value)
    {
        return value == NA_INTEGER;
    }
};

template <>
struct RNaTraits<const int> {
    static bool is_na(const int value)
    {
        return value == NA_INTEGER;
    }
};

// =====================
// Img implementation
// =====================

/*
 * Construct an Img object.
 * @param data_ Pointer to image data in band-interleaved format.
 *   Pixels are stored either row-wise (order = 'C') or column-wise
 *   (order = 'F'), with bands stacked. A pixel at (r, c) and band b is
 *   located at:
 *     - row-major: data_[r * width + c + b * (width * height)]
 *     - column-major: data_[r + c * height + b * (width * height)]
 *   The buffer typically comes from C++ (row-major) or R (column-major)
 *   arrays with dimensions (height x width x bands).
 * @param height Image height (number of rows)
 * @param width Image width (number of columns)
 * @param bands Number of bands (channels) in the image
 * @param order Layout flag: 'C' for row-major (C-style), 'F' for
 *   column-major (Fortran/R style).
 */
template <typename T>
Img<T>::Img(T* data_, int height, int width, int bands, char order_)
    : data(data_), w(width), h(height), b(bands), order(order_),
      n_valid_(-1)
{
    if (!data) {
        throw std::invalid_argument("Image data pointer must not be null");
    }
    if (w <= 0 || h <= 0) {
        throw std::invalid_argument("Image width and height must be positive");
    }
    if (b <= 0) {
        throw std::invalid_argument("Number of bands must be positive");
    }
    if (order != 'C' && order != 'F') {
        throw std::invalid_argument("Image order must be 'C' (row-major) or 'F' (column-major)");
    }

    const long long n_ = static_cast<long long>(h) *
      static_cast<long long>(w);
    if (n_ > std::numeric_limits<int>::max()) Rf_error("Image too large");
    n = static_cast<int>(n_);

    // Compute mask
    mask_.resize(static_cast<std::size_t>(n));
    int valid = 0;
    for (int pid = 0; pid < n; ++pid) {
        const bool has_na = has_nan(pid);
        mask_[pid] = has_na ? 0u : 1u;
        if (!has_na) ++valid;
    }
    n_valid_ = valid;
}

template <typename T>
int Img<T>::idx(int r, int c) const
{
    if (order == 'C') {
        return c + r * w;
    }
    return r + c * h;
}

template <typename T>
int Img<T>::row(int pid) const
{
    if (order == 'C') {
        return pid / w;
    }
    return pid % h;
}

template <typename T>
int Img<T>::col(int pid) const
{
    if (order == 'C') {
        return pid % w;
    }
    return pid / h;
}

template <typename T>
void Img<T>::vals(int pid, std::vector<double>& out) const
{
    out.resize(b);
    const std::size_t base = static_cast<std::size_t>(pid);
    const std::size_t stride = static_cast<std::size_t>(n);
    for (int bi = 0; bi < b; ++bi) {
        out[bi] = static_cast<double>(
            data[base + stride * static_cast<std::size_t>(bi)]
        );
    }
}

template <typename T>
void Img<T>::neighbors4(int pid, std::vector<int>& out) const
{
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

template <typename T>
bool Img<T>::has_nan(int pid) const
{
    const std::size_t base = static_cast<std::size_t>(pid);
    const std::size_t stride = static_cast<std::size_t>(n);
    for (int bi = 0; bi < b; ++bi) {
        const auto value = data[base + stride * static_cast<std::size_t>(bi)];
        if (RNaTraits<T>::is_na(value)) {
            return true;
        }
    }
    return false;
}

template <typename T>
int Img<T>::valid_pixel_count() const
{
    return n_valid_;
}

template <typename T>
const std::vector<std::uint8_t>& Img<T>::mask() const
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

void Clu::update_with_vals(const std::vector<double>& px_val,
                           int row, int col)
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
    double dval_sq = 0.0;
    for (int i = 0; i < k; ++i) {
        const double diff = px_val[i] - val[i];
        dval_sq += diff * diff;
    }

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

Node::Node(double d_, int pid_, int cid_)
    : d(d_), pid(pid_), cid(cid_)
{
}

bool NodeGreater::operator()(const Node& a, const Node& b) const
{
    return a.d > b.d;
}

// =====================
// Free functions
// =====================

/*
 * SNIC segmentation function
 * Fills 'out' with segmentation labels (size img.n). Pixels masked out remain 0.
 * Segments are labeled from 1 to number of seeds.
 */
template <typename T>
void snic_run(const Img<T>& img,
              std::vector<int>& seed_rows,
              std::vector<int>& seed_cols,
              double compactness,
              std::vector<int>& out)
{
    if (img.n <= 0) {
        throw std::runtime_error("Image must contain at least one pixel");
    }

    const std::vector<std::uint8_t>& mask = img.mask();
    if (mask.size() != static_cast<std::size_t>(img.n)) {
      throw std::runtime_error("Mask size mismatch for image size");
    }
    const int valid = img.valid_pixel_count();
    if (valid == 0) {
      throw std::runtime_error("All pixels contain NA values; SNIC cannot segment");
    }

    if (seed_rows.empty()) {
      throw std::runtime_error("No seeds provided for SNIC segmentation");
    }

    // prepare SNIC loop

    // initilize clusters
    std::vector<Clu> clus;
    clus.reserve(seed_rows.size());
    std::vector<double> px_val;
    px_val.reserve(static_cast<std::size_t>(img.b));

    // min-heap
    std::priority_queue<Node, std::vector<Node>, NodeGreater> pq;

    for (int i = 0; i < static_cast<int>(seed_rows.size()); i++) {
        const int r = seed_rows[i];
        const int c = seed_cols[i];
        const int pid = img.idx(r, c);
        if (pid < 0 || pid >= img.n) {
          throw std::runtime_error("Seed index out of bounds for provided image dimensions");
        }
        if (!mask[pid]) {
          continue;
        }
        // get pixel values at seed's pixel
        img.vals(pid, px_val);
        // populate cluster
        const int cid = static_cast<int>(clus.size());
        clus.emplace_back(r, c, px_val);
        // push to min-heap: distance, pixel, cluster
        pq.emplace(0.0, pid, cid);
    }

    if (clus.empty()) {
      throw std::runtime_error("All seeds fall on pixels containing NA values; SNIC cannot segment");
    }

    // alloc output
    out.assign(static_cast<std::size_t>(img.n), 0);

    // spat-scale parameter
    const double spat_scale = std::sqrt(
        static_cast<double>(valid) / static_cast<double>(clus.size())
    );

    // neighbors vector
    std::vector<int> neighbors;

    // start SNIC loop
    while (!pq.empty()) {
        const Node node = pq.top();
        pq.pop();

        const int pid = node.pid;
        const int cid = node.cid;

        if (pid < 0 || pid >= img.n) {
            continue;
        }
        if (out[pid] != 0) {
            continue;
        }
        if (!mask[pid]) {
            continue;
        }

        img.vals(pid, px_val);

        const int r = img.row(pid);
        const int c = img.col(pid);
        clus[cid].update_with_vals(px_val, r, c);
        out[pid] = cid + 1;

        neighbors.clear();
        neighbors.reserve(4);
        img.neighbors4(pid, neighbors);
        for (int nid : neighbors) {
            if (nid < 0 || nid >= img.n) continue;
            if (!mask[nid]) continue;
            if (out[nid] != 0) continue;

            img.vals(nid, px_val);
            const double d2 = clus[cid].dist_sq_to_vals(
                px_val, img.row(nid), img.col(nid),
                spat_scale, compactness
            );

            pq.emplace(d2, nid, cid);
        }
    }
    if (static_cast<size_t>(out.size()) !=
        static_cast<size_t>(img.w) * static_cast<size_t>(img.h)) {
        Rf_error("Internal error: segmentation length mismatch");
    }
}

} // namespace snic

/*
 * This is the R interface to the SNIC segmentation function.
 *
 * @param imgSEXP Numeric array (height x width x bands) representing the image.
 *   Pixels are stored row-wise, with bands stacked. A pixel at (r, c) and
 *   band b is located at img[c + r * width + b * (width * height)].
 *   The array must have exactly three dimensions and strictly positive sizes.
 * @param seedsSEXP Integer matrix (m x 2) of seed coordinates (row, column).
 *   Coordinates are 1-based (R style). Values must be within image bounds.
 * @param compactSEXP Single numeric value (scalar) representing the
 *   compactness parameter; must be finite and non-negative.
 * @return Integer vector (length width * height) with same ordering
 *  as the input and defined by orderSEXP parameter. Pixels masked out
 *  remain NA. Segments are labeled from 1 to number of seeds.
 */
extern "C" SEXP _snic(SEXP imgSEXP,
                      SEXP seedsSEXP,
                      SEXP compactSEXP,
                      SEXP orderSEXP)
{
    if (!Rf_isReal(imgSEXP)) {
        Rf_error("argument 'img' must be a numeric array");
    }
    SEXP dim = getAttrib(imgSEXP, R_DimSymbol);
    if (dim == R_NilValue) {
        Rf_error("argument 'img' must have dimensions");
    }
    if (!Rf_isInteger(dim) || LENGTH(dim) != 3) {
        Rf_error("argument 'img' must be a numeric array with three dimensions");
    }

    const int h = INTEGER(dim)[0];
    const int w = INTEGER(dim)[1];
    const int b = INTEGER(dim)[2];
    if (h == NA_INTEGER || h <= 0 ||
        w == NA_INTEGER || w <= 0 ||
        b == NA_INTEGER || b <= 0) {
        Rf_error("argument 'img' dimensions must be positive integers");
    }

    if (!Rf_isReal(compactSEXP) || LENGTH(compactSEXP) != 1) {
        Rf_error("argument 'compactness' must be a numeric scalar");
    }
    const double compact = REAL(compactSEXP)[0];
    if (!R_finite(compact) || compact < 0.0) {
        Rf_error("argument 'compactness' must be a non-negative finite number");
    }

    if (!Rf_isString(orderSEXP) || LENGTH(orderSEXP) != 1) {
        Rf_error("argument 'order' must be a single character string");
    }
    const char order = CHAR(STRING_ELT(orderSEXP, 0))[0];
    if (order != 'C' && order != 'F') {
        Rf_error("argument 'order' must be either 'C' (row-major) or 'F' (column-major)");
    }

    const double* data = REAL(imgSEXP);
    snic::Img<const double> img(data, h, w, b, order);

    const int n_valid = img.valid_pixel_count();
    if (n_valid == 0) {
      Rf_error("All pixels contain NA values; SNIC cannot segment.");
    }

    if (seedsSEXP == R_NilValue) {
        Rf_error("argument 'seeds' must be provided");
    }
    if (!Rf_isMatrix(seedsSEXP) || !Rf_isInteger(seedsSEXP)) {
        Rf_error("argument 'seeds' must be an integer matrix");
    }

    SEXP seedDim = getAttrib(seedsSEXP, R_DimSymbol);
    if (seedDim == R_NilValue) {
        Rf_error("argument 'seeds' must have dimensions");
    }

    const int n_seeds = INTEGER(seedDim)[0];
    const int n_dim_cols = INTEGER(seedDim)[1];
    if (n_dim_cols != 2) {
        Rf_error("argument 'seeds' must have two columns (row, column)");
    }
    if (n_seeds <= 0) {
        Rf_error("argument 'seeds' must contain at least one coordinate");
    }
    if (LENGTH(seedsSEXP) != n_seeds * n_dim_cols) {
        Rf_error("argument 'seeds' length mismatch");
    }

    std::vector<int> seed_rows;
    std::vector<int> seed_cols;
    seed_rows.reserve(n_seeds);
    seed_cols.reserve(n_seeds);

    const int* row_ptr = INTEGER(seedsSEXP);
    const int* col_ptr = row_ptr + n_seeds;

    for (int i = 0; i < n_seeds; ++i) {
        const int r = row_ptr[i];
        const int c = col_ptr[i];
        if (r == NA_INTEGER || c == NA_INTEGER) {
            Rf_error("argument 'seeds' cannot contain NA coordinates");
        }
        if (r < 1 || r > h || c < 1 || c > w) {
            Rf_error("argument 'seeds' coordinates must lie within image bounds");
        }
        // transform to 0-based row and col
        seed_rows.push_back(r - 1);
        seed_cols.push_back(c - 1);
    }

    std::vector<int> seg;
    try {
        snic::snic_run(img, seed_rows, seed_cols, compact, seg);
    } catch (const std::runtime_error& err) {
        Rf_error("%s", err.what());
    }

    // prepare and fill output segmentation
    const R_xlen_t n_ = static_cast<R_xlen_t>(w) * static_cast<R_xlen_t>(h);
    if (n_ > INT_MAX) Rf_error("Image too large");
    const int n = static_cast<int>(n_);

    SEXP outSEXP = PROTECT(Rf_allocVector(INTSXP, n));
    int* out_ptr = INTEGER(outSEXP);
    for (int pid = 0; pid < n; ++pid) {
        if (seg[pid] == 0) {
            out_ptr[pid] = NA_INTEGER;
        } else {
            out_ptr[pid] = seg[pid];
        }
    }

    UNPROTECT(1);
    return outSEXP;
}

// Explicit template instantiations for commonly used pixel types.
/*
 * Set a new dimension on an existing atomic R object in place.
 *
 * @param imgSEXP Any atomic vector or array (numeric, integer, logical, complex, raw, character)
 * @param newdimSEXP Integer vector specifying new dimensions
 * @return The same object, with updated dimension attribute.
 */
extern "C" SEXP _set_dim(SEXP imgSEXP, SEXP newdimSEXP)
{
    if (!isVectorAtomic(imgSEXP)) {
        Rf_error("'img' must be an atomic R object");
    }

    if (!Rf_isInteger(newdimSEXP)) {
        Rf_error("'newdim' must be an integer vector");
    }

    const R_xlen_t len_img = XLENGTH(imgSEXP);
    const R_xlen_t len_dim = XLENGTH(newdimSEXP);

    if (len_dim < 1) {
        Rf_error("'newdim' must contain at least one dimension");
    }

    // Validate new dimensions
    const int *dims = INTEGER(newdimSEXP);
    R_xlen_t prod_dim = 1;
    for (R_xlen_t i = 0; i < len_dim; i++) {
        if (dims[i] <= 0) {
            Rf_error("'newdim' contains non-positive value at position %lld", (long long)(i + 1));
        }
        prod_dim *= (R_xlen_t)dims[i];
    }

    if (prod_dim != len_img) {
        Rf_error("Product of 'newdim' (%lld) does not match object length (%lld)",
                 (long long)prod_dim, (long long)len_img);
    }

    setAttrib(imgSEXP, R_DimSymbol, newdimSEXP);

    return imgSEXP;
}
