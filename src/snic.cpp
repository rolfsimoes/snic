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

    Img(const double* data, int width, int height, int bands);

    int idx(int r, int c) const;
    int row(int pid) const;
    int col(int pid) const;
    void vals(int pid, std::vector<double>& out) const;
    void neighbors4(int pid, std::vector<int>& out) const;
    bool has_nan(int pid) const;
};

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

namespace detail {

double dist_sq_eu_buf(const double* a, const double* b, int len)
{
    double sum = 0.0;
    for (int i = 0; i < len; ++i) {
        const double d = a[i] - b[i];
        sum += d * d;
    }
    return sum;
}

} // namespace detail

// =====================
// Img implementation
// =====================

Img::Img(const double* data_, int width, int height, int bands)
    : data(data_), w(width), h(height), b(bands), n(width * height)
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
}

int Img::idx(int r, int c) const
{
    return r * w + c;
}

int Img::row(int pid) const
{
    return pid / w;
}

int Img::col(int pid) const
{
    return pid % w;
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
    const double dval_sq = detail::dist_sq_eu_buf(px_val.data(), val.data(), k);

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

int build_valid_mask(const Img& img, std::vector<std::uint8_t>& mask)
{
    mask.assign(static_cast<std::size_t>(img.n), static_cast<std::uint8_t>(1));

    int valid = 0;
    for (int pid = 0; pid < img.n; ++pid) {
        const bool ok = !img.has_nan(pid);
        mask[pid] = ok ? static_cast<std::uint8_t>(1) : static_cast<std::uint8_t>(0);
        if (ok) {
            ++valid;
        }
    }
    return valid;
}

std::vector<int> grid_seeds(const Img& img,
                            int grid_step,
                            const std::vector<std::uint8_t>& mask)
{
    std::vector<int> out;
    if (grid_step <= 0) {
        return out;
    }
    if (mask.size() != static_cast<std::size_t>(img.n)) {
        throw std::runtime_error("Mask size mismatch for grid seeding.");
    }

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

// Returns segmentation vector (size img.n). Pixels masked out remain 0.
std::vector<int> snic_segment(const Img& img,
                              const std::vector<int>& seeds,
                              const std::vector<std::uint8_t>& mask,
                              double compactness)
{
    if (img.n <= 0) {
        throw std::runtime_error("Image must contain at least one pixel.");
    }
    if (mask.size() != static_cast<std::size_t>(img.n)) {
        throw std::runtime_error("Mask size must match the number of pixels in the image.");
    }
    if (seeds.empty()) {
        throw std::runtime_error("No seeds provided for SNIC segmentation.");
    }

    if (compactness < 0.0) {
        compactness = 0.0;
    }

    const int valid = std::accumulate(mask.begin(), mask.end(), 0);
    if (valid == 0) {
        throw std::runtime_error("All pixels contain NA values; SNIC cannot segment.");
    }

    for (int pid : seeds) {
        if (pid < 0 || pid >= img.n) {
            throw std::runtime_error("Seed index out of bounds for provided image dimensions.");
        }
        if (!mask[pid]) {
            throw std::runtime_error("Seed placed on pixel containing NA values.");
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

extern "C" SEXP _snic(SEXP imgSEXP,
                      SEXP wSEXP,
                      SEXP hSEXP,
                      SEXP seedsSEXP,
                      SEXP compactSEXP)
{
    if (!Rf_isMatrix(imgSEXP) || !Rf_isReal(imgSEXP)) {
        Rf_error("`img` must be a numeric matrix");
    }
    if (wSEXP == R_NilValue || hSEXP == R_NilValue) {
        Rf_error("`w` and `h` must be provided");
    }
    if (!Rf_isInteger(wSEXP) || LENGTH(wSEXP) < 1 ||
        !Rf_isInteger(hSEXP) || LENGTH(hSEXP) < 1) {
        Rf_error("`w` and `h` must be integer");
    }
    const int w = INTEGER(wSEXP)[0];
    const int h = INTEGER(hSEXP)[0];

    SEXP dim = getAttrib(imgSEXP, R_DimSymbol);
    if (dim == R_NilValue) {
        Rf_error("`img` must have dimensions");
    }
    if (LENGTH(dim) < 2) {
        Rf_error("`img` must be a matrix with two dimensions");
    }
    const int n = INTEGER(dim)[0];
    const int b = INTEGER(dim)[1]; // bands
    if (n != w * h) {
        Rf_error("nrow(`img`) must equal `width` * `height`");
    }

    const double* data = REAL(imgSEXP);
    snic::Img img(data, w, h, b);

    if (compactSEXP == R_NilValue) {
      Rf_error("`compactness` must be provided");
    }
    if (!Rf_isReal(compactSEXP) || LENGTH(compactSEXP) < 1) {
      Rf_error("`compactness` must be a numeric");
    }

    const double compact = REAL(compactSEXP)[0];
    if (!R_finite(compact) || compact < 0.0) {
      Rf_error("`compactness` must be a non-negative finite number");
    }

    if (seedsSEXP == R_NilValue || !Rf_isMatrix(seedsSEXP) || !Rf_isInteger(seedsSEXP)) {
        Rf_error("`seeds` must be an integer matrix with two columns");
    }
    SEXP seedDim = getAttrib(seedsSEXP, R_DimSymbol);
    if (seedDim == R_NilValue) {
        Rf_error("`seeds` must have dimensions");
    }
    const int seed_rows = INTEGER(seedDim)[0];
    const int seed_cols = INTEGER(seedDim)[1];
    if (seed_cols != 2) {
        Rf_error("`seeds` must have two columns (row, column)");
    }
    if (seed_rows <= 0) {
        Rf_error("`seeds` must contain at least one coordinate");
    }
    if (LENGTH(seedsSEXP) != seed_rows * seed_cols) {
        Rf_error("`seeds` length mismatch");
    }

    const int* row_ptr = INTEGER(seedsSEXP);
    const int* col_ptr = row_ptr + seed_rows;
    std::vector<int> seeds;
    seeds.reserve(seed_rows);

    for (int i = 0; i < seed_rows; ++i) {
        const int r = row_ptr[i];
        const int c = col_ptr[i];
        if (r == NA_INTEGER || c == NA_INTEGER) {
          Rf_error("`seeds` cannot contain NA coordinates");
        }
        if (r < 1 || r > h || c < 1 || c > w) {
            Rf_error("`seeds` coordinates must lie within the image dimensions");
        }
        seeds.push_back(img.idx(r - 1, c - 1));
    }

    std::vector<std::uint8_t> mask;
    const int n_valid = snic::build_valid_mask(img, mask);
    if (n_valid == 0) {
      Rf_error("All pixels contain NA values; SNIC cannot segment.");
    }
    std::vector<int> seg;
    try {
        seg = snic::snic_segment(img, seeds, mask, compact);
    } catch (const std::runtime_error& err) {
        Rf_error("%s", err.what());
    }

    SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
    int* out_ptr = INTEGER(out);
    for (int pid = 0; pid < n; ++pid) {
        if (seg[pid] == 0) {
            out_ptr[pid] = NA_INTEGER;
        } else {
            out_ptr[pid] = seg[pid];
        }
    }
    UNPROTECT(1);
    return out;
}

extern "C" SEXP _seed_grid(SEXP imgSEXP,
                           SEXP wSEXP,
                           SEXP hSEXP,
                           SEXP stepSEXP)
{
    if (!Rf_isMatrix(imgSEXP) || !Rf_isReal(imgSEXP)) {
        Rf_error("`img` must be a numeric matrix");
    }
    if (!Rf_isInteger(wSEXP) || LENGTH(wSEXP) < 1 ||
        !Rf_isInteger(hSEXP) || LENGTH(hSEXP) < 1) {
        Rf_error("`width` and `height` must be integer");
    }
    const int w = INTEGER(wSEXP)[0];
    const int h = INTEGER(hSEXP)[0];

    if (!Rf_isInteger(stepSEXP) || LENGTH(stepSEXP) < 1) {
        Rf_error("`step` must be an integer");
    }

    const int step = INTEGER(stepSEXP)[0];
    if (step <= 0) {
        Rf_error("`step` must be a positive integer");
    }

    SEXP dim = getAttrib(imgSEXP, R_DimSymbol);
    if (dim == R_NilValue) {
        Rf_error("`img` must have dimensions");
    }
    if (LENGTH(dim) < 2) {
        Rf_error("`img` must be a matrix with two dimensions");
    }
    const int n = INTEGER(dim)[0];
    const int b = INTEGER(dim)[1]; // bands
    if (n != w * h) {
        Rf_error("nrow(`img`) must equal `width` * `height`");
    }

    const double* data = REAL(imgSEXP);
    snic::Img img(data, w, h, b);

    std::vector<std::uint8_t> mask;
    const int n_valid = snic::build_valid_mask(img, mask);
    if (n_valid == 0) {
        Rf_error("All pixels contain NA values; no seeds can be placed.");
    }

    std::vector<int> seeds;
    try {
        seeds = snic::grid_seeds(img, step, mask);
    } catch (const std::runtime_error& err) {
        Rf_error("%s", err.what());
    }

    if (seeds.empty()) {
        Rf_error("Grid seeding produced no valid seeds; adjust `step` or mask.");
    }

    const int m = static_cast<int>(seeds.size());
    SEXP out = PROTECT(Rf_allocMatrix(INTSXP, m, 2));
    int* out_ptr = INTEGER(out);
    for (int i = 0; i < m; ++i) {
        const int pid = seeds[i];
        const int r = img.row(pid);
        const int c = img.col(pid);
        out_ptr[i] = r + 1;
        out_ptr[i + m] = c + 1;
    }
    UNPROTECT(1);
    return out;
}
