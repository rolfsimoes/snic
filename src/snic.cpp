#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <queue>
#include <stdexcept>
#include <vector>

namespace {

struct Node {
    Node() : dst(0.0), pid(-1), cid(-1) {}
    Node(double d, int p, int c) : dst(d), pid(p), cid(c) {}

    double dst;
    int pid;
    int cid;
};

struct NodeCmp {
    bool operator()(const Node &a, const Node &b) const {
        return a.dst > b.dst;
    }
};

struct Clu {
    std::vector<double> val;
    double r;
    double c;
    int n;
};

struct Img {
    const double *m;
    const int w;
    const int h;
    const int b;
    const int n;

    Img(const double *m_, int w_, int h_, int b_)
        : m(m_), w(w_), h(h_), b(b_), n(w_ * h_) {}
};

inline int idx(int r, int c, int w) {
    return r * w + c;
}

inline void coord(int pid, int w, int &r, int &c) {
    r = pid / w;
    c = pid % w;
}

void px_vals(const Img &img, int pid, std::vector<double> &out) {
    out.resize(img.b);
    for (int bi = 0; bi < img.b; ++bi) {
        out[bi] = img.m[pid + static_cast<long long>(img.n) * bi];
    }
}

bool px_has_na(const Img &img, int pid) {
    for (int bi = 0; bi < img.b; ++bi) {
        double val = img.m[pid + static_cast<long long>(img.n) * bi];
        if (ISNAN(val)) {
            return true;
        }
    }
    return false;
}

double dist_sq_eu(const std::vector<double> &a, const std::vector<double> &b) {
    double sum = 0.0;
    const int len = static_cast<int>(a.size());
    for (int i = 0; i < len; ++i) {
        double diff = a[i] - b[i];
        sum += diff * diff;
    }
    return sum;
}

double dist_sq_clu(const Img &img,
                   int pid,
                   const Clu &clu,
                   std::vector<double> &val,
                   double spat_scale,
                   double compactness) {
    px_vals(img, pid, val);
    double df_sq = dist_sq_eu(val, clu.val);
    int r, c;
    coord(pid, img.w, r, c);
    double dr = static_cast<double>(r) - clu.r;
    double dc = static_cast<double>(c) - clu.c;
    double ratio = 0.0;
    if (spat_scale > 0.0) {
        ratio = compactness / spat_scale;
    }
    return df_sq + ratio * ratio * (dr * dr + dc * dc);
}

void clu_update(Clu &clu, const std::vector<double> &px, int row, int col) {
    clu.n += 1;
    const double inv_n = 1.0 / static_cast<double>(clu.n);
    const double wt = 1.0 - inv_n;
    const std::size_t len = clu.val.size();
    for (std::size_t bi = 0; bi < len; ++bi) {
        clu.val[bi] = clu.val[bi] * wt + px[bi] * inv_n;
    }
    clu.r = clu.r * wt + static_cast<double>(row) * inv_n;
    clu.c = clu.c * wt + static_cast<double>(col) * inv_n;
}

void nbr4(int pid, int w, int h, std::vector<int> &nb) {
    nb.clear();
    int r, c;
    coord(pid, w, r, c);
    if (r > 0) {
        nb.push_back(idx(r - 1, c, w));
    }
    if (r + 1 < h) {
        nb.push_back(idx(r + 1, c, w));
    }
    if (c > 0) {
        nb.push_back(idx(r, c - 1, w));
    }
    if (c + 1 < w) {
        nb.push_back(idx(r, c + 1, w));
    }
}

void nbr8(int pid, int w, int h, std::vector<int> &nb) {
    nb.clear();
    int r, c;
    coord(pid, w, r, c);

    if (r > 0) {
        if (c > 0) {
            nb.push_back(idx(r - 1, c - 1, w));
        }
        nb.push_back(idx(r - 1, c, w));
        if (c + 1 < w) {
            nb.push_back(idx(r - 1, c + 1, w));
        }
    }

    if (c > 0) {
        nb.push_back(idx(r, c - 1, w));
    }
    if (c + 1 < w) {
        nb.push_back(idx(r, c + 1, w));
    }

    if (r + 1 < h) {
        if (c > 0) {
            nb.push_back(idx(r + 1, c - 1, w));
        }
        nb.push_back(idx(r + 1, c, w));
        if (c + 1 < w) {
            nb.push_back(idx(r + 1, c + 1, w));
        }
    }
}

// update the closest distance of each pixel to a given seed
void update_closest_dist_sq(const std::vector<int> &ids,
                           const std::vector<char> &chosen,
                           std::vector<double> &dists_sq,
                           int seed_idx,
                           int w) {
    int sr, sc;
    coord(ids[seed_idx], w, sr, sc);
    const std::size_t total = ids.size();
    for (std::size_t i = 0; i < total; ++i) {
        if (chosen[i]) {
            continue;
        }
        int r, c;
        coord(ids[i], w, r, c);
        double dr = static_cast<double>(r - sr);
        double dc = static_cast<double>(c - sc);
        double dist_sq = dr * dr + dc * dc;
        if (dist_sq < dists_sq[i]) {
            dists_sq[i] = dist_sq;
        }
    }
}

std::vector<int> seed_init(const Img &img, int k_req, const std::vector<unsigned char> &mask) {
    std::vector<int> seeds;
    if (k_req <= 0) {
        return seeds;
    }

    const int w = img.w;
    const int h = img.h;
    const int n = img.n;

    // valid pixel ids
    std::vector<int> ids;
    ids.reserve(n);

    for (int pid = 0; pid < n; ++pid) {
        if (!mask[pid]) {
            continue;
        }
        ids.push_back(pid);
    }

    const int valid = static_cast<int>(ids.size());
    if (valid == 0) {
        return seeds;
    }

    // desired number of seeds is at most the number of valid pixels
    const int target = std::min(k_req, valid);
    seeds.reserve(target);

    const double pi = std::acos(-1.0);

    // average cluster area in pixel unit
    double area = static_cast<double>(valid) / static_cast<double>(k_req);
    if (!R_finite(area) || area <= 0.0) {
        area = 1.0;
    }
    double min_dist = std::sqrt(area / pi);
    if (!R_finite(min_dist) || min_dist < 1.0) {
        min_dist = 1.0;
    }
    const double min_dist_sq = min_dist * min_dist;

    // initialize pixels chosen as seeds to 0 (not chosen)
    std::vector<char> chosen(ids.size(), 0);

    // TODO: center of mass of valid pixels
    const double center_r = (h - 1) / 2.0;
    const double center_c = (w - 1) / 2.0;

    // find the valid pixel closest to the matrix center as the first seed
    int seed_idx = -1;
    double best_center = std::numeric_limits<double>::infinity();
    for (std::size_t i = 0; i < ids.size(); ++i) {
        int r, c;
        coord(ids[i], w, r, c);
        double dr = static_cast<double>(r) - center_r;
        double dc = static_cast<double>(c) - center_c;
        double dist_sq = dr * dr + dc * dc;
        if (dist_sq < best_center) {
            best_center = dist_sq;
            seed_idx = static_cast<int>(i);
        }
    }

    // if no valid pixel is found, return empty seeds
    if (seed_idx == -1) {
        return seeds;
    }

    // initialize distances to closest seed to infinity
    std::vector<double> dists_sq(ids.size(), std::numeric_limits<double>::infinity());

    // add the pixel closest to the matrix center as the first seed
    seeds.push_back(ids[seed_idx]);
    chosen[seed_idx] = 1;

    // update distances of pixels to a given seed if the new distance is closer
    update_closest_dist_sq(ids, chosen, dists_sq, seed_idx, w);

    // add the remaining seeds as the pixel farthest from any seed
    while (static_cast<int>(seeds.size()) < target) {
        seed_idx = -1;
        double farthest = min_dist_sq;
        for (std::size_t i = 0; i < ids.size(); ++i) {
            if (chosen[i]) {
                continue;
            }

            // find the valid pixel farthest from any seed
            if (dists_sq[i] >= farthest) {
                farthest = dists_sq[i];
                seed_idx = static_cast<int>(i);
            }
        }

        if (seed_idx == -1) {
            break;
        }

        // add the chosen pixel as a seed
        seeds.push_back(ids[seed_idx]);
        chosen[seed_idx] = 1;

        // update distances of pixels to a given seed if the new distance is closer
        update_closest_dist_sq(ids, chosen, dists_sq, seed_idx, w);
    }

    return seeds;
}

std::vector<int> snic_cpp(const Img &img,
                          int k_req,
                          int nbr_type,
                          int &k_eff,
                          double compactness) {
    if (img.w <= 0 || img.h <= 0 || k_req <= 0) {
        throw std::runtime_error("`width`, `height`, and `k` must be positive integers");
    }
    if (nbr_type != 4 && nbr_type != 8) {
        throw std::runtime_error("`nbr_type` must be either 4 or 8");
    }
    if (compactness < 0.0) {
        compactness = 0.0;
    }

    const int n = img.n;
    const int w = img.w;
    const int h = img.h;

    // mask pixels with NA values
    std::vector<unsigned char> mask(n, 1);
    for (int pid = 0; pid < n; ++pid) {
        if (px_has_na(img, pid)) {
            mask[pid] = 0;
        }
    }

    // count number of valid pixels
    const int valid = std::accumulate(mask.begin(), mask.end(), 0);
    if (valid == 0) {
        throw std::runtime_error("All pixels contain NA values; SNIC cannot segment.");
    }

    // initialize seeds
    k_eff = std::min(k_req, valid);
    std::vector<int> seeds = seed_init(img, k_eff, mask);

    // k_eff is the number of effective seeds
    k_eff = static_cast<int>(seeds.size());
    if (seeds.empty()) {
        throw std::runtime_error("Unable to place seeds on valid pixels.");
    }

    // initialize clusters
    const int m = static_cast<int>(seeds.size());
    std::vector<Clu> clus;
    clus.reserve(m);
    std::vector<double> val;

    // for each seed, initialize cluster
    for (int ci = 0; ci < m; ++ci) {
        int pid = seeds[ci];
        Clu clu;
        px_vals(img, pid, val);
        clu.val = val;
        int seed_r, seed_c;
        coord(pid, w, seed_r, seed_c);
        clu.r = static_cast<double>(seed_r);
        clu.c = static_cast<double>(seed_c);
        clu.n = 0;
        clus.push_back(clu);
    }

    // initialize segments (output of the algorithm)
    //   -1: unassigned
    std::vector<int> seg(n, -1);

    // initialize min-heap with seed pixels
    std::vector<double> dists_sq(n, std::numeric_limits<double>::infinity());
    std::priority_queue<Node, std::vector<Node>, NodeCmp> pq;
    for (int ci = 0; ci < m; ++ci) {
        int pid = seeds[ci];
        dists_sq[pid] = 0.0;
        pq.push(Node(0.0, pid, ci));
    }

    // initialize neighbors
    std::vector<int> nb;

    // main SNIC loop
    while (!pq.empty()) {
        Node node = pq.top();
        pq.pop();

        int pid = node.pid; // pixel id
        int cid = node.cid; // cluster id

        // skip if pixel is already assigned
        if (seg[pid] != -1) {
            continue;
        }
        // skip if pixel is NA
        if (!mask[pid]) {
            continue;
        }

        // update cluster values and assign pixel to cluster
        int r, c;
        coord(pid, w, r, c);
        px_vals(img, pid, val);
        clu_update(clus[cid], val, r, c);
        seg[pid] = cid;

        // find neighbors
        if (nbr_type == 8) {
            nbr8(pid, w, h, nb);
        } else {
            nbr4(pid, w, h, nb);
        }

        // calculate spatial scale and compactness
    const double spat_scale = std::sqrt(static_cast<double>(valid) / static_cast<double>(k_eff));

        // update min-heap with neighbors
        for (std::size_t ni = 0; ni < nb.size(); ++ni) {
            int nid = nb[ni];

            // skip if neighbor pixel is NA
            if (!mask[nid]) {
                continue;
            }

            // skip if neighbor pixel is already assigned
            if (seg[nid] != -1) {
                continue;
            }

            // calculate distance to cluster
            double dist_sq = dist_sq_clu(img, nid, clus[cid], val, spat_scale, compactness);
            if (dist_sq < dists_sq[nid]) {
                dists_sq[nid] = dist_sq;
                pq.push(Node(dist_sq, nid, cid));
            }
        }
    }

    return seg;
}

} // namespace

extern "C" SEXP snic(SEXP imgSEXP,
                       SEXP wSEXP,
                       SEXP hSEXP,
                       SEXP kSEXP,
                       SEXP nbrtypeSEXP,
                       SEXP compactSEXP) {
    if (!Rf_isMatrix(imgSEXP) || !Rf_isReal(imgSEXP)) {
        Rf_error("`img` must be a numeric matrix");
    }

    int w = INTEGER(wSEXP)[0];
    int h = INTEGER(hSEXP)[0];
    int k_req = INTEGER(kSEXP)[0];
    int nbr_type = INTEGER(nbrtypeSEXP)[0];

    SEXP dim = getAttrib(imgSEXP, R_DimSymbol);
    if (dim == R_NilValue) {
        Rf_error("`img` must have dimensions");
    }
    int n = INTEGER(dim)[0];
    int b = INTEGER(dim)[1];
    if (n != w * h) {
        Rf_error("nrow(`img`) must equal `width` * `height`");
    }

    const double *data = REAL(imgSEXP);
    Img img(data, w, h, b);

    double compact = 10.0;
    if (compactSEXP != R_NilValue) {
        compact = Rf_asReal(compactSEXP);
        if (!R_finite(compact) || compact < 0.0) {
            Rf_error("`compactness` must be a non-negative finite number");
        }
    }

    int k_eff = k_req;
    std::vector<int> seg;
    try {
        seg = snic_cpp(img, k_req, nbr_type, k_eff, compact);
    } catch (const std::runtime_error &err) {
        Rf_error("%s", err.what());
    }

    if (k_eff < k_req) {
        Rf_warning("`k` reduced to %d to match number of valid pixels", k_eff);
    }

    SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
    int *out_ptr = INTEGER(out);
    for (int pid = 0; pid < n; ++pid) {
        if (seg[pid] < 0) {
            out_ptr[pid] = NA_INTEGER;
        } else {
            out_ptr[pid] = seg[pid] + 1;
        }
    }

    UNPROTECT(1);
    return out;
}
