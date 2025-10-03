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
    std::vector<double> ctr;
    int cnt;
};

inline int idx(int r, int c, int w) {
    return r * w + c;
}

inline void coord(int pid, int w, int &r, int &c) {
    r = pid / w;
    c = pid % w;
}

void px_vals(const double *img, int n, int b, int pid, std::vector<double> &out) {
    out.resize(b);
    for (int bi = 0; bi < b; ++bi) {
        out[bi] = img[pid + static_cast<long long>(n) * bi];
    }
}

bool px_has_na(const double *img, int n, int b, int pid) {
    for (int bi = 0; bi < b; ++bi) {
        double val = img[pid + static_cast<long long>(n) * bi];
        if (ISNAN(val)) {
            return true;
        }
    }
    return false;
}

double dist_eu(const std::vector<double> &a, const std::vector<double> &b) {
    double sum = 0.0;
    const int len = static_cast<int>(a.size());
    for (int i = 0; i < len; ++i) {
        double diff = a[i] - b[i];
        sum += diff * diff;
    }
    return std::sqrt(sum);
}

double dist_clu(const double *img, int n, int b, int pid, const Clu &clu,
                std::vector<double> &buf) {
    px_vals(img, n, b, pid, buf);
    return dist_eu(buf, clu.ctr);
}

void clu_update(Clu &clu, const std::vector<double> &px) {
    clu.cnt += 1;
    const double inv_n = 1.0 / static_cast<double>(clu.cnt);
    const double wt = 1.0 - inv_n;
    const std::size_t len = clu.ctr.size();
    for (std::size_t bi = 0; bi < len; ++bi) {
        clu.ctr[bi] = clu.ctr[bi] * wt + px[bi] * inv_n;
    }
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

void update_best_dist(const std::vector<int> &rows,
                      const std::vector<int> &cols,
                      const std::vector<char> &chosen,
                      std::vector<double> &best,
                      int seed_idx) {
    const int sr = rows[seed_idx];
    const int sc = cols[seed_idx];
    const std::size_t total = rows.size();
    for (std::size_t i = 0; i < total; ++i) {
        if (chosen[i]) {
            continue;
        }
        double dr = static_cast<double>(rows[i] - sr);
        double dc = static_cast<double>(cols[i] - sc);
        double dist_sq = dr * dr + dc * dc;
        if (dist_sq < best[i]) {
            best[i] = dist_sq;
        }
    }
}

std::vector<int> seed_init(int w, int h, int k_req, const std::vector<unsigned char> &mask) {
    std::vector<int> seeds;
    if (k_req <= 0) {
        return seeds;
    }

    const int n = w * h;
    std::vector<int> ids;
    ids.reserve(n);
    std::vector<int> rows;
    rows.reserve(n);
    std::vector<int> cols;
    cols.reserve(n);

    for (int pid = 0; pid < n; ++pid) {
        if (!mask[pid]) {
            continue;
        }
        ids.push_back(pid);
        int r, c;
        coord(pid, w, r, c);
        rows.push_back(r);
        cols.push_back(c);
    }

    const int valid = static_cast<int>(ids.size());
    if (valid == 0) {
        return seeds;
    }

    const int target = std::min(k_req, valid);
    seeds.reserve(target);

    const double pi = std::acos(-1.0);
    double area = static_cast<double>(valid) / static_cast<double>(k_req);
    if (!R_finite(area) || area <= 0.0) {
        area = 1.0;
    }
    double min_dist = std::sqrt(area / pi);
    if (!R_finite(min_dist) || min_dist < 1.0) {
        min_dist = 1.0;
    }
    const double min_dist_sq = min_dist * min_dist;

    std::vector<double> best(ids.size(), std::numeric_limits<double>::infinity());
    std::vector<char> chosen(ids.size(), 0);

    const double center_r = (h - 1) / 2.0;
    const double center_c = (w - 1) / 2.0;

    int first_idx = -1;
    double best_center = std::numeric_limits<double>::infinity();
    for (std::size_t i = 0; i < ids.size(); ++i) {
        double dr = static_cast<double>(rows[i]) - center_r;
        double dc = static_cast<double>(cols[i]) - center_c;
        double dist_sq = dr * dr + dc * dc;
        if (dist_sq < best_center) {
            best_center = dist_sq;
            first_idx = static_cast<int>(i);
        }
    }

    if (first_idx == -1) {
        return seeds;
    }

    seeds.push_back(ids[first_idx]);
    chosen[first_idx] = 1;
    update_best_dist(rows, cols, chosen, best, first_idx);

    while (static_cast<int>(seeds.size()) < target) {
        int next_idx = -1;
        double farthest = min_dist_sq;
        for (std::size_t i = 0; i < ids.size(); ++i) {
            if (chosen[i]) {
                continue;
            }
            if (best[i] >= farthest) {
                farthest = best[i];
                next_idx = static_cast<int>(i);
            }
        }

        if (next_idx == -1) {
            break;
        }

        seeds.push_back(ids[next_idx]);
        chosen[next_idx] = 1;
        update_best_dist(rows, cols, chosen, best, next_idx);
    }

    return seeds;
}

std::vector<int> snic_cpp(const double *img, int w, int h, int b, int k_req, int nbr_type,
                          int &k_eff) {
    if (w <= 0 || h <= 0 || k_req <= 0) {
        throw std::runtime_error("`width`, `height`, and `k` must be positive integers");
    }
    if (nbr_type != 4 && nbr_type != 8) {
        throw std::runtime_error("`nbr_type` must be either 4 or 8");
    }

    const int n = w * h;
    std::vector<unsigned char> mask(n, 1);
    for (int pid = 0; pid < n; ++pid) {
        if (px_has_na(img, n, b, pid)) {
            mask[pid] = 0;
        }
    }

    const int valid = std::accumulate(mask.begin(), mask.end(), 0);
    if (valid == 0) {
        throw std::runtime_error("All pixels contain NA values; SNIC cannot segment.");
    }

    k_eff = std::min(k_req, valid);
    std::vector<int> seeds = seed_init(w, h, k_eff, mask);
    k_eff = static_cast<int>(seeds.size());
    if (seeds.empty()) {
        throw std::runtime_error("Unable to place seeds on valid pixels.");
    }

    const int m = static_cast<int>(seeds.size());
    std::vector<Clu> clus;
    clus.reserve(m);
    std::vector<double> buf;

    for (int ci = 0; ci < m; ++ci) {
        int pid = seeds[ci];
        Clu clu;
        px_vals(img, n, b, pid, buf);
        clu.ctr = buf;
        clu.cnt = 0;
        clus.push_back(clu);
    }

    std::vector<int> seg(n, -1);
    for (int pid = 0; pid < n; ++pid) {
        if (!mask[pid]) {
            seg[pid] = -2;
        }
    }

    std::vector<double> best(n, std::numeric_limits<double>::infinity());
    std::priority_queue<Node, std::vector<Node>, NodeCmp> pq;
    for (int ci = 0; ci < m; ++ci) {
        int pid = seeds[ci];
        best[pid] = 0.0;
        pq.push(Node(0.0, pid, ci));
    }

    std::vector<int> nb;

    while (!pq.empty()) {
        Node node = pq.top();
        pq.pop();

        int pid = node.pid;
        int cid = node.cid;

        if (seg[pid] != -1) {
            continue;
        }
        if (!mask[pid]) {
            continue;
        }

        px_vals(img, n, b, pid, buf);
        clu_update(clus[cid], buf);
        seg[pid] = cid;

        if (nbr_type == 8) {
            nbr8(pid, w, h, nb);
        } else {
            nbr4(pid, w, h, nb);
        }

        for (std::size_t ni = 0; ni < nb.size(); ++ni) {
            int nid = nb[ni];
            if (!mask[nid]) {
                continue;
            }
            if (seg[nid] != -1) {
                continue;
            }
            double dst = dist_clu(img, n, b, nid, clus[cid], buf);
            if (dst < best[nid]) {
                best[nid] = dst;
                pq.push(Node(dst, nid, cid));
            }
        }
    }

    return seg;
}

} // namespace

extern "C" SEXP snic(SEXP imgSEXP, SEXP wSEXP, SEXP hSEXP, SEXP kSEXP, SEXP nbrtypeSEXP) {
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

    const double *img = REAL(imgSEXP);

    int k_eff = k_req;
    std::vector<int> seg;
    try {
        seg = snic_cpp(img, w, h, b, k_req, nbr_type, k_eff);
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
