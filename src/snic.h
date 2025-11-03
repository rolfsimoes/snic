#ifndef SNIC_H
#define SNIC_H

#include <cstdint>
#include <vector>

namespace snic {

template <typename T>
struct Img {
    using value_type = T;
    value_type* data;
    int w;
    int h;
    int b;
    int n;
    char order;
    std::vector<std::uint8_t> mask_;
    int n_valid_;

    Img(value_type* data, int height, int width, int bands, char order = 'C');

    int idx(int r, int c) const;
    int row(int pid) const;
    int col(int pid) const;
    void vals(int pid, std::vector<double>& out) const;
    void neighbors4(int pid, std::vector<int>& out) const;
    bool has_nan(int pid) const;
    int valid_pixel_count() const;
    const std::vector<std::uint8_t>& mask() const;
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
    double d;
    int pid;
    int cid;

    Node(double d_, int pid_, int cid_);
};

struct NodeGreater {
    bool operator()(const Node& a, const Node& b) const;
};

template <typename T>
std::vector<int> grid_seeds(const Img<T>& img, int grid_step);

template <typename T>
void snic_run(const Img<T>& img,
              const std::vector<int>& seeds,
              double compactness,
              std::vector<int>& out);

}  // namespace snic

#endif  // SNIC_H
