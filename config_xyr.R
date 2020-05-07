folder = "xyr"

source("params.R")

reps = 4

base_data = list(PX = 12,
                 PY = 12,
                 PZ_sub = 2,
                 PZ_coat = 1,
                 N = 30,
                 X = 0.010,
                 Y = 0.010,
                 Z_sub = 0.0025,
                 Z_coat = 0.001,
                 density_sub = 8700.0,
                 density_coat = 4500.0,
                 c11_sub = 2.5,
                 c44_sub = 1.4,
                 c12_sub = 1.5,
                 cu = c(0.1, 0.2, 0.3))

if(!dir.exists(folder)) {
  dir.create(folder)
}

csr = base_data[["Z_coat"]] / base_data[["Z_sub"]]
car = 20
xs = seq(4, 20, length = 16) * 0.001
ys = car * 2 * base_data[["Z_coat"]] - xs

tibble(xs = xs,
       ys = ys,
       cars = (xs + ys) / (2 * base_data[["Z_coat"]]),
       z_sub = base_data[["Z_sub"]],
       z_coat = base_data[["Z_coat"]],
       csr = z_coat / z_sub,
       xyr = xs / ys)
