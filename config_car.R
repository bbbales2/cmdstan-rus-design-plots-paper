folder = "car"

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

cars = exp(seq(log(5), log(100), length = 16))
bs = base_data[["Z_coat"]] * 2 * cars / (base_data[["X"]] + base_data[["Y"]])
xs = bs * base_data[["X"]]
ys = bs * base_data[["Y"]]

tibble(cars =  cars,
       xs = xs,
       ys = ys,
       z_sub = base_data[["Z_sub"]],
       z_coat = base_data[["Z_coat"]],
       csr = z_coat / z_sub)
