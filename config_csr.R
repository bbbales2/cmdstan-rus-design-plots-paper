folder = "csr"

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

car = 10
csrs = exp(seq(log(0.05), log(0.5), length = 16))
Z_subs = base_data[["Z_coat"]] / csrs

tibble(csr =  csrs,
       xs = base_data[["X"]],
       ys = base_data[["Y"]],
       z_sub = Z_subs,
       z_coat = base_data[["Z_coat"]],
       car = car)
