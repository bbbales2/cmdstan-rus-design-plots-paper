library(tidyverse)
library(ggplot2)
library(rstan)

source("config_xyr.R")

cmds = NULL

for(i in 1:length(xs)) {
  data = base_data
  data[["X"]] = xs[i]
  data[["Y"]] = ys[i]
  stan_rdump(names(data), paste0(folder, "/model.", i, ".dat"), env = list2env(data))
  cmds = c(cmds, paste0("../hex-gen sample num_samples=1 algorithm=fixed_param init=params.init data file=model.", i, ".dat output file=generated.", i, ".csv >& log.gen.", i, ".txt"))
}

writeLines(cmds, paste0(folder, "/run_commands.sh"))
stan_rdump(names(params), paste0(folder, "/params.init"), env = list2env(params))
writeLines("cat run_commands.sh | parallel -j16", paste0(folder, "/run.sh"))

system(paste0("cd ", folder, "; bash run.sh; cd ../"))

fit_cmds = NULL

k = 1
for(r in 1:reps) {
  for(i in 1:length(xs)) {
    data = base_data
    data[["X"]] = xs[i]
    data[["Y"]] = ys[i]
    data[["sigma"]] = params[["sigma"]]
  
    calcfq = read_csv(paste0(folder, "/generated.", i, ".csv"), comment = "#") %>%
      select(starts_with("calcfq")) %>%
      as.numeric()
  
    data[["measfq"]] = rlnorm(length(calcfq), meanlog = log(calcfq), sdlog = params[["sigma"]])
  
    stan_rdump(names(data), paste0(folder, "/fit_generated.", k, ".dat"), env = list2env(data))
    fit_cmds = c(fit_cmds, paste0("../hex-fit optimize save_iterations=1 laplace_draws=200 init=params.init data file=fit_generated.", k, ".dat output file=fitted.", k, ".csv >& log.fit.", k, ".txt"))
    k = k + 1
  }
}

writeLines(fit_cmds, paste0(folder, "/run_fits.sh"))
