library(tidyverse)
library(ggplot2)
library(rstan)

source("config_xyr.R")

read_laplace_csv = function(file) {
  lines = readLines(file)
  starts_with_pound = lapply(lines, function(line) {
    return(substr(line, 1, 1) == "#")
  }) %>%
    unlist
  
  #Optimization terminated with error:
  #Line search failed to achieve a sufficient decrease, no more progress can be made
  
  skip = which(starts_with_pound) %>% tail(1)
  n_max = which(starts_with_pound) %>% tail(1) - which(starts_with_pound) %>% tail(2) %>% head(1) - 2
  
  list(df_opt = read_csv(file, comment = "#", n_max = n_max),
       df_laplace = read_csv(file, comment = "#", skip = skip))
}

dfl = list()

k = 1
for(r in 1:reps) {
  for(i in 1:length(xs)) {
    tryCatch({
      dfl[[k]] = read_laplace_csv(paste0(folder, "/fitted.", k, ".csv"))$df_laplace %>%
        mutate(x = xs[i],
               y = ys[i],
               rep = r)
    }, error = function(e) {
      print(c(k, e))
      dfl[[k]] = NULL
    })
    k = k + 1
  }
}

df = dfl %>%
  bind_rows

write_csv(df, "xyr.csv")
