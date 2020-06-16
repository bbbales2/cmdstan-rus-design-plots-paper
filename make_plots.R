library(tidyverse)
library(ggplot2)
library(latex2exp)
library(cowplot)

source("params.R")

reference = do.call(tibble, params) %>%
  select(-sigma) %>%
  mutate(c66_coat = (c11_coat - c12_coat) / 2.0) %>%
  rename("C[11]" = "c11_coat",
         "C[12]" = "c12_coat",
         "C[13]" = "c13_coat",
         "C[33]" = "c33_coat",
         "C[44]" = "c44_coat",
         "C[66]" = "c66_coat") %>%
  gather(param, value) %>%
  mutate(value = 100 * value)

df = read_csv("combined.csv")

df_khat = df %>%
  group_by(whiche, rep, x) %>%
  mutate(log_ratio = log_p - log_g) %>%
  select(whiche, x, rep, log_ratio) %>%
  summarize(khat = loo::psis(log_ratio)$diagnostics$pareto_k)

number_ticks = function(n) {
  function(limits) {
    R = 10
    while(TRUE) {
      m = round(mean(limits) / R) * R
      l = round(quantile(limits, 0.15) / R) * R
      u = round(quantile(limits, 0.85) / R) * R
      out = c(l, m, u)
      
      if(length(unique(out)) == 3) {
        return(out)
      }
      
      R = R / 2
    }
  }
}

fmt_dcimals = function(){
  function(x) {
    if(abs(x - 1.0) < 0.1) {
      return(sprintf("%.4f", x))
    } else {
      return(sprintf("%.3f", x))
    }
  }
}

fmt_dcimals2 = function(){
  function(x) {
    return(sprintf("%.0f", x))
  }
}

df2 = df %>%
  select(x, rep, whiche, c11_coat, c12_coat, c13_coat, c33_coat, c44_coat, c66_coat) %>%
  rename("C[11]" = "c11_coat",
         "C[12]" = "c12_coat",
         "C[13]" = "c13_coat",
         "C[33]" = "c33_coat",
         "C[44]" = "c44_coat",
         "C[66]" = "c66_coat") %>%
  gather(param, value, -x, -whiche, -rep) %>%
  group_by(x, rep, whiche, param) %>%
  mutate(value = value * 100) %>%
  summarize(ql = quantile(value, 0.1),
            qh = quantile(value, 0.9),
            median = median(value))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

names = c("C[11]", "C[12]", "C[13]", "C[33]", "C[44]", "C[66]")
colors = gg_color_hue(6)
scales = list(c(0, 160), # These are the scales of the different y-axes, one for each parameter
              c(-50, 100),
              c(-50, 100),
              c(0, 300),
              c(0, 90),
              c(0, 40))

plots = lapply(1:length(names), function(i) {
  name = names[i]
  out = df2 %>%
    filter(param == name) %>%
    ggplot() +
    geom_point(aes(x, median), color = colors[i]) +
    geom_ribbon(aes(x = x, ymin = ql, ymax = qh, group = rep), fill = colors[i], alpha = 0.10) +
    geom_hline(data = reference %>%
                 filter(param == name), aes(yintercept = value), size = 0.5) +
    scale_y_continuous(breaks = number_ticks(3), labels = fmt_dcimals2()) +
    coord_cartesian(ylim = scales[[i]]) +
    theme(text = element_text(size = 20),
          panel.background = element_rect(linetype = "solid"),
          panel.grid.major = element_line(size = 0.35, linetype = 'solid', colour = "gray80"),
          axis.ticks.length=unit(-0.15, "cm"), # negative value puts tick marks to the inside
          #axis.text.x.bottom = element_text(margin = margin(t = 0.3, b = 0.0, unit = "cm")), #adjusts the margins after adjusting tick size
          axis.text.y = element_text(margin = margin(r = 0.3, l = 0.0, unit = "cm")), #adjusts the margins after adjusting tick size
          plot.margin = unit(c(0.1,0.1,0.4,0.1),"cm"), #specify margins starting from the top, then right, bottom and left
          axis.text.y.right = element_blank(), # remove tick labels from right axes
          axis.title.y.right = element_blank(),
          axis.title.y = element_blank(),
          strip.background.x = element_blank(),
          strip.placement.x = "outside") +
    facet_grid(param ~ whiche, scales = "free", labeller = label_parsed, switch = "x") +
    scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.4, 0.6, 1.0, 5, 10, 20, 40, 100), sec.axis = dup_axis())
  
  if(i < length(params)) {
    out = out + theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      strip.text.x = element_blank())
  } else {
    out = out + theme(axis.text.x.bottom = element_text(margin = margin(t = 0.3, b = 0.0, unit = "cm")), #adjusts the margins after adjusting tick size
                      axis.text.x.top = element_blank(), # remove tick labels from top axes
                      axis.title.x.top = element_blank(), # remove title from top axes
                      axis.title.x = element_blank(),
                      axis.ticks.x = element_blank())
  }
  
  return(out)
})

plot_grid(plots[[1]],
          plots[[2]],
          plots[[3]],
          plots[[4]],
          plots[[5]],
          plots[[6]], nrow = length(plots),
          # These are the relative y-scalings for the plots
          #  We make the last one larger so the x-labels can fit
          rel_heights = c(1.0, 1.0, 1.0, 1.0, 1.0, 2.0), align = "v")

df_khat %>%
  mutate(quality = ifelse(khat < 0.5, "good", ifelse(khat < 0.7, "okay", "bad"))) %>%
  group_by(quality) %>%
  summarize(n = n()) %>%
  mutate(f = n / sum(n))

df_khat %>%
  mutate(quality = ifelse(khat < 0.5, "good", ifelse(khat < 0.7, "okay", "bad")),
         inrange = ifelse(khat < 1.09, 16, 4),
         khat = pmin(khat, 1.09)) %>%
  ggplot() +
  theme_set(theme_bw(base_size = 10)) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  geom_hline(yintercept = 0.7, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_point(aes(x, khat, color = quality, shape = inrange), size = 1.5, stroke = 1.0) +
  scale_x_log10(breaks = c(0.05, 0.1, 0.2, 0.4, 0.6, 1.0, 5, 10, 20, 40, 100), sec.axis = dup_axis()) +
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), limits = c(0.0, 1.1)) +
  ylab(TeX('$\\hat{k}')) +
  scale_shape_identity() +
  scale_colour_manual(values = c("red", "green2", "blue")) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        panel.background = element_rect(linetype = "solid"),
        panel.grid.major = element_line(size = 0.35, linetype = 'solid', colour = "gray80"),
        axis.ticks.length=unit(-0.15, "cm"), # negative value puts tick marks to the inside
        axis.text.x.bottom = element_text(margin = margin(t = 0.3, b = 0.0, unit = "cm")), #adjusts the margins after adjusting tick size
        axis.text.y = element_text(margin = margin(r = 0.3, l = 0.0, unit = "cm")), #adjusts the margins after adjusting tick size
        plot.margin = unit(c(0.5,0.4,0.2,0.0),"cm"), #specify margins starting from the top, then right, bottom and left
        axis.text.x.top = element_blank(), # remove tick labels from top axes
        axis.text.y.right = element_blank(), # remove tick labels from right axes
        axis.title.x.top = element_blank(), # remove title from top axes
        axis.title.y.right = element_blank(),
        axis.title.x = element_blank(),
        strip.background.x = element_blank(),
        strip.placement.x = "outside") +
  facet_grid(~ whiche, scales = "free_x")
