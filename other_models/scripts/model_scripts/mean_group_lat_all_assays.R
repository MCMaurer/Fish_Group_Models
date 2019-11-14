# get session info
sess <- sessionInfo()
on_farm <- stringr::str_detect(sess$running, "Ubuntu")

getwd()

library(tidyverse)

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (on_farm) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.6")
} else {library(brms)}

# read data
if(on_farm){
  d <- readRDS("data/cleaned/known_lat_all_chases_data.rds") 
} else {d <- readRDS("other_models/data/cleaned/mean_group_lat_all_assays.rds")}

d <- d %>% 
  ungroup() %>% 
  mutate_at(.vars = vars(known_mean_lat, pred_mean_lat), .funs = list(c = function(x) scale(x)[,1]))

# fit hurdle-inflated negbinom
model <- brm(data = d, family = hurdle_gamma,
             bf(novel_mean_lat ~ 1 + known_mean_lat_c + pred_mean_lat_c + treatment + trial +
                  (1 | group_ID) +
                  (1 | tank),
                hu ~ 1 + known_mean_lat_c + pred_mean_lat_c + treatment + trial +
                  (1 | group_ID) +
                  (1 | tank)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 10000, warmup = 2000, chains = 3, cores = 3, 
             control = list(adapt_delta = 0.9999, max_treedepth = 15))

# save model
saveRDS(model, "other_models/fit_models/mean_group_lat_all_assays_tt_fitted.rds")
