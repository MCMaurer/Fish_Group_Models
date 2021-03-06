# get session info
sess <- sessionInfo()
on_farm <- stringr::str_detect(sess$running, "Ubuntu")

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (on_farm) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.5.1")
} else {library(brms)}

# read data
if(on_farm){
  d <- readRDS("data/cleaned/latency_pred_cue_final.rds") 
} else {d <- readRDS("latency/data/cleaned/latency_pred_cue_final.rds")}

# fit hurdle-inflated negbinom
model <- brm(data = d, family = hurdle_negbinomial,
               bf(latency ~ 1 + treatment + trial +
                 (1 | group_ID) +
                 (1 | tank),
                 hu ~ 1 + treatment + trial +
                   (1 | group_ID) +
                   (1 | tank)),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
               control = list(adapt_delta = 0.98, max_treedepth = 15))

# save model
saveRDS(model, "fit_models/lat_pred_int_hurd_nbin_hu_fit.rds")
