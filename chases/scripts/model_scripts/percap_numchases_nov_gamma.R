# get session info
sess <- sessionInfo()

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (stringr::str_detect(sess$running, "Ubuntu")) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.5.1")
} else {library(brms)}

# read data
d <- readRDS("data/cleaned/only_half_chase_group_size.rds")


# fit gamma
model <- brm(data = d, family = Gamma, 
               percap_chases ~ 1 + treatment + assay +
                 (1 + treatment + assay | group_ID) +
                 (1 + treatment + assay | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
               control = list(adapt_delta = 0.98, max_treedepth = 15))

# save model
saveRDS(model, "fit_models/percap_numchases_nov_gamma_fit.rds")
