# get session info
sess <- sessionInfo()

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (stringr::str_detect(sess$running, "Ubuntu")) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.5.1")
} else {library(brms)}
library(tidyverse)

# read data
d <- readRDS("food_eaten/data/cleaned/typical_food_proportion_eaten_group_size.rds") %>% 
  rename(trial = Trial, tank = Tank, group_ID = Group) %>% 
  mutate_at(vars(tank, group_ID), as.factor)

d

# fit beta
model <- brm(data = d, family = binomial(), 
               food_eaten | trials(food_input) ~ 1 + treatment + trial +
                 (1 | group_ID) +
                 (1 | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
               control = list(adapt_delta = 0.98, max_treedepth = 15))

# save model
saveRDS(model, "food_eaten/fit_models/prop_typ_eaten_aggbinom_fit.rds")
