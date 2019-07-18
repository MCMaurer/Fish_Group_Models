library(brms)
library(tidyverse)

model <- readRDS("food_eaten/fit_models/prop_typ_eaten_aggress_1_aggbinom_fit.rds")
launch_shinystan(model)

marginal_effects(model)
