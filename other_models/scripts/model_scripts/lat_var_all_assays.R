library(tidyverse)
d <- readRDS("latency/data/cleaned/latency_typical_food.rds")
d

d2 <- d %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID, treatment, trial, tank) %>% 
  summarise(lat_var = var(latency, na.rm = T))

d2 <- d2 %>% 
  ungroup() %>% 
  mutate_at(vars(treatment, trial), .funs = list(c = function(x)scale(x)[,1]))


# fit hurdle-inflated negbinom
model <- brm(data = d2, family = hurdle_gamma,
             bf(lat_var ~ 1 + treatment_c + trial_c +
                  (1 | group_ID) +
                  (1 | tank)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 5000, warmup = 2000, chains = 4, cores = 4, 
             control = list(adapt_delta = 0.9999999, max_treedepth = 15))

# save model
saveRDS(model, "other_models/fit_models/known_lat_var_fitted.rds")


# novel var ---------------------------------------------------------------

d <- read_rds("latency/data/cleaned/latency_novel_food.rds") %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID, treatment, trial, tank) %>% 
  summarise(lat_var = var(latency, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_at(vars(treatment, trial), .funs = list(c = function(x)scale(x)[,1]))

model <- brm(data = d, family = hurdle_gamma,
             bf(lat_var ~ 1 + treatment_c + trial_c +
                  (1 | group_ID) +
                  (1 | tank)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 10000, warmup = 2000, chains = 4, cores = 4, 
             control = list(adapt_delta = 0.9999999, max_treedepth = 15))

# save model
saveRDS(model, "other_models/fit_models/novel_lat_var_fitted.rds")

# pred var ----------------------------------------------------------------

d <- read_rds("latency/data/cleaned/latency_pred_cue_final.rds") %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID, treatment, trial, tank) %>% 
  summarise(lat_var = var(latency, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_at(vars(treatment, trial), .funs = list(c = function(x)scale(x)[,1]))

model <- brm(data = d, family = hurdle_gamma,
             bf(lat_var ~ 1 + treatment_c + trial_c +
                  (1 | group_ID) +
                  (1 | tank)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 5000, warmup = 2000, chains = 4, cores = 4, 
             control = list(adapt_delta = 0.999999999, max_treedepth = 15))

# save model
saveRDS(model, "other_models/fit_models/pred_lat_var_fitted.rds")

