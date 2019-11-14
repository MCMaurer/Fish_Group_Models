library(tidyverse)
library(brms)

d <- read_rds("other_models/data/cleaned/known_first2_diff.rds")
d %>% 
  ggplot(aes(x = diff)) +
  geom_histogram()

model <- brm(formula = diff ~ 1,
             family = negbinomial,
             data = d,
             prior = c(set_prior("normal(0, 1)", class = "Intercept")),
                       # set_prior("normal(0, 1)", class = "b"),
                       # set_prior("cauchy(0, 2)", class = "sd")),
             iter = 5000, warmup = 2000, chains = 3, cores = 3, 
             control = list(adapt_delta = 0.9, max_treedepth = 15))

# the thought here is that there are really 2 scenarios that could lead to a difference of 0: one is where there are 2 fast fish, and they don't pay attention to each other but are equally fast. The other is a scenario where there is a leader and a follower, but the time difference between the two can't be recorded, so they're given the same latency time

model2 <- brm(formula = diff ~ 1,
              family = zero_inflated_negbinomial,
              data = d,
              prior = c(set_prior("normal(0, 1)", class = "Intercept")),
              # set_prior("normal(0, 1)", class = "b"),
              # set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 2000, chains = 3, cores = 3, 
              control = list(adapt_delta = 0.9, max_treedepth = 15))


model3 <- brm(formula = diff ~ 1,
              family = hurdle_negbinomial,
              data = d,
              prior = c(set_prior("normal(0, 1)", class = "Intercept")),
              # set_prior("normal(0, 1)", class = "b"),
              # set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 2000, chains = 3, cores = 3, 
              control = list(adapt_delta = 0.9, max_treedepth = 15))

waic(model, model2, model3)


# known first 2 -----------------------------------------------------------


model3.1 <- brm(formula = bf(diff ~ 1 + treatment + trial +
                     (1 | group_ID) +
                     (1 | tank),
                   hu ~ 1 + treatment + trial +
                  (1 | group_ID) +
                  (1 | tank)),
                family = hurdle_negbinomial,
                data = d,
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("cauchy(0, 2)", class = "sd")),
                iter = 5000, warmup = 2000, chains = 3, cores = 3, 
                control = list(adapt_delta = 0.9, max_treedepth = 15))

saveRDS(model3.1, "other_models/fit_models/known_first2_diff_fitted.rds")


# known last 2 ------------------------------------------------------------

known_last2 <- read_rds("other_models/data/cleaned/known_last2_diff.rds")

model4 <- brm(formula = bf(diff ~ 1 + treatment + trial +
                               (1 | group_ID) +
                               (1 | tank),
                             hu ~ 1 + treatment + trial +
                               (1 | group_ID) +
                               (1 | tank)),
                family = hurdle_negbinomial,
                data = known_last2,
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b"),
                          set_prior("cauchy(0, 2)", class = "sd")),
                iter = 5000, warmup = 2000, chains = 3, cores = 3, 
                control = list(adapt_delta = 0.999, max_treedepth = 15))

saveRDS(model4, "other_models/fit_models/known_last2_diff_fitted.rds")

# novel first 2 -----------------------------------------------------------

novel_first2 <- read_rds("other_models/data/cleaned/novel_first2_diff.rds")

model5 <- brm(formula = bf(diff ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank),
                           hu ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank)),
              family = hurdle_negbinomial,
              data = novel_first2,
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 2000, chains = 3, cores = 3, 
              control = list(adapt_delta = 0.999, max_treedepth = 15))

saveRDS(model5, "other_models/fit_models/novel_first2_diff_fitted.rds")


# novel last 2 ------------------------------------------------------------

novel_last2 <- read_rds("other_models/data/cleaned/novel_last2_diff.rds")

model6 <- brm(formula = bf(diff ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank),
                           hu ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank)),
              family = hurdle_negbinomial,
              data = novel_last2,
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 2000, chains = 3, cores = 3, 
              control = list(adapt_delta = 0.9999, max_treedepth = 15))

saveRDS(model6, "other_models/fit_models/novel_last2_diff_fitted.rds")


# pred first 2 ------------------------------------------------------------

pred_first2 <- read_rds("other_models/data/cleaned/pred_first2_diff.rds")

model7 <- brm(formula = bf(diff ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank),
                           hu ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank)),
              family = hurdle_negbinomial,
              data = pred_first2,
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 2000, chains = 3, cores = 3, 
              control = list(adapt_delta = 0.9999, max_treedepth = 15))

saveRDS(model7, "other_models/fit_models/pred_first2_diff_fitted.rds")


# pred last 2 -------------------------------------------------------------

pred_last2 <- read_rds("other_models/data/cleaned/pred_last2_diff.rds")

model8 <- brm(formula = bf(diff ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank),
                           hu ~ 1 + treatment + trial +
                             (1 | group_ID) +
                             (1 | tank)),
              family = hurdle_negbinomial,
              data = pred_last2,
              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                        set_prior("normal(0, 1)", class = "b"),
                        set_prior("cauchy(0, 2)", class = "sd")),
              iter = 5000, warmup = 2000, chains = 3, cores = 3, 
              control = list(adapt_delta = 0.9999, max_treedepth = 15))

saveRDS(model8, "other_models/fit_models/pred_last2_diff_fitted.rds")
