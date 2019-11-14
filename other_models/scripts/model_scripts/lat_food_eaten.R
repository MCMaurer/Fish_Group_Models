library(tidyverse)
library(brms)

# food <- read_rds("food_eaten/data/cleaned/typical_food_proportion_eaten_group_size.rds") %>% 
#   rename(trial = Trial, group_ID = Group, tank = Tank) %>% 
#   mutate(tank = as.factor(tank))
# latency <- read_rds("latency/data/cleaned/latency_typical_food.rds")
# 
# food
# 
# d <- left_join(latency, food) %>% 
#   mutate(group_ID = as.factor(group_ID))
# 
# d
# 
# 
# lat_food <- brm(data = d, family = hurdle_negbinomial,
#              bf(latency ~ 1 + prop_eaten_food +
#                   (1 | group_ID) +
#                   (1 | tank),
#                 hu ~ 1 + prop_eaten_food +
#                   (1 | group_ID) +
#                   (1 | tank)),
#              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
#                        set_prior("normal(0, 1)", class = "b"),
#                        set_prior("cauchy(0, 2)", class = "sd")),
#              iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
#              control = list(adapt_delta = 0.98, max_treedepth = 15))
# saveRDS(lat_food, "other_models/fit_models/lat_food_known.rds")


# novel food --------------------------------------------------------------


food <- read_rds("food_eaten/data/cleaned/novel_food_proportion_eaten_group_size.rds") %>% 
  rename(trial = Trial, group_ID = Group, tank = Tank) %>% 
  mutate(tank = as.factor(tank)) %>% 
  select(treatment, trial, tank, group_ID, novel_eaten, prop_eaten_novel, novel_input)
latency <- read_rds("latency/data/cleaned/latency_novel_food.rds") %>% 
  select(trial, tank, group_ID, novel_food, latency, treatment)
latency
food %>% print(n=Inf)

d <- left_join(latency, food) %>% 
  mutate(group_ID = as.factor(group_ID))

d


lat_food <- brm(data = d, family = hurdle_negbinomial,
                bf(latency ~ 1 + prop_eaten_novel +
                     (1 | group_ID) +
                     (1 | tank),
                   hu ~ 1 + prop_eaten_novel +
                     (1 | group_ID) +
                     (1 | tank)),
                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                          set_prior("normal(0, 1)", class = "b"),
                          set_prior("cauchy(0, 2)", class = "sd")),
                iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
                control = list(adapt_delta = 0.98, max_treedepth = 15))
saveRDS(lat_food, "other_models/fit_models/lat_food_novel.rds")

