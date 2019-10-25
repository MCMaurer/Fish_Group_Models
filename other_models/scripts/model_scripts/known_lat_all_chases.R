# get session info
sess <- sessionInfo()
on_farm <- stringr::str_detect(sess$running, "Ubuntu")

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (on_farm) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.6")
} else {library(brms)}

# read data
if(on_farm){
  d <- readRDS("data/cleaned/known_lat_all_chases_data.rds") 
} else {d <- readRDS("other_models/data/cleaned/known_lat_all_chases_data.rds")}

# fit hurdle-inflated negbinom
 model <- brm(data = d, family = hurdle_negbinomial,
              bf(latency ~ 1 + treatment + trial + activity + food + novel + pred +
                   (1 | group_ID) +
                   (1 | tank),
                 hu ~ 1 + treatment + trial + activity + food + novel + pred +
                   (1 | group_ID) +
                   (1 | tank)),
               prior = c(set_prior("normal(1, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd")),
               iter = 5000, warmup = 2000, chains = 4, cores = 4, 
               control = list(adapt_delta = 0.99, max_treedepth = 15))

# save model
saveRDS(model, "other_models/fit_models/known_lat_all_chases.rds")
model %>% 
  gather_draws(`^b.*`, regex = T) %>% 
  median_hdi() %>% 
  select(.variable, .value, .lower, .upper) %>% 
  rename(variable = .variable, median = .value, hdi_2.5 = .lower, hdi_97.5 = .upper) %>% 
  mutate(overlap_zero = hdi_2.5 < 0 & hdi_97.5 > 0) 

model %>% 
  gather_draws(`^b.*`, regex = T) %>% 
  ggplot(aes(x = .value, y = .variable)) +
  geom_halfeyeh(size = 1, show_interval = T) +
  geom_vline(xintercept = 0) +
  MCMsBasics::minimal_ggplot_theme() +
  coord_cartesian(xlim = c(-1,1))
