library(tidyverse)

chase_data <- read_rds("chases/data/cleaned/full_chase_group_size.rds")
known_lat_data <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(-date_time, -camera, -video)

all_chase_spread <- chase_data %>% 
  filter(measurement == "numchases") %>% 
  mutate(percap_numchases = value/treatment) %>% 
  select(treatment, trial, group_ID, assay, percap_numchases) %>% 
  spread(key = assay, value = percap_numchases)


d <- left_join(known_lat_data, all_chase_spread) %>% 
  mutate(group_ID = factor(group_ID))

saveRDS(d, "other_models/data/cleaned/known_lat_all_chases_data.rds")
