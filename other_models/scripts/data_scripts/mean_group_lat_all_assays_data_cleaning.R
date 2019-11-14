known <- read_rds("latency/data/cleaned/latency_typical_food.rds")
novel <- read_rds("latency/data/cleaned/latency_novel_food.rds")
pred <- read_rds("latency/data/cleaned/latency_pred_cue_final.rds")

known <- known %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID, trial, treatment, tank) %>% 
  summarise(known_mean_lat = mean(latency))

novel <- novel %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID, trial, treatment, tank) %>% 
  summarise(novel_mean_lat = mean(latency))

pred <- pred %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID, trial, treatment, tank) %>% 
  summarise(pred_mean_lat = mean(latency))

d <- left_join(known, novel) %>% 
  left_join(pred)

d$pred_mean_lat

d %>% saveRDS("other_models/data/cleaned/mean_group_lat_all_assays.rds")

hist(d$novel_mean_lat)
d %>% 
  ggplot(aes(x = novel_mean_lat)) +
  geom_histogram()

d %>% 
  arrange(novel_mean_lat)
