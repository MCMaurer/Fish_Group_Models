library(tidyverse)

lat_diff <- function(data, first_or_last = "first"){
  d <- data %>% 
    group_by(group_ID, trial) %>% 
    mutate(order = case_when(
      first_or_last == "first" ~ rank(latency, ties.method = "first"),
      first_or_last == "last" ~ rank(-latency, ties.method = "first")
    )) %>% 
    filter(order %in% 1:2) %>% 
    spread(key = order, value = latency)
  
  if(first_or_last == "first"){
   d <- d %>%
      rename(first = `1`, second = `2`) %>%
      mutate(diff = second-first)
  }
  else {
    d <- d %>% rename(last = `1`, second_last = `2`) %>%
      mutate(diff = last-second_last)
  }
  return(d)
}

known_lat <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(-video, -individual, -camera)

known_lat %>% 
  lat_diff(first_or_last = "first") %>% 
  saveRDS("other_models/data/cleaned/known_first2_diff.rds")

known_lat %>% 
  lat_diff(first_or_last = "last") %>% 
  saveRDS("other_models/data/cleaned/known_last2_diff.rds")

novel_lat <- read_rds("latency/data/cleaned/latency_novel_food.rds") %>% 
  select(-individual, -`Video Folder`, -`Video #`, -`Novel Start`) %>% 
  filter(!is.na(latency))

novel_lat %>% 
  lat_diff(first_or_last = "first") %>% 
  saveRDS("other_models/data/cleaned/novel_first2_diff.rds")

novel_lat %>% 
  lat_diff(first_or_last = "last") %>% 
  saveRDS("other_models/data/cleaned/novel_last2_diff.rds")

pred_lat <- read_rds("latency/data/cleaned/latency_pred_cue_final.rds") %>% 
  select(-camera, -move_numchases, -move_timechases, -individual) %>% 
  filter(!is.na(latency))
pred_lat

pred_lat %>% 
  lat_diff(first_or_last = "first") %>% 
  saveRDS("other_models/data/cleaned/pred_first2_diff.rds")

pred_lat %>% 
  lat_diff(first_or_last = "last") %>% 
  saveRDS("other_models/data/cleaned/pred_last2_diff.rds")






first2 %>% 
  ggplot(aes(x = factor(treatment, ordered = T, levels = c("2", "4", "8")), y = diff)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  MCMsBasics::minimal_ggplot_theme() +
  xlab("Group Size") +
  ylab("Difference in Latency Between 1st and 2nd Individual")

last2 %>% 
  ggplot(aes(x = factor(treatment, ordered = T, levels = c("2", "4", "8")), y = diff)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  MCMsBasics::minimal_ggplot_theme() +
  xlab("Group Size") +
  ylab("Difference in Latency Between Slowest and 2nd Slowest Individual")


# latency plateau plot ---------------------------------------------------

data <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(group_ID, trial, treatment, latency)

data <- data %>%
  filter(!is.na(latency)) %>% 
  group_by(group_ID, trial, treatment) %>% 
  arrange(latency) %>% 
  mutate(order = rank(latency, ties.method = "first")) %>% 
  arrange(desc(treatment), group_ID, desc(order)) %>% 
  ungroup() %>% 
  group_by(group_ID, trial, latency, treatment) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(group_ID, trial, treatment) %>% 
  arrange(latency) %>% 
  mutate(cumu_n = cumsum(n)) %>% 
  arrange(desc(treatment), group_ID, trial, latency) %>% 
  select(-n) %>% 
  ungroup()

data %>% 
  filter(treatment == 2) %>% 
  filter(!is.na(latency)) %>% 
  select(-individual, -tank, -camera, -video) %>% 
  split(f = list(data$group_ID, data$trial, data$treatment)) %>% 
  map(~add_row(., latency = 0)) %>% 
  bind_rows()

starts <- data %>%
  distinct(trial, group_ID, treatment) %>% 
  mutate(latency = 0, cumu_n = 0)

data %>% 
  filter(treatment == 8, trial == 1)

testplot <- data %>% 
  #filter(treatment == 8, trial == 1) %>% 
  ggplot(aes(x = latency, y = cumu_n, group = interaction(group_ID, trial), color = trial)) +
  geom_step() +
  facet_wrap(~treatment)
testplot
ggplotly(testplot)

ends <- data %>% 
  group_by(treatment, group_ID, trial) %>% 
  top_n(n = 1, wt = cumu_n) %>% 
  ungroup() %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))

# regular x scale
data2 <- bind_rows(data, starts) %>% 
  arrange(group_ID, trial, cumu_n) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))
data2
ends

treatment_names <- c(
  `2` = "Groups of 2",
  `4` = "Groups of 4",
  `8` = "Groups of 8"
)

ggplot() +
  geom_step(data = data2, alpha = 0.4, direction = "hv", aes(x = latency, y = cumu_n, color = trial, group = interaction(group_ID, trial))) +
  geom_point(data = ends, alpha = 0.4, aes(x = latency, y = cumu_n, color = trial, 
                                           group = interaction(group_ID, trial))) +
  facet_grid(trial~treatment, labeller = labeller(
    trial = c(`1` = "Trial 1", `2` = "Trial 2", `3` = "Trial 3"),
    treatment = c(`2` = "Groups of 2", `4` = "Groups of 4", `8` = "Groups of 8")
  )) +
  ylab("# of Fish That Have Eaten") +
  xlab("Trial Time (s)") +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = NA, color = "gray80"), panel.spacing = unit(0, "mm"))

# log x scale
bind_rows(data, starts) %>% 
  arrange(group_ID, trial, cumu_n) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3)) %>% 
  ggplot(aes(x = latency+1, y = cumu_n, color = trial, 
             group = interaction(group_ID, trial))) +
  geom_step(alpha = 0.5, direction = "hv") +
  facet_wrap(~treatment) +
  scale_x_log10()


# quick look at slowest individuals ---------------------------------------


data <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(group_ID, trial, treatment, latency) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))

data %>% 
  group_by(group_ID, treatment, trial) %>% 
  top_n(n = 1, wt = latency) %>% 
  arrange(treatment, trial, group_ID) %>% 
  ggplot(aes(x = treatment, y = latency)) +
  geom_violin() +
  geom_jitter(aes(color = trial)) +
  ylab("latency of slowest individual per group")

# it looks like the slowest individual across all the treatments doesn't really differ that much! which is interesting, as it suggests a group of 8 isn't really any more likely to have a super slow individual than a group of 2, which suggests that something is *actually* going on here

data %>% 
  group_by(group_ID, treatment, trial) %>% 
  arrange(latency) %>% 
  top_n(n = -1, wt = latency) %>% 
  arrange(treatment, trial, group_ID) %>% 
  ggplot(aes(x = treatment, y = latency)) +
  geom_violin() +
  geom_jitter(aes(color = trial)) +
  ylab("latency of fastest individual per group")