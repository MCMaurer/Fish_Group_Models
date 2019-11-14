# talked through this stuff with Eric Payne

lat_typ_int


# calculating icc for negative binomial model
sigma_a_sq <- lat_typ_int %>% 
  tidy_draws() %>% 
  select(sd_group_ID__Intercept) %>% 
  colMeans() %>% 
  as.vector()

sigma_tank_sq <- lat_typ_int %>% 
  tidy_draws() %>% 
  select(sd_tank__Intercept) %>% 
  colMeans() %>% 
  as.vector()

sigma_a_sq

# appendix of Nakagawa et al 2017 says that sigma a should be calculated from a model with NO FIXED EFFECTS, similar to how you do an intercept only model to get beta_0

theta <- lat_typ_int %>% 
  tidy_draws() %>% 
  select(shape) %>% 
  colMeans() %>% 
  as.vector()

# theta is shape parameter
# lambda is expected value
# lambda = exp(beta_0 + 0.5*sigma_a_sq)
# beta_0 is the mean value on the latent scale (i.e. β0 from the intercept-only model)

# you can also estimate lambda as the average of all observed outcomes

d <- lat_typ_int$data

intercept_only_model <- brm(latency ~ 1, data = d, family = negbinomial)
beta_0 <- intercept_only_model %>% 
  tidy_draws() %>% 
  select(b_Intercept) %>% 
  colMeans() %>% 
  as.vector()

beta_0

lambda <- exp(beta_0 + 0.5*(sigma_a_sq + sigma_tank_sq))

# other method of getting lambda is just getting the expectation as the mean of the outcome
lambda2 <- mean(d$latency[d$latency > 0])

icc_negbinom <- sigma_a_sq / (sigma_a_sq + log(1 + (1/lambda2) + (1/theta)))
icc_negbinom

source("icc_functions.R")
my_icc_tibble(lat_typ_int, total_re.form = ~(1|group_ID) + (1|tank), lesser_re.form = ~(1|tank))

# maybe I should be calculating the intercept only model for only data that clear the hurdle?

d <- lat_typ_int$data %>% 
  filter(latency > 0)

intercept_only_model <- brm(latency ~ 1, data = d, family = negbinomial)
beta_0 <- intercept_only_model %>% 
  tidy_draws() %>% 
  select(b_Intercept) %>% 
  colMeans() %>% 
  as.vector()

beta_0

lambda <- exp(beta_0 + 0.5*sigma_a_sq)

icc_negbinom <- sigma_a_sq / (sigma_a_sq + log(1 + (1/lambda)+ (1/theta)))
icc_negbinom

?var()

performance::r2_bayes(lat_typ_int)
performance:::.r2_posterior

performance::r2_bayes(food_eaten)
food_eaten

my_icc_tibble

# one other thing would be taking differences between ppd and ppd_0, calculating the mean, then re-drawing from the posteriors, calculating the mean difference again, and doing this a bunch of times, so you would never go below zero, because they're not actually raw draws, they're means of distributions of raw draws
model <- food_eaten
total_re.form = ~(1|group_ID) + (1|tank)
lesser_re.form = ~(1|tank)


PPD <- posterior_predict(model, re.form = total_re.form)
vars <- apply(PPD, MARGIN = 1, FUN = var)

PPD_0 <- posterior_predict(model, re.form = lesser_re.form)
vars_0 <- apply(PPD_0, MARGIN = 1, FUN = var)

vars <- sort(vars)
vars_0 <- sort(vars_0)

icc_sorted_draws <- 1 - (vars_0/vars)
hist(icc_sorted_draws, breaks = 100)
median(icc_sorted_draws)
