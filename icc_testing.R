# trying to test out the sjstats icc function by running the raw code

library(tidyverse)
library(sjstats)
library(brms)
library(performance)

typ_int <- readRDS("latency/fit_models/lat_typ_int_hurd_nbin_hu_fit.rds")
typ_hu <- readRDS("latency/fit_models/lat_typ_hurd_nbin_hu_fit.rds")

waic(typ_int, typ_hu)
loo(typ_int, typ_hu)
kfold(typ_int, typ_hu, K = 10)

?loo.brmsfit

icc.brmsfit


icc(typ_int, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 | group_ID))
performance::icc(typ_int, re.form = ~(1 | group_ID), robust = T)
performance::icc(typ_hu, re.form = ~(1 | group_ID), robust = T)

methods(icc)
?methods

getAnywhere(icc.brmsfit)



my_icc <- function (x, re.form = NULL, typical = "mean", prob = 0.89, ppd = FALSE, newdata = NULL,
                    ...) 
{
  if (!requireNamespace("brms", quietly = TRUE)) 
    stop("Please install and load package `brms` first.", 
         call. = F)
  fitfam <- insight::model_info(x)
  if (missing(ppd) && !fitfam$is_linear) {
    message("Variance decomposition for non-Gaussian models should be based on the posterior predictive distribution. To do this, set `ppd = TRUE`.")
  }
  if (ppd) {
    PPD <- brms::posterior_predict(x, re.form = re.form, newdata = newdata, 
                                   summary = FALSE)
    total_var <- apply(PPD, MARGIN = 1, FUN = stats::var)
    PPD_0 <- brms::posterior_predict(x, re.form = NA, newdata = newdata, summary = FALSE)
    tau.00 <- apply(PPD_0, MARGIN = 1, FUN = stats::var)
    ri.icc <- tau.00/total_var
    resid.var <- total_var - tau.00
    icc_ <- c(1 - sjmisc::typical_value(ri.icc, fun = typical), 
              sjmisc::typical_value(tau.00, fun = typical), sjmisc::typical_value(resid.var, 
                                                                                  fun = typical), sjmisc::typical_value(total_var, 
                                                                                                                        fun = typical))
    attr(icc_, "hdi.icc") <- rev(1 - hdi(ri.icc, prob = prob))
    attr(icc_, "hdi.tau.00") <- hdi(tau.00, prob = prob)
    attr(icc_, "hdi.resid") <- hdi(resid.var, prob = prob)
    attr(icc_, "hdi.total") <- hdi(total_var, prob = prob)
    attr(icc_, "prob") <- prob
    attr(icc_, "re.form") <- re.form
    attr(icc_, "ranef") <- x$ranef$group[1]
    has_rnd_slope <- FALSE
    names(icc_) <- c("icc", "tau.00", "resid.var", "total.var")
    class(icc_) <- c("icc_ppd", class(icc_))
  }
  else {
    reva <- brms::VarCorr(x, summary = FALSE)
    reva.resid <- reva[names(reva) == "residual__"]
    reva <- reva[!(names(reva) == "residual__")]
    vars <- purrr::map(reva, ~.x$sd[, 1]^2)
    tau.00 <- purrr::map(vars, ~.x)
    tau.11 <- purrr::map(reva, ~.x$cov[, 2, 2])
    sig <- reva.resid[["residual__"]]$sd[, 1]
    if (is.null(sig)) {
      if (fitfam$is_binomial) 
        sig <- sqrt((pi^2)/3)
      else sig <- 1
    }
    resid.var <- sig^2
    total_var <- apply(as.data.frame(vars), MARGIN = 1, FUN = sum) + 
      resid.var
    if (length(resid.var) == 1) 
      resid.var <- rep(resid.var, length(total_var))
    ri.icc <- purrr::map(tau.00, ~.x/total_var)
    tau.11 <- purrr::map_if(tau.11, is.null, ~rep(NA, length(resid.var)))
    names(ri.icc) <- sprintf("icc_%s", names(ri.icc))
    names(tau.00) <- sprintf("tau.00_%s", names(tau.00))
    names(tau.11) <- sprintf("tau.11_%s", names(tau.11))
    icc_ <- purrr::map_dbl(ri.icc, ~sjmisc::typical_value(.x, 
                                                          fun = typical))
    attr(icc_, "tau.00") <- purrr::map_dbl(tau.00, ~sjmisc::typical_value(.x, 
                                                                          fun = typical))
    attr(icc_, "hdi.icc") <- purrr::map(ri.icc, ~hdi(.x, 
                                                     prob = prob))
    attr(icc_, "hdi.tau.00") <- purrr::map(tau.00, ~hdi(.x, 
                                                        prob = prob))
    attr(icc_, "sigma_2") <- sjmisc::typical_value(resid.var, 
                                                   fun = typical)
    attr(icc_, "hdi.sigma_2") <- hdi(resid.var, prob = prob)
    attr(icc_, "prob") <- prob
    check_tau <- purrr::map_lgl(tau.11, ~sjmisc::all_na(.x))
    if (any(!check_tau)) {
      tau.11 <- tau.11[!check_tau]
      attr(icc_, "tau.11") <- purrr::map_dbl(tau.11, ~sjmisc::typical_value(.x, 
                                                                            fun = typical))
      attr(icc_, "hdi.tau.11") <- purrr::map(tau.11, ~hdi(.x, 
                                                          prob = prob))
    }
    has_rnd_slope <- any(isTRUE(purrr::map_lgl(brms::ranef(x), 
                                               ~dim(.x)[3] > 1)))
    if (has_rnd_slope) 
      message("Caution! ICC for random-slope-intercept models usually not meaningful. See 'Note' in `?icc`.")
    class(icc_) <- c("sj_icc_brms", class(icc_))
  }
  attr(icc_, "family") <- fitfam$family
  attr(icc_, "link") <- fitfam$link_function
  attr(icc_, "formula") <- stats::formula(x)
  attr(icc_, "model") <- "Bayesian mixed model"
  attr(ri.icc, "rnd.slope.model") <- any(has_rnd_slope)
  icc_
}



icc_2 <- typ_int$data %>% 
  filter(treatment == 2) %>% 
  my_icc(typ_int, adjusted = F, ppd = TRUE, typical = "median", 
         re.form = ~(1 | group_ID), newdata = ., prob = 0.95)

icc_4 <- typ_int$data %>% 
  filter(treatment == 4) %>% 
  my_icc(typ_int, adjusted = F, ppd = TRUE, typical = "median", 
         re.form = ~(1 | group_ID), newdata = ., prob = 0.95)

icc_8 <- typ_int$data %>% 
  filter(treatment == 8) %>% 
  my_icc(typ_int, adjusted = F, ppd = TRUE, typical = "median", 
         re.form = ~(1 | group_ID), newdata = ., prob = 0.95)


icc_2
icc_4
icc_8

icc_8_1 <- typ_int$data %>% 
  filter(treatment == 8, trial == 1) %>% 
  my_icc(typ_int, adjusted = F, ppd = TRUE, typical = "median", 
         re.form = ~(1 | group_ID), newdata = ., prob = 0.95)

icc_8_1

icc_8_2 <- typ_int$data %>% 
  filter(treatment == 8, trial == 2) %>% 
  my_icc(typ_int, adjusted = F, ppd = TRUE, typical = "median", 
         re.form = ~(1 | group_ID), newdata = ., prob = 0.95)

icc_8_2

icc_8_3 <- typ_int$data %>% 
  filter(treatment == 8, trial == 3) %>% 
  my_icc(typ_int, adjusted = F, ppd = TRUE, typical = "median", 
         re.form = ~(1 | group_ID), newdata = ., prob = 0.95)

icc_8_3


# essentially what we're doing here is taking our model and using it to make predicted outcome values based on some series of inputs. with an icc of the whole model, this means we're feeding it all of our data, it gets predicted outcome values WITH the random effects and WITHOUT the random effects. if groups matter, then adding in the random effects will make the predicted outcomes vary a lot more than they do without the random effects, since the group-level parameters will add variance to the outcomes. if the groups do NOT matter, then the random effects will be small, and the variance will be about the same. when we're taking the icc values for different group sizes, all we're doing is getting predicted outcome values for groups of 2, 4, or 8, and calculating the same exact thing.


# now icc by group and trial ----------------------------------------------

my_icc_subset <- function(treatment, trial, model, prob){
  
  data <- model$data
  
  if(treatment != "all"){
    data <- data %>% 
      filter(treatment == !!treatment)
  }
  
  if(trial != "all"){
    data <- data %>% 
      filter(trial == !!trial)
  }
  
  
  result <- my_icc(model, adjusted = F, ppd = TRUE, prob = prob, typical = "median", 
           re.form = ~(1 | group_ID), newdata = data)
    icc_value <- as.numeric(result[1])
    icc_hdi_lower <- attributes(result)$hdi.icc[1]
    icc_hdi_upper <- attributes(result)$hdi.icc[2]
    
    model_formula <- as.character(model$formula$formula)
    model_formula <- paste(model_formula[2], model_formula[1], model_formula[3])
    model_formula <- paste(model_formula, ";;;", model$formula$pforms)
    
    result <- tibble(model_formula, treatment, trial, 
                     icc_value, icc_hdi_lower, icc_hdi_upper, hdi_prob = prob)
    result
}

test <- my_icc_subset(treatment = 8, trial = "all", model = typ_int, prob = 0.95)
test

conditions <- expand(tibble(treatment = c(2,4,8), trial = 1:3), treatment, trial)

icc_group_trial <- pmap_dfr(.l = conditions, .f = my_icc_subset, model = typ_int, prob = 0.95)
icc_group_trial

icc_group <- map_dfr(.x = c(2,4,8), .f = my_icc_subset, model = typ_int, prob = 0.95, trial = "all")
icc_group

my_icc_subset(treatment = "all", trial = "all", model = typ_int, prob = 0.95)
