
# this one needs some work, particularly in generating the initial conditions

marginal_effects_plot <- function(model, effects, color = effects[2], ci_type = "band", ...){
  initial_conditions = list()
  
  # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
  for (x in 1:length(effects)) {
      initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
    }
  
  # if you just have one effect, just use its name for effects
   if(length(effects) == 1){
    e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions, ... = ...)
  } else {
  # otherwise, you have to paste your effect names together
  e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions, ... = ...)
  }
  
  response <- attributes(e[[1]])$response
  if(response == "hu"){
    response <- "probability of not crossing hurdle"
  }
  
  # pull out the dataframe from the marginal effects call
  e <- e[[1]]
  
  if(length(effects) == 1){
    e %>% 
      ggplot(aes(x= !!sym(effects[1]), y=estimate__), color= "black") +
      geom_line() +
      geom_ribbon(aes(ymin=lower__, ymax=upper__), fill = "black", 
                  color = "transparent", alpha = 0.1) +
      ylab(response)
      
  } else{
    if(ci_type == "both"){
      e %>% 
        ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
        geom_line(size = 1.5) +
        geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.1) +
        geom_line(aes(x = !!sym(effects[1]), y = lower__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.5) +
        geom_line(aes(x = !!sym(effects[1]), y = upper__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.5) +
        scale_fill_viridis_d() +
        scale_color_viridis_d() +
        ylab(response)
      } else{
        if(ci_type == "lines"){
      e %>% 
        ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
        geom_line(size = 1.5) +
        geom_line(aes(x = !!sym(effects[1]), y = lower__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.7) +
        geom_line(aes(x = !!sym(effects[1]), y = upper__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.7) +
        scale_fill_viridis_d() +
        scale_color_viridis_d() +
        ylab(response)
    } else{
      e %>% 
        ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
        geom_line(size = 1.5) +
        geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.3) +
        scale_fill_viridis_d() +
        scale_color_viridis_d() +
        ylab(response)
    }
      }
  }
}


# marginal_effects_plot(typ_hu, effects = c("treatment", "trial"))
# marginal_effects_plot(typ_hu, effects = c("treatment", "trial"), ci_type = "both")
# marginal_effects_plot(typ_int, effects = c("treatment"), dpar = "hu")
# marginal_effects_plot(typ_int, effects = c("treatment"))


# # 2nd attempt at marginal effects plots -----------------------------------
# 
# model = typ_int
# effects = c("treatment", "trial")
# names(typ_int$data)[1]
# 
# initial_conditions = list()
# initial_conditions
# 
# length(effects)
# 
# # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
# for (x in 1:length(effects)) {
#   initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
# }
# 
# initial_conditions
# 
# 
# # if you just have one effect, just use its name for effects
# if(length(effects) == 1){
#   e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions, ... = ...)
# } else {
#   # otherwise, you have to paste your effect names together
#   e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions)
# }
# 
# e
# 
# response <- attributes(e[[1]])$response
# 
# # pull out the dataframe from the marginal effects call
# e <- e[[1]]
# 
# if(length(effects) == 1){
#   e %>% 
#     ggplot(aes(x= !!sym(effects[1]), y=estimate__), color= "black") +
#     geom_line() +
#     geom_ribbon(aes(ymin=lower__, ymax=upper__), fill = "black", 
#                 color = "transparent", alpha = 0.1) +
#     ylab(response)
#   
# } else{
#   e %>% 
#     ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
#     geom_line() +
#     geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.1) +
#     geom_line(aes(x = !!sym(effects[1]), y = lower__, color = !!sym(color), linetype = 2)) +
#     scale_fill_viridis_d() +
#     scale_color_viridis_d() +
#     ylab(response)
# }
# 
# names(e)
# 
# e %>% 
#   ggplot(aes(x = treatment, y = latency)) +
#   geom_line(aes(color = trial))

# param estimate plots ----------------------------------------------------

param_estimate_plot <- function(model, num_params = 4){
  p <- posterior_summary(model) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    head(n=num_params)
  pp <- p %>% 
    mutate(variable = rownames(p)) %>% 
    ggplot(aes(y=Estimate, x = variable))+
    geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 3/5, shape = 20) +
    geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
    coord_flip() +
    ylab("Estimate with 95% credible interval")
  return(pp)
}
