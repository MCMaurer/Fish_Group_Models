# A running to-do list for me

## List as of 8/9

**could knowing the ORDER of an individual measurement be used as a predictor? for instance, if you knew that an individual had already moved, would that help you identify at what time another individual would move?**

1. Does group ID matter differently depending on treatment group?
-try calculating ICC for each different treatment to try and answer this

*update 9/18*: did this, it's rad

2. Is there a difference in the intitiator-follower relationship depending on treatment?
-try plottting the change in time between intiation and first follow for each treament
(x-axis: treament; y-axis: change in time between first and second lantency)

*update 9/18*: done

3. Interested in how variance between individuals within a group changes with trial. Our current models would have this information?

*could you get at this by doing ICC with trial as well?*

looks like there's no difference here


4. Is there a relationship between the amount of food eaten and the group's latencies?
-try plotting amount eaten on the y-axis and average group latency on the x-axos, with one line per group size. try plotting amount eaten on the y-axis and average group latency on the x-axos, with one line per group size-also try plotting amount eaten on the y-axis and proportion of individuals that fed in the group on the the x-axos, 3 plots, one for each treatment group

model <- brm(data = d, family = binomial(), 
               food_eaten | trials(food_input) ~ 1 + treatment + trial + mean_group_lat
                 (1 + treatment + trial + mean_group_lat | group_ID) +
                 (1 + treatment + trial + mean_group_lat | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
               control = list(adapt_delta = 0.98, max_treedepth = 15))

5. Plateau plot (a reverse survival plot) where proportion of group that eats is on the y-axis

done!

*this is actually latency values on x axis*

6. Any inspiration from the McDonald paper?

## Updated list as of 7/4/19

1) Finish all the hurdle models
    - fit them and summary plots and outputs
        - ME plots and point+interval parameter plots
*update 7/5/19: running lat_nov model, which should be the last one*
    - for all the plots here, the credible interval bands can be hard to see, maybe add dashed lines to delineate them
    *update 7/18/19: all the analyses have been run and plots generated*
    
2) Acceleration model for groups of 2 and 4
    - Already completed for 8
    - Do for both diff between individuals and diff from 1st individual
    - Generate summaries + plots
    *update 7/18/19: all the analyses have been run and plots generated*
    
3) Bayesian ICC stuff
    - note that this may not work with varying slopes models
        - *update 7/5/19: submitted intercept only model lat_typ_int*
            - *update 7/14/19: model didn't fit, check and run again*
            - ok I resubmitted it, I think the problem was having both iterations and warmup set as 1000, so I think it maybe discarded all of the samples
    - **may have to redo models w/o varying slopes to get at this**
    - need to **bulletproof** arguments here
    
*update 7/17/19*

Have some really good stuff on why the ICC is being computed correctly, and I also have comparisons between ICC calculated on a random slopes vs. random intercepts only model. The ICC estimates for the two are not far off from each other. The intercepts only model ICC is fairly straightforward, but for the random slopes model, here's how it's done: 
"To get a meaningful ICC also for models with random slopes, use adjusted = TRUE. The adjusted ICC uses the mean random effect variance, which is based on the random effect variances for each value of the random slope (see Johnson et al. 2014)."
It makes sense that the two ICC values you get out are close to each other but slightly different. It looks like both the intraclass variance AND total variance are different across the two models; BOTH are lower in the random slopes model. The random slopes model, for the typical food trial, also fits better according to both waic and loo, though it's not a massive difference.

https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.2304 is a paper actually showing how you can use posterior sample medians and distributions to calculate the ICC, which is what the `sjstats` package does.

4) Do certain groups eat proportion of food more consistently than others?
    - I think this would be like ICC for the prop_food_eaten models
        - this would be ICC of prop_food_eaten across trial, not how similar latencies of individuals are within groups
        
5) Do groups that eat a lot of normal food also eat a lot of novel food? This would be for 1st trial only
    - just looking at correlation between food eaten in first trial of typical food vs. brine shrimp?









1) In latency models, add explicit predictors for the hurdle portion
  - novel food **script made, but some small issues**
  - typical food **done**
  - predator cue **done with partial dataset**
  
**done with this**
7) acceleration of individuals- time between 0 and 1st, 1st and 2nd, 2nd and 3rd
  - take each group of 8, order the latencies, and calculate the time differential between each individual, and look at what that "acceleration" curve looks like
  - make fake groups of 8, sampled from all the individuals in groups of 8, and look at their acceleration curves
  
8) dig into ICC more, and do a more thorough writeup of how it's calculated and what the results show

**not quite ready**
2) Run the model "food_eaten/scripts/model_scripts/prop_typ_eaten_aggress_1_aggbinom"
  - this should be a per-capita chase, or maybe not?
  - could also try time_chases, since this is 
  - could it be an interaction `num_chases*treatment`?

3) for "number of chases" model, should run it once with a per-capita measure of # of chases
    - problem here is can't use nbinom model because we don't have integers any more
    - should plot these data and look at the per-capita rate
    - look into using something like `numchases | trials(group_size)`, like in the aggregated binomial model of % food eaten, this would be instead of a per-capita numchase values
    - but I can't do this as an aggregated binomial, since there's not a "possible number of chases"
    - I already have a Gamma model for `numchases / group_size` and I'm wondering if this isn't the best way to go here
    
4) perhaps the time_chases model could be an aggregated binomial, given that it has an upper limit on chase possibilities?
  - `time_chases | trials(300)`?

    
6) ICC Stuff: look at `sjstats:::icc.brmsfit` code, also look into how to use `brms::predicted` with a single value for a predictor, ie how to use treatment = 2. Check [this link](https://github.com/paul-buerkner/brms/issues/82)

**the high ICC value is a great defense against the idea that Brownian motion is driving higher rates of interaction with food in bigger groups. If this was the case, each group of 8 would be the same, they'd all just be knocking into food more quickly, but the ICC suggests that each group of 8 is MORE similar to itself than to other groups of 8**



# Other To-Do

1) Figure out way to push large model `.rds` files to GitHub or look into other storage method
2) Finish plot-generating functions
3) Clean up pre-analysis data cleaning scripts
4) Make a **separate** post-analysis script for *each* model
5) Shade the parameter density plots with a 95% posterior density interval
