# A running to-do list for me

## Updated list as of 7/4/19

1) Finish all the hurdle models
    - fit them and summary plots and outputs

2) Acceleration model for groups of 2 and 4
    - Already completed for 8
    - Do for both diff between individuals and diff from 1st individual
    - Generate summaries + plots
    
3) Bayesian ICC stuff
    - note that this may not work with varying slopes models
    - **may have to redo models w/o varying slopes to get at this**
    - need to **bulletproof** arguments here

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
