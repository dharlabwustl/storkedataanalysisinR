package_list<-c("labelled",   # labeling data
"rstatix",    # summary statistics
"ggpubr",     # convenient summary statistics and plots
"GGally"  ,   # advanced plot
"car",        # useful for anova/wald test
"Epi" ,       # easy getting CI for model coef/pred
"lme4"  ,     # linear mixed-effects models
"lmerTest"  , # test for linear mixed-effects models
"emmeans" ,   # marginal means
"multcomp",   # CI for linear combinations of model coef
"geepack",    # generalized estimating equations
"ggeffects",  # marginal effects, adjusted predictions
"gt",         # nice tables

"tidyverse")
for (x in package_list){
  tryCatch(
    expr = {
    library(x)
}
,
error = function(e){ 
  install.packages(x)
  print("FAILED AT fill_cohort_name_in_a_numeric_column")
  # (Optional)
  # Do this if an error is caught...
},
warning = function(w){
  # (Optional)
  # Do this if a warning is caught...
},
finally = {
  # (Optional)
  # Do this at the end before quitting the tryCatch structure...
}
  )
}