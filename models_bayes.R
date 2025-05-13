#### models.R
#### project: cgir_survey_parser
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#
# log: estimate regression models using 
# imputed survey data, depends on process_imputed.R
#----------------------------------------

library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores())

dat<-read_csv("./data/imputed.csv")

# push into list for brm_multiple fitting
dat_list<-list()
for(i in 1:max(dat$.imp)){
  dat_list[[i]]<-dat %>% 
    filter(.imp==i)
}
# weakly informative priors
priorsN<-c(
  prior(normal(0,1), class = "b"),
  prior(normal(0,1), class = "Intercept"))
# function to estimate Normal likelihood model 
estimate_Normal<-function(x, outcome, family){
  f<-as.formula(
    paste(outcome,
          "factor(Wave) + GROUP:factor(Wave) + County + (1|PIN)",
          sep = "~"))
  m0<-brm_multiple(f,
            data = x, 
            family = family,
            prior = priorsN,
            iter = 1e4,
            cores = 8)
  return(m0)
}

# list outcomes for Normal likelihood
outcomesNormal<-names(dat)[34:48]
# add scale to z transform
outcomesNormal<-paste("scale(", 
                      outcomesNormal,
                      ")",
                      sep = "")

# fit models to normalized outcomes
for(i in 1:length(outcomesNormal)){
  outcome<-outcomesNormal[i]
  family<-"Gaussian"
  modelsNormal<-estimate_Normal(
    dat_list,
    outcome,
    family)
  filename <- paste("./models/", i, "Models.RDS", sep = '')
  saveRDS(modelsNormal, file = filename)
  gc()
}


## consider categorical outcomes case by case
