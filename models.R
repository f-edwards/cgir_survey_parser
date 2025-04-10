#### TITLE 
#### project: 
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: 
#
#----------------------------------------

library(tidyverse)
library(lme4)
library(brms)
options(mc.cores = parallel::detectCores())

dat<-read_csv("./data/imputed.csv")

# push into list for brm_multiple fitting
dat_list<-list()
for(i in 1:max(dat$.imp)){
  dat_list[[i]]<-dat %>% 
    filter(.imp==i)
}

# two models to consider
# one: t * w, each wave a time unit
# two: 

# model outcomes in order
# come back to categoricals and non-index vars

m_MissSchool<-brm_multiple(ChildMissSchool_mn ~ 
                      GROUP * factor(Wave) +
                      (1|PIN) + (1|County),
                    data = dat_list)

### m1 as pooled treatment period + pre + post vs control

