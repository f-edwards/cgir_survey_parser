#### models.R
#### project: cgir_survey_parser
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#
# log: estimate regression models using 
# imputed survey data, depends on process_imputed.R
#----------------------------------------

library(tidyverse)
library(mice)
library(broom.mixed)
dat<-read_csv("./data/imputed.csv")

mods_out<-list()
for(i in 1:max(dat$.imp)){
  temp<-dat %>% 
    filter(.imp==i)
  mods_out[[i]]<-lmer(scale(ParentalStressTotal) ~
                        GROUP:factor(Wave) + County + 
                        (1|PIN), data = temp)
}

summary(pool(mods_out))
