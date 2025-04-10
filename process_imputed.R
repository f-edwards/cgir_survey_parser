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
library(mice)

imp1<-readRDS("./data/imputed1.RDS")
imp2<-readRDS("./data/imputed2.RDS")
imp3<-readRDS("./data/imputed3.RDS")
imp4<-readRDS("./data/imputed4.RDS")
imp5<-readRDS("./data/imputed5.RDS")
imp6<-readRDS("./data/imputed6.RDS")


# complete and join -------------------------------------------------------

imp1<-mice::complete(imp1, action = "long")
imp2<-mice::complete(imp2, action = "long")
imp3<-mice::complete(imp3, action = "long")
imp4<-mice::complete(imp4, action = "long")
imp5<-mice::complete(imp5, action = "long")
imp6<-mice::complete(imp6, action = "long")

# retain demographics from imp1, drop others
imp_join<-imp1 %>% 
  left_join(imp2 %>% 
              select(K10_1_W1:.id)) %>% 
  left_join(imp3 %>% 
              select(Hsg_Stat_W1:.id)) %>% 
  left_join(imp4 %>% 
              select(AM_1_W1:.id)) %>% 
  left_join(imp5 %>% 
              select(AH_1_W1:.id)) %>% 
  left_join(imp6 %>% 
              select(ParentInsurance_W1:.id))

# # visualize diagnostics ---------------------------------------------------
# run these into pdfs that can be knit

### Two stage pivot, make super long, then mid wide
imp_long<-imp_join %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = -c(PIN:SexOrient, .imp:.id),
               names_sep = "_W",
               names_to = c("variable", "Wave")) %>% 
  pivot_wider(id_cols = c(PIN:.id, "Wave"),
              names_from = "variable",
              values_from = "value")

# COMPUTE INSTRUMENTS
# sf36
# https://www.rand.org/health-care/surveys_tools/mos/36-item-short-form/scoring.html
# omits: item 2, 17 - 32
# produce available scales: physical functioning, role limitations, general health
imp_long<-imp_long %>% 
  mutate(across(SF_HealthGen:SF_HealthState_4, as.numeric)) %>% 
  mutate(SF_PhysicalFunctioning = 
           (SF_HealthLim_1 + SF_HealthLim_2 + SF_HealthLim_3 + 
              SF_HealthLim_4 + SF_HealthLim_5 + SF_HealthLim_6 + 
              SF_HealthLim_7 + SF_HealthLim_8 + SF_HealthLim_9 + 
              SF_HealthLim_10)/10,
         SF_RoleLimitationsPhys = 
           (SF_Phys_1 + SF_Phys_2 + SF_Phys_3 + SF_Phys_4)/4,
         SF_GenHealth = 
           (SF_HealthGen + SF_HealthState_1 + SF_HealthState_2 + 
              SF_HealthState_3 + SF_HealthState_4) / 5) 
### kessler 10
imp_long<-imp_long %>% 
  mutate(across(K10_1:K10phys, as.numeric)) %>% 
  mutate(Kessler_Score = rowSums(across(K10_1:K10_10)))

# perceived stress
imp_long<-imp_long %>% 
  mutate(across(PS_2:PS_10, as.numeric),
         PS_ShortTotal = rowSums(across(PS_2:PS_10)))

# chaos
imp_long<-imp_long %>% 
  mutate(across(Chaos_1:Chaos_15, as.numeric),
         Chaos_Score = rowSums(across(Chaos_1:Chaos_15)))

# adult mattering
imp_long<-imp_long %>% 
  mutate(across(AM_1:AM_24, as.numeric),
         AM_Awareness = 
           AM_1 + AM_4 + AM_7 + AM_9 + AM_13 + AM_16 + AM_18 + AM_21,
         AM_Importance = 
           AM_3 + AM_6 + AM_8 + AM_11 + AM_14 + AM_15 + 
           AM_19 + AM_20 + AM_22 + AM_23,
         AM_Reliance = 
           AM_2 + AM_5 + AM_10 + AM_12 + AM_17 + AM_24)
# adult hope
imp_long<-imp_long %>% 
  mutate(across(AH_1:AH_12, as.numeric),
         AH_Agency = AH_2 + AH_9 + AH_10 + AH_12,
         AH_Pathway = AH_1 + AH_4 + AH_6 + AH_8,
         AH_Total = AH_Agency + AH_Pathway)
# financial well-being
imp_long<-imp_long %>% 
  mutate(across(FW_1_1:FW_2_4, as.numeric),
         Total_FW_Score = FW_1_1 + FW_1_2 + FW_1_3 + 
           FW_1_4 + FW_1_5 + FW_1_6 + FW_2_1 + 
           FW_2_2 + FW_2_3 + FW_2_4)
# parental stress
imp_long<-imp_long %>% 
  mutate(across(ParentalStressScale_1:ParentalStressScale_18, as.numeric),
         ParentalStressTotal = rowSums(across(ParentalStressScale_1:ParentalStressScale_18)))

# condense for export
imp_long<-imp_long %>% 
  select(.imp, Wave, PIN:SexOrient,
         Hsg_Stat:Hsg_MortDefault,
         HHFI_Insuf:HHFI_Less,
         ParentInsurance:MedicalAccess,
         ChildMissSchool_mn,
         SF_PhysicalFunctioning:ParentalStressTotal)
# export ------------------------------------------------------------------

write_csv(imp_long, file = "./data/imputed.csv")
