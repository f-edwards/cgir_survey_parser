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
library(miceadds)

dat<-read_csv("./data/cgir_NY_ocfs_FEClean04225.csv")
### drop vars not imputed (except PIN for join)


#### MISC CLEANING FROM APPX.RMD
dat<-dat %>% 
  mutate(GROUP = factor(GROUP),
         County = factor(County),
         Wave = factor(Wave),
         Gender = factor(Gender),
         Ethnicity = factor(Ethnicity),
         MaritalStat = factor(MaritalStat),
         Race = factor(Race),
         Edu = factor(Edu),
         LangHome = factor(LangHome),
         SexOrient = factor(
           case_when(
             SexOrient == "Straight or Heterosexual" ~ "Straight or Heterosexual",
             SexOrient != "Straight or Heterosexual" ~ "Not Straight or Heterosexual"
           ))) %>% 
  mutate(Hsg_Stat = ifelse(Hsg_Stat == "Rent-to-own", "Renter", Hsg_Stat)) %>% 
  mutate(across(c(Hsg_Stat, Hsg_Qual, Hsg_NeighQual), as.factor))%>% 
  rowwise() %>% 
  mutate(ChildMissSchool_mn = mean(c(
    ChildMissSchool1,ChildMissSchool2,
    ChildMissSchool2, ChildMissSchool3,
    ChildMissSchool4, ChildMissSchool5,
    ChildMissSchool6),
    na.rm=T))

excluded<-c("SurveyStart", "DOB", 
            "PartnerStat", "PartnerGender", "EduOther",
            "LangOther", "Hsg_StatOther",
            "RentAssist", "Hsg_Current", "Util",
            "RelativeCare", "NonRelativeCare", "CenterCare",
            "MedicalAccess_t", "ChildMissSchool1",
            "ChildMissSchool2", "ChildMissSchool3",
            "ChildMissSchool4", "ChildMissSchoo5",
            "ChildMissSchool6",
            "Fin_", "HH_Children",
            "Services")
excludeds<-paste(excluded, collapse = "|")
excludeds<-names(dat)[grep(excludeds, names(dat))]
dat<-dat %>% 
  select(-one_of(excludeds), Wave)

### RE WORK NOW THAT I DONT HAVE THE BLOCKS 
### SET DEMOGRAPHICS
temp_demog<-dat %>% 
  select(PIN:SexOrient) %>% 
  filter(Wave == 1) %>% 
  select(-Wave) 

temp_demog<-temp_demog %>% 
  mutate(
    Gender = factor(case_when(
      Gender == "Male" ~ "Male",
      Gender != "Male" ~ "Female or other")),
    LangHome = factor(case_when(
      LangHome == "English" ~ "English",
      LangHome != "English" ~ "Not English"
    )),
    Edu = factor(case_when(
      Edu == "Elementary school (through Grade 5)" | 
        Edu == "No formal education" ~ "Low or no formal education",
      T ~ Edu
    )),
    Race = factor(case_when(
      Race == "HPI" ~ "API",
      Race == "Asian" ~ "API",
      T ~ Race
    ))
  ) 

dat<-dat %>% 
  select(PIN, Wave, SF_HealthGen:ChildMissSchool_mn)

# MOD HERE TO APPEND BLOCK BY BLOCK ---------------------------------------

# 
# ### add childmissschool_mn
temp<-temp_demog %>% 
  left_join(dat %>%
      pivot_wider(id_cols = PIN, names_from = Wave, names_sep = "_W",
                  values_from = SF_HealthGen:ChildMissSchool_mn))


### use names index position
demog = names(temp)[which(names(temp)=="GROUP") : 
                     which(names(temp)=="SexOrient")]



### redefine variable blocks
varBlocks=list(
  demog = names(temp)[which(names(temp)=="PIN") : 
                        which(names(temp)=="SexOrient")],
  sf = names(temp)[which(names(temp)=="SF_HealthGen_W1") : 
                which(names(temp)=="SF_HealthState_4_W4")],
  k10 = names(temp)[which(names(temp)=="K10_1_W1") : 
                    which(names(temp)=="K10phys_W4")],
  ps = names(temp)[which(names(temp)=="PS_2_W1") : 
                   which(names(temp)=="PS_10_W4")],
  hsg = names(temp)[which(names(temp)=="Hsg_Stat_W1") : 
                      which(names(temp)=="Hsg_MortDefault_W4")],
  chaos = names(temp)[which(names(temp)=="Chaos_1_W1") : 
                        which(names(temp)=="Chaos_15_W4")],
  am = names(temp)[which(names(temp)=="AM_1_W1") : 
                     which(names(temp)=="AM_24_W4")],
  hhfi = names(temp)[which(names(temp)=="HHFI_Insuf_W1") : 
                       which(names(temp)=="HHFI_Less_W4")],
  ah = names(temp)[which(names(temp)=="AH_1_W1") : 
                     which(names(temp)=="AH_12_W4")],
  fw = names(temp)[which(names(temp)=="FW_1_1_W1") : 
                     which(names(temp)=="FW_2_4_W4")],
  ins = names(temp)[which(names(temp)=="ParentInsurance_W1") : 
                      which(names(temp)=="MedicalAccess_W4")],
  pss = names(temp)[which(names(temp)=="ParentalStressScale_1_W1") : 
                      which(names(temp)=="ParentalStressScale_18_W4")],
  scl = names(temp)[which(names(temp)=="ChildMissSchool_mn_W1") : 
                      which(names(temp)=="ChildMissSchool_mn_W4" )])
### EVAL THE RIGHT HAND OBJECT, REMOVE
## UNNECESSARY COLUMNS, ASSIGN NEW BLOCK POS
### DEMOG IN ALL

temp_1<-temp %>% 
  select(any_of(c(varBlocks$demog, varBlocks$sf)))

temp_2<-temp %>% 
  select(any_of(c(varBlocks$demog, varBlocks$k10,
                  varBlocks$ps)))

temp_3<-temp %>% 
  select(any_of(c(varBlocks$demog, varBlocks$hsg,
                  varBlocks$chaos)))

temp_4<-temp %>% 
  select(any_of(c(varBlocks$demog, varBlocks$am,
                  varBlocks$hhfi)))

temp_5<-temp %>% 
  select(any_of(c(varBlocks$demog, varBlocks$ah,
                  varBlocks$fw)))

temp_6<-temp %>% 
  select(any_of(c(varBlocks$demog, varBlocks$ins,
                  varBlocks$pss, varBlocks$scl)))

imp1<-mice(temp_1,
          m = 10,
          maxit = 10,
          method = "pmm")

imp2<-mice(temp_2,
           m = 10,
           maxit = 10,
           method = "pmm")

imp3<-mice(temp_3,
           m = 10,
           maxit = 10,
           method = "pmm")

imp4<-mice(temp_4,
           m = 10,
           maxit = 10,
           method = "pmm")

imp5<-mice(temp_5,
           m = 10,
           maxit = 10,
           method = "pmm")

imp6<-mice(temp_6,
           m = 10,
           maxit = 10,
           method = "pmm")
## this didnt work....
### go block by block as in the rmd appx, then bind

saveRDS(imp1, file = "./data/imputed1.RDS")
saveRDS(imp2, file = "./data/imputed2.RDS")
saveRDS(imp3, file = "./data/imputed3.RDS")
saveRDS(imp4, file = "./data/imputed4.RDS")
saveRDS(imp5, file = "./data/imputed5.RDS")
saveRDS(imp6, file = "./data/imputed6.RDS")

