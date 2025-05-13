# read.r ------------------------------------------------------------------
# author: frank.edwards@rutgers.edu
# created: 11/27/24
# updated: 3/26/25
# info: read CGIR qualtrics survey output and harmonize variables
# log: parse all w0 instruments. 
# pre-registered protocol: https://clinicaltrials.gov/study/NCT05857553

# packages ----------------------------------------------------------------
library(tidyverse)

# import ------------------------------------------------------------------
# use Age to count for Children2
w1<-read_csv("./data/w1_clean.csv",
             col_types = cols(.default = "c"))%>% 
  mutate(Children2 =  as.character(
    6 - 
      (is.na(Child1Age) + 
         is.na(Child2Age) + 
         is.na(Child3Age) + 
         is.na(Child4Age) + 
         is.na(Child5Age) + 
         is.na(Child6Age))))

w2<-read_csv("./data/w2_clean.csv",
             col_types = cols(.default = "c"))

w3<-read_csv("./data/w3_clean.csv",
             col_types = cols(.default = "c")) 

w4<-read_csv("./data/w4_clean.csv",
             col_types = cols(.default = "c")) 

# make index crosswalk ----------------------------------------------------
## use w1 to make baseline demographics to hold constant
w1 <- w1 |> 
  mutate(DOB = paste(
    Q6_1, Q6_2, Q6_3)) |> 
  select(-Q6_1, -Q6_2, -Q6_3)

xwalk <- w1 |> 
  select(PIN, GROUP, County, Consent, DOB, Race, RaceOther, Ethnicity)


# Bind for long -----------------------------------------------------------
### use xwalk to left join all, retain unit and item non-response explicitly
# unit non-response recorded with InWave
w1_c <- w1 |> 
  mutate(Wave = 1) |> 
  mutate_all(as.character)

w2_c <- xwalk |> 
  left_join(w2) |> 
  mutate(Wave = 2,
         InWave = ifelse(is.na(InWave), F, InWave))|> 
  mutate_all(as.character)

w3_c <- xwalk |> 
  left_join(w3) |> 
  mutate(Wave = 3,
         InWave = ifelse(is.na(InWave), F, InWave))|> 
  mutate_all(as.character)

w4_c <- xwalk |> 
  left_join(w4) |> 
  mutate(Wave = 4,
         InWave = ifelse(is.na(InWave), F, InWave))|> 
  mutate_all(as.character)

dat_long<-w1_c |> 
  bind_rows(
    w2_c,
    w3_c,
    w4_c)

# parse instruments -------------------------------------------------------
# in codebook order: 

# age
dat_long<-dat_long %>% 
  mutate(Age_c = floor(interval(mdy(DOB), mdy_hm(StartDate)) %/%  years(1)))

# gender
dat_long<-dat_long |> 
  mutate(Gender_s = Gender,
         Gender = case_when(Gender == "Male" ~ 1,
                            Gender == "Female" ~ 2,
                            Gender == "Other (Non-binary, agender, gender-fluid, etc):" ~ 3))
## HH comp
dat_long<-dat_long |> 
  rename(HH_Size = HHSize,
         HH_Children = Children) |> 
  mutate(HH_Children = case_when(HH_Children == "Yes" ~ 1,
                                 HH_Children == "No" ~ 0)) |> 
  rename(HH_NumChild_c = Children2) |> 
  rename(HH_ChildAge1 = Child1Age,
         HH_ChildAge2 = Child2Age,
         HH_ChildAge3 = Child3Age,
         HH_ChildAge4 = Child4Age,
         HH_ChildAge5 = Child5Age,
         HH_ChildAge6 = Child6Age,
         HH_AdultsAll = AdultsAll,
         HH_AdultsSome = AdultsSome)

# Marital / partner stat
dat_long <- dat_long |> 
  rename(MaritalStat = MaritalStatus,
         PartnerStat = PartnerStatus) |> 
  mutate(
    MaritalStat = case_when(
      MaritalStat == "Single" ~ 3,
      MaritalStat == "Partnered/In a Relationship" ~ 2,
      MaritalStat == "Married" ~ 1),
    PartnerStat = case_when(
      PartnerStat == "Yes" ~ 1,
      PartnerStat == "No" ~ 0),
    PartnerGender = case_when(
      PartnerGender == "Male" ~ 1,
      PartnerGender == "Female" ~ 2,
      PartnerGender == "Other (Non-binary, agender, gender-fluid, etc):" ~ 3,
    )
  )

# race/ethnicity
dat_long <- dat_long |> 
  mutate(Ethnicity = case_when(
    Ethnicity == "Yes" ~ 1,
    Ethnicity == "No" ~ 0),
    Race_White = Race == "White",
    Race_Black = Race == "Black or African American",
    Race_AIAN = Race == "American Indian or Alaska Native or First Nations",
    Race_Asian = Race == "Asian",
    Race_HawPI = Race == "Native Hawaiian and Other Pacific Islander",
    Race_TwoOrMore = (Race == "Two or more races") | 
      str_detect(Race, ","),
    Race_Other = Race == "Some other race:") |> 
  rename(RaceOtherSpecify = RaceOther)

# education
dat_long <- dat_long |> 
  rename(Edu_s = Edu) |> 
  mutate(Edu = case_when(
    Edu_s == "No formal education" ~ 1,
    Edu_s == "Elementary school (through Grade 5)" ~ 2,
    Edu_s == "Middle school (6th grade - 9th grade)" ~ 3,
    Edu_s == "High school (10th - 12th grades)" ~ 4,
    Edu_s == "High school diploma" ~ 5,
    Edu_s == "GED (diploma equivalency test)" ~ 6,
    Edu_s == "Trade or technical school" ~ 7,
    Edu_s == "Associate's Degree (2-year college degree)" ~ 8,
    Edu_s == "Some college" ~ 9,
    Edu_s == "Bachelor's Degree (4-year college degree)" ~ 10,
    Edu_s == "Other post-graduate degree" ~ 11,
    Edu_s == "Other education choice not listed" ~ 12))

# language
dat_long <- dat_long |> 
  mutate(
    LangOther = ifelse(
      !LangHome%in%c("Spanish", "English"),
      LangHome,
      NA),
    LangHome = case_when(
      LangHome == "Spanish" ~ 2,
      LangHome == "English" ~ 1,
      is.na(LangHome) ~ NA,
      !LangHome%in%c("Spanish", "English") ~ 99))

# sexual orientation
dat_long <- dat_long |> 
  rename(SexOrient = Orientation) |> 
  mutate(SexOrient_Straight = str_detect(SexOrient, "Straight"),
         SexOrient_Gay = str_detect(SexOrient, "Gay"),
         SexOrient_Queer = str_detect(SexOrient, "Queer"),
         SexOrient_Lesbian = str_detect(SexOrient, "Lesbian"),
         SexOrient_Bisexual = str_detect(SexOrient, "Bisexual"),
         SexOrient_Questioning = str_detect(SexOrient, "Questioning"))

# - insurance 
dat_long<-dat_long %>% 
  mutate(ParentInsurance = ParentInsurance == "Yes",
         ChildHealthInsurance = ChildHealthInsurance == "No",
         ChildInsuranceNo = as.numeric(ifelse(
           is.na(ChildInsuranceNo),
           0,
           ChildInsuranceNo))) 

# - SF36
dat_long<-dat_long %>% 
  rename(SF_HealthGen = HealthGeneral,
         SF_HealthLim_1 = HealthLimits_1,
         SF_HealthLim_2 = HealthLimits_2,
         SF_HealthLim_3 = HealthLimits_3,
         SF_HealthLim_4 = HealthLimits_4,
         SF_HealthLim_5 = HealthLimits_5,
         SF_HealthLim_6 = HealthLimits_6,
         SF_HealthLim_7 = HealthLimits_7,
         SF_HealthLim_8 = HealthLimits_8,
         SF_HealthLim_9 = HealthLimits_9,
         SF_HealthLim_10 = HealthLimits_10,
         SF_Phys_1 = PhysicalProbs_1,
         SF_Phys_2 = PhysicalProbs_2,
         SF_Phys_3 = PhysicalProbs_3,
         SF_Phys_4 = PhysicalProbs_4,
         SF_HealthState_1 = HealthStatements_1,
         SF_HealthState_2 = HealthStatements_2,
         SF_HealthState_3 = HealthStatements_3,
         SF_HealthState_4 = HealthStatements_4) %>% 
  mutate(
    SF_HealthGen= case_when(
      SF_HealthGen== "Excellent" ~ 100,
      SF_HealthGen== "Very good" ~ 75,
      SF_HealthGen== "Good" ~ 50,
      SF_HealthGen== "Fair" ~ 25,
      SF_HealthGen== "Poor" ~ 0,
      is.na(SF_HealthGen) ~ NA),
    across(SF_HealthLim_1:SF_HealthLim_10,
           ~ case_when(
             . == "Yes, limited a lot" ~ 0,
             . == "Yes, limited a little" ~ 50,
             . == "No, not limited at all" ~ 100,
             is.na(.) ~ NA)),
    across(SF_Phys_1:SF_Phys_4,
           ~ case_when(
             . == "Yes" ~ 0,
             . == "No" ~ 100,
             is.na(.) ~ NA)),
    SF_HealthState_1 = case_when(
      SF_HealthState_1 == "Definitely True" ~ 0,
      SF_HealthState_1 == "Mostly true" ~ 25,
      SF_HealthState_1 == "Don't know" ~ 50,
      SF_HealthState_1 == "Mostly false" ~ 75,
      SF_HealthState_1 == "Definitely false" ~ 100,
      is.na(SF_HealthState_1) ~ NA),
    SF_HealthState_2 = case_when(
      SF_HealthState_2 == "Definitely True" ~ 100,
      SF_HealthState_2 == "Mostly true" ~ 75,
      SF_HealthState_2 == "Don't know" ~ 50,
      SF_HealthState_2 == "Mostly false" ~ 25,
      SF_HealthState_2 == "Definitely false" ~ 0,
      is.na(SF_HealthState_2) ~ NA),
    SF_HealthState_3 = case_when(
      SF_HealthState_3 == "Definitely True" ~ 0,
      SF_HealthState_3 == "Mostly true" ~ 25,
      SF_HealthState_3 == "Don't know" ~ 50,
      SF_HealthState_3 == "Mostly false" ~ 75,
      SF_HealthState_3 == "Definitely false" ~ 100,
      is.na(SF_HealthState_3) ~ NA),
    SF_HealthState_4 = case_when(
      SF_HealthState_4 == "Definitely True" ~ 100,
      SF_HealthState_4 == "Mostly true" ~ 75,
      SF_HealthState_4 == "Don't know" ~ 50,
      SF_HealthState_4 == "Mostly false" ~ 25,
      SF_HealthState_4 == "Definitely false" ~ 0,
      is.na(SF_HealthState_4) ~ NA)) |> 
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

# - Kessler 10+
dat_long<-dat_long %>%
  mutate(across(K10_1:K10_10,
                ~ case_when(
                  . == "None of the time" ~ 1,
                  . == "A little of the time" ~ 2,
                  . == "Some of the time" ~ 3,
                  . == "Most of the time" ~ 4,
                  . == "All of the time" ~ 5,
                  is.na(.) ~ NA)),
         K10physical_1 = case_when(
           K10physical_1 == "None of the time" ~ 1,
           K10physical_1 == "A little of the time" ~ 2,
           K10physical_1 == "Some of the time" ~ 3,
           K10physical_1 == "Most of the time" ~ 4,
           K10physical_1 == "All of the time" ~ 5)) %>% 
  rename(K10phys = K10physical_1,
         K10med = K10med_1) |> 
  mutate(Kessler_Score = rowSums(across(K10_1:K10_10)))

# - Perceived Stress Scale
# 0 - never, 1 - almost never, 2 - sometimes, 3 - fairly often, 4 - very often
# reverse code 4, 5, 7, 8
# retain 4 qs in survey: q2 (1); q4 (2); q5 (3); q10 (4)

dat_long<-dat_long %>%
  mutate(across(c(PerceivedStress_1, PerceivedStress_4),
                ~ case_when(
                  . == "Never" ~ 0,
                  . == "Almost Never" ~ 1,
                  . == "Sometimes" ~ 2,
                  . == "Fairly Often" ~ 3,
                  . == "Very Often" ~ 4,
                  is.na(.) ~ NA)),
         across(c(PerceivedStress_2, PerceivedStress_3),
                ~ case_when(
                  . == "Never" ~ 4,
                  . == "Almost Never" ~ 3,
                  . == "Sometimes" ~ 2,
                  . == "Fairly Often" ~ 1,
                  . == "Very Often" ~ 0,
                  is.na(.) ~ NA))) %>% 
  rename(PS_2 = PerceivedStress_1,
         PS_4 = PerceivedStress_2,
         PS_5 = PerceivedStress_3,
         PS_10 = PerceivedStress_4) |> 
  mutate(across(PS_2:PS_10, as.numeric),
         PS_ShortTotal = rowSums(across(PS_2:PS_10)))

# - financial
dat_long <- dat_long %>% 
  rename(Fin_Emergency = FinEmergency,
         Fin_LateFee = LateFee,
         Fin_CredScore = CreditScore,
         Fin_Emp = Employ,
         Fin_Occ = Occupation,
         Fin_Edu = Student,
         Fin_IncSrcHH = IncSrcHH) %>% 
  mutate(Fin_Emergency = case_when(
    Fin_Emergency == "Pay using cash" ~ 1,
    Fin_Emergency == "Pay using a credit card paid in full" ~ 2,
    Fin_Emergency == "Pay using debt" ~ 3,
    Fin_Emergency == "Pay by selling something" ~ 4,
    Fin_Emergency == "I could not pay." ~ 5,
    Fin_Emergency == "Pay by borrowing money from a family member or friend" ~ 6),
    Fin_Emergency_exp_c = as.numeric(Fin_Emergency<=2),
    Fin_LateFee = as.numeric(Fin_LateFee=="Yes"),
    Fin_CredScore = 
      case_when(
        Fin_CredScore == "Very bad" ~ 1,
        Fin_CredScore == "Bad" ~ 2,
        Fin_CredScore == "About average" ~ 3,
        Fin_CredScore == "Good" ~ 4,
        Fin_CredScore == "Very good" ~ 5,
        Fin_CredScore == "I don't have a credit score." ~ 6,
        Fin_CredScore == "I don't know." ~ 7),
    Fin_Emp = case_when(
      Fin_Emp == "Employed full time" ~ 1,
      Fin_Emp == "Employed part time" ~ 2,
      Fin_Emp == "Unemployed looking for work" ~ 3,
      Fin_Emp == "Unemployed not looking for work" ~ 4,
      Fin_Emp == "Retired" ~ 5,
      Fin_Emp == "I work seasonally" ~ 6,
      Fin_Emp == "Disabled" ~ 7,
      Fin_Emp == "Stay at home parent or caregiver" ~ 8,
      Fin_Emp == "I am a student and do not work." ~ 9),
    Fin_Occ = case_when(
      Fin_Occ == "Private for-profit company" ~ 1,
      Fin_Occ == "Non-profit organization" ~ 2,
      Fin_Occ == "Government worker" ~ 3,
      Fin_Occ == "Self-employed" ~ 4,
      Fin_Occ == "Self-employed in the gig economy (For example Uber, Lyft, or Door Dash)" ~ 5,
      Fin_Occ == "Unpaid stay-at home caregiver" ~ 6,
      Fin_Occ == "Business owner"  ~ 7),
    Fin_Edu = case_when(
      Fin_Edu == "I am not currently a student." ~ 1,
      Fin_Edu == "I am a full-time student." ~ 2,
      Fin_Edu == "I am a part-time student." ~ 3,
      Fin_Edu == "I am in the process of applying to college or trade school." ~ 4),
    Employment_c = case_when(
      #FT
      Fin_Emp == 1 & (Fin_Occ != 4 & Fin_Occ != 5 & Fin_Occ != 6) ~ 1,
      #PT
      (Fin_Emp == 2 | Fin_Emp == 6) & (Fin_Occ != 4 & Fin_Occ != 5 & Fin_Occ != 6) ~ 2,
      #stayathome
      Fin_Emp == 8 | ((Fin_Emp == 1 | Fin_Emp == 2) & (Fin_Occ == 6)) ~ 3,
      #selfemployed
      (Fin_Emp == 1 | Fin_Emp == 2) & (Fin_Occ == 4 | Fin_Occ == 7) ~ 4,
      #gig
      (Fin_Emp == 1 | Fin_Emp == 2) & (Fin_Occ == 5) ~ 5,
      #retired/disabled
      (Fin_Emp == 5 | Fin_Emp == 7) ~ 6,
      #student
      Fin_Emp == 9 ~ 7,
      #unemplabforce
      Fin_Emp == 3 ~ 8,
      #unempNolabforce
      Fin_Emp == 4 ~ 9),
    Student_c = as.numeric((Fin_Emp == 9) | (Fin_Edu == 2) | (Fin_Edu == 3)))|> 
  mutate(Fin_IncSrcHH_1 = 
           case_when(
             is.na(Fin_IncSrcHH) ~ NA,
             str_detect("I bring", Fin_IncSrcHH) == T ~
               "I bring money into the household",
             T ~ ""),
         Fin_IncSrcHH_2 = 
           case_when(
             is.na(Fin_IncSrcHH) ~ NA,
             str_detect("My partner or spouse brings money into the household", Fin_IncSrcHH) == T ~
               "My partner or spouse brings money into the household",
             T ~ ""),
         Fin_IncSrcHH_3 = 
           case_when(
             is.na(Fin_IncSrcHH) ~ NA,
             str_detect("Other adults that live with me bring money into the household", Fin_IncSrcHH) == T ~
               "Other adults that live with me bring money into the household",
             T ~ ""),
         Fin_IncSrcHH_4 = 
           case_when(
             is.na(Fin_IncSrcHH) ~ NA,
             str_detect("Other adults that do not live with me contribute money to the household", Fin_IncSrcHH) == T ~
               "Other adults that do not live with me contribute money to the household",
             T ~ ""),
         Fin_IncSrcHH_5 = 
           case_when(
             is.na(Fin_IncSrcHH) ~ NA,
             str_detect("Child Support", Fin_IncSrcHH) == T ~
               "Child Support",
             T ~ ""),
         Fin_IncSrcHH_6 = 
           case_when(
             is.na(Fin_IncSrcHH) ~ NA,
             str_detect("Veteran's Benefits", Fin_IncSrcHH) == T ~
               "Veteran's Benefits",
             T ~ "")) %>% 
  rename(Fin_IncAmtHH_1 = IncAmtHH_16,
         Fin_IncAmtHH_2 = IncAmtHH_18,
         Fin_IncAmtHH_3 = IncAmtHH_1,
         Fin_IncAmtHH_4 = IncAmtHH_5,
         Fin_IncAmtPers_1 = IncAmtPers_1,
         Fin_IncAmtPers_2 = IncAmtPers_4,
         Fin_IncAmtPers_3 = IncAmtPers_5,
         Fin_IncAmtPers_4 = IncAmtPers_6,
         Fin_IncSrcPers_Job1 = IncSrcPers_1,
         Fin_IncSrcPers_Job2 = IncSrcPers_2,
         Fin_IncSrcPers_Job3 = IncSrcPers_3,
         Fin_IncSrcPers_Under = IncSrcPers_4,
         Fin_IncSrcPers_Gift = IncSrcPers_5,
         Fin_IncSrcPers_Loan = IncSrcPers_6,
         Fin_IncSrcPers_TANF = IncSrcPers_14, 
         Fin_IncSrcPers_Disab = IncSrcPers_8,
         Fin_IncSrcPers_SS = IncSrcPers_9, 
         Fin_IncSrcPers_Unemp = IncSrcPers_10,
         Fin_IncSrcPers_Retire = IncSrcPers_12, 
         Fin_IncSrcPers_Other = IncSrcPers_13) |> 
  mutate(Num_jobs_c = case_when(
    !is.na(Fin_IncSrcPers_Job3) & Fin_IncSrcPers_Job3 != 0 ~ 3 ,
    !is.na(Fin_IncSrcPers_Job2) & Fin_IncSrcPers_Job2 !=0 & 
      (is.na(Fin_IncSrcPers_Job3) | Fin_IncSrcPers_Job3 == 0) ~ 2,
    is.na(Fin_IncSrcPers_Job1) &Fin_IncSrcPers_Job1 != 0 & 
      (is.na(Fin_IncSrcPers_Job2) | Fin_IncSrcPers_Job2 == 0) & 
      (is.na(Fin_IncSrcPers_Job3) | Fin_IncSrcPers_Job3 == 0) ~ 1,
    is.na(Fin_IncSrcPers_Job1) & 
      (Employment_c == 1 | Employment_c == 2 | Employment_c == 4 | Employment_c == 5) ~ 1,
    (is.na(Fin_IncSrcPers_Job1) & is.na(Fin_IncSrcPers_Job2) & 
       is.na(Fin_IncSrcPers_Job3)) & (Employment_c == 6 | Employment_c == 8) ~ 0)) |> 
  rename(Fin_Services = Services) %>% 
  mutate(
    Services_SSI = case_when(
      is.na(Fin_Services) ~ NA,
      str_detect("SSI", Fin_Services) == T ~ 1,
      T ~ 0),
    Services_SNAP = case_when(
      is.na(Fin_Services) ~ NA,
      str_detect("SNAP", Fin_Services) == T ~ 1,
      T ~ 0),
    Services_YMCA = case_when(
      is.na(Fin_Services) ~ NA,
      str_detect("YMCA", Fin_Services) == T ~ 1,
      T ~ 0),
    Services_WIC = case_when(
      is.na(Fin_Services) ~ NA,
      str_detect("WIC", Fin_Services) == T ~ 1,
      T ~ 0),
    Services_SSDI = case_when(
      is.na(Fin_Services) ~ NA,
      str_detect("SSDI", Fin_Services) == T ~ 1,
      T ~0)) %>% 
  rename(Fin_Debt = Debt) %>% 
  mutate(
    Fin_Debt_House = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Mortgage", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_Util = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("utility", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_Med = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Medical", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_BkLoan = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Bank", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_Advance = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Payday", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_FamLoan = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("family", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_Other = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Other", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_VehLoan = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Vehicle", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_Cred = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Credit", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_PersStudLoan = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Student loans for myself", Fin_Debt)==T ~ 1,
      T~0),
    Fin_Debt_FamStudLoan = case_when(
      is.na(Fin_Debt) ~ NA,
      str_detect("Student loans for my family members", Fin_Debt)==T ~ 1,
      T~0)) %>% 
  rename(Fin_DebtPay = DebtPay,
         Fin_Savings = Savings) %>% 
  mutate(
    Fin_DebtPay = case_when(
      Fin_DebtPay == "I want to, but I couldn't last month" ~ -99,
      Fin_DebtPay == "Yes" ~ 1,
      Fin_DebtPay == "No" ~ 0),
    Fin_Savings = case_when(
      Fin_Savings == "$0-$50" ~ 1,
      Fin_Savings == "$51-$100" ~ 2,
      Fin_Savings == "$101-$200" ~ 3,
      Fin_Savings == "$201-$300" ~ 4,
      Fin_Savings == "$301-$400" ~ 5,
      Fin_Savings == "$401-$500" ~ 6,
      Fin_Savings == "$501-$1000" ~ 7,
      Fin_Savings == "More than $1000" ~ 8)) %>% 
  rename(Fin_FamHelp = FinFamHelp,
         Fin_HelpFam = FinHelpFam,
         Fin_HelpFamAmt = FinHelpFamAmt,
         Fin_HelpFamRsn = FinHelpFamRsn) %>% 
  mutate(
    Fin_FamHelp = case_when(
      Fin_FamHelp == "I used to, but they cannot help me anymore." ~ -99,
      Fin_FamHelp == "Yes" ~ 1,
      Fin_FamHelp == "No" ~ 0),
    Fin_HelpFam = as.numeric(Fin_HelpFam=="Yes")) |> 
  mutate(
    Fin_HelpFamRsn_1 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("Housing expenses" , Fin_HelpFamRsn) == T ~ 
        "Housing expenses like rent, mortgage, or security deposit",
      T~""),
    Fin_HelpFamRsn_2 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("Monthly bills" , Fin_HelpFamRsn) == T ~ 
        "Monthly bills like phone, utilities, etc.",
      T~""),
    Fin_HelpFamRsn_3 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("Housing expenses" , Fin_HelpFamRsn) == T ~ 
        "Housing expenses like rent, mortgage, or security deposit",
      T~""),
    Fin_HelpFamRsn_4 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("Fines" , Fin_HelpFamRsn) == T ~ 
        "Fines and fees associated with the justice system",
      T~""),
    Fin_HelpFamRsn_5 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("Childcare expenses" , Fin_HelpFamRsn) == T ~ 
        "Childcare expenses",
      T~""),
    Fin_HelpFamRsn_6 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("car repair" , Fin_HelpFamRsn) == T ~ 
        "Transportation expenses like a car repair, bus pass, or car payment",
      T~""),
    Fin_HelpFamRsn_7 = case_when(
      is.na(Fin_HelpFamRsn) ~ NA,
      str_detect("Education expenses" , Fin_HelpFamRsn) == T ~ 
        "Education expenses",
      T~"")) %>% 
  rename(Fin_Volunteer = FinHelpFamTime) %>% 
  mutate(
    Fin_Volunteer_1 = 
      case_when(
        is.na(Fin_Volunteer) ~ NA,
        str_detect("Child care", Fin_Volunteer) == T ~
          "Child care",
        T~""),
    Fin_Volunteer_2 = 
      case_when(
        is.na(Fin_Volunteer) ~ NA,
        str_detect("Transportation", Fin_Volunteer) == T ~
          "Transportation",
        T~""),
    Fin_Volunteer_3 = 
      case_when(
        is.na(Fin_Volunteer) ~ NA,
        str_detect("Taking care of someone who is ill", Fin_Volunteer) == T ~
          "Taking care of someone who is ill",
        T~""),
    Fin_Volunteer_4 = 
      case_when(
        is.na(Fin_Volunteer) ~ NA,
        str_detect("Elder care", Fin_Volunteer) == T ~
          "Elder care",
        T~""),
    Fin_Volunteer_5 = 
      case_when(
        is.na(Fin_Volunteer) ~ NA,
        str_detect("Yard work, home repair, or car repair", Fin_Volunteer) == T ~
          "Yard work, home repair, or car repair",
        T~""),
    Fin_Volunteer_6 = 
      case_when(
        is.na(Fin_Volunteer) ~ NA,
        str_detect("I have not volunteered my time for these tasks", Fin_Volunteer) == T ~
          "I have not volunteered my time for these tasks",
        T~""))

# - Housing

dat_long<-dat_long %>% 
  rename(Hsg_Stat = HsgStatus,
         Hsg_StatOther = HsgStatusOther,
         Hsg_RentAssist = RentAssist) %>% 
  mutate(Hsg_RentAssist = 
           case_when(
             is.na(Hsg_RentAssist) ~ NA,
             Hsg_RentAssist == "Yes" ~ 1,
             T ~ 0)) %>% 
  rename(Hsg_RentAssistType = RentAssistType) %>% 
  mutate(Hsg_RentAssistType_1 = 
           case_when(
             is.na(Hsg_RentAssistType) ~ NA,
             str_detect("Public Housing", Hsg_RentAssistType)==T ~ 
               "Public Housing",
             T ~ ""),
         Hsg_RentAssistType_2 = 
           case_when(
             is.na(Hsg_RentAssistType) ~ NA,
             str_detect("Section 8 Voucher/Housing Choice Voucher", Hsg_RentAssistType)==T ~ 
               "Section 8 Voucher/Housing Choice Voucher",
             T ~ ""),
         Hsg_RentAssistType_3 = 
           case_when(
             is.na(Hsg_RentAssistType) ~ NA,
             str_detect("Landlord-reduced rent because someone in your household works for them or for the super", Hsg_RentAssistType)==T ~ 
               "Landlord-reduced rent because someone in your household works for them or for the super",
             T ~ ""),
         Hsg_RentAssistType_4 = 
           case_when(
             is.na(Hsg_RentAssistType) ~ NA,
             str_detect("Landlord-reduced rent because someone in the household is related to them or to the super", Hsg_RentAssistType)==T ~ 
               "Landlord-reduced rent because someone in the household is related to them or to the super",
             T ~ ""),
         Hsg_RentAssistType_5 = 
           case_when(
             is.na(Hsg_RentAssistType) ~ NA,
             str_detect("I receive free or reduced housing as a part of my job", Hsg_RentAssistType)==T ~ 
               "I receive free or reduced housing as a part of my job",
             T ~ ""),
         Hsg_RentAssistType_6 = 
           case_when(
             is.na(Hsg_RentAssistType) ~ NA,
             str_detect("Other voucher or reduction", Hsg_RentAssistType)==T ~ 
               "Other voucher or reduction",
             T ~ "")
  ) %>% 
  rename(Hsg_Current = CurrentHsg,
         Hsg_Move = Moves,
         Hsg_Qual = HsgQual,
         Hsg_NeighQual = NeighQual,
         Hsg_RentAmt = RentAmt,
         Hsg_UtilAmt = UtilAmt,
         Hsg_Evict = Evict,
         Hsg_MortDefault = MortDefault)

# - CHAOS scale
dat_long<-dat_long %>% 
  mutate(across(c(CHAOS_1, CHAOS_2,
                  CHAOS_4, CHAOS_7,
                  CHAOS_12, CHAOS_14,
                  CHAOS_15),
                ~ case_when(
                  . == "Very much" ~ 1,
                  . == "Somewhat" ~ 2,
                  . == "A little bit" ~ 3,
                  . == "Not at all" ~ 4)),
         across(c(CHAOS_3, CHAOS_5,
                  CHAOS_6, CHAOS_8,
                  CHAOS_9, CHAOS_10,
                  CHAOS_11, CHAOS_13),
                ~ case_when(
                  . == "Very much" ~ 4,
                  . == "Somewhat" ~ 3,
                  . == "A little bit" ~ 2,
                  . == "Not at all" ~ 1))) %>% 
  rename_with(str_to_title, starts_with("CHAOS")) |> 
  mutate(across(Chaos_1:Chaos_15, as.numeric),
         Chaos_Score = rowSums(across(Chaos_1:Chaos_15)))

# - Adult Mattering Scale
dat_long<-dat_long %>%
  # regular 1, 2, 5, 6, 9, 12, 17, 19, 20, 21, 23, 24
  mutate(across(c(
    AdultMattering_1, AdultMattering_2,
    AdultMattering_5, AdultMattering_6, 
    AdultMattering_9, AdultMattering_12, 
    AdultMattering_17, AdultMattering_19, 
    AdultMattering_20, AdultMattering_21,
    AdultMattering_23, AdultMattering_24),
    ~ case_when(
      . == "Strongly disagree" ~ 1,
      . == "Strongly disagree\n1" ~ 1,
      . == "1" ~ 1,
      . == "Disagree" ~ 2,
      . == "Disagree\n2" ~ 2,
      . == "2" ~ 2,
      . == "Neutral" ~ 3,
      . == "Neutral\n3" ~ 3,
      . == "3" ~ 3,
      . == "Agree" ~ 4,
      . == "Agree\n4" ~ 4,
      . == "4" ~ 4,
      . == "Strongly agree" ~ 5,
      . == "Strongly agree\n5" ~ 5,
      . == "5" ~ 5,
      is.na(.) ~ NA))) %>% 
  # reverse 3, 4, 7, 8, 10, 11, 13, 14, 15, 16, 18, 22
  mutate(across(c(
    AdultMattering_3, AdultMattering_4,
    AdultMattering_7, AdultMattering_8, 
    AdultMattering_10, AdultMattering_11, 
    AdultMattering_13, AdultMattering_14, 
    AdultMattering_15, AdultMattering_16,
    AdultMattering_18, AdultMattering_22),
    ~ case_when(
      . == "Strongly disagree" ~ 5,
      . == "Strongly disagree\n1" ~ 5,
      . == "1" ~ 5,
      . == "Disagree" ~ 4,
      . == "Disagree\n2" ~ 4,
      . == "2" ~ 4,
      . == "Neutral" ~ 3,
      . == "Neutral\n3" ~ 3,
      . == "3" ~ 3,
      . == "Agree" ~ 2,
      . == "Agree\n4" ~ 2,
      . == "4" ~ 2,
      . == "Strongly agree" ~ 1,
      . == "Strongly agree\n5" ~ 1,
      . == "5" ~ 1,
      is.na(.) ~ NA))) 
# handle names
pos<-which(grepl("AdultMattering" , names(dat_long)))
nums<-1:24
names(dat_long)[pos]<-paste("AM", nums, sep = "_")
dat_long<-dat_long |> 
  mutate(across(AM_1:AM_24, as.numeric),
         AM_Awareness = 
           AM_1 + AM_4 + AM_7 + AM_9 + AM_13 + AM_16 + AM_18 + AM_21,
         AM_Importance = 
           AM_3 + AM_6 + AM_8 + AM_11 + AM_14 + AM_15 + 
           AM_19 + AM_20 + AM_22 + AM_23,
         AM_Reliance = 
           AM_2 + AM_5 + AM_10 + AM_12 + AM_17 + AM_24)


# - Household Food Insecurity
dat_long<-dat_long %>%
  mutate(across(FoodInsecurity1:FoodInsecurityUtil,
                ~ case_when(
                  . == "Yes" ~ 1,
                  . == "No" ~ 0,
                  is.na(.) ~ NA
                ))) %>% 
  rename(HHFI_Insuf = FoodInsecurity1,
         HHFI_Pref = FoodInsecurity2,
         HHFI_NotPref = FoodInsecurity4,
         HHFI_Less = FoodInsecurity6,
         HHFI_Util = FoodInsecurityUtil)

# - Adult Hope Scale
dat_long<-dat_long %>% 
  mutate(across(AdultHope_1:AdultHope_12,
                ~ case_when(
                  . == "Definitely False" ~ 1,
                  . == "Mostly False" ~ 2,
                  . == "Somewhat False" ~ 3,
                  . == "Slightly False" ~ 4,
                  . == "Slightly True" ~ 5,
                  . == "Somewhat True" ~ 6,
                  . == "Mostly True" ~ 7,
                  . == "Definitely True" ~ 8,
                  is.na(.) ~ NA)))
# handle names
pos<-which(grepl("AdultHope" , names(dat_long)))
nums<-1:12
names(dat_long)[pos]<-paste("AH", nums, sep = "_")

dat_long <- dat_long |> 
  mutate(across(AH_1:AH_12, as.numeric),
         AH_Agency = AH_2 + AH_9 + AH_10 + AH_12,
         AH_Pathway = AH_1 + AH_4 + AH_6 + AH_8,
         AH_Total = AH_Agency + AH_Pathway)

# - Financial Well-Being Scale
dat_long<-dat_long %>% 
  mutate(across(c(FinWell1Statement_1:FinWell1Statement_2,
                  FinWell1Statement_4),
                ~ case_when(
                  . == "Completely" ~ 4,
                  . == "Very well" ~ 3,
                  . == "Somewhat" ~ 2,
                  . == "Very little" ~ 1,
                  . == "Not at all" ~ 0,
                  is.na(.) ~ NA)),
         across(c(FinWell1Statement_3,FinWell1Statement_5,
                  FinWell1Statement_6),
                ~ case_when(
                  . == "Completely" ~ 0,
                  . == "Very well" ~ 1,
                  . == "Somewhat" ~ 2,
                  . == "Very little" ~ 3,
                  . == "Not at all" ~ 4,
                  is.na(.) ~ NA)),
         across(c(FinWell2Statment_1, FinWell2Statment_3,
                  FinWell2Statment_4),
                ~ case_when(
                  . == "Always" ~ 0,
                  . == "Often" ~ 1,
                  . == "Sometimes" ~ 2,
                  . == "Rarely" ~ 3,
                  . == "Never" ~ 4,
                  is.na(.) ~ NA)),
         FinWell2Statment_2 =
           case_when(FinWell2Statment_2 == "Always" ~ 4,
                     FinWell2Statment_2 == "Often" ~ 3,
                     FinWell2Statment_2 == "Sometimes" ~ 2,
                     FinWell2Statment_2 == "Rarely" ~ 1,
                     FinWell2Statment_2 == "Never" ~ 0,
                     is.na(FinWell2Statment_2) ~ NA)) %>% 
  rename(FW_1_1 = FinWell1Statement_1,
         FW_1_2 = FinWell1Statement_2,
         FW_1_3 = FinWell1Statement_3,
         FW_1_4 = FinWell1Statement_4,
         FW_1_5 = FinWell1Statement_5,
         FW_1_6 = FinWell1Statement_6,
         FW_2_1 = FinWell2Statment_1,
         FW_2_2 = FinWell2Statment_2,
         FW_2_3 = FinWell2Statment_3,
         FW_2_4 = FinWell2Statment_4) |> 
  mutate(across(FW_1_1:FW_2_4, as.numeric),
         Total_FW_Score = FW_1_1 + FW_1_2 + FW_1_3 + 
           FW_1_4 + FW_1_5 + FW_1_6 + FW_2_1 + 
           FW_2_2 + FW_2_3 + FW_2_4)

# - medical care 
dat_long<-dat_long %>% 
  mutate(MedicalAccess = (MedicalAccess == "Yes")) %>%
  mutate(MedicalAccess_t_Medical = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Medical Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Dental = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Dental Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Vision = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Vision Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Hearing = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Hearing Care", MedicalAccessNo),
             NA),
         MedicalAccess_t_Mental = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Mental Health Services", MedicalAccessNo),
             NA),
         MedicalAccess_t_Other = 
           ifelse(
             !(is.na(MedicalAccessNo)),
             grepl("Other: please specify", MedicalAccessNo),
             NA))

# - schooling 
dat_long <- dat_long %>% 
  mutate(across(ChildMissSchool1:ChildMissSchool6,
                ~ case_when(
                  . == "No missed school days" ~ 0,
                  . == "1-3 days" ~ 1,
                  . == "4-6 days" ~ 4,
                  . == "7-10 days" ~ 7,
                  . == "11 or more days" ~ 11))) 

# - Parental Stress Scale
dat_long<-dat_long %>%
  mutate(across(c(ParentalStressScale_1, ParentalStressScale_2,
                  ParentalStressScale_5:ParentalStressScale_8,
                  ParentalStressScale_17:ParentalStressScale_18),
                ~ case_when(
                  . == "Strongly Agree" ~ 1,
                  . == "Agree" ~ 2,
                  . == "Undecided" ~ 3,
                  . == "Disagree" ~ 4,
                  . == "Strongly Disagree" ~ 5,
                  is.na(.) ~ NA)),
         across(c(ParentalStressScale_3:ParentalStressScale_4,
                  ParentalStressScale_9:ParentalStressScale_16),
                ~ case_when(
                  . == "Strongly Agree" ~ 5,
                  . == "Agree" ~ 4,
                  . == "Undecided" ~ 3,
                  . == "Disagree" ~ 2,
                  . == "Strongly Disagree" ~ 1,
                  is.na(.) ~ NA))) %>% 
  mutate(across(ParentalStressScale_1:ParentalStressScale_18, as.numeric),
         ParentalStressTotal = rowSums(across(ParentalStressScale_1:ParentalStressScale_18)))


# put in codebook order
dat_long<-dat_long %>% 
  rename(SurveyStart = StartDate,
         SurveyEnd = EndDate) |> 
  select(PIN, GROUP, County,
         Wave, InWave,
         SurveyStart, SurveyEnd,
         Consent, DOB, Age_c,
         Gender, Gender_s,
         HH_Size, HH_Children,
         HH_NumChild_c, 
         HH_ChildAge1:HH_ChildAge6,
         HH_AdultsAll,
         HH_AdultsSome, MaritalStat, 
         PartnerStat, PartnerGender,
         Ethnicity, Race, Race_White:Race_Other, RaceOtherSpecify,
         Edu, Edu_s, EduOther, 
         LangHome, LangOther, 
         SexOrient, SexOrient_Straight:SexOrient_Questioning, 
         SF_HealthGen:SF_HealthState_4, SF_PhysicalFunctioning:SF_GenHealth,
         K10_1:K10phys, Kessler_Score,
         PS_2:PS_10, PS_ShortTotal,
         Fin_Emergency, Fin_Emergency_exp_c, Fin_LateFee:Fin_Edu,
         Employment_c, Student_c,
         Fin_IncSrcHH,
         Fin_IncSrcHH_1:Fin_IncSrcHH_6,
         Fin_IncAmtHH_1, Fin_IncAmtHH_2,
         Fin_IncAmtHH_3, Fin_IncAmtHH_4,
         Fin_IncAmtPers_1, Fin_IncAmtPers_2, 
         Fin_IncAmtPers_3, Fin_IncAmtPers_4,
         Fin_IncSrcPers_Job1, Fin_IncSrcPers_Job2,
         Fin_IncSrcPers_Job3, Fin_IncSrcPers_Under,
         Fin_IncSrcPers_Gift, Fin_IncSrcPers_Loan,
         Fin_IncSrcPers_TANF, Fin_IncSrcPers_Disab,
         Fin_IncSrcPers_SS, Fin_IncSrcPers_Unemp,
         Fin_IncSrcPers_Retire, Fin_IncSrcPers_Other,
         Num_jobs_c, Fin_Services, Services_SSI:Services_SSDI,
         Fin_Debt, Fin_Debt_House:Fin_Debt_FamStudLoan,
         Fin_DebtPay, Fin_Savings, Fin_FamHelp,
         Fin_HelpFam, Fin_HelpFamAmt, Fin_HelpFamRsn,
         Fin_HelpFamRsn_1:Fin_HelpFamRsn_7,
         Fin_Volunteer, Fin_Volunteer_1:Fin_Volunteer_6,
         Hsg_Stat, Hsg_StatOther, Hsg_RentAssist,
         Hsg_RentAssistType_1: Hsg_RentAssistType_6,
         Hsg_Current, Hsg_Move, Hsg_Qual,
         Hsg_NeighQual, Hsg_RentAmt,
         Hsg_UtilAmt, Hsg_Evict, Hsg_MortDefault,
         Chaos_1:Chaos_15, Chaos_Score,
         AM_1:AM_24, AM_Awareness:AM_Reliance,
         HHFI_Insuf:HHFI_Util,
         AH_1:AH_12, AH_Agency:AH_Total,
         FW_1_1:FW_2_4, Total_FW_Score,
         ParentInsurance:MedicalAccess,
         MedicalAccess_t_Medical:MedicalAccess_t_Other,
         ChildMissSchool1:ChildMissSchool6,
         RelativeCareHours1:CenterCareHours6,
         ParentalStressScale_1:ParentalStressScale_18,
         ParentalStressTotal) 

write_csv(dat_long,
          "./data/cgir_ocfs_clean_long.csv")

dat_long |> 
  pivot_wider(names_from = Wave,
              names_sep = "_W",
              values_from = GROUP:ParentalStressTotal,
              names_vary = "slowest") |> 
  write_csv("./data/cgir_ocfs_clean_wide.csv")

