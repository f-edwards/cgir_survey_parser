library(tidyverse)
library(mice)
library(miceadds)
library(lme4)


### WIDE IS MOST PRINCIPLED, but LOOKS BIASED
## GOING TO REVISE THE IMP MODEL 
## SCRIPT, CAN CONSIDER y = b0 + b1pre + b2G
# as mancova analogue, but will be v tedious


dat<-read_csv("./data/cgir_NY_ocfs_FEClean04225.csv")
dat<-dat %>% 
  mutate(Total_FW_Score = rowSums(across(FW_1_1:FW_2_4))) 

dat %>% 
  group_by(GROUP, Wave) %>% 
  summarize(Total_FW_Score_mn = mean(Total_FW_Score, na.rm=T))

# bad data!
dat_i<-read_csv("./data/imputed.csv")

dat_i %>% 
  group_by(GROUP, Wave) %>% 
  summarize(Total_FW_Score_mn = mean(Total_FW_Score))


### drop vars not imputed (except PIN for join)

dat<-dat %>% 
  mutate(Total_FW_Score = rowSums(across(FW_1_1:FW_2_4), na.rm=T)) 

## how much item non-response is there?

pdat<-dat %>% 
  ungroup() %>% 
  select(PIN, Wave, GROUP, County, FW_1_1:FW_2_4)
missmap(pdat)



m0<-lmer(Total_FW_Score ~ 
         factor(Wave) + GROUP:factor(Wave) + County + 
         (1|PIN),
       data = dat)

m0_i<-lmer(Total_FW_Score ~ 
           factor(Wave) + GROUP:factor(Wave) + County + 
           (1|PIN),
         data = dat_i)

### YEP MY IMPUTATION MODEL SUCKS


# long --------------------------------------------------------------------

dat_i %>% 
  group_by(GROUP, Wave) %>% 
  summarize(Total_FW_Score_mn = mean(Total_FW_Score))


imp_temp<-dat %>% 
  ungroup() %>% 
  select(Wave, GROUP, County, FW_1_1:FW_2_4)

### manually configure binaries for model specification
imp1 <- imp_temp %>% 
  mutate(W2 = Wave == 2,
         W3 = Wave == 3,
         W4 = Wave == 4,
         CountyO = County == "Onondaga County",
         CountyW = County == "Westchester County",
         WaveT1 = (Wave == 1) * (GROUP == "T"),
         WaveT2 = (Wave == 2) * (GROUP == "T"),
         WaveT3 = (Wave == 3) * (GROUP == "T"),
         WaveT4 = (Wave == 4) * (GROUP == "T")) 

imp1_m<-mice(imp1, maxit = 0)
## configure predmat for interaction model no FE
pred1<-imp1_m$predictorMatrix
#disable wave, group county but retain in object
pred1[1:3,]<-0
pred1[,1:3]<-0
#impute
imp1_ml<-mice(imp1,
             predictorMatrix = pred1,
             m=20)

imputed_l<-mice::complete(imp1_ml,
                          action = "long",
                          include = T)

# wide --------------------------------------------------------------------

### now compare to wide
imp_temp<-dat %>% 
  ungroup() %>% 
  select(PIN, Wave, GROUP, County, FW_1_1:FW_2_4)

imp_w<-imp_temp %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c(PIN, GROUP, County),
    names_from = Wave, 
    names_sep = "_W",
    values_from = FW_1_1:FW_2_4) %>% 
  mutate(GROUP = factor(GROUP),
         County = factor(County))

imp_w1<-mice(imp_w,
             m=20)
imp_wide<-mice::complete(imp_w1,
                         action = "long",
                         include = T)

imputed_w<-imp_wide %>% 
  pivot_longer(cols = -c(PIN, GROUP, County, .imp, .id),
               names_sep = "_W",
               names_to = c("variable", "Wave")) |> 
  pivot_wider(id_cols = c(PIN:.id, "Wave"),
              names_from = "variable",
              values_from = "value")



# compare -----------------------------------------------------------------

imputed_l<-imputed_l %>% 
  mutate(Total_FW_Score = rowSums(across(FW_1_1:FW_2_4))) |> 
  mutate(type = "long")
imputed_w<-imputed_w %>% 
  mutate(Total_FW_Score = rowSums(across(FW_1_1:FW_2_4))) |> 
  mutate(type = "wide")


library(tidybayes)

p1<-imputed_l %>% 
  ggplot(aes(
    x = Total_FW_Score)) + 
  stat_slab(alpha = 0.5) + 
  facet_wrap(.imp==0~GROUP)+
  labs(subtitle = "long")

p2<-imputed_w %>% 
  ggplot(aes(
    x = Total_FW_Score)) + 
  stat_slab(alpha = 0.5) + 
  facet_wrap(.imp==0~GROUP)+
  labs(subtitle = "wide")

library(patchwork)
p1/p2

# direct mean comparison
p1<-imputed_l |> 
  group_by(GROUP, Wave, .imp) |> 
  summarize(Total_FW_Score = mean(Total_FW_Score, na.rm=T)) |> 
  ggplot(aes(x = Total_FW_Score,
             y = .imp,
             color = .imp==0)) + 
  geom_point(alpha = 0.5) + 
  facet_grid(GROUP~Wave) +
  labs(subtitle = "long")


p2<-imputed_w |> 
  group_by(GROUP, Wave, .imp) |> 
  summarize(Total_FW_Score = mean(Total_FW_Score, na.rm=T)) |> 
  ggplot(aes(x = Total_FW_Score,
             y = .imp,
             color = .imp==0)) + 
  geom_point(alpha = 0.5) + 
  facet_grid(GROUP~Wave) +
  labs(subtitle = "wide")

p1/p2

## diff in means by imp
p1<-imputed_w |> 
  group_by(GROUP, Wave, .imp==0) |> 
  summarize(Total_FW_Score = mean(Total_FW_Score, na.rm=T)) |> 
  pivot_wider(id_cols = c(Wave, `.imp == 0`),
              names_from = GROUP,
              values_from = Total_FW_Score) |> 
  mutate(diff = `T` - `C`) |> 
  ggplot(aes(x = diff, y = Wave,
             color = `.imp == 0`)) + 
  geom_point() + 
  labs(subtitle = "wide")

p2<-imputed_l |> 
  group_by(GROUP, Wave, .imp==0) |> 
  summarize(Total_FW_Score = mean(Total_FW_Score, na.rm=T)) |> 
  pivot_wider(id_cols = c(Wave, `.imp == 0`),
              names_from = GROUP,
              values_from = Total_FW_Score) |> 
  mutate(diff = `T` - `C`)|> 
  ggplot(aes(x = diff, y = Wave,
             color = `.imp == 0`)) + 
  geom_point() + 
  labs(subtitle = "long")

p2/p1
## long looks better IMO, more symmetry around observed
# cross imputation variance for w vs l
v1<-imputed_w |> 
  filter(.imp>0) |> 
  group_by(GROUP, Wave, .imp) |> 
  summarize(Total_FW_Score = mean(Total_FW_Score, na.rm=T)) |> 
  summarize(varW = var(Total_FW_Score))

# cross imputation variance for w vs l
v2<-imputed_l |> 
  filter(.imp>0) |> 
  group_by(GROUP, Wave, .imp) |> 
  summarize(Total_FW_Score = mean(Total_FW_Score, na.rm=T)) |> 
  summarize(varL = var(Total_FW_Score))

comp<-v1 |> 
  left_join(v2 |> 
              mutate(Wave = as.character(Wave))) |> 
  mutate(ratio = varL / varW)



#options: 1; set up the regression model (substantive) manually for mice
#2: try smcfcs with normal models on variables, but may be funky. 
# 3: bayesian where inputs are explicitly in the model

### check marginal distributions of focal variables (demographics) conditional on missingness post wave 1

dat |> 
  group_by(Race, Wave, GROUP) |> 
  summarize(mean(is.na(FW_1_1))) |> 
  print(n=100)

### try race

# long --------------------------------------------------------------------




imp_temp<-dat %>% 
  ungroup() %>% 
  select(Wave, GROUP, County, FW_1_1:FW_2_4)

### manually configure binaries for model specification
imp1 <- imp_temp %>% 
  mutate(W2 = Wave == 2,
         W3 = Wave == 3,
         W4 = Wave == 4,
         CountyO = County == "Onondaga County",
         CountyW = County == "Westchester County",
         WaveT1 = (Wave == 1) * (GROUP == "T"),
         WaveT2 = (Wave == 2) * (GROUP == "T"),
         WaveT3 = (Wave == 3) * (GROUP == "T"),
         WaveT4 = (Wave == 4) * (GROUP == "T")) 

imp1_m<-mice(imp1, maxit = 0)
## configure predmat for interaction model no FE
pred1<-imp1_m$predictorMatrix
#disable wave, group county but retain in object
pred1[1:3,]<-0
pred1[,1:3]<-0
#impute
imp1_ml<-mice(imp1,
              predictorMatrix = pred1,
              m=20)

imputed_l<-mice::complete(imp1_ml,
                          action = "long",
                          include = T)
