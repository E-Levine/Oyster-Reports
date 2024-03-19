###CERP 4490 Final Report
##2019-2023 Data
##Running 2019-2023 then 2005-2023 
#
#
#Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr,  #DF manipulation
               readxl,          #Excel
               lubridate,         #Dates
               lme4, car, #glmer function to replace GLMMIX
               afex, emmeans, #working
               knitr, here, 
               flextable,
               install = TRUE)
#
#
####Surevey data####
#
##Load data, check, and reformat columns as needed
Counts <- read_excel("//fwc-spfs1/FishBio/Molluscs/Oysters/SAS Data Analysis/SAS Data/SurveyCountsSAS.xlsx", 
                     sheet = "Counts", skip = 0, col_names = TRUE, na = c("", "Z"), trim_ws = TRUE, .name_repair = "universal")
#
head(Counts)
glimpse(Counts)
Counts <- Counts %>% mutate_at(c(3:8, 10:12), as.factor) %>% rename(Live_raw = Live, Dead_raw = Dead)
#
##Add columns for converted counts and select data to analyze
(Counts_df <- Counts %>% 
  mutate(Live = case_when(Live_raw < 0 & Date >= '2019-01-01' & (Season == 'Spr'|Season == 'Fal') ~ LiveQtr*4,
         TRUE ~ NA),
         Dead = case_when(Dead_raw < 0 & Date >= '2019-01-01' & (Season == 'Spr'|Season == 'Fal') ~ DeadQtr*4,
                          TRUE ~ NA),
         Total = Live + Dead) %>%
  subset(Date >= as.Date('2019-01-01', format = "%Y-%m-%d") & (Site == "SL-C" | Site == "SL-N" | Site == "SL-S" | Site == "LX-N" | Site == "LX-S")) %>%
  droplevels() %>% mutate(across(everything(), ~replace(., . == -999, NA)))) 
#
###Run with negative binomial, poisson, and normal distribution - compare to determine best model
Counts_mod_NB <- lme4::glmer.nb(Live ~ Site * Year + (1|Site:Year),
                      data = Counts_df)         
Counts_mod_P <- glmer(Live ~ Site * Year + (1|Site:Year),
                          data = Counts_df, family = poisson) 
Counts_mod_N <- lmer(Live ~ Site * Year + (1|Site:Year),
                      data = Counts_df) 
anova(Counts_mod_NB) #Similar F values
#mixed(Live ~ Site * Year + (1|Site:Year), #Incorrect dfs
#        data = Counts_df, method = "KR")
test(emmeans(Counts_mod_NB, "Site")) #Correct LSmeans table but missing mean and se mean - mean and se from "response"
pairs(emmeans(Counts_mod_NB, "Site")) #Same as diff of Site LS means table

cbind(as.data.frame(test(emmeans(Counts_mod_NB, "Site"))),
          as.data.frame(emmeans(Counts_mod_NB, "Site", type = "response")) %>% dplyr::select(Site:SE) %>% rename(Mean = response))
