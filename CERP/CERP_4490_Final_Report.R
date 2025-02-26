###CERP 4490 Final Report - Testing with 4203 analyses
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
               stats, emmeans, #working
               MASS, nlme, #afex, #lmerTest, #MASS, #glm.nb
               knitr, here, 
               flextable,
               install = TRUE)
#
#
####Survey data####
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
  mutate(Live = case_when(Live_raw >= 0 & Date <= '2019-01-01' & (Season == 'Spr'|Season == 'Fal') ~ Live_raw,
                          Live_raw < 0 & Date >= '2019-01-01' & (Season == 'Spr'|Season == 'Fal') ~ LiveQtr*4,
                          TRUE ~ LiveQtr*4),
         Dead = case_when(Dead_raw >= 0 & Date <= '2019-01-01' & (Season == 'Spr'|Season == 'Fal') ~ Dead_raw,
                          Dead_raw < 0 & Date >= '2019-01-01' & (Season == 'Spr'|Season == 'Fal') ~ DeadQtr*4,
                          TRUE ~ DeadQtr*4),
         Total = Live + Dead) %>%
  subset(Date <= as.Date('2019-01-01', format = "%Y-%m-%d") & (Site == "SL-C" | Site == "SL-N" | Site == "SL-S" | Site == "LX-N" | Site == "LX-S") &
           (Season == 'Spr' | Season == 'Fal')) %>%
  droplevels() %>% mutate(across(everything(), ~replace(., . == -999, NA)))) 
#
#
#
options(contrasts = c(factor = "contr.SAS", ordered = "contr.poly"))
###Run with negative binomial, poisson, and normal distribution - compare to determine best model
set.seed(54321)
Counts_mod_NB <- lme4::glmer.nb(Live ~ Site * Year + (1|Site:Year),
                      data = Counts_df)     
Counts_mod_NB2 <- glm.nb(Live ~ Site * Year, Counts_df, link = log, contrasts = list(Site = "contr.SAS", Year = "contr.SAS"))
Counts_mod_NB3 <- nlme::lme(Live ~ Site * Year, random = ~1|Station, method = "ML", data = Counts_df)

Counts_mod_P <- glmer(Live ~ Site * Year + (1|Site:Year),
                          data = Counts_df, family = poisson) 
Counts_mod_N <- lmer(Live ~ Site * Year + (1|Site:Year),
                      data = Counts_df)
#
Model_eval <- list()
models <- list(Counts_mod_NB, Counts_mod_N, Counts_mod_P)
model_names <- c("Neg Bi", "Normal", "Poisson")
for (i in seq_along(models)){
  model_info <- model_names[i]
  model_info$Chi2 <- sum(residuals(models[[i]], type = "pearson")^2)
  model_info$Obs <-  stats::nobs(models[[i]])
  model_info$Chi_df <- sum(residuals(models[[i]], type = "pearson")^2)/stats::nobs(models[[i]])
  model_info$Diff <- abs(sum(residuals(models[[i]], type = "pearson")^2)/stats::nobs(models[[i]]) - 1)
  #model_info %>% as.data.frame()
  Model_eval <- cbind(Model_eval, model_info)
}
(Model_eval %>% as.data.frame())
#Compare and select best model based on smallest "Diff"erence
#
anova(Counts_mod_NB, type = 3, ddf = "Kenward-Roger")
anova(Counts_mod_NB3, type = "sequential")
anova(Counts_mod_NB2, test = "F")
Anova(Counts_mod_NB2, type = (3), test.statistic="F")
mixed(Counts_mod_NB2, data = Counts_df)
#
(Live_by_Site <- left_join(data.frame(cbind(as.data.frame(test(emmeans(Counts_mod_NB, "Site", lmer.df = "kenward-roger", type = "unlink"))),
                                            as.data.frame(emmeans(Counts_mod_NB, "Site", lmer.df = "kenward-roger", type = "unlink")) %>% 
                                              dplyr::select(Site:SE) %>% rename(Mean = response))),
                           multcomp::cld(emmeans(Counts_mod_NB, "Site", lmer.df = "kenward-roger", type = "unlink"), Letters = letters, alpha = 0.05) %>% 
                             data.frame() %>% dplyr::select(Site, .group) %>%
                             rename(Letters = '.group')))
pairs(emmeans(Counts_mod_NB, "Year", lmer.df = "kenward-roger", type = "unlink"), adjust = "tukey") #Same as diff of Site LS means table
#
#
(Live_by_Year <- left_join(data.frame(cbind(as.data.frame(test(emmeans(Counts_mod_NB, "Year", lmer.df = "kenward-roger", type = "unlink"))),
                                            as.data.frame(emmeans(Counts_mod_NB, "Year", lmer.df = "kenward-roger", type = "unlink")) %>% 
                                              dplyr::select(Year:SE) %>% rename(Mean = response))),
                           multcomp::cld(emmeans(Counts_mod_NB, "Year", lmer.df = "kenward-roger", type = "unlink"), Letters = letters, alpha = 0.05) %>% 
                             data.frame() %>% dplyr::select(Year, .group) %>%
                             rename(Letters = '.group')))
#
#
#
###Testing
Counts <- read_excel("//fwc-spfs1/FishBio/Molluscs/Oysters/SAS Data Analysis/SAS Data/CERP4203_Final_Live_Data_Test.xlsx", 
                     sheet = "Sheet1", skip = 0, col_names = TRUE, na = c("", "Z"), trim_ws = TRUE, .name_repair = "universal")
#
head(Counts)
glimpse(Counts)
Counts <- Counts %>% mutate_at(c(3:8, 10:12), as.factor) 
#
Counts_glmer <- glmer(Live ~ Site * Year + (1|Site:Year), data = Counts, family = negative.binomial)
summary(Counts_glmer)
Anova(Counts_glmer, type = 3, ddf = "Kenward-Roger", test.statistic = "F")
lsmeans(Counts_glmer, specs = "Site|Year", type = "response")
#
Counts_NB <- glm.nb(Live ~ Site * Year, Counts, link = log)#, contrasts = list(Site = "contr.SAS", Year = "contr.SAS"))
summary(Counts_NB)
Anova(Counts_NB, type = 3, test.statistic = "F")
lsmeans(Counts_NB, ~ Site, type = "response", ddf = "Kenward-Roger", pairwise = TRUE)
lsmeans(Counts_NB, ~ Year, type = "response", ddf = "Kenward-Roger")
lsmeans(Counts_NB, ~ Year|Site, type = "response")
multcomp::cld(emmeans(Counts_NB, "Site"), adjust = "none")
expand.grid(Site = levels(Counts$Site), Year = levels(Counts$Year))

Counts_mod_NB3 <- nlme::lme(Live ~ Site * Year, random = ~1|Station, method = "ML", data = Counts_df)
Anova(Counts_mod_NB3, type = 3, test.statistic = "F", ddf = "Kenward-Roger")
lsmeans(Counts_mod_NB3, "Site", type = "response")

t <- lme4::lmer(Live ~ Site * Year + (1|Station), data = Counts, REML = TRUE)
summary(t)
anova(t)
t2 <- afex::mixed(Live ~ Site * Year + (1|Station), data = Counts, method = "KR")
summary(t2)
anova(t2)
emmeans(t2)
#
Counts_NB <- glm.nb(Live ~ Site * Year, Counts, link = "log")#, contrasts = list(Site = "contr.SAS", Year = "contr.SAS"))
summary(Counts_NB)
Anova(Counts_NB, type = 3, test.statistic = "F")
NB_ls <- lsmeans(Counts_NB, ~Year|Site)
multcomp::cld(NB_ls)
wilcox.test(Live ~ Site, data = Counts)
#
#####Within comps####
#
##Only missing comparisons within sites among years - ignore rest and work with comparisons

Counts %>% group_by(Site) %>% summarise(mean = mean(Live))
wilcox.test(Live ~ Year, data = Counts, subset = Site %in% c("LX-N"))
