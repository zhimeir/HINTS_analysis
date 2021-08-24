## Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(nnet))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(multcomp))

## Set working directory
dir <- "~/Documents/GitHub/HINTS"
source("./utils.R")

## Read data
file_dir <- sprintf("%s/HINTS5_Cycle4_SAS_20210309/hints5_cycle4_public.sas7bdat", dir)
data <- read_sas(file_dir) %>%
  ## Remove Missing data
  filter(BirthGender >= 0) %>%
  ## Code the gender as 0/1 variables: 0-male; 1-female
  mutate(BirthGender = BirthGender - 1) %>%
  ## Code the pandemic status as 0/1 variables: 0-before; 1-after
  mutate(Pandemic = (Pandemic + 1) / 2) %>%
  ## Creating the interaction term
  mutate(GenderXPandemic = BirthGender * Pandemic)

## Preprocessing: group BMI
data$BMIGrp <- cut(data$BMI, breaks = c(0, 18.5, 25, 30, Inf)) 

## Analyze characteristics corresponding to the use of internet to find information
feature_list <- c("Electronic_SelfHealthInfo", "Electronic_TalkDoctor",
                  "Electronic_TestResults", "Electronic_MadeAppts", 
                  "TabletHealthWellnessApps", "UsedHealthWellnessApps", 
                  "Tablet_AchieveGoal", "Tablet_MakeDecision", 
                  "Tablet_DiscussionsHCP", "WearableDevTrackHealth",
                  "FreqWearDevTrackHealth", "WillingShareData_HCP", 
                  "WillingShareData_YourFamily", "WillingShareData_YourFriends", 
                  "SharedHealthDeviceInfo", "IntRsn_VisitedSocNet", 
                  "IntRsn_SharedSocNet", "IntRsn_SupportGroup", 
                  "IntRsn_YouTube")
res <- interaction_mdl(data, "Electronic_SelfHealthInfo", "discrete")
result <- lapply(feature_list, interaction_mdl, data = data, type = "discrete") 
result <- do.call("rbind", result)
colnames(result) <- c("Gender (pre)", "pval", "Gender (post)", "pval", 
                      "Pandemic (male)", "pval", "Pandemic (female).", "pval",
                      "Gender x Pan.", "pval", "feature", "level", "# missing", "% missing")
## sink("../notes/pval_table.txt")
## result %>% kbl("latex", booktabs = T, align = "c", digits = c(0, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
##                caption = "Results for ``use of internet to find information''.", 
##                label = "internet", position = "ht") %>%
##   kable_styling(position = "center", latex_options = c("striped", "scale_down"))
## sink()
write_excel_csv(result, "../tabs/internet.csv")

## Mental health
## Continuous responses
feature_list <- c("GeneralHealth", "OwnAbilityTakeCareHealth", 
                  "MedConditions_Depression", "LittleInterest",
                  "Hopeless", "Nervous", "Worrying", "Threatened_Values", 
                  "Threatened_Strengths")
interaction_mdl(data, "GeneralHealth", "cts")
result <- lapply(feature_list, interaction_mdl, data = data, type = "cts")
result <- do.call("rbind", result)

## Discrete responses
new_result <- interaction_mdl(data, "MostImportantValues", "discrete")
result <- rbind(result, new_result)

## Attach the feature names
colnames(result) <- c("Gender (pre)", "pval", "Gender (post)", "pval", 
                      "Pandemic (male)", "pval", "Pandemic (female).", "pval",
                      "Gender x Pan.", "pval", "feature", "level", 
                      "# missing", "% missing")
## sink("../notes/pval_table.txt")
## result %>% kbl("latex", booktabs = T, align = "c", digits = c(0, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
##                caption = "Results for ``use of internet to find information''.", 
##                label = "internet", position = "ht") %>%
##   kable_styling(position = "center", latex_options = c("striped", "scale_down"))
## sink()
write_excel_csv(result, "../tabs/mental.csv")

## Alcohol
## Continuous responses
feature_list <- c("DrinkDaysPerWeek", "DrinksPerDay")
result <- lapply(feature_list, interaction_mdl, data = data, type = "cts")
result <- do.call("rbind", result)

## Discrete responses
new_result <- interaction_mdl(data, "DrinksOneOccasion", "discrete")
result <- rbind(result, new_result)
colnames(result) <- c("Gender (pre)", "pval", "Gender (post)", "pval", 
                      "Pandemic (male)", "pval", "Pandemic (female).", "pval",
                      "Gender x Pan.", "pval", "feature", "level", "# missing", "% missing")
## sink("../notes/pval_table.txt")
## result %>% kbl("latex", booktabs = T, align = "c", digits = c(0, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
##                caption = "Results for ``use of internet to find information''.", 
##                label = "internet", position = "ht") %>%
##   kable_styling(position = "center", latex_options = c("striped", "scale_down"))
## sink()
write_excel_csv(result, "../tabs/alcohol.csv")

## Physical Activity
feature_list <- c("HowLongModerateExerciseMinutes", "AverageTimeSitting", "TimesStrengthTraining")
result <- lapply(feature_list, interaction_mdl, data = data, type = "cts")
result <- do.call("rbind", result)
colnames(result) <- c("Gender (pre)", "pval", "Gender (post)", "pval", 
                      "Pandemic (male)", "pval", "Pandemic (female).", "pval",
                      "Gender x Pan.", "pval", "feature", "level", "# missing", "% missing")
## sink("../notes/pval_table.txt")
## result %>% kbl("latex", booktabs = T, align = "c", digits = c(0, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
##                caption = "Results for ``use of internet to find information''.", 
##                label = "internet", position = "ht") %>%
##   kable_styling(position = "center", latex_options = c("striped", "scale_down"))
## sink()
write_excel_csv(result, "../tabs/activity.csv")


