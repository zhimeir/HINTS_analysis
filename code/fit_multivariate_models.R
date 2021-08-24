## Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(nnet))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(multcomp))

## Source utility functions
source("utils.R")

## Read data
cat("Fitting models...")
file_dir <- "hints5_cycle4_public.sas7bdat"
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
feature_list <- c("Electronic_MadeAppts", "Electronic_SelfHealthInfo", 
                  "Electronic_TalkDoctor", "Electronic_TestResults",                   
                  "FreqWearDevTrackHealth", "IntRsn_SharedSocNet",
                  "IntRsn_SupportGroup", "IntRsn_VisitedSocNet", 
                  "IntRsn_YouTube","SharedHealthDeviceInfo",
                  "Tablet_AchieveGoal", "Tablet_DiscussionsHCP",
                  "Tablet_MakeDecision", "TabletHealthWellnessApps", 
                  "UsedHealthWellnessApps", "WearableDevTrackHealth",
                  "WillingShareData_HCP", "WillingShareData_YourFamily",
                  "WillingShareData_YourFriends")
result <- lapply(feature_list, interaction_mdl, data = data, type = "discrete") 
result <- do.call("rbind", result)
colnames(result) <- c("gender_pre", "pval_gender_pre", "gender_post", "pval_gender_post", 
                      "pandemic_male", "pval_pandemic_male", "pandemic_female", "pval_pandemic_female",
                      "gender_pandemic", "gender_pandemic_pval", "feature", "level")
result <- cbind(result[,11:12], result[,1:10])

## Store the results 
write_csv(result, "../results/table_2_use_of_internet.csv")

## Mental health
## Continuous responses
feature_list <- c("GeneralHealth", "OwnAbilityTakeCareHealth", 
                  "MedConditions_Depression", "LittleInterest",
                  "Hopeless", "Nervous", "Worrying", "Threatened_Values", 
                  "Threatened_Strengths")
result <- lapply(feature_list, interaction_mdl, data = data, type = "cts")
result <- do.call("rbind", result)

## Discrete responses
new_result <- interaction_mdl(data, "MostImportantValues", "discrete")
result <- rbind(result, new_result)

## Attach the feature names
colnames(result) <- c("gender_pre", "pval_gender_pre", "gender_post", "pval_gender_post", 
                      "pandemic_male", "pval_pandemic_male", "pandemic_female", "pval_pandemic_female",
                      "gender_pandemic", "gender_pandemic_pval", "feature", "level")
result <- cbind(result[,11:12], result[,1:10])

## Store the results
write_csv(result, "../results/table_3_mental_health.csv")

## Alcohol
## Continuous responses
feature_list <- c("DrinkDaysPerWeek", "DrinksPerDay")
result <- lapply(feature_list, interaction_mdl, data = data, type = "cts")
result <- do.call("rbind", result)

## Discrete responses
new_result <- interaction_mdl(data, "DrinksOneOccasion", "discrete")
result <- rbind(result, new_result)
colnames(result) <- c("gender_pre", "pval_gender_pre", "gender_post", "pval_gender_post", 
                      "pandemic_male", "pval_pandemic_male", "pandemic_female", "pval_pandemic_female",
                      "gender_pandemic", "gender_pandemic_pval", "feature", "level")
result <- cbind(result[,11:12], result[,1:10])
write_csv(result, "../results/table_4_alcohol_use.csv")

## Physical Activity
feature_list <- c("HowLongModerateExerciseMinutes", "AverageTimeSitting", "TimesStrengthTraining")
result <- lapply(feature_list, interaction_mdl, data = data, type = "cts")
result <- do.call("rbind", result)
colnames(result) <- c("gender_pre", "pval_gender_pre", "gender_post", "pval_gender_post", 
                      "pandemic_male", "pval_pandemic_male", "pandemic_female", "pval_pandemic_female",
                      "gender_pandemic", "gender_pandemic_pval", "feature", "level")
result <- cbind(result[,11:12], result[,1:10])
write_csv(result, "../results/activity.csv")
cat("done.\n")

