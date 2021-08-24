## Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(nnet))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(multcomp))

## Source utility functions
source("./utils.R")

## Read data
cat("making plots...")
file_dir <- "hints5_cycle4_public.sas7bdat"
data <- read_sas(file_dir) %>%
  filter(BirthGender > 0) %>%
  mutate(BirthGender = BirthGender - 1) %>%
  mutate(Pandemic = (Pandemic + 1) / 2) %>%
  mutate(GenderXPandemic = BirthGender * Pandemic)

## Plots
data <- data %>% 
  filter(HowLongModerateExerciseMinutes >=0, AverageTimeSitting >= 0) %>%
  mutate(Pandemic = factor(Pandemic, levels = c(0,1), labels = c("Before Pandemic", "After Pandemic"))) %>%
  mutate(BirthGender = factor(BirthGender, levels = c(0,1), labels = c("Male", "Female")))

## Honglong moderate exercise
pp <- data %>% filter(HowLongModerateExerciseMinutes > 0) %>%
  ggplot(aes(x = BirthGender, y = HowLongModerateExerciseMinutes)) +
  theme_bw() + 
  geom_boxplot() +
  facet_grid(.~Pandemic) +
  ylim(c(0, 200)) +
  xlab("") +
  ylab("Moderate exercise per day (min)")
pdf("../figs/exercise.pdf", 6, 4)
print(pp)
dev.off()

## Average time sitting
pp <- data %>% filter(AverageTimeSitting > 0) %>%
  ggplot(aes(x = as.factor(BirthGender), y = AverageTimeSitting)) +
  theme_bw() + 
  geom_boxplot() +
  facet_grid(.~Pandemic) +
  xlab("")+
  ylab("Sitting hours per day")
pdf("../figs/sitting.pdf", 6, 4)
print(pp)
dev.off()

## Average time sitting
pp <- data %>% filter(TimesStrengthTraining > 0) %>% 
  ggplot(aes(x = as.factor(BirthGender), y = TimesStrengthTraining)) +
  theme_bw() + 
  geom_boxplot() +
  facet_grid(.~Pandemic) +
  xlab("") +
  ylab("Frequency of strength training per week")
pdf("../figs/strength.pdf", 6, 4)
print(pp)
dev.off()
cat("done.")

