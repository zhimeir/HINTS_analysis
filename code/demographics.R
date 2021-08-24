## Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(haven))

## Source utility functions
source("utils.R")

## Read data
cat("Analyzing demographic covariates...")
file_dir <- "hints5_cycle4_public.sas7bdat"
data <- read_sas(file_dir) %>%
  ## Remove missing data
  filter(BirthGender != -9) %>%
  ## Code the gender as 0/1 variables: 0-male; 1-female
  mutate(BirthGender = BirthGender - 1) %>%
  ## Code the pandemic status as 0/1 variables: 0-before; 1-after
  mutate(Pandemic = (Pandemic + 1) / 2) %>%
  ## Creating the interaction term
  mutate(GenderXPandemic = BirthGender * Pandemic)

## Preprocessing: cluter BMI into subgrougs: 
## 1-(0,18.5] (underweight); 2-(18.5,25] (normal weight); 
## 3-(25,30] (overweight); 4-(30,Inf] (obesity)

data <- data %>% 
  mutate(BMIgrp = cut(BMI, breaks = c(0, 18.5, 25, 30, Inf))) %>% 
  mutate(BMIgrp = factor(BMIgrp, levels = levels(BMIgrp), labels = 1:4)) %>%
  mutate(BMIgrp = as.numeric(BMIgrp), BMIgrp = ifelse(is.na(BMIgrp), -9, BMIgrp))

## Produce the table
feature_list <- c("AgeGrpB", "BMIgrp", "RaceEthn5",
                  "MaritalStatus", "Education", "IncomeRanges") 
cat_res <- lapply(feature_list, t_test_cat, data)
cat_res <- do.call("rbind", cat_res)
cat_res <- data.frame(matrix(unlist(cat_res), nrow = dim(cat_res)))

## Renaming the columns
names(cat_res) <- c("Characteristic", "Category", "Prop.", "Weighted Prop.",
                    "Prop. (before)", "Weighted Prop. (before)",
                    "Prop. (after)", "Weighted Prop. (after)",
                    "# of Samples", "P-value")

## Store the results
write_csv(cat_res, "../results/demographics.csv")
cat("done.\n")

