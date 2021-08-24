#############################
## Model fitting function ###
#############################
interaction_mdl <- function(data, feature, type){

  ## Remove missing data
  idxna <- which(data[[feature]] < 0)
  num_missing <- length(idxna)
  perc_missing <- num_missing / (nrow(data))
  data <- data[-idxna, ]
  
  ## Modelling formula
  fmla <- as.formula(sprintf("%s ~ BirthGender + Pandemic + GenderXPandemic + AgeGrpB + BMIGrp + as.factor(RaceEthn5) + as.factor(MaritalStatus) + Education +  IncomeRanges", feature))

  if(type == "cts"){
      mdl <- lm(fmla, data) 
      main <- summary(mdl)$coef[2:4,1]
      pvals <- summary(mdl)$coef[2:4,4]
      
      mdl_lc <- glht(mdl, linfct = c("BirthGender + GenderXPandemic=0"))
      extra_pvals <- summary(mdl_lc)$test$pvalues
      mdl_lc <- glht(mdl, linfct = c("Pandemic + GenderXPandemic=0"))
      extra_pvals <- c(extra_pvals, summary(mdl_lc)$test$pvalues)

      all_output <- c(main[1], pvals[1], main[1] + main[3], extra_pvals[1], 
                      main[2], pvals[2], main[2] + main[3], extra_pvals[2],
                      main[3], pvals[3])
      all_output <- data.frame(t(all_output)) %>% mutate(feature = feature, sub = feature)
    }

  if(type == "discrete"){
      n_lev <- length(unique(data[[feature]]))
      feature_lev <- sort(unique(data[[feature]]))
      all_output <- c()
      for(i in 1:(n_lev - 1)){
        new_data <- data[data[[feature]] %in% c(feature_lev[1], feature_lev[i + 1]),]
        new_data[[feature]] <- factor(new_data[[feature]], 
                                  levels = c(feature_lev[1], feature_lev[i + 1]),
                                  labels = 0:1)
        mdl <- glm(fmla, new_data, family = binomial()) 
        main <- summary(mdl)$coef[2:4,1]
        pvals <- summary(mdl)$coef[2:4,4]
        mdl_lc <- glht(mdl, linfct = c("BirthGender + GenderXPandemic=0"))
        extra_pvals <- summary(mdl_lc)$test$pvalues
        mdl_lc <- glht(mdl, linfct = c("Pandemic + GenderXPandemic=0"))
        extra_pvals <- c(extra_pvals, summary(mdl_lc)$test$pvalues)
  
        new_output <- c(main[1], pvals[1], main[1] + main[3], extra_pvals[1], 
                        main[2], pvals[2], main[2] + main[3], extra_pvals[2], 
                        main[3], pvals[3])
        new_output <- data.frame(t(new_output)) %>% mutate(feature = feature, sub = feature_lev[i+1])
        all_output <- rbind(all_output, new_output)
      }
    }
  
  output <- all_output 
  return(output)
}


######################################################
## Function for pairwise comparison (categorical rv)
######################################################
t_test_cat <- function(feature, data_all){
  ## Extract the feature of interest
  x_all <- data_all[[feature]]

  ## Number of samples and missing percentage
  id_notna <- which(x_all>=0)
  n <- length(id_notna)

  ## Exclude the NAs
  x_all <- x_all[id_notna]
  data_all <- data_all[id_notna,]
  
  ## Merging groups for Age
  if(feature == "AgeGrpB"){
    x_all[x_all == 5] <- 4
  }
    
  if(feature == "MaritalStatus"){
    x_all[x_all %in% c(2,5,6)] <- 2
  }

  ## Before and after the pandemic
  id_before <- which(data_all$Pandemic == 0)
  id_after <- which(data_all$Pandemic == 1)
  x_before <- x_all[id_before]
  x_after <- x_all[id_after] 
  
  if(is.null(x_before) | is.null(x_after)) return(NULL)
  joint_tab <- rbind(table(x_before), table(x_after))
  res <- chisq.test(joint_tab)
  pval <- res$p.value
  
  ## Compute frequencies
  ## The levels of the features
  x_levels <- unique(x_all)

  ## Compute the frequency table 
  res_df <- data.frame()
  
  for(i in 1:length(x_levels)){

    level_id <- which(x_all == x_levels[i])
   
    ## percentage of the sub category
    freq <- length(level_id) / n
    
    ## weighted percentage of the sub category
    wfreq <- sum(data_all$PERSON_FINWT0[level_id]) / sum(data_all$PERSON_FINWT0)

    ## percentage of the sub category pre-pandemic
    level_id_before <- id_before[which(x_before == x_levels[i])]
    freq_before <- length(level_id_before) / length(x_before)

    ## weighted percentage of the sub category pre-pandemic
    wfreq_before <- sum(data_all$PERSON_FINWT0[level_id_before]) / sum(data_all$PERSON_FINWT0[id_before])

    ## percentage of the sub category post-pandemic
    level_id_after <- id_after[which(x_after == x_levels[i])]
    freq_after <- length(level_id_after) / length(x_after)

    ## weighted percentage of the sub category pre-pandemic
    wfreq_after <- sum(data_all$PERSON_FINWT0[level_id_after]) / sum(data_all$PERSON_FINWT0[id_after])

    new_df <- data.frame(feature = feature, sub = x_levels[i],
                         freq = freq, wfreq = wfreq, 
                         freq_before = freq_before, wfreq_before = wfreq_before, 
                         freq_after = freq_after, wfreq_after = wfreq_after)
    res_df <- rbind(res_df, new_df)
  }

  res_df <- res_df %>% mutate(n = n, pval = pval)
  return(res_df)
}


