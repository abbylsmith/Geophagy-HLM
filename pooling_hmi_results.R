fin_geo <- readRDS('geo_final_mids.rds')


#mids_obj= missing data object
#m = number of imputations
convert_complete_to_list <- function(mids_obj, m){ 
  complete_lists <- list()
  for (i in 1:m) {
    complete_data <- complete(mids_obj, i)
    
    #filter by visits were hemoglobin was measured
    complete_data <- data.frame(complete_data %>%
                                group_by(pid) %>%
                                filter(row_number() == 2 | row_number() == 5 | 
                                         row_number() == 6 | row_number() == 7))
    
    complete_data$hemo_avg <- rep(aggregate(complete_data$hemo, list(complete_data$pid), 
                                          mean)$x, each = 4) #4 visits where hemoglobin was measured per person
    
    complete_data$hemo_avg <- complete_data$hemo_avg - mean(complete_data$hemo_avg)
    
    complete_lists[[i]] <- complete_data
  }
  
  new_mids <- datlist2mids(complete_lists)
  
  return(new_mids)
}


#complete_data = initial data frame + all missing values filled in for imputations
Rubins_rules <- function(complete_data){

  complete_data <- data.frame(complete_data %>%
                              group_by(pid) %>%
                              filter(row_number() == 2 | row_number() == 5 | row_number() == 6 | row_number() == 7))
  

  complete_data$hemo_avg <- rep(aggregate(complete_data$hemo, list(complete_data$pid), mean)$x, each = 4) #for the 4 measurements

  ##Group-mean centering "Level 1" predictors (predictors that are time-variant)##
  complete_data <- complete_data %>% group_by(pid) %>% mutate(hemo_grp_c = (hemo - mean(hemo)))
  complete_data <- complete_data %>% group_by(pid) %>% mutate(depscore_grp_c = (depscore - mean(depscore)))
  complete_data <- complete_data %>% group_by(pid) %>% mutate(stress_grp_c = (stress - mean(stress)))
  complete_data <- complete_data %>% group_by(pid) %>% mutate(fias_grp_c = (fias - mean(fias)))
  complete_data <- complete_data %>% group_by(pid) %>% mutate(fefood_grp_c = (fefood - mean(fefood)))
  
  
  ##Grand-mean centering "Level 2" predictors (time-constant)##
  complete_data$hhloc_grand_c <- complete_data$hhloc - mean(complete_data$hhloc)
  complete_data$gravidity_grand_c <- complete_data$gravidity - mean(complete_data$gravidity)
  complete_data$hemo_avg_grand_c <- complete_data$hemo_avg - mean(complete_data$hemo_avg)
  
  parameters_of_interest <- list()
  
  #note the adaptive quadrature setting
  my_model <- glmer(mod, data = complete_data, family = binomial(link = "logit"),
                    control = glmerControl('bobyqa'),nAGQ = 25)

  parameters_of_interest[[1]] <- fixef(my_model)[1]
  parameters_of_interest[[2]] <- fixef(my_model)[2]
  parameters_of_interest[[3]] <- fixef(my_model)[3]
  parameters_of_interest[[4]] <- fixef(my_model)[4]
  parameters_of_interest[[5]] <- fixef(my_model)[5]
  parameters_of_interest[[6]] <- fixef(my_model)[6]
  parameters_of_interest[[7]] <- fixef(my_model)[7]
  parameters_of_interest[[8]] <- fixef(my_model)[8]
  parameters_of_interest[[9]] <- fixef(my_model)[9]
  parameters_of_interest[[10]] <- fixef(my_model)[10]
  parameters_of_interest[[11]] <- fixef(my_model)[11]
  parameters_of_interest[[12]] <- VarCorr(my_model)[[1]][1, 1]
  
  
  names(parameters_of_interest) <- c("beta_intercept","beta_posneg","beta_hemo_avg",
                                     "beta_geo_child", "beta_preg", "beta_diar", "beta_stress","beta_hhloc","beta_gravidity","beta_depscore","beta_nausea",
                                     "random intercepts var")
  
  return(parameters_of_interest)
}



#m = number of imputations
#mids_obj = missing data object
#df_meth= specification of degrees of freedom (Satterwahite)
get_pvals <- function(m,mids_obj,formula, df_meth){
  v <- sapply(1:m, function(i) {
    complete_data <- complete(mids_obj, i)
    complete_data <- data.frame(complete_data %>%
                                group_by(pid) %>%
                                filter(row_number() == 2 | row_number() == 5 | 
                                         row_number() == 6 | row_number() == 7))
    
    complete_data$hemo_avg <- rep(aggregate(complete_data$hemo, list(complete_data$pid), mean)$x, each = 4)

    ##Group-mean centering "Level 1" predictors (things that are changing)##
    complete_data <- complete_data %>% group_by(pid) %>% mutate(hemo_grp_c = (hemo - mean(hemo)))
    complete_data <- complete_data %>% group_by(pid) %>% mutate(depscore_grp_c = (depscore - mean(depscore)))
    complete_data <- complete_data %>% group_by(pid) %>% mutate(stress_grp_c = (stress - mean(stress)))
    complete_data <- complete_data %>% group_by(pid) %>% mutate(fias_grp_c = (fias - mean(fias)))
    complete_data <- complete_data %>% group_by(pid) %>% mutate(fefood_grp_c = (fefood - mean(fefood)))
    
    ##Grand-mean centering "Level 2" predictors (time-constant)##
    complete_data$hhloc_grand_c <- complete_data$hhloc - mean(complete_data$hhloc)
    complete_data$gravidity_grand_c <- complete_data$gravidity - mean(complete_data$gravidity)
    complete_data$hemo_avg_grand_c <- complete_data$hemo_avg - mean(complete_data$hemo_avg)
    
    fit <- glmer(formula, data = complete_data, family = binomial(link = "logit"), 
                 control =glmerControl('bobyqa'), nAGQ = 25)
  })
  
  
  df <- data.frame(coefficients(summary(v[[1]])))
  for (i in 2:m) {
    df <- cbind(df, coefficients(summary(v[[i]])))
  }
  
  pooledMean <- apply(df[,which(colnames(df) == 'Estimate')], 1, mean)
  withinVar <- apply(df[,which(colnames(df) == 'Std. Error')]^2, 1, mean)
  
  ##calculating betweenVar
  coeff_cols <- df[,which(colnames(df) == 'Estimate')]
  to_sum <- data.frame()
  for (i in 1:m) {
    to_sum <- rbind(to_sum,(coeff_cols[,i] - pooledMean)^2) #variance of variances
  }
  names(to_sum) <- names(pooledMean) 
  betweenVar <- (1/(length(v) - 1))*apply(to_sum, 2, sum)
  
  totVar <- withinVar + betweenVar*dfCorrection # total variance
  dfCorrection <- (length(v) + 1)/(length(v)) # correction for degrees of freedom 
  pooledSE <- sqrt(totVar) # standard error
  lambda <- (1 + (1/m))*(betweenVar/totVar)
  n <- nrow(mids_obj$data)
  k <- length(coefficients(summary(v[[1]])))
  
  #different options for degrees of freedom
  if (df_meth == "BR") {
    nu_old <- (m - 1)/(lambda^2) 
    nu_com <- n - k
    nu_obs <- ((nu_com + 1)/(nu_com + 3))*nu_com*(1 - lambda)
    nu <- (nu_old*nu_obs)/(nu_old + nu_obs)
  } 
  if (df_meth == 'SAT') {
    nu <- (m - 1)*(1 + (1/(m + 1))*(withinVar/betweenVar))^2
  }
  
  p_vals <- pt(q = abs(pooledMean)/pooledSE, df = nu, lower.tail = FALSE) * 2 #get p-val
  return(p_vals)
 
}


#for HLM, we need to input SPSS files
#mids_obj= missing data object
#m = number of imputations
convert_to_SPSS <- function(mids_obj, m){
  library(haven)
  for (i in 1:m) {
    
    #NOTE: here we use 6 time points (visits), because we will look at visits where hemoglobin OR GI distress are measured
    complete_data <- data.frame(complete(mids_obj, i) %>%
                                group_by(pid) %>%
                                filter(row_number() == 2 | row_number()==5 | row_number() == 6 | 
                                         row_number() == 7 | row_number()==8 | row_number()==9))
    
    complete_data$pica_avg <- rep(aggregate(complete_data$pica_yn, list(complete_data$pid), mean)$x, each = 6)
    complete_data$geo_avg <- rep(aggregate(complete_data$geo, list(complete_data$pid), mean)$x, each = 6)
    write_sav(complete_data,paste("complete_hemo_", i, ".sav", sep = ""))
  }
  
}


