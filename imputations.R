#necessary libraries
require(data.table)
require(readxl)

#imputation packages utilized
require(mice)
install.packages("C:/Users/Acer/Documents/hmi_0.9.8.zip", repos = NULL, type = "source")
library("hmi")

#######HMI PACKAGE IMPUTATIONS#######

pica_df <- data.frame(read_excel("Pica_Longitudinal_2018_02_15_kf.xlsx"))

# we center all time-variant variables
pica_df$hemo_c <- pica_df$hemo - mean(pica_df$hemo,na.rm = T)
pica_df$fepill_c <- pica_df$fepill - mean(pica_df$fepill,na.rm = T)
pica_df$fefood_c <- pica_df$fefood - mean(pica_df$fefood,na.rm = T)
pica_df$fefood_yn_c <- pica_df$fefood_yn - mean(pica_df$fefood_yn, na.rm = T)
pica_df$socialstat_comm_c <- pica_df$socialstat_comm - mean(pica_df$socialstat_comm, na.rm = T)
pica_df$socialstat_country_c <- pica_df$socialstat_country - mean(pica_df$socialstat_country, na.rm = T)
pica_df$depscore_c <- pica_df$depscore - mean(pica_df$depscore, na.rm = T)
pica_df$dep_cat_c <- pica_df$dep_cat - mean(pica_df$dep_cat, na.rm = T)
pica_df$stress_c <- pica_df$stress - mean(pica_df$stress, na.rm = T)
pica_df$socialsupp_c <- pica_df$socialsupp - mean(pica_df$socialsupp, na.rm = T)
pica_df$primary_wat_c <- pica_df$primary_wat - mean(pica_df$primary_wat, na.rm = T)
pica_df$wat_improve_c <- pica_df$wat_improve - mean(pica_df$wat_improve, na.rm = T)
pica_df$wat_tx_c <- pica_df$wat_tx - mean(pica_df$wat_tx, na.rm = T)
pica_df$season_c <- pica_df$season - mean(pica_df$season, na.rm = T)
pica_df$season_drywet_c <- pica_df$season_drywet - mean(pica_df$season_drywet,na.rm = T)
pica_df$season_score_c <- pica_df$season_score - mean(pica_df$season_score,na.rm = T)
pica_df$preg_c <- pica_df$preg-mean(pica_df$preg,na.rm=T)
pica_df$geo_c <- pica_df$geo - mean(pica_df$geo, na.rm = T)
pica_df$amylo_c <- pica_df$amylo - mean(pica_df$amylo, na.rm = T)
pica_df$other_c <- pica_df$other - mean(pica_df$other, na.rm = T)
pica_df$pica_yn_c <- pica_df$pica_yn - mean(pica_df$pica_yn, na.rm = T)
pica_df$fias_c <- pica_df$fias - mean(pica_df$fias, na.rm = T)
pica_df$fi_tert_c <- pica_df$fi_tert - mean(pica_df$fi_tert, na.rm = T)
pica_df$hhs_c <- pica_df$hhs - mean(pica_df$hhs, na.rm = T)
pica_df$hemo_c <- pica_df$hemo - mean(pica_df$hemo, na.rm = T)
pica_df$anemic_c <- pica_df$anemic - mean(pica_df$anemic, na.rm = T)
pica_df$nausea_c <- pica_df$nausea - mean(pica_df$nausea, na.rm = T)
pica_df$diar_c <- pica_df$diar - mean(pica_df$diar, na.rm = T)
pica_df$gidis_yn_c <- pica_df$gidis_yn - mean(pica_df$gidis_yn, na.rm = T)


pica_df$pid <- as.factor(pica_df$pid)
list_of_types <- list_of_types_maker(pica_df[,c('geo', 'pid','hemo_c', 'amylo_c', 'hhs_c', 'stress_c',
                                             'preg_c' , 'diar_c','nausea_c', 'asset_index',
                                             'other_c', 'fefood_c', 'posneg', 'depscore_c')])

#coding of semi-continous variables isn't supported yet by hmi package
list_of_types$hhs_c <- "cont"
list_of_types$stress_c <- "cont"
list_of_types$asset_index <- "cont"
list_of_types$fefood_c <- "cont"

final_formula<-geo~1+hemo_c+amylo_c + hhs_c + stress_c + preg_c + diar_c + 
  nausea_c + asset_index + other_c + fefood_c  + posneg + depscore_c + (1|pid)
set.seed(1)

##NOTE: This takes a long time to run! 
hmi_imp <- hmi(data = pica_df[,c('geo','pid', 'hemo_c', 'amylo_c', 'hhs_c', 'stress_c',
                             'preg_c' , 'diar_c','nausea_c', 'asset_index',
                             'other_c', 'fefood_c', 'posneg', 'depscore_c')],
             model_formula = as.formula(final_formula),list_of_types = list_of_types, M = 10,
             nitt = 100000, maxit = 10)


#######ADDING DERIVED VARIABLES TO HMI IMPUTATIONS#######

geo_final <- readRDS("final_big_imp_hmi.rds")

#checks of model convergence
bwplot(geo_final)
densityplot(geo_final)

geo_complete <- mice::complete(geo_final,"long",inc=TRUE)
colnames(geo_complete)[c(4,5,6,7,9:12,14,18)] <- c("hemo", "amylo", "hhs", "stress", "fefood", "preg",
                                "nausea", "diar", "depscore", "other")

#adding derived variables 
geo_complete$visit <- rep(1:9, times = 4081) #4081 women in our dataset
geo_complete$pica_yn <- ifelse((geo_complete$geo == 1 | geo_complete$amylo == 1 | 
                                  geo_complete$other == 1),1,0)
geo_complete$anemic <- ifelse((geo_complete$preg == 1 & geo_complete$hemo < 11) | 
                                (geo_complete$preg == 0 & geo_complete$hemo < 12),1,0)
geo_complete$dep_cat <- ifelse((geo_complete$depscore > 16),1,0)
geo_complete$gidis_yn <- ifelse((geo_complete$nausea == 1 | geo_complete$diar == 1),1,0)
geo_complete$fefood_yn <- ifelse((geo_complete$fefood > 0),1,0)


geo_complete <- split(geo_complete,geo_complete$.imp)
add_wealth <- function(subset_imp){
  subset_imp <- subset_imp %>% group_by(pid) %>% mutate(hemo_avg=(mean(hemo,na.rm = T)))
  subset_imp$wealth <- ifelse(subset_imp$asset_index > quantile(subset_imp$asset_index, 
                                                                probs = seq(0,1,1/3),na.rm = T)[3],3,
                              ifelse(subset_imp$asset_index >= quantile(subset_imp$asset_index, 
                                                                  probs = seq(0,1,1/3),na.rm = T)[2],2,1))
  return(subset_imp)
}

geo_complete_list <- lapply(geo_complete, FUN = add_wealth)
geo_complete <- rbindlist(geo_complete_list) #using function from data.table



#######ADDING MICE IMPUATIONS##### 

##KATIE: Can you make sure the predictor matrix includes:amylo_child", "fias", "geo_child", "gravidity", "hhloc", "other_child", 
#"parity", "religion", "site", "tribe", "anemic", "dep_cat", "fefood_yn", "fi_tert", "gidis_yn", "pica_child", "pica_yn", "wealth"

prediction_matrix <- as.matrix(data.frame(read_excel("pica_wide_prediction_matrix.xlsx")))
rownames(prediction_matrix) <- prediction_matrix[,1]

# Add a variable for how many times the id*cat combination has occured
tmp <- ddply(pica_df, .(pid, visit), transform, newid = paste(pid, seq_along(visit)))

pica_df_wide <- dcast(setDT(tmp), newid + pid ~  visit, value.var = c("fepill","hemo",
                                                                      "socialstat_comm",
                                                                      "socialstat_country", 
                                                                      "pica_yn", "preg", "fefood", 
                                                                      "socialsupp", "stress", "depscore",
                                                                      "dep_cat","primary_wat",
                                                                      "wat_improve","wat_tx","nausea", 
                                                                      "diar", "gidis_yn", "anemic",
                                                                      "season","season_drywet",
                                                                      "season_score","geo","amylo",
                                                                      "other","fias","fi_tert","hhs",
                                                                      "idate","gestation","fefood_yn"),fill=NA_real_)  #all time variant variables

pica_df_wide <- pica_df_wide[,-1]

#add time constant variables
`%ni%` <- Negate(`%in%`)

#these are time varying 
to_remove <- c("visit","fepill","hemo","socialstat_comm","socialstat_country", 
             "pica_yn", "preg", "fefood", "socialsupp", "stress", "depscore",
             "dep_cat","primary_wat","wat_improve","wat_tx","nausea", "diar", "gidis_yn", 
             "anemic","season","season_drywet","season_score","geo","amylo","other","fias",
             "fi_tert","hhs", "anemic","idate","gestation","fefood_yn")
final_pica_df_wide<-merge(pica_df_wide, subset(pica_df[!duplicated(pica_df$pid),],
                                               select = colnames(pica_df) %ni% to_remove))

#add last visit for each woman
split_pica_by_pid <- split(pica_df, pica_df$pid)
last_visit <- lapply(split_pica_by_pid, function(x) max(x$visit))
last_visit <- data.frame(pid = as.numeric(names(last_visit)), last_visit = unlist(last_visit))
final_pica_df_wide <- merge(final_pica_df_wide, last_visit,by = 'pid')

#make categorical variables factors
cat_vars <- c("pid","site", "tribe", "religion", "relatstat","hhloc",
          "wealth","posneg","pica_child","status", "geo_child", "amylo_child", "other_child","last_visit", 
          paste("pica_yn_", 1:9, sep=''),paste("preg_", 1:9, sep = ''), paste("wat_improve_", 1:9, sep = ''), paste("diar_", 1:9, sep = ''), paste("nausea_", 1:9, sep=''),paste("season_drywet_", 1:9, sep=''),
          paste("geo_", 1:9, sep=''),paste("amylo_", 1:9, sep=''),paste("other_", 1:9, sep=''))
final_pica_df_wide[,cat_vars] <- lapply(final_pica_df_wide[,cat_vars, with = FALSE], factor)


to_impute <- subset(final_pica_df_wide, select = c(colnames(prediction_matrix)))
imp <- mice(to_impute,m = 1,predictorMatrix = prediction_matrix,maxit = 0)
meth <- imp$method

#change imputation methods as necessary to match variable type
#update imputation method for non-continuous variables
imp <- mice(to_impute,m = 10,predictorMatrix = prediction_matrix,maxit = 20,meth = meth)

#convergence checks
bwplot(imp)
densityplot(imp)
plot(imp)

#save completed from MICE
complete <- mice::complete(imp,"long",inc = TRUE) 

#need hhs??!?!?  amylo_child?!?
patterns <- c("hemo", "preg", "fefood", "stress", "depscore", "nausea", 
            "diar", "fias", "geo_child", "amylo_child")
matches <- unique(grep(paste(patterns, collapse="|"), colnames(complete), value=T))
full_time_variant <- reshape(
    complete[, c(".imp", ".id", "pid", matches)],
    idvar = 'pid',
    varying = c(4:75),
    v.names = patterns,
    new.row.names = 1:(nrow(complete) * 9),
    sep = "_",
    direction = 'long'
  )

colname_length<-length(colnames(complete))
full_time_invariant<-complete[,c(1,3, (colname_length- 12):colname_length)]

expand <- full_time_invariant[rep(seq_len(nrow(full_time_invariant)), each=9),]
expand$time <- rep(1:9, times=371)

###KATIE: WHAT IS THIS DOING##
imp_full<-merge(full_time_variant,expand,by=c("pid",".imp","time"))
imp_full$preg[imp_full$time == 1] <- 1
imp_full$preg[imp_full$time == 3] <- 0
imp_full$preg[imp_full$time == 4] <- 0

imp_full<-imp_full[order(imp_full$.imp,imp_full$pid,imp_full$time),]
row.names(imp_full) <- 1:nrow(imp_full)

#rename time to visit
colnames(imp_full)[3] <- "visit"



##add back in derived variables
imp_full$anemic <- ifelse((imp_full$preg == 1 & imp_full$hemo < 11) | 
                            (imp_full$preg == 0 & imp_full$hemo < 12),1,0)
imp_full$gidis_yn <- ifelse((imp_full$nausea == 1 | imp_full$diar == 1),1,0)
imp_full$fefood_yn <- ifelse((imp_full$fefood > 0),1,0)
imp_full$pica_child <- ifelse((imp_full$geo_child == 1 | imp_full$amylo_child == 1 
                               | imp_full$other_child == 1),1,0)
imp_full$pica_yn <- ifelse((imp_full$geo == 1 | imp_full$amylo == 1 | imp_full$other == 1),1,0)
imp_full$visit_2 <- (imp_full$visit)^2
imp_full$dep_cat <- ifelse((imp_full$depscore > 16),1,0)
 


imp_list <- split(imp_full,imp_full$.imp)
add_wealth_fi_tert <- function(subset_imp){
  subset_imp <- subset_imp %>% group_by(pid) %>% mutate(hemo_avg =(mean(hemo,na.rm = T)))
  subset_imp$wealth <- ifelse(subset_imp$asset_index > quantile(subset_imp$asset_index, 
                                                                probs = seq(0,1,1/3),na.rm = T)[3],3,
                              ifelse(subset_imp$asset_index >= quantile(subset_imp$asset_index, 
                                                                        probs = seq(0,1,1/3),na.rm = T)[2],2,1))
  subset_imp$fi_tert <- ifelse(subset_imp$fias > quantile(subset_imp$fias, probs=seq(0,1,1/3),na.rm=T)[3],3,
                               ifelse(subset_imp$fias > quantile(subset_imp$fias, probs=seq(0,1,1/3),na.rm=T)[2],2,1))
  return(subset_imp)
}

mice_complete_list <- lapply(imp_list, FUN = add_wealth_fi_tert)
mice_final <- rbindlist(mice_complete_list) #using function from data.table



#pull the mice-imputed vars
add_to_hmi <- mice_final[,c("amylo_child", "fias", "geo_child", "gravidity", "hhloc", "other_child",
                            "parity", "religion", "site", "tribe", "anemic", "dep_cat", "fefood_yn",
                            "fi_tert", "gidis_yn", "pica_child", "pica_yn", "wealth")]

geo_final<- merge(geo_complete, add_to_hmi,by=c("pid",".imp","visit"))

geo_final<-geo_final[order(geo_final$.imp,geo_final$pid,geo_final$visit),]
row.names(geo_final)<-1:nrow(geo_final)

#some last minute processing
geo_final<-geo_final[,-c(17,21,37)]
colnames(geo_final)[15]<-"tribe"
colnames(geo_final)[30]<-"pica_child"

