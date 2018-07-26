#necessary libraries
require(data.table)
require(readxl)
require(plyr)

#imputation packages utilized
require(mice)
install.packages("/hmi_0.9.8.zip", repos = NULL, type = "source")
library("hmi")

#######HMI PACKAGE IMPUTATIONS#######

pica_df <- data.frame(read_excel("Pica_Longitudinal_2018_02_15_kf.xlsx"))

# we center all time-variant variables
variables_to_center<-c("hemo", "fepill", "fefood", "fefood_yn", "socialstat_comm", 
                       "socialstat_country", "depscore", "dep_cat", "stress", "socialsupp",
                       "primary_wat", "wat_improve", "wat_tx", "season", "season_drywet",
                       "season_score", "preg", "geo", "amylo", "other", "pica_yn",
                       "fias", "fi_tert", "hhs", "nausea", "anemic", "diar", "gidis_yn")

pica_df[,variables_to_center] <- apply(pica_df[, variables_to_center], 2, function(x) x - mean(x, na.rm = T))
pica_df$pid <- as.factor(pica_df$pid)
list_of_types <- list_of_types_maker(pica_df[,c('geo', 'pid','hemo', 'amylo', 'hhs', 'stress',
                                             'preg' , 'diar','nausea', 'asset_index',
                                             'other', 'fefood', 'posneg', 'depscore')])

#coding of semi-continous variables isn't supported yet by hmi package
list_of_types$hhs <- "cont"
list_of_types$stress<- "cont"
list_of_types$asset_index <- "cont"
list_of_types$fefood <- "cont"

final_formula<-geo~1+hemo +amylo + hhs + stress + preg + diar + 
  nausea + asset_index + other + fefood  + posneg + depscore + (1|pid)
set.seed(1)

##NOTE: This takes a long time to run! 
hmi_imp <- hmi(data = pica_df[,c('geo','pid', 'hemo', 'amylo', 'hhs', 'stress',
                             'preg' , 'diar','nausea', 'asset_index',
                             'other', 'fefood', 'posneg', 'depscore')],
             model_formula = as.formula(final_formula),list_of_types = list_of_types, M = 10,
             nitt = 100000, maxit = 10)


#######ADDING DERIVED VARIABLES TO HMI IMPUTATIONS#######

#the above takes a long time to run! 
geo_final <- readRDS("final_big_imp_hmi.rds")

#checks of model convergence
bwplot(geo_final)
densityplot(geo_final)

geo_complete <- mice::complete(geo_final,"long",inc=TRUE)[c(1,4,5,6,7,9:12,14,18, 19)]
colnames(geo_complete) <- c(".imp","hemo", "amylo", "hhs", "stress", "fefood", "preg",
                                "nausea", "diar", "depscore", "other", "pid")
# 
# ##If you want to add in derived variables based on the hmi imputations
# geo_complete$visit <- rep(1:9, times = 4081) #4081 women in our dataset
# geo_complete$pica_yn <- ifelse((geo_complete$geo == 1 | geo_complete$amylo == 1 | 
#                                   geo_complete$other == 1),1,0)
# geo_complete$anemic <- ifelse((geo_complete$preg == 1 & geo_complete$hemo < 11) | 
#                                 (geo_complete$preg == 0 & geo_complete$hemo < 12),1,0)
# geo_complete$dep_cat <- ifelse((geo_complete$depscore > 16),1,0)
# geo_complete$gidis_yn <- ifelse((geo_complete$nausea == 1 | geo_complete$diar == 1),1,0)
# geo_complete$fefood_yn <- ifelse((geo_complete$fefood > 0),1,0)
# 
# 
#geo_complete <- split(geo_complete,geo_complete$.imp)
# add_wealth <- function(subset_imp){
#   subset_imp <- subset_imp %>% group_by(pid) %>% mutate(hemo_avg=(mean(hemo,na.rm = T)))
#   subset_imp$wealth <- ifelse(subset_imp$asset_index > quantile(subset_imp$asset_index, 
#                                                                 probs = seq(0,1,1/3),na.rm = T)[3],3,
#                               ifelse(subset_imp$asset_index >= quantile(subset_imp$asset_index, 
#                                                                   probs = seq(0,1,1/3),na.rm = T)[2],2,1))
#   return(subset_imp)
# }

# geo_complete_list <- lapply(geo_complete, FUN = add_wealth)
# geo_complete <- rbindlist(geo_complete_list) #using function from data.table



#######ADDING MICE IMPUATIONS##### 

prediction_matrix <- as.matrix(data.frame(read_excel("prediction_matrix.xlsx")))
prediction_matrix<-prediction_matrix[,-1] #first column unnecessary
rownames(prediction_matrix) <- prediction_matrix[,1]


pica_df <- data.frame(read_excel("Pica_Longitudinal_2018_02_15_kf.xlsx"))
pica_df$pid<-factor(pica_df$pid)
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
final_pica_df_wide <- merge(pica_df_wide, subset(pica_df[!duplicated(pica_df$pid),],
                                               select = colnames(pica_df) %ni% to_remove))

#add last visit for each woman
split_pica_by_pid <- split(pica_df, pica_df$pid)
last_visit <- lapply(split_pica_by_pid, function(x) max(x$visit))
last_visit <- data.frame(pid = as.factor(names(last_visit)), last_visit = unlist(last_visit))
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
complete <- mice::complete(imp,"long",include= TRUE) 

patterns <- c("hemo", "preg", "fefood", "stress", "depscore", "nausea", 
            "diar", "fias")
matches <- grep(paste(patterns, collapse="|"), colnames(complete), value=T)
complete_wide<-complete[, c(".id", ".imp", "pid", matches)]

full_time_variant <- list()
for(i in 1:length(patterns)){
  full_time_variant[[i]]<-reshape(complete_wide[, c(".id", ".imp","pid", grep(patterns[i], colnames(complete_wide), value = T))], 
                    varying=c(4:12),
                    idvar="pid", 
                    new.row.names = 1:(nrow(complete) * 9), sep='_', direction='long')
}

full_time_variant<-Reduce(merge, full_time_variant)


full_time_invariant<- complete[, 1:14]
full_time_invariant <- full_time_invariant[rep(seq_len(nrow(full_time_invariant)), each=9),]
full_time_invariant$time <- as.factor(rep(1:9, times=371))
full_time_invariant<-full_time_invariant[,-2] #deletes pointless ".id" column

imp_full<-merge(full_time_variant,full_time_invariant,by=c("pid",".imp","time"))
imp_full$preg[imp_full$time == 1] <- 1
imp_full$preg[imp_full$time == 3] <- 0
imp_full$preg[imp_full$time == 4] <- 0

imp_full<-imp_full[order(imp_full$.imp,imp_full$pid,imp_full$time),]
row.names(imp_full) <- 1:nrow(imp_full)

#rename time to visit
colnames(imp_full)[3] <- "visit"

##add back in derived variables
imp_full$hemo <-as.numeric(imp_full$hemo)
imp_full$fefood <-as.numeric(imp_full$fefood)
imp_full$depscore <-as.numeric(imp_full$depscore)

imp_full$anemic <- ifelse((imp_full$preg == 1 & imp_full$hemo < 11) | 
                            (imp_full$preg == 0 & imp_full$hemo < 12),1,0)
imp_full$gidis_yn <- ifelse((imp_full$nausea == 1 | imp_full$diar == 1),1,0)
imp_full$fefood_yn <- ifelse((imp_full$fefood > 0),1,0)
imp_full$pica_child <- ifelse((imp_full$geo_child == 1 | imp_full$amylo_child == 1 
                               | imp_full$other_child == 1),1,0)
#imp_full$pica_yn <- ifelse((imp_full$geo == 1 | imp_full$amylo == 1 | imp_full$other == 1),1,0)
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
add_to_hmi <- mice_final[,c("pid", ".imp", "amylo_child", "fias", "geo_child", "gravidity", "hhloc", "other_child",
                            "parity", "religion", "site", "tribe", "anemic", "dep_cat", "fefood_yn",
                            "fi_tert", "gidis_yn", "pica_child",  "wealth", "visit")]
geo_complete$pid <-as.factor(geo_complete$pid)
add_to_hmi$pid <-as.factor(add_to_hmi$pid)


#when all else fails and merge doesn't work
new_geo_final <- cbind(geo_complete, add_to_hmi)
new_geo_final <- new_geo_final [, !duplicated(colnames(new_geo_final))]

#move pid to first column 
new_geo_final<- new_geo_final[,c(which(colnames(new_geo_final)=="pid"),which(colnames(new_geo_final)!="pid"))]

#delete ".imp"==0 *(orig data)
new_geo_final<- new_geo_final[which(new_geo_final$.imp !=0), ]

saveRDS(new_geo_final, 'fin_geo.RDS')