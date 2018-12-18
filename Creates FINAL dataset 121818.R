library(dplyr)
library(mice)
library(miceadds)
library(WriteXLS)

fin_geo <- readRDS("./fin_geo.rds")
geo_complete <- mice::complete(fin_geo, include = T, action = "long")
geo_complete <- geo_complete[,-which(names(geo_complete) %in% "avg_hemo")]
geo_complete <- geo_complete %>% group_by(.imp, pid) %>% mutate(hemo_avg = mean(hemo, na.rm = T))
geo_complete_without_original <- geo_complete[geo_complete$.imp != 0, ]
WriteXLS(geo_complete, "geo_complete_121818.xlsx")
WriteXLS(geo_complete_without_original, "geo_complete_without_orignial_121818.xlsx")

geo_complete <- as.mids(geo_complete)
saveRDS(geo_complete, "geo_complete_121818.RDS")
