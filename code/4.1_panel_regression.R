rm(list = ls())

library(tidyverse)
library(plm)

load("data/zillow_county.RData")
load("data/fema.RData")

### Prepare panel data ----
# fema_panel <- expand.grid("fips_code" = unique(fema$place_code), "date" = unique(zillow_county$date))
# fema_panel$nr_dis_lag_999 <- NA
# fema_panel$nr_dis_lag_0.5 <- NA
# fema_panel$nr_dis_lag_1 <- NA
# fema_panel$nr_dis_lag_2 <- NA
# fema_panel$nr_dis_lag_3 <- NA
# fema_panel$nr_dis_lag_4 <- NA
# fema_panel$nr_dis_lag_5 <- NA
# fema_panel$nr_dis_lag_10 <- NA
# fema_panel$nr_dis_lag_15 <- NA
# fema_panel$nr_dis_lag_20 <- NA
# 
# average_time <- c()
# 
# for (i in 1:nrow(fema_panel)){
#   start_time <- Sys.time()
#   fema_panel$nr_dis_lag_999[i] <- length(unique(subset(fema, date_designated <= fema_panel$date[i] & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_0.5[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 182.625) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_1[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 365.25) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_2[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 730.5) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_3[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 1095.75) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_4[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 1461) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_5[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 1826.25) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_10[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 3652.5) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_15[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 5478.75) & place_code == fema_panel$fips_code[i])$disaster_number))
#   fema_panel$nr_dis_lag_20[i] <- length(unique(subset(fema, (date_designated <= fema_panel$date[i] & date_designated >= fema_panel$date[i] - 7305) & place_code == fema_panel$fips_code[i])$disaster_number))
#   average_time <- c(average_time, Sys.time() - start_time)
#   
#   if (i %% 500 == 0){
#     print(paste0(i, "/", nrow(fema_panel), " - Mean row time: ", mean(average_time), " - ETA: ", (nrow(fema_panel) - i) * mean(average_time)))
#   }
# }

# Cross product approach
source("code/1_data merging.R")

fema_panel <- data.frame()

for (sc in unique(zillow_county$state_code_fips)){
  zillow_county_slice <- subset(zillow_county, state_code_fips == sc)
  
  fema_panel_temp <- merge(data.frame("fips_code" = fips_pad(zillow_county_slice$state_code_fips, zillow_county_slice$municipal_code_fips), "date" = zillow_county_slice$date),
                           select(fema, disaster_number, place_code, date_designated),
                           by.x = "fips_code", by.y = "place_code", all.x = TRUE)
  
  fema_panel_temp <- merge(merge(merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated), paste, collapse = ", "), fips_code, date, "dis_lag_999" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 182.625)), paste, collapse = ", "), fips_code, date, "dis_lag_0.5" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 365.25)), paste, collapse = ", "), fips_code, date, "dis_lag_1" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 730.5)), paste, collapse = ", "), fips_code, date, "dis_lag_2" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 1095.75)), paste, collapse = ", "), fips_code, date, "dis_lag_3" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 1461)), paste, collapse = ", "), fips_code, date, "dis_lag_4" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 1826.25)), paste, collapse = ", "), fips_code, date, "dis_lag_5" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 3652.5)), paste, collapse = ", "), fips_code, date, "dis_lag_10" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 by = c("fips_code", "date"), all = TRUE),
                           merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 5478.75)), paste, collapse = ", "), fips_code, date, "dis_lag_15" = disaster_number),
                                 select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 7305)), paste, collapse = ", "), fips_code, date, "dis_lag_20" = disaster_number),
                                 by = c("fips_code", "date"), all = TRUE),
                           by = c("fips_code", "date"), all = TRUE)
  
  fema_panel <- rbind(fema_panel, fema_panel_temp)
  print(paste0(sc, " - ", which(unique(zillow_county$state_code_fips) == sc), "/", length(unique(zillow_county$state_code_fips))))
}

# Save binary
save(fema_panel, file = "data/fema_panel.RData")


simple_panel <- merge(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series),
                      data.frame("fips_code" = fema_panel$fips_code, "date" = fema_panel$date,
                                 "nr_dis_lag_999" = sapply(fema_panel$dis_lag_999, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_0.5" = sapply(fema_panel$dis_lag_0.5, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_1" = sapply(fema_panel$dis_lag_1, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_2" = sapply(fema_panel$dis_lag_2, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_3" = sapply(fema_panel$dis_lag_3, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_4" = sapply(fema_panel$dis_lag_4, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_5" = sapply(fema_panel$dis_lag_5, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_10" = sapply(fema_panel$dis_lag_10, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_15" = sapply(fema_panel$dis_lag_15, function(x){length(unique(unlist(str_split(x, ", "))))}),
                                 "nr_dis_lag_20" = sapply(fema_panel$dis_lag_20, function(x){length(unique(unlist(str_split(x, ", "))))})),
                      by = c("fips_code", "date"), all.x = T)


simple_fe <- plm(zhvi ~ nr_dis_lag_1 + data_series, data = simple_panel, index = c("fips_code", "date"), model = "within")
summary(simple_fe)

simple_re <- plm(zhvi ~ nr_dis_lag_1 + data_series, data = simple_panel, index = c("fips_code", "date"), model = "random")
