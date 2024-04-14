rm(list = ls())

library(tidyverse)
library(plm)

load("data/zillow_county.RData")
load("data/fema.RData")

### Prepare panel data ----
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



### Number of occurrences ----
nr_occ_panel <- merge(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series),
                      data.frame("fips_code" = fema_panel$fips_code, "date" = fema_panel$date,
                                 "nr_dis_lag_999" = sapply(fema_panel$dis_lag_999, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_0.5" = sapply(fema_panel$dis_lag_0.5, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_1" = sapply(fema_panel$dis_lag_1, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_2" = sapply(fema_panel$dis_lag_2, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_3" = sapply(fema_panel$dis_lag_3, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_4" = sapply(fema_panel$dis_lag_4, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_5" = sapply(fema_panel$dis_lag_5, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_10" = sapply(fema_panel$dis_lag_10, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_15" = sapply(fema_panel$dis_lag_15, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_20" = sapply(fema_panel$dis_lag_20, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))})),
                      by = c("fips_code", "date"), all.x = T)


spec_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 1.3))

spec_1_hausman <- rbind(data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 1.3))


### Dummy occurrence ----
dummy_occ_panel <- merge(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series),
                         data.frame("fips_code" = fema_panel$fips_code, "date" = fema_panel$date,
                                    "nr_dis_lag_999" = sapply(fema_panel$dis_lag_999, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_0.5" = sapply(fema_panel$dis_lag_0.5, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_1" = sapply(fema_panel$dis_lag_1, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_2" = sapply(fema_panel$dis_lag_2, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_3" = sapply(fema_panel$dis_lag_3, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_4" = sapply(fema_panel$dis_lag_4, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_5" = sapply(fema_panel$dis_lag_5, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_10" = sapply(fema_panel$dis_lag_10, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_15" = sapply(fema_panel$dis_lag_15, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_20" = sapply(fema_panel$dis_lag_20, function(x){ifelse(is.na(x), 0, 1)})),
                         by = c("fips_code", "date"), all.x = T)

spec_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 2.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 2.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 2.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 2.3))

spec_2_hausman <- rbind(data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 2.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 2.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 2.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 2.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 2.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 2.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 2.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 2.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_top_tier", "spec" = 2.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random")), "data_series" = "all_homes_bottom_tier", "spec" = 2.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random")), "data_series" = "single_family_homes", "spec" = 2.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(dummy_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random")), "data_series" = "condo_coop", "spec" = 2.3))


### Number of occurrences by disaster type ----
nr_occ_type_panel <- splitstackshape::cSplit(fema_panel, splitCols = c("dis_lag_999", "dis_lag_0.5", "dis_lag_1", "dis_lag_2", "dis_lag_3", "dis_lag_4", "dis_lag_5", "dis_lag_10", "dis_lag_15", "dis_lag_20"), sep = ", ", direction = "long")
nr_occ_type_panel$dis_lag_999 <- as.integer(nr_occ_type_panel$dis_lag_999)
nr_occ_type_panel$dis_lag_0.5 <- as.integer(nr_occ_type_panel$dis_lag_0.5)
nr_occ_type_panel$dis_lag_1 <- as.integer(nr_occ_type_panel$dis_lag_1)
nr_occ_type_panel$dis_lag_2 <- as.integer(nr_occ_type_panel$dis_lag_2)
nr_occ_type_panel$dis_lag_3 <- as.integer(nr_occ_type_panel$dis_lag_3)
nr_occ_type_panel$dis_lag_4 <- as.integer(nr_occ_type_panel$dis_lag_4)
nr_occ_type_panel$dis_lag_5 <- as.integer(nr_occ_type_panel$dis_lag_5)
nr_occ_type_panel$dis_lag_10 <- as.integer(nr_occ_type_panel$dis_lag_10)
nr_occ_type_panel$dis_lag_15 <- as.integer(nr_occ_type_panel$dis_lag_15)
nr_occ_type_panel$dis_lag_20 <- as.integer(nr_occ_type_panel$dis_lag_20)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_999" = incident_type), by.x = "dis_lag_999", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_0.5" = incident_type), by.x = "dis_lag_0.5", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_1" = incident_type), by.x = "dis_lag_1", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_2" = incident_type), by.x = "dis_lag_2", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_3" = incident_type), by.x = "dis_lag_3", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_4" = incident_type), by.x = "dis_lag_4", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_5" = incident_type), by.x = "dis_lag_5", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_10" = incident_type), by.x = "dis_lag_10", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_15" = incident_type), by.x = "dis_lag_15", by.y = "disaster_number", all.x = TRUE)
nr_occ_type_panel <- merge(nr_occ_type_panel, select(fema, disaster_number, "dis_type_lag_20" = incident_type), by.x = "dis_lag_20", by.y = "disaster_number", all.x = TRUE)

