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
                           select(fema, disaster_number, place_code, date_incident_begin),
                           by.x = "fips_code", by.y = "place_code", all.x = TRUE)
  
  fema_panel_temp <- merge(merge(merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_incident_begin), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_999" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_incident_begin & date_incident_begin >= (date - 91.3125)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.25" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_incident_begin & date_incident_begin >= (date - 182.625)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.5" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 91.3125) >= date_incident_begin & date_incident_begin >= (date - 182.625)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.5_e" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_incident_begin & date_incident_begin >= (date - 365.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_1" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 182.625) >= date_incident_begin & date_incident_begin >= (date - 365.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_1_e" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 365.25) >= date_incident_begin & date_incident_begin >= (date - 730.5)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_2_e" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 730.5) >= date_incident_begin & date_incident_begin >= (date - 1095.75)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_3_e" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 by = c("fips_code", "date"), all = TRUE),
                           merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 1095.75) >= date_incident_begin & date_incident_begin >= (date - 1826.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_5_e" = disaster_number),
                                 select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 1826.25) >= date_incident_begin & date_incident_begin >= (date - 3652.5)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_10_e" = disaster_number),
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
                                 "nr_dis_lag_0.25" = sapply(fema_panel$dis_lag_0.25, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_0.5" = sapply(fema_panel$dis_lag_0.5, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_0.5_e" = sapply(fema_panel$dis_lag_0.5_e, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_1" = sapply(fema_panel$dis_lag_1, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_1_e" = sapply(fema_panel$dis_lag_1_e, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_2_e" = sapply(fema_panel$dis_lag_2_e, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_3_e" = sapply(fema_panel$dis_lag_3_e, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_5_e" = sapply(fema_panel$dis_lag_5_e, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))}),
                                 "nr_dis_lag_10_e" = sapply(fema_panel$dis_lag_10_e, function(x){ifelse(is.na(x), 0, length(unique(unlist(str_split(x, ", ")))))})),
                      by = c("fips_code", "date"), all.x = T)

save(nr_occ_panel, file = "data/prepared_panels.RData")

spec_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.3))

spec_1_f_test <- rbind(data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.3))

spec_1_hausman <- rbind(data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.3))

openxlsx::write.xlsx(list("results" = spec_1, "f-tests" = spec_1_f_test, "hausman-tests" = spec_1_hausman), file = "results/panel_spec_1.xlsx")

### Dummy occurrence ----
dummy_occ_panel <- merge(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series),
                         data.frame("fips_code" = fema_panel$fips_code, "date" = fema_panel$date,
                                    "nr_dis_lag_999" = sapply(fema_panel$dis_lag_999, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_0.25" = sapply(fema_panel$dis_lag_0.25, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_0.5" = sapply(fema_panel$dis_lag_0.5, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_0.5_e" = sapply(fema_panel$dis_lag_0.5_e, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_1" = sapply(fema_panel$dis_lag_1, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_1_e" = sapply(fema_panel$dis_lag_1_e, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_2_e" = sapply(fema_panel$dis_lag_2_e, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_3_e" = sapply(fema_panel$dis_lag_3_e, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_5_e" = sapply(fema_panel$dis_lag_5_e, function(x){ifelse(is.na(x), 0, 1)}),
                                    "nr_dis_lag_10_e" = sapply(fema_panel$dis_lag_10_e, function(x){ifelse(is.na(x), 0, 1)})),
                         by = c("fips_code", "date"), all.x = T)

save(nr_occ_panel, dummy_occ_panel, file = "data/prepared_panels.RData")


# # Evaluates to singular matrix
# spec_2 <- nlme::lme(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, random = ~ date | fips_code, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), na.action = na.omit)

spec_2_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 2.1))

spec_2_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 2.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 2.2))


# Add time fixed effects per year
spec_2_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 2.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 2.3))

openxlsx::write.xlsx(rbind(spec_2_1, spec_2_2, spec_2_3), file = "results/panel_spec_2.xlsx")



### Number of occurrences by disaster type ----
nr_occ_type_panel <- splitstackshape::cSplit(fema_panel, splitCols = c("dis_lag_999", "dis_lag_0.25", "dis_lag_0.5", "dis_lag_0.5_e", "dis_lag_1", "dis_lag_1_e", "dis_lag_2_e", "dis_lag_3_e", "dis_lag_5_e", "dis_lag_10_e"), sep = ", ", direction = "long")
nr_occ_type_panel$dis_lag_999 <- as.integer(nr_occ_type_panel$dis_lag_999)
nr_occ_type_panel$dis_lag_0.25 <- as.integer(nr_occ_type_panel$dis_lag_0.25)
nr_occ_type_panel$dis_lag_0.5 <- as.integer(nr_occ_type_panel$dis_lag_0.5)
nr_occ_type_panel$dis_lag_0.5_e <- as.integer(nr_occ_type_panel$dis_lag_0.5_e)
nr_occ_type_panel$dis_lag_1 <- as.integer(nr_occ_type_panel$dis_lag_1)
nr_occ_type_panel$dis_lag_1_e <- as.integer(nr_occ_type_panel$dis_lag_1_e)
nr_occ_type_panel$dis_lag_2_e <- as.integer(nr_occ_type_panel$dis_lag_2_e)
nr_occ_type_panel$dis_lag_3_e <- as.integer(nr_occ_type_panel$dis_lag_3_e)
nr_occ_type_panel$dis_lag_5_e <- as.integer(nr_occ_type_panel$dis_lag_5_e)
nr_occ_type_panel$dis_lag_10_e <- as.integer(nr_occ_type_panel$dis_lag_10_e)

# Disaster 1110 has two qualifications: Fire and NA --> remove NA
incident_type_dict <- subset(unique(select(fema, disaster_number, incident_type)), !(is.na(incident_type) & disaster_number == 1110))
# 4 Disasters have no qualification: move to Other category
incident_type_dict$incident_type[is.na(incident_type_dict$incident_type)] <- "Other"

incident_map <- setNames(incident_type_dict$incident_type, incident_type_dict$disaster_number)
nr_occ_type_panel$dis_lag_999 <- incident_map[unlist(nr_occ_type_panel$dis_lag_999)]
nr_occ_type_panel$dis_lag_0.25 <- incident_map[unlist(nr_occ_type_panel$dis_lag_0.25)]
nr_occ_type_panel$dis_lag_0.5 <- incident_map[unlist(nr_occ_type_panel$dis_lag_0.5)]
nr_occ_type_panel$dis_lag_0.5_e <- incident_map[unlist(nr_occ_type_panel$dis_lag_0.5_e)]
nr_occ_type_panel$dis_lag_1 <- incident_map[unlist(nr_occ_type_panel$dis_lag_1)]
nr_occ_type_panel$dis_lag_1_e <- incident_map[unlist(nr_occ_type_panel$dis_lag_1_e)]
nr_occ_type_panel$dis_lag_2_e <- incident_map[unlist(nr_occ_type_panel$dis_lag_2_e)]
nr_occ_type_panel$dis_lag_3_e <- incident_map[unlist(nr_occ_type_panel$dis_lag_3_e)]
nr_occ_type_panel$dis_lag_5_e <- incident_map[unlist(nr_occ_type_panel$dis_lag_5_e)]
nr_occ_type_panel$dis_lag_10_e <- incident_map[unlist(nr_occ_type_panel$dis_lag_10_e)]

nr_occ_type_panel <- subset(nr_occ_type_panel, !(is.na(dis_lag_999) & is.na(dis_lag_0.25) & is.na(dis_lag_0.5) & is.na(dis_lag_0.5_e) & is.na(dis_lag_1) & is.na(dis_lag_1_e) & is.na(dis_lag_2_e) & is.na(dis_lag_3_e) & is.na(dis_lag_5_e) & is.na(dis_lag_10_e)))

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")


# Aggregate into frequency table
temp_nr_occ_type_panel <- data.frame()
for (it in unique(incident_type_dict$incident_type)){
  # This is not ideal but the agg functions fail otherwise
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_999), dis_lag_999 == it)) > 0){
    temp_1 <- select(data.frame(aggregate(dis_lag_999 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_999), dis_lag_999 == it), function(x){ifelse(is.na(x), 0, length(x))}), "incident_type" = it), fips_code, date, incident_type, "nr_dis_lag_999" = dis_lag_999)
  } else {
    temp_1 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_999" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.25), dis_lag_0.25 == it)) > 0){
    temp_2 <- select(aggregate(dis_lag_0.25 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.25), dis_lag_0.25 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_0.25" = dis_lag_0.25)
  } else {
    temp_2 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.25" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it)) > 0){
    temp_3 <- select(aggregate(dis_lag_0.5 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_0.5" = dis_lag_0.5)
  } else {
    temp_3 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.5_e), dis_lag_0.5_e == it)) > 0){
    temp_4 <- select(aggregate(dis_lag_0.5_e ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.5_e), dis_lag_0.5_e == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_0.5_e" = dis_lag_0.5_e)
  } else {
    temp_4 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5_e" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it)) > 0){
    temp_5 <- select(aggregate(dis_lag_1 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_1" = dis_lag_1)
  } else {
    temp_5 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_1" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_1_e), dis_lag_1_e == it)) > 0){
    temp_6 <- select(aggregate(dis_lag_1_e ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_1_e), dis_lag_1_e == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_1_e" = dis_lag_1_e)
  } else {
    temp_6 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_1_e" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_2_e), dis_lag_2_e == it)) > 0){
    temp_7 <- select(aggregate(dis_lag_2_e ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_2_e), dis_lag_2_e == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_2_e" = dis_lag_2_e)
  } else {
    temp_7 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_2_e" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_3_e), dis_lag_3_e == it)) > 0){
    temp_8 <- select(aggregate(dis_lag_3_e ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_3_e), dis_lag_3_e == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_3_e" = dis_lag_3_e)
  } else {
    temp_8 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_3_e" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_5_e), dis_lag_5_e == it)) > 0){
    temp_9 <- select(aggregate(dis_lag_5_e ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_5_e), dis_lag_5_e == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_5_e" = dis_lag_5_e)
  } else {
    temp_9 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_5_e" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_10_e), dis_lag_10_e == it)) > 0){
    temp_10 <- select(aggregate(dis_lag_10_e ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_10_e), dis_lag_10_e == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_10_e" = dis_lag_10_e)
  } else {
    temp_10 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_10_e" = NA)
  }
  
  temp_nr_occ_type_panel <- rbind(temp_nr_occ_type_panel,
                                  merge(merge(merge(merge(temp_1,
                                                          temp_2,
                                                          by = c("fips_code", "date"), all = TRUE),
                                                    merge(temp_3,
                                                          temp_4,
                                                          by = c("fips_code", "date"), all = TRUE),
                                                    by = c("fips_code", "date"), all = TRUE),
                                              merge(merge(temp_5,
                                                          temp_6,
                                                          by = c("fips_code", "date"), all = TRUE),
                                                    merge(temp_7,
                                                          temp_8,
                                                          by = c("fips_code", "date"), all = TRUE),
                                                    by = c("fips_code", "date"), all = TRUE),
                                              by = c("fips_code", "date"), all = TRUE),
                                        merge(temp_9,
                                              temp_10,
                                              by = c("fips_code", "date"), all = TRUE),
                                        by = c("fips_code", "date"), all = TRUE))
  print(paste0(it, " - ", which(unique(incident_type_dict$incident_type) == it), "/", length(unique(incident_type_dict$incident_type))))
}

nr_occ_type_panel <- arrange(temp_nr_occ_type_panel, fips_code, date, incident_type)

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")

nr_occ_type_panel <- merge(splitstackshape::cSplit(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series, "incident_type" = paste(unique(incident_type_dict$incident_type), collapse = ", ")), "incident_type", ", ", "long"),
                           nr_occ_type_panel,
                           by = c("fips_code", "date", "incident_type"), all.x = T)

nr_occ_type_panel$nr_dis_lag_999 <- sapply(nr_occ_type_panel$nr_dis_lag_999, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_0.25 <- sapply(nr_occ_type_panel$nr_dis_lag_0.25, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_0.5 <- sapply(nr_occ_type_panel$nr_dis_lag_0.5, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_0.5_e <- sapply(nr_occ_type_panel$nr_dis_lag_0.5_e, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_1 <- sapply(nr_occ_type_panel$nr_dis_lag_1, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_1_e <- sapply(nr_occ_type_panel$nr_dis_lag_1_e, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_2_e <- sapply(nr_occ_type_panel$nr_dis_lag_2_e, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_3_e <- sapply(nr_occ_type_panel$nr_dis_lag_3_e, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_5_e <- sapply(nr_occ_type_panel$nr_dis_lag_5_e, function(x){ifelse(is.null(x), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_10_e <- sapply(nr_occ_type_panel$nr_dis_lag_10_e, function(x){ifelse(is.null(x), 0, unlist(x))})

nr_occ_type_panel$nr_dis_lag_999[is.na(nr_occ_type_panel$nr_dis_lag_999)] <- 0
nr_occ_type_panel$nr_dis_lag_0.25[is.na(nr_occ_type_panel$nr_dis_lag_0.25)] <- 0
nr_occ_type_panel$nr_dis_lag_0.5[is.na(nr_occ_type_panel$nr_dis_lag_0.5)] <- 0
nr_occ_type_panel$nr_dis_lag_0.5_e[is.na(nr_occ_type_panel$nr_dis_lag_0.5_e)] <- 0
nr_occ_type_panel$nr_dis_lag_1[is.na(nr_occ_type_panel$nr_dis_lag_1)] <- 0
nr_occ_type_panel$nr_dis_lag_1_e[is.na(nr_occ_type_panel$nr_dis_lag_1_e)] <- 0
nr_occ_type_panel$nr_dis_lag_2_e[is.na(nr_occ_type_panel$nr_dis_lag_2_e)] <- 0
nr_occ_type_panel$nr_dis_lag_3_e[is.na(nr_occ_type_panel$nr_dis_lag_3_e)] <- 0
nr_occ_type_panel$nr_dis_lag_5_e[is.na(nr_occ_type_panel$nr_dis_lag_5_e)] <- 0
nr_occ_type_panel$nr_dis_lag_10_e[is.na(nr_occ_type_panel$nr_dis_lag_10_e)] <- 0

check_panel <- aggregate(zhvi ~ fips_code + date + data_series + incident_type, nr_occ_type_panel, function(x){length(na.omit(x))})
stopifnot(sum(check_panel$zhvi > 1) == 0)

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")


spec_3_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 3.1))

spec_3_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 3.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 3.2))


# Add time fixed effects per year
spec_3_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 3.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 3.3))

openxlsx::write.xlsx(rbind(spec_3_1, spec_3_2, spec_3_3), file = "results/panel_spec_3.xlsx")


### Number of occurrences by disaster type including economic variables ----
load("data/bea_gdp.RData")
bea <- subset(bea, industry_id == 1 & (unit == "Millions of current dollars" | unit == "Thousands of dollars"))
bea <- subset(bea, !duplicated(select(bea, fips_code, year)))

# Assimilate units
bea$gdp_value <- as.numeric(bea$gdp_value)
bea$gdp_value[bea$unit == "Thousands of dollars"] <- bea$gdp_value[bea$unit == "Thousands of dollars"] / 1e3
bea$table_name <- "MAGDP2"
bea$unit <- "Millions of current dollars"

# Adjust BEA coded FIPS codes
bea_codes <- subset(bea, fips_code %in% bea_fips_mod$BEA.FIPS)
bea <- subset(bea, !fips_code %in% bea_fips_mod$BEA.FIPS)

# Add number of counties that BEA has aggregated to not inflate GDP numbers after assignment
bea_fips_mod <- merge(bea_fips_mod,
                      select(aggregate(FIPS ~ BEA.FIPS, bea_fips_mod, function(x){length(unique(x))}), BEA.FIPS, "nr_counties" = FIPS),
                      by = "BEA.FIPS", all.x = TRUE)

bea_codes <- merge(splitstackshape::cSplit(data.frame("fips_code" = bea_fips_mod$FIPS,
                                                      "nr_counties" = bea_fips_mod$nr_counties,
                                                      "fips_code_bea" = as.character(bea_fips_mod$BEA.FIPS),
                                                      "year" = paste(min(bea$year):max(bea$year), collapse = ", ")), "year", ", ", "long"),
                   bea_codes,
                   by.x = c("fips_code_bea", "year"), by.y = c("fips_code", "year"), all.x = TRUE)

bea_codes <- subset(bea_codes, !is.na(table_name))
bea_codes$gdp_value <- bea_codes$gdp_value / bea_codes$nr_counties
bea_codes$fips_code_bea <- NULL
bea_codes$nr_counties <- NULL

bea <- rbind(bea, bea_codes); rm(bea_codes)


# Extend type panel
nr_occ_type_panel$year <- lubridate::year(nr_occ_type_panel$date)
nr_occ_type_panel <- merge(nr_occ_type_panel,
                           select(bea, fips_code, year, gdp_value),
                           by = c("fips_code", "year"), all.x = TRUE)


# Add BLS LAUS data
load("data/bls_laus.RData")

laus$fips_code <- fips_pad(laus$state_code, laus$county_code)
laus$year <- as.integer(laus$year)
laus$unemployment_rate <- as.numeric(laus$unemployment_rate)

nr_occ_type_panel <- merge(nr_occ_type_panel,
                           select(laus, fips_code, year, unemployment_rate),
                           by = c("fips_code", "year"), all.x = TRUE)


# Add BLS QCEW data
load("data/bls_qcew.RData")

nr_occ_type_panel$quarter <- lubridate::quarter(nr_occ_type_panel$date)

qcew <- subset(qcew, agglvl_code == 70)

# Prune avg weekly wage of 0
qcew <- subset(qcew, avg_wkly_wage != 0)

nr_occ_type_panel <- merge(nr_occ_type_panel,
                           select(qcew, "fips_code" = area_fips, year, "quarter" = qtr, avg_wkly_wage),
                           by = c("fips_code", "year", "quarter"), all.x = TRUE)


save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")

# Run specification 4
spec_4_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 4.1))

spec_4_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 4.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 4.2))


# Add time fixed effects per year
spec_4_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 4.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 4.3))

openxlsx::write.xlsx(rbind(spec_4_1, spec_4_2, spec_4_3), file = "results/panel_spec_4.xlsx")


# Including GDP (reduces observations to 2017-2022)
spec_5_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 5.1))

spec_5_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 5.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 5.2))


# Add time fixed effects per year
spec_5_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 5.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 5.3))

openxlsx::write.xlsx(rbind(spec_5_1, spec_5_2, spec_5_3), file = "results/panel_spec_5.xlsx")




### Dummy occurrence by type ----
dummy_occ_type_panel <- splitstackshape::cSplit(fema_panel, splitCols = c("dis_lag_999", "dis_lag_0.25", "dis_lag_0.5", "dis_lag_0.5_e", "dis_lag_1", "dis_lag_1_e", "dis_lag_2_e", "dis_lag_3_e", "dis_lag_5_e", "dis_lag_10_e"), sep = ", ", direction = "long")
dummy_occ_type_panel$dis_lag_999 <- as.integer(dummy_occ_type_panel$dis_lag_999)
dummy_occ_type_panel$dis_lag_0.25 <- as.integer(dummy_occ_type_panel$dis_lag_0.25)
dummy_occ_type_panel$dis_lag_0.5 <- as.integer(dummy_occ_type_panel$dis_lag_0.5)
dummy_occ_type_panel$dis_lag_0.5_e <- as.integer(dummy_occ_type_panel$dis_lag_0.5_e)
dummy_occ_type_panel$dis_lag_1 <- as.integer(dummy_occ_type_panel$dis_lag_1)
dummy_occ_type_panel$dis_lag_1_e <- as.integer(dummy_occ_type_panel$dis_lag_1_e)
dummy_occ_type_panel$dis_lag_2_e <- as.integer(dummy_occ_type_panel$dis_lag_2_e)
dummy_occ_type_panel$dis_lag_3_e <- as.integer(dummy_occ_type_panel$dis_lag_3_e)
dummy_occ_type_panel$dis_lag_5_e <- as.integer(dummy_occ_type_panel$dis_lag_5_e)
dummy_occ_type_panel$dis_lag_10_e <- as.integer(dummy_occ_type_panel$dis_lag_10_e)

dummy_occ_type_panel$dis_lag_999 <- incident_map[unlist(dummy_occ_type_panel$dis_lag_999)]
dummy_occ_type_panel$dis_lag_0.25 <- incident_map[unlist(dummy_occ_type_panel$dis_lag_0.25)]
dummy_occ_type_panel$dis_lag_0.5 <- incident_map[unlist(dummy_occ_type_panel$dis_lag_0.5)]
dummy_occ_type_panel$dis_lag_0.5_e <- incident_map[unlist(dummy_occ_type_panel$dis_lag_0.5_e)]
dummy_occ_type_panel$dis_lag_1 <- incident_map[unlist(dummy_occ_type_panel$dis_lag_1)]
dummy_occ_type_panel$dis_lag_1_e <- incident_map[unlist(dummy_occ_type_panel$dis_lag_1_e)]
dummy_occ_type_panel$dis_lag_2_e <- incident_map[unlist(dummy_occ_type_panel$dis_lag_2_e)]
dummy_occ_type_panel$dis_lag_3_e <- incident_map[unlist(dummy_occ_type_panel$dis_lag_3_e)]
dummy_occ_type_panel$dis_lag_5_e <- incident_map[unlist(dummy_occ_type_panel$dis_lag_5_e)]
dummy_occ_type_panel$dis_lag_10_e <- incident_map[unlist(dummy_occ_type_panel$dis_lag_10_e)]

dummy_occ_type_panel <- subset(dummy_occ_type_panel, !(is.na(dis_lag_999) & is.na(dis_lag_0.25) & is.na(dis_lag_0.5) & is.na(dis_lag_0.5_e) & is.na(dis_lag_1) & is.na(dis_lag_1_e) & is.na(dis_lag_2_e) & is.na(dis_lag_3_e) & is.na(dis_lag_5_e) & is.na(dis_lag_10_e)))

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, dummy_occ_type_panel, file = "data/prepared_panels.RData")


# Aggregate into frequency table
temp_dummy_occ_type_panel <- data.frame()
for (it in unique(incident_type_dict$incident_type)){
  # This is not ideal but the agg functions fail otherwise
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_999), dis_lag_999 == it)) > 0){
    temp_1 <- select(data.frame(aggregate(dis_lag_999 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_999), dis_lag_999 == it), function(x){ifelse(is.na(x), 0, 1)}), "incident_type" = it), fips_code, date, incident_type, "nr_dis_lag_999" = dis_lag_999)
  } else {
    temp_1 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_999" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.25), dis_lag_0.25 == it)) > 0){
    temp_2 <- select(aggregate(dis_lag_0.25 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.25), dis_lag_0.25 == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_0.25" = dis_lag_0.25)
  } else {
    temp_2 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.25" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it)) > 0){
    temp_3 <- select(aggregate(dis_lag_0.5 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_0.5" = dis_lag_0.5)
  } else {
    temp_3 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5_e), dis_lag_0.5_e == it)) > 0){
    temp_4 <- select(aggregate(dis_lag_0.5_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5_e), dis_lag_0.5_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_0.5_e" = dis_lag_0.5_e)
  } else {
    temp_4 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it)) > 0){
    temp_5 <- select(aggregate(dis_lag_1 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_1" = dis_lag_1)
  } else {
    temp_5 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_1" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1_e), dis_lag_1_e == it)) > 0){
    temp_6 <- select(aggregate(dis_lag_1_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1_e), dis_lag_1_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_1_e" = dis_lag_1_e)
  } else {
    temp_6 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_1_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_2_e), dis_lag_2_e == it)) > 0){
    temp_7 <- select(aggregate(dis_lag_2_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_2_e), dis_lag_2_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_2_e" = dis_lag_2_e)
  } else {
    temp_7 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_2_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_3_e), dis_lag_3_e == it)) > 0){
    temp_8 <- select(aggregate(dis_lag_3_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_3_e), dis_lag_3_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_3_e" = dis_lag_3_e)
  } else {
    temp_8 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_3_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_5_e), dis_lag_5_e == it)) > 0){
    temp_9 <- select(aggregate(dis_lag_5_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_5_e), dis_lag_5_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_5_e" = dis_lag_5_e)
  } else {
    temp_9 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_5_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_10_e), dis_lag_10_e == it)) > 0){
    temp_10 <- select(aggregate(dis_lag_10_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_10_e), dis_lag_10_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_10_e" = dis_lag_10_e)
  } else {
    temp_10 <- data.frame("fips_code" = "99999", "date" = as.Date("2000-01-31"), "nr_dis_lag_10_e" = NA)
  }
  
  temp_dummy_occ_type_panel <- rbind(temp_dummy_occ_type_panel,
                                     merge(merge(merge(merge(temp_1,
                                                             temp_2,
                                                             by = c("fips_code", "date"), all = TRUE),
                                                       merge(temp_3,
                                                             temp_4,
                                                             by = c("fips_code", "date"), all = TRUE),
                                                       by = c("fips_code", "date"), all = TRUE),
                                                 merge(merge(temp_5,
                                                             temp_6,
                                                             by = c("fips_code", "date"), all = TRUE),
                                                       merge(temp_7,
                                                             temp_8,
                                                             by = c("fips_code", "date"), all = TRUE),
                                                       by = c("fips_code", "date"), all = TRUE),
                                                 by = c("fips_code", "date"), all = TRUE),
                                           merge(temp_9,
                                                 temp_10,
                                                 by = c("fips_code", "date"), all = TRUE),
                                           by = c("fips_code", "date"), all = TRUE))
  print(paste0(it, " - ", which(unique(incident_type_dict$incident_type) == it), "/", length(unique(incident_type_dict$incident_type))))
}

dummy_occ_type_panel <- arrange(temp_dummy_occ_type_panel, fips_code, date, incident_type)

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, dummy_occ_type_panel, file = "data/prepared_panels.RData")

dummy_occ_type_panel <- merge(splitstackshape::cSplit(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series, "incident_type" = paste(unique(incident_type_dict$incident_type), collapse = ", ")), "incident_type", ", ", "long"),
                              dummy_occ_type_panel,
                              by = c("fips_code", "date", "incident_type"), all.x = T)

dummy_occ_type_panel$nr_dis_lag_999 <- sapply(dummy_occ_type_panel$nr_dis_lag_999, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_0.25 <- sapply(dummy_occ_type_panel$nr_dis_lag_0.25, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_0.5 <- sapply(dummy_occ_type_panel$nr_dis_lag_0.5, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_0.5_e <- sapply(dummy_occ_type_panel$nr_dis_lag_0.5_e, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_1 <- sapply(dummy_occ_type_panel$nr_dis_lag_1, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_1_e <- sapply(dummy_occ_type_panel$nr_dis_lag_1_e, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_2_e <- sapply(dummy_occ_type_panel$nr_dis_lag_2_e, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_3_e <- sapply(dummy_occ_type_panel$nr_dis_lag_3_e, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_5_e <- sapply(dummy_occ_type_panel$nr_dis_lag_5_e, function(x){ifelse(is.null(x), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_10_e <- sapply(dummy_occ_type_panel$nr_dis_lag_10_e, function(x){ifelse(is.null(x), 0, unlist(x))})

dummy_occ_type_panel$nr_dis_lag_999[is.na(dummy_occ_type_panel$nr_dis_lag_999)] <- 0
dummy_occ_type_panel$nr_dis_lag_0.25[is.na(dummy_occ_type_panel$nr_dis_lag_0.25)] <- 0
dummy_occ_type_panel$nr_dis_lag_0.5[is.na(dummy_occ_type_panel$nr_dis_lag_0.5)] <- 0
dummy_occ_type_panel$nr_dis_lag_0.5_e[is.na(dummy_occ_type_panel$nr_dis_lag_0.5_e)] <- 0
dummy_occ_type_panel$nr_dis_lag_1[is.na(dummy_occ_type_panel$nr_dis_lag_1)] <- 0
dummy_occ_type_panel$nr_dis_lag_1_e[is.na(dummy_occ_type_panel$nr_dis_lag_1_e)] <- 0
dummy_occ_type_panel$nr_dis_lag_2_e[is.na(dummy_occ_type_panel$nr_dis_lag_2_e)] <- 0
dummy_occ_type_panel$nr_dis_lag_3_e[is.na(dummy_occ_type_panel$nr_dis_lag_3_e)] <- 0
dummy_occ_type_panel$nr_dis_lag_5_e[is.na(dummy_occ_type_panel$nr_dis_lag_5_e)] <- 0
dummy_occ_type_panel$nr_dis_lag_10_e[is.na(dummy_occ_type_panel$nr_dis_lag_10_e)] <- 0

# check_panel <- aggregate(zhvi ~ fips_code + date + data_series + incident_type, dummy_occ_type_panel, function(x){length(na.omit(x))})

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, dummy_occ_type_panel, file = "data/prepared_panels.RData")


# Dummy spec with incident type
spec_6_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 6.1))

spec_6_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 6.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 6.2))


# Add time fixed effects per year
spec_6_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 6.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 6.3))

openxlsx::write.xlsx(rbind(spec_6_1, spec_6_2, spec_6_3), file = "results/panel_spec_6.xlsx")


# Add economic variables
dummy_occ_type_panel$year <- lubridate::year(dummy_occ_type_panel$date)
dummy_occ_type_panel$quarter <- lubridate::quarter(dummy_occ_type_panel$date)

dummy_occ_type_panel <- merge(dummy_occ_type_panel,
                              select(bea, fips_code, year, gdp_value),
                              by = c("fips_code", "year"), all.x = TRUE)

dummy_occ_type_panel <- merge(dummy_occ_type_panel,
                              select(laus, fips_code, year, unemployment_rate),
                              by = c("fips_code", "year"), all.x = TRUE)

dummy_occ_type_panel <- merge(dummy_occ_type_panel,
                              select(qcew, "fips_code" = area_fips, year, "quarter" = qtr, avg_wkly_wage),
                              by = c("fips_code", "year", "quarter"), all.x = TRUE)


# Run dummy panel with incident type and econ variables
spec_7_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 7.1))

spec_7_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 7.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 7.2))


# Add time fixed effects per year
spec_7_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 7.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 7.3))

openxlsx::write.xlsx(rbind(spec_7_1, spec_7_2, spec_7_3), file = "results/panel_spec_7.xlsx")


# Including GDP (reduces observations to 2017-2022)
spec_8_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 8.1))

spec_8_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 8.2),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 8.2))


# Add time fixed effects per year
spec_8_3 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 8.3))

openxlsx::write.xlsx(rbind(spec_8_1, spec_8_2, spec_8_3), file = "results/panel_spec_8.xlsx")



### Disaster cost panel ----
cost_dis_panel <- unique(select(fema, disaster_number, place_code, date_incident_begin, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))

load("data/county_population_estimate.RData")
cost_dis_panel$year <- lubridate::year(cost_dis_panel$date_incident_begin)
pop_norm <- merge(unique(select(cost_dis_panel, disaster_number, place_code, year)),
                  select(county_population, fips_code, year, population_estimate),
                  by.x = c("place_code", "year"), by.y = c("fips_code", "year"), all.x = TRUE)

pop_norm <- select(aggregate(population_estimate ~ disaster_number + year, pop_norm, sum, na.rm = TRUE), disaster_number, year, "total_pop_aff" = population_estimate)

cost_dis_panel <- merge(subset(cost_dis_panel, !is.na(year)),
                        pop_norm,
                        by = c("disaster_number", "year"), all.x = TRUE)

cost_dis_panel <- subset(cost_dis_panel, !is.na(total_pop_aff))
cost_dis_panel <- subset(cost_dis_panel, !(is.na(amount_ihp_approved) & is.na(amount_ha_approved) & is.na(amount_ona_approved) & is.na(amount_pa_obligated) & is.na(amount_catab_obligated) & is.na(amount_catc2g_obligated) & is.na(amount_hmgp_obligated)))

cost_dis_panel$pp_ihp <- cost_dis_panel$amount_ihp_approved / cost_dis_panel$total_pop_aff
cost_dis_panel$pp_pa <- cost_dis_panel$amount_pa_obligated / cost_dis_panel$total_pop_aff
cost_dis_panel$pp_hmgp <- cost_dis_panel$amount_hmgp_obligated / cost_dis_panel$total_pop_aff

# See separate reasoning
cost_dis_panel[is.na(cost_dis_panel)] <- 0

save(cost_dis_panel, file = "data/cost_dis_panel.RData")


# Create lagged panel with cost received as disaster severity proxy
cost_panel <- splitstackshape::cSplit(fema_panel, splitCols = c("dis_lag_999", "dis_lag_0.25", "dis_lag_0.5", "dis_lag_0.5_e", "dis_lag_1", "dis_lag_1_e", "dis_lag_2_e", "dis_lag_3_e", "dis_lag_5_e", "dis_lag_10_e"), sep = ", ", direction = "long")
cost_panel$dis_lag_999 <- as.integer(cost_panel$dis_lag_999)
cost_panel$dis_lag_0.25 <- as.integer(cost_panel$dis_lag_0.25)
cost_panel$dis_lag_0.5 <- as.integer(cost_panel$dis_lag_0.5)
cost_panel$dis_lag_0.5_e <- as.integer(cost_panel$dis_lag_0.5_e)
cost_panel$dis_lag_1 <- as.integer(cost_panel$dis_lag_1)
cost_panel$dis_lag_1_e <- as.integer(cost_panel$dis_lag_1_e)
cost_panel$dis_lag_2_e <- as.integer(cost_panel$dis_lag_2_e)
cost_panel$dis_lag_3_e <- as.integer(cost_panel$dis_lag_3_e)
cost_panel$dis_lag_5_e <- as.integer(cost_panel$dis_lag_5_e)
cost_panel$dis_lag_10_e <- as.integer(cost_panel$dis_lag_10_e)

cost_panel <- pivot_longer(cost_panel, cols = starts_with("dis_lag_"), names_to = "lag", values_to = "disaster_number", values_drop_na = TRUE)

# Check if single column merge condition is given
stopifnot(length(unique(cost_dis_panel$disaster_number)) == nrow(unique(select(cost_dis_panel, disaster_number, year))))
cost_panel <- merge(cost_panel,
                    select(cost_dis_panel, "fips_code" = place_code, disaster_number, starts_with("pp_")),
                    by = c("disaster_number", "fips_code"), all.x = TRUE)

cost_panel <- subset(cost_panel, !(is.na(pp_ihp) & is.na(pp_pa) & is.na(pp_hmgp)))

cost_panel <- pivot_longer(cost_panel, cols = starts_with("pp_"), names_to = "assistance_type", values_to = "pp_assistance")
cost_panel <- pivot_wider(cost_panel, id_cols = c("disaster_number", "fips_code", "date", "assistance_type"), names_from = "lag", values_from = "pp_assistance")

cost_panel$dis_lag_999 <- sapply(cost_panel$dis_lag_999, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_0.25 <- sapply(cost_panel$dis_lag_0.25, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_0.5 <- sapply(cost_panel$dis_lag_0.5, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_0.5_e <- sapply(cost_panel$dis_lag_0.5_e, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_1 <- sapply(cost_panel$dis_lag_1, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_1_e <- sapply(cost_panel$dis_lag_1_e, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_2_e <- sapply(cost_panel$dis_lag_2_e, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_3_e <- sapply(cost_panel$dis_lag_3_e, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_5_e <- sapply(cost_panel$dis_lag_5_e, function(x){ifelse(is.na(x), 0, x)})
cost_panel$dis_lag_10_e <- sapply(cost_panel$dis_lag_10_e, function(x){ifelse(is.na(x), 0, x)})

# Save binary
save(cost_panel, file = "data/prepared_cost_panel.RData")


# Add disaster type
cost_panel$disaster_number <- as.character(cost_panel$disaster_number)
cost_panel$incident_type <- incident_map[unlist(cost_panel$disaster_number)]

# Aggregate if multiple disasters in time / entity
cost_panel <- aggregate(cbind(dis_lag_999, dis_lag_0.25, dis_lag_0.5, dis_lag_0.5_e, dis_lag_1_e, dis_lag_1, dis_lag_2_e, dis_lag_3_e, dis_lag_5_e, dis_lag_10_e) ~ fips_code + date + incident_type + assistance_type, cost_panel, sum, na.rm = TRUE)

save(cost_panel, file = "data/prepared_cost_panel.RData")

# Merge with ZHVI
cost_panel <- merge(splitstackshape::cSplit(splitstackshape::cSplit(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series, "incident_type" = paste(unique(cost_panel$incident_type), collapse = ", "), "assistance_type" = "pp_ihp, pp_pa, pp_hmgp"), "incident_type", ", ", "long"), "assistance_type", ", ", "long"),
                    cost_panel,
                    by = c("fips_code", "date", "incident_type", "assistance_type"), all.x = T)


cost_panel$dis_lag_999 <- sapply(cost_panel$dis_lag_999, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_0.25 <- sapply(cost_panel$dis_lag_0.25, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_0.5 <- sapply(cost_panel$dis_lag_0.5, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_0.5_e <- sapply(cost_panel$dis_lag_0.5_e, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_1 <- sapply(cost_panel$dis_lag_1, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_1_e <- sapply(cost_panel$dis_lag_1_e, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_2_e <- sapply(cost_panel$dis_lag_2_e, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_3_e <- sapply(cost_panel$dis_lag_3_e, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_5_e <- sapply(cost_panel$dis_lag_5_e, function(x){ifelse(is.null(x), 0, unlist(x))})
cost_panel$dis_lag_10_e <- sapply(cost_panel$dis_lag_10_e, function(x){ifelse(is.null(x), 0, unlist(x))})

cost_panel$dis_lag_999[is.na(cost_panel$dis_lag_999)] <- 0
cost_panel$dis_lag_0.25[is.na(cost_panel$dis_lag_0.25)] <- 0
cost_panel$dis_lag_0.5[is.na(cost_panel$dis_lag_0.5)] <- 0
cost_panel$dis_lag_0.5_e[is.na(cost_panel$dis_lag_0.5_e)] <- 0
cost_panel$dis_lag_1[is.na(cost_panel$dis_lag_1)] <- 0
cost_panel$dis_lag_1_e[is.na(cost_panel$dis_lag_1_e)] <- 0
cost_panel$dis_lag_2_e[is.na(cost_panel$dis_lag_2_e)] <- 0
cost_panel$dis_lag_3_e[is.na(cost_panel$dis_lag_3_e)] <- 0
cost_panel$dis_lag_5_e[is.na(cost_panel$dis_lag_5_e)] <- 0
cost_panel$dis_lag_10_e[is.na(cost_panel$dis_lag_10_e)] <- 0


# Add economic variables
cost_panel$year <- lubridate::year(cost_panel$date)
cost_panel$quarter <- lubridate::quarter(cost_panel$date)
cost_panel <- merge(cost_panel,
                    select(bea, fips_code, year, gdp_value),
                    by = c("fips_code", "year"), all.x = TRUE)

cost_panel <- merge(cost_panel,
                    select(laus, fips_code, year, unemployment_rate),
                    by = c("fips_code", "year"), all.x = TRUE)

cost_panel <- merge(cost_panel,
                    select(qcew, "fips_code" = area_fips, year, "quarter" = qtr, avg_wkly_wage),
                    by = c("fips_code", "year", "quarter"), all.x = TRUE)

# check_panel <- aggregate(zhvi ~ fips_code + date + data_series, cost_panel, function(x){length(na.omit(x))})

save(cost_panel, file = "data/prepared_cost_panel.RData")


# Run cost spec
spec_9_1 <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if (nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0){
        eval(parse(text = paste0("spec_9_1_part <- data.frame(plm_results(plm(zhvi ~ dis_lag_0.25, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = 9.1)")))
      } else {
        spec_9_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = '"entity"', "spec" = 9.1)
      }
      spec_9_1 <- rbind(spec_9_1, spec_9_1_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

spec_9_2 <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if (nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0){
        eval(parse(text = paste0("spec_9_2_part <- data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = 9.2)")))
      } else {
        spec_9_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = '"entity"', "spec" = 9.2)
      }
      spec_9_2 <- rbind(spec_9_2, spec_9_2_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

# Add time fixed effects per year
spec_9_3 <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if (nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0){
        eval(parse(text = paste0("spec_9_3_part <- data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + factor(lubridate::year(date)), subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'both', 'spec' = 9.3)")))
      } else {
        spec_9_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = '"entity"', "spec" = 9.3)
      }
      spec_9_3 <- rbind(spec_9_3, spec_9_3_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

openxlsx::write.xlsx(rbind(spec_9_1, spec_9_2, spec_9_3), file = "results/panel_spec_9.xlsx")


# Run cost spec with economic variables
spec_10_1 <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if (nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0){
        eval(parse(text = paste0("spec_10_1_part <- data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = 10.1)")))
      } else {
        spec_10_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = '"entity"', "spec" = 10.1)
      }
      spec_10_1 <- rbind(spec_10_1, spec_10_1_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

spec_10_2 <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if (nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0){
        eval(parse(text = paste0("spec_10_2_part <- data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = 10.2)")))
      } else {
        spec_10_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = '"entity"', "spec" = 10.2)
      }
      spec_10_2 <- rbind(spec_10_2, spec_10_2_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

# Add time fixed effects per year
spec_10_3 <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if (nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0){
        eval(parse(text = paste0("spec_10_3_part <- data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + gdp_value + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = 10.3)")))
      } else {
        spec_10_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = '"entity"', "spec" = 10.3)
      }
      spec_10_3 <- rbind(spec_10_3, spec_10_3_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

openxlsx::write.xlsx(rbind(spec_10_1, spec_10_2, spec_10_3), file = "results/panel_spec_10.xlsx")


# Aggregate all assistance cost
cost_agg_panel <- aggregate(cbind(dis_lag_999, dis_lag_0.25, dis_lag_0.5, dis_lag_0.5_e, dis_lag_1_e, dis_lag_1, dis_lag_2_e, dis_lag_3_e, dis_lag_5_e, dis_lag_10_e) ~ fips_code + date + data_series + incident_type, cost_panel, sum, na.rm = TRUE)

# Add economic variables
cost_agg_panel <- merge(cost_agg_panel,
                        select(bea, fips_code, year, gdp_value),
                        by = c("fips_code", "year"), all.x = TRUE)

cost_agg_panel <- merge(cost_agg_panel,
                        select(laus, fips_code, year, unemployment_rate),
                        by = c("fips_code", "year"), all.x = TRUE)

cost_agg_panel <- merge(cost_agg_panel,
                        select(qcew, "fips_code" = area_fips, year, "quarter" = qtr, avg_wkly_wage),
                        by = c("fips_code", "year", "quarter"), all.x = TRUE)

save(cost_agg_panel, file = "data/prepared_cost_agg_panel.RData")


# Run aggregated cost spec
spec_11_1 <- rbind(data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 11.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 11.1))

spec_11_2 <- rbind(data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 11.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 11.2))


# Add time fixed effects per year
spec_11_3 <- rbind(data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 11.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 11.3))

openxlsx::write.xlsx(rbind(spec_11_1, spec_11_2, spec_11_3), file = "results/panel_spec_11.xlsx")

# Run cost spec with economic variables
spec_12_1 <- rbind(data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 12.1),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 12.1))

spec_12_2 <- rbind(data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 12.2),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 12.2))


# Add time fixed effects per year
spec_12_3 <- rbind(data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 12.3),
                   data.frame(plm_results(plm(zhvi ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 12.3))

openxlsx::write.xlsx(rbind(spec_12_1, spec_12_2, spec_12_3), file = "results/panel_spec_12.xlsx")
