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
  
  fema_panel_temp <- merge(merge(merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_999" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 91.3125)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.25" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 182.625)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.5" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 91.3125) >= date_designated & date_designated >= (date - 182.625)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.5_e" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 365.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_1" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 182.625) >= date_designated & date_designated >= (date - 365.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_1_e" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 365.25) >= date_designated & date_designated >= (date - 730.5)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_2_e" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 730.5) >= date_designated & date_designated >= (date - 1095.75)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_3_e" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 by = c("fips_code", "date"), all = TRUE),
                           merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 1095.75) >= date_designated & date_designated >= (date - 1826.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_5_e" = disaster_number),
                                 select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, (date - 1826.25) >= date_designated & date_designated >= (date - 3652.5)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_10_e" = disaster_number),
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
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.1),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.3))

spec_1_f_test <- rbind(data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.1),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.3))

spec_1_hausman <- rbind(data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.1),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.3),
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
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 2.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 2.1))

spec_2_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 2.2),
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

check_panel <- aggregate(zhvi ~ fips_code + date + data_series, nr_occ_type_panel, function(x){length(na.omit(x))})

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")

nr_occ_type_panel <- merge(splitstackshape::cSplit(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series, "incident_type" = paste(unique(incident_type_dict$incident_type), collapse = ", ")), "incident_type", ", ", "long"),
                           nr_occ_type_panel,
                           by = c("fips_code", "date", "incident_type"), all.x = T)

nr_occ_type_panel$nr_dis_lag_999 <- sapply(nr_occ_type_panel$nr_dis_lag_999, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_0.25 <- sapply(nr_occ_type_panel$nr_dis_lag_0.25, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_0.5 <- sapply(nr_occ_type_panel$nr_dis_lag_0.5, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_0.5_e <- sapply(nr_occ_type_panel$nr_dis_lag_0.5_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_1 <- sapply(nr_occ_type_panel$nr_dis_lag_1, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_1_e <- sapply(nr_occ_type_panel$nr_dis_lag_1_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_2_e <- sapply(nr_occ_type_panel$nr_dis_lag_2_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_3_e <- sapply(nr_occ_type_panel$nr_dis_lag_3_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_5_e <- sapply(nr_occ_type_panel$nr_dis_lag_5_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
nr_occ_type_panel$nr_dis_lag_10_e <- sapply(nr_occ_type_panel$nr_dis_lag_10_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})


save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")


spec_3_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 3.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 3.1))

spec_3_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 3.2),
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
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 4.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 4.1))

spec_4_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 4.2),
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
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 5.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 5.1))

spec_5_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 5.2),
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
    temp_1 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_999" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.25), dis_lag_0.25 == it)) > 0){
    temp_2 <- select(aggregate(dis_lag_0.25 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.25), dis_lag_0.25 == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_0.25" = dis_lag_0.25)
  } else {
    temp_2 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.25" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it)) > 0){
    temp_3 <- select(aggregate(dis_lag_0.5 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_0.5" = dis_lag_0.5)
  } else {
    temp_3 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5_e), dis_lag_0.5_e == it)) > 0){
    temp_4 <- select(aggregate(dis_lag_0.5_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_0.5_e), dis_lag_0.5_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_0.5_e" = dis_lag_0.5_e)
  } else {
    temp_4 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it)) > 0){
    temp_5 <- select(aggregate(dis_lag_1 ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_1" = dis_lag_1)
  } else {
    temp_5 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_1" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1_e), dis_lag_1_e == it)) > 0){
    temp_6 <- select(aggregate(dis_lag_1_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_1_e), dis_lag_1_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_1_e" = dis_lag_1_e)
  } else {
    temp_6 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_1_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_2_e), dis_lag_2_e == it)) > 0){
    temp_7 <- select(aggregate(dis_lag_2_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_2_e), dis_lag_2_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_2_e" = dis_lag_2_e)
  } else {
    temp_7 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_2_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_3_e), dis_lag_3_e == it)) > 0){
    temp_8 <- select(aggregate(dis_lag_3_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_3_e), dis_lag_3_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_3_e" = dis_lag_3_e)
  } else {
    temp_8 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_3_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_5_e), dis_lag_5_e == it)) > 0){
    temp_9 <- select(aggregate(dis_lag_5_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_5_e), dis_lag_5_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_5_e" = dis_lag_5_e)
  } else {
    temp_9 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_5_e" = NA)
  }
  
  if (nrow(subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_10_e), dis_lag_10_e == it)) > 0){
    temp_10 <- select(aggregate(dis_lag_10_e ~ fips_code + date, subset(select(dummy_occ_type_panel, fips_code, date, dis_lag_10_e), dis_lag_10_e == it), function(x){ifelse(is.na(x), 0, 1)}), fips_code, date, "nr_dis_lag_10_e" = dis_lag_10_e)
  } else {
    temp_10 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_10_e" = NA)
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

check_panel <- aggregate(zhvi ~ fips_code + date + data_series, dummy_occ_type_panel, function(x){length(na.omit(x))})

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, dummy_occ_type_panel, file = "data/prepared_panels.RData")

dummy_occ_type_panel <- merge(splitstackshape::cSplit(data.frame("fips_code" = fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips), "date" = zillow_county$date, "zhvi" = zillow_county$zhvi, "data_series" = zillow_county$data_series, "incident_type" = paste(unique(incident_type_dict$incident_type), collapse = ", ")), "incident_type", ", ", "long"),
                              dummy_occ_type_panel,
                              by = c("fips_code", "date", "incident_type"), all.x = T)

dummy_occ_type_panel$nr_dis_lag_999 <- sapply(dummy_occ_type_panel$nr_dis_lag_999, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_0.25 <- sapply(dummy_occ_type_panel$nr_dis_lag_0.25, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_0.5 <- sapply(dummy_occ_type_panel$nr_dis_lag_0.5, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_0.5_e <- sapply(dummy_occ_type_panel$nr_dis_lag_0.5_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_1 <- sapply(dummy_occ_type_panel$nr_dis_lag_1, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_1_e <- sapply(dummy_occ_type_panel$nr_dis_lag_1_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_2_e <- sapply(dummy_occ_type_panel$nr_dis_lag_2_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_3_e <- sapply(dummy_occ_type_panel$nr_dis_lag_3_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_5_e <- sapply(dummy_occ_type_panel$nr_dis_lag_5_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})
dummy_occ_type_panel$nr_dis_lag_10_e <- sapply(dummy_occ_type_panel$nr_dis_lag_10_e, function(x){ifelse(is.null(unlist(x)) | is.na(unlist(x)), 0, unlist(x))})


save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, dummy_occ_type_panel, file = "data/prepared_panels.RData")


# Dummy spec with incident type
spec_6_1 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 6.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 6.1))

spec_6_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 6.2),
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
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 7.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 7.1))

spec_7_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 7.2),
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
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = 8.1),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = 8.1))

spec_8_2 <- rbind(data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 8.2),
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
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = 8.3),
                  data.frame(plm_results(plm(zhvi ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = 8.3))

openxlsx::write.xlsx(rbind(spec_8_1, spec_8_2, spec_8_3), file = "results/panel_spec_8.xlsx")



