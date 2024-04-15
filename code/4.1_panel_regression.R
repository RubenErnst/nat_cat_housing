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
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 182.625)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_0.5" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 365.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_1" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 730.5)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_2" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 merge(merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 1095.75)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_3" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 1461)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_4" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 1826.25)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_5" = disaster_number),
                                             select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 3652.5)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_10" = disaster_number),
                                             by = c("fips_code", "date"), all = TRUE),
                                       by = c("fips_code", "date"), all = TRUE),
                                 by = c("fips_code", "date"), all = TRUE),
                           merge(select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 5478.75)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_15" = disaster_number),
                                 select(aggregate(disaster_number ~ fips_code + date, subset(fema_panel_temp, date >= date_designated & date_designated >= (date - 7305)), function(x){paste(unique(x), collapse = ", ")}), fips_code, date, "dis_lag_20" = disaster_number),
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
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.2),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = 1.3),
                data.frame(plm_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = 1.3))

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
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.2),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "model" = "within", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "model" = "random", "spec" = 1.3),
                       data.frame(f_test_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways"),
                                                 plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "model" = "random", "spec" = 1.3))

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
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.2),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "individual", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "twoways", "spec" = 1.3),
                        data.frame(hausman_results(plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways"),
                                                   plm(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "twoways", "spec" = 1.3))

openxlsx::write.xlsx(list("results" = spec_1, "f-tests" = spec_1_f_test, "hausman-tests" = spec_1_hausman), file = "results/panel_spec_1.xlsx")

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

save(nr_occ_panel, dummy_occ_panel, file = "data/prepared_panels.RData")


# Evaluates to singular matrix
spec_2 <- nlme::lme(zhvi ~ nr_dis_lag_999 + nr_dis_lag_0.5 + nr_dis_lag_1 + nr_dis_lag_5, random = ~ date | fips_code, data = subset(dummy_occ_panel, data_series == "all_homes_top_tier"), na.action = na.omit)



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

incident_type_dict <- subset(unique(select(fema, disaster_number, incident_type)), !(is.na(incident_type) & disaster_number == 1110))
incident_type_dict$incident_type[is.na(incident_type_dict$incident_type)] <- "Other"

incident_map <- setNames(incident_type_dict$incident_type, incident_type_dict$disaster_number)
nr_occ_type_panel$dis_lag_999 <- incident_map[unlist(nr_occ_type_panel$dis_lag_999)]
nr_occ_type_panel$dis_lag_0.5 <- incident_map[unlist(nr_occ_type_panel$dis_lag_0.5)]
nr_occ_type_panel$dis_lag_1 <- incident_map[unlist(nr_occ_type_panel$dis_lag_1)]
nr_occ_type_panel$dis_lag_2 <- incident_map[unlist(nr_occ_type_panel$dis_lag_2)]
nr_occ_type_panel$dis_lag_3 <- incident_map[unlist(nr_occ_type_panel$dis_lag_3)]
nr_occ_type_panel$dis_lag_4 <- incident_map[unlist(nr_occ_type_panel$dis_lag_4)]
nr_occ_type_panel$dis_lag_5 <- incident_map[unlist(nr_occ_type_panel$dis_lag_5)]
nr_occ_type_panel$dis_lag_10 <- incident_map[unlist(nr_occ_type_panel$dis_lag_10)]
nr_occ_type_panel$dis_lag_15 <- incident_map[unlist(nr_occ_type_panel$dis_lag_15)]
nr_occ_type_panel$dis_lag_20 <- incident_map[unlist(nr_occ_type_panel$dis_lag_20)]

nr_occ_type_panel <- subset(nr_occ_type_panel, !(is.na(dis_lag_999) & is.na(dis_lag_0.5) & is.na(dis_lag_1) & is.na(dis_lag_2) & is.na(dis_lag_3) & is.na(dis_lag_4) & is.na(dis_lag_5) & is.na(dis_lag_10) & is.na(dis_lag_15) & is.na(dis_lag_20)))

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
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it)) > 0){
    temp_2 <- select(aggregate(dis_lag_0.5 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_0.5), dis_lag_0.5 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_0.5" = dis_lag_0.5)
  } else {
    temp_2 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_0.5" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it)) > 0){
    temp_3 <- select(aggregate(dis_lag_1 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_1), dis_lag_1 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_1" = dis_lag_1)
  } else {
    temp_3 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_1" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_2), dis_lag_2 == it)) > 0){
    temp_4 <- select(aggregate(dis_lag_2 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_2), dis_lag_2 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_2" = dis_lag_2)
  } else {
    temp_4 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_2" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_3), dis_lag_3 == it)) > 0){
    temp_5 <- select(aggregate(dis_lag_3 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_3), dis_lag_3 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_3" = dis_lag_3)
  } else {
    temp_5 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_3" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_4), dis_lag_4 == it)) > 0){
    temp_6 <- select(aggregate(dis_lag_4 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_4), dis_lag_4 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_4" = dis_lag_4)
  } else {
    temp_6 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_4" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_5), dis_lag_5 == it)) > 0){
    temp_7 <- select(aggregate(dis_lag_5 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_5), dis_lag_5 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_5" = dis_lag_5)
  } else {
    temp_7 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_5" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_10), dis_lag_10 == it)) > 0){
    temp_8 <- select(aggregate(dis_lag_10 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_10), dis_lag_10 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_10" = dis_lag_10)
  } else {
    temp_8 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_10" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_15), dis_lag_15 == it)) > 0){
    temp_9 <- select(aggregate(dis_lag_15 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_15), dis_lag_15 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_15" = dis_lag_15)
  } else {
    temp_9 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_15" = NA)
  }
  
  if (nrow(subset(select(nr_occ_type_panel, fips_code, date, dis_lag_20), dis_lag_20 == it)) > 0){
    temp_10 <- select(aggregate(dis_lag_20 ~ fips_code + date, subset(select(nr_occ_type_panel, fips_code, date, dis_lag_20), dis_lag_20 == it), function(x){ifelse(is.na(x), 0, length(x))}), fips_code, date, "nr_dis_lag_20" = dis_lag_20)
  } else {
    temp_10 <- data.frame("fips_code" = "01100", "date" = as.Date("2000-01-31"), "nr_dis_lag_20" = NA)
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

nr_occ_type_panel <- select(nr_occ_type_panel, -dis_lag_999, -dis_lag_0.5, -dis_lag_1, -dis_lag_2, -dis_lag_3, -dis_lag_4, -dis_lag_5, -dis_lag_10, -dis_lag_15, -dis_lag_20)

nr_occ_type_panel$nr_dis_lag_999[is.na(nr_occ_type_panel$nr_dis_lag_999)] <- 0
nr_occ_type_panel$nr_dis_lag_0.5[is.na(nr_occ_type_panel$nr_dis_lag_0.5)] <- 0
nr_occ_type_panel$nr_dis_lag_1[is.na(nr_occ_type_panel$nr_dis_lag_1)] <- 0
nr_occ_type_panel$nr_dis_lag_2[is.na(nr_occ_type_panel$nr_dis_lag_2)] <- 0
nr_occ_type_panel$nr_dis_lag_3[is.na(nr_occ_type_panel$nr_dis_lag_3)] <- 0
nr_occ_type_panel$nr_dis_lag_4[is.na(nr_occ_type_panel$nr_dis_lag_4)] <- 0
nr_occ_type_panel$nr_dis_lag_5[is.na(nr_occ_type_panel$nr_dis_lag_5)] <- 0
nr_occ_type_panel$nr_dis_lag_10[is.na(nr_occ_type_panel$nr_dis_lag_10)] <- 0
nr_occ_type_panel$nr_dis_lag_15[is.na(nr_occ_type_panel$nr_dis_lag_15)] <- 0
nr_occ_type_panel$nr_dis_lag_20[is.na(nr_occ_type_panel$nr_dis_lag_20)] <- 0

save(nr_occ_panel, dummy_occ_panel, nr_occ_type_panel, file = "data/prepared_panels.RData")


spec_3 <- plm(zhvi ~ nr_dis_lag_0.5 + nr_dis_lag_1 + data_series + incident_type, select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, nr_dis_lag_1), index = c("fips_code", "date"), model = "within", effect = "individual")
