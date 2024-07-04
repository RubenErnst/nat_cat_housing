rm(list = ls())

library(tidyverse)
library(plm)

source("code/1_data merging.R")

# Definitions
incident_type_excl <- c("Biological", "Chemical", "Other", "Toxic Substances")
time_period_spec <- list("pre_gfc" = c("2000-01-31", "2007-12-31"),
                         "post_gfc" = c("2009-01-31", "2019-12-31"),
                         "post_covid" = c("2021-01-31", "2024-02-29"),
                         "bush" = c("2001-01-31", "2008-12-31"),
                         "obama" = c("2009-01-31", "2016-12-31"),
                         "trump" = c("2017-01-31", "2020-12-31"),
                         "biden" = c("2021-01-31", "2024-02-29"))

### Spec 6.1 (Econ time frames) ----
load("data/spec_5_3_panel_maj_wide.RData")

spec_6_1_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$pre_gfc[1] & date <= time_period_spec$pre_gfc[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "pre_gfc", "spec" = "6.1"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$post_gfc[1] & date <= time_period_spec$post_gfc[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "post_gfc", "spec" = "6.1"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$post_covid[1] & date <= time_period_spec$post_covid[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "post_covid", "spec" = "6.1"))

openxlsx::write.xlsx(spec_6_1_final_maj, file = "results/final/spec_6_1_final_maj.xlsx")


### Spec 6.2 (Political time frames) ----
spec_6_2_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$bush[1] & date <= time_period_spec$bush[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "bush", "spec" = "6.2"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$obama[1] & date <= time_period_spec$obama[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "obama", "spec" = "6.2"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$trump[1] & date <= time_period_spec$trump[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "trump", "spec" = "6.2"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$biden[1] & date <= time_period_spec$biden[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "biden", "spec" = "6.2"))

openxlsx::write.xlsx(spec_6_2_final_maj, file = "results/final/spec_6_2_final_maj.xlsx")


### Spec 6.3 