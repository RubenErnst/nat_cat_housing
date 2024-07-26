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


### Spec 6.3 (Credit constraint / credit quality using HMDA) ----
load("data/hmda_panels.RData")

ltv_panel_median <- subset(ltv_panel_median, !is.na(median_loan_to_value_ratio))
dti_panel_median <- subset(dti_panel_median, !is.na(median_debt_to_income_ratio))

spec_5_3_panel_maj$year <- lubridate::year(spec_5_3_panel_maj$date)
spec_5_3_panel_maj <- merge(spec_5_3_panel_maj,
                            ltv_panel_median,
                            by.x = c("fips_code", "year"), by.y = c("fips_code", "as_of_year"), all.x = T)
spec_5_3_panel_maj <- merge(spec_5_3_panel_maj,
                            dti_panel_median,
                            by.x = c("fips_code", "year"), by.y = c("fips_code", "as_of_year"), all.x = T)

ltv_mapping <- list("ltv_low" = c(0, 80),
                    "ltv_medium" = c(80, 95),
                    "ltv_high" = c(95, 100))

dti_mapping <- list("dti_low" = c(0, 35),
                    "dti_medium" = c(35, 50),
                    "dti_high" = c(50, 999))

spec_6_3_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_low[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_low", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_medium[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_medium", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_high[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_high", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_low[1] & median_debt_to_income_ratio <= dti_mapping$dti_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_low", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_medium[1] & median_debt_to_income_ratio <= dti_mapping$dti_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_medium", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_high[1] & median_debt_to_income_ratio <= dti_mapping$dti_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_high", "spec" = "6.3"))

openxlsx::write.xlsx(spec_6_3_final_maj, file = "results/final/spec_6_3_final_maj.xlsx")

# Compile non-amortizing loan features as subprime proxies
subprime_panel <- subset(subprime_panel, as_of_year >= 2018)

subprime_mapping <- list("sp_low" = c(0, 0.05),
                         "sp_medium" = c(0.05, 0.15),
                         "sp_high" = c(0.15, 1))

spec_6_3_extensions_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & perc_at_least_one_namo > subprime_mapping$sp_low[1] & perc_at_least_one_namo <= subprime_mapping$sp_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "sp_perc_at_least_one_low", "spec" = "6.3_e"),
                                 data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & perc_at_least_one_namo > subprime_mapping$sp_medium[1] & perc_at_least_one_namo <= subprime_mapping$sp_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "sp_perc_at_least_one_medium", "spec" = "6.3_e"),
                                 data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & perc_at_least_one_namo > subprime_mapping$sp_high[1] & perc_at_least_one_namo <= subprime_mapping$sp_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "sp_perc_at_least_one_high", "spec" = "6.3_e"),
                                 data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & perc_three_namo > subprime_mapping$sp_low[1] & perc_three_namo <= subprime_mapping$sp_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "sp_perc_three_low", "spec" = "6.3_e"),
                                 data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & perc_three_namo > subprime_mapping$sp_medium[1] & perc_three_namo <= subprime_mapping$sp_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "sp_perc_three_medium", "spec" = "6.3_e"),
                                 data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & perc_three_namo > subprime_mapping$sp_high[1] & perc_three_namo <= subprime_mapping$sp_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "sp_perc_three_high", "spec" = "6.3_e"))

openxlsx::write.xlsx(spec_6_3_extensions_maj, file = "results/final/spec_6_3_extensions_maj.xlsx")


### Spec 6.4 (Housing type) ----
spec_6_4_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "stratum" = "all_homes_bottom_tier", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "all_homes_middle_tier", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "stratum" = "all_homes_top_tier", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "stratum" = "single_family_homes", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "stratum" = "condo_coop", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "one_bedroom", "effect" = "both", "stratum" = "one_bedroom", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "two_bedroom", "effect" = "both", "stratum" = "two_bedroom", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "three_bedroom", "effect" = "both", "stratum" = "three_bedroom", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "four_bedroom", "effect" = "both", "stratum" = "four_bedroom", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "five_plus_bedroom", "effect" = "both", "stratum" = "five_plus_bedroom", "spec" = "6.4"))

openxlsx::write.xlsx(spec_6_4_final_maj, file = "results/final/spec_6_4_final_maj.xlsx")

