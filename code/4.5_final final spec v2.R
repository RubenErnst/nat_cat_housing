rm(list = ls())

library(tidyverse)
library(plm)
library(DBI)
library(RSQLite)

load("data/fema.RData")
load("data/fema_panel.RData")
load("results/fema_multis.RData")

load("data/nr_occ_type_panel.RData")
load("data/nr_occ_type_panel_maj.RData")

source("code/1_data merging.R")

# Definitions
incident_type_excl <- c("Biological", "Chemical", "Drought", "Fishing Losses", "Other", "Toxic Substances")
main_db <- dbConnect(DBI::dbDriver("SQLite"), "data/assistance_panels.sqlite")


### Spec 5.1 ----
load("data/spec_5_1_panel_maj_fg.RData")

spec_1 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.2"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.3"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.4"))

openxlsx::write.xlsx(spec_1, file = "results/final final/v2/spec_1_maj.xlsx")

load("data/spec_5_1_b_panel_maj_fg.RData")

spec_1_b <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (zero == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (zero == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"))

openxlsx::write.xlsx(spec_1_b, file = "results/final final/v2/spec_1_b_maj.xlsx")



### Spec 5.2 ----
load("data/spec_5_2_panel_fg.RData")
load("data/decl_inc_group_type_panel_fg.RData")

spec_5_2_panel_group <- merge(spec_5_2_panel,
                              select(decl_inc_group_type_panel, fips_code, date, incident_category_man, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                              by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)
spec_5_2_panel_group$perc_maj_0.25[is.na(spec_5_2_panel_group$perc_maj_0.25)] <- 0
spec_5_2_panel_group$perc_maj_0.5[is.na(spec_5_2_panel_group$perc_maj_0.5)] <- 0
spec_5_2_panel_group$perc_maj_0.5_e[is.na(spec_5_2_panel_group$perc_maj_0.5_e)] <- 0
spec_5_2_panel_group$perc_maj_1[is.na(spec_5_2_panel_group$perc_maj_1)] <- 0
spec_5_2_panel_group$perc_maj_1_e[is.na(spec_5_2_panel_group$perc_maj_1_e)] <- 0

spec_2 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.1"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.2"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.3"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Cold"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Cold", "effect" = "both", "spec" = "2.4"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.5"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.6"))

openxlsx::write.xlsx(spec_2, file = "results/final final/v2/spec_2.xlsx")



### Spec 5.3 ----
load("data/spec_5_3_panel_wide_fg.RData")

spec_3 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_cold + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_cold * perc_maj_0.25_cold + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.1"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.2"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_cold + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * perc_maj_1_water + nr_dis_lag_1_wind * perc_maj_1_wind + nr_dis_lag_1_geological * perc_maj_1_geological + nr_dis_lag_1_cold * perc_maj_1_cold + nr_dis_lag_1_fire * perc_maj_1_fire + nr_dis_lag_1_human_cause * perc_maj_1_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.3"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_cold + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_cold * perc_maj_0.25_cold + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.4"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.5"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_cold + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * perc_maj_1_water + nr_dis_lag_1_wind * perc_maj_1_wind + nr_dis_lag_1_geological * perc_maj_1_geological + nr_dis_lag_1_cold * perc_maj_1_cold + nr_dis_lag_1_fire * perc_maj_1_fire + nr_dis_lag_1_human_cause * perc_maj_1_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.6"))

openxlsx::write.xlsx(spec_3, file = "results/final final/v2/spec_3.xlsx")



### Spec 6.1&2 ----
spec_5_3_panel_maj <- subset(select(nr_occ_type_panel_maj, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_3_panel_maj <- merge(spec_5_3_panel_maj, fema_incident_mapping_final, by = "incident_type", all.x = TRUE)

save(spec_5_3_panel_maj, file = "data/spec_5_3_panel_fg_maj.RData")

temp <- select(spec_5_3_panel_maj, -starts_with("nr_dis_lag"), -incident_type)[!(duplicated(spec_5_3_panel_maj[,c("fips_code", "date", "data_series", "incident_category_man")])),]
spec_5_3_panel_maj <- merge(aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1) ~ fips_code + date + data_series + incident_category_man, spec_5_3_panel_maj, sum, na.rm = TRUE),
                            temp,
                            by = c("fips_code", "date", "data_series", "incident_category_man"), all.x = TRUE);rm(temp)

save(spec_5_3_panel_maj, file = "data/spec_5_3_panel_fg_maj.RData")

load("data/multi_occ_maj.RData")
spec_5_3_panel_maj <- anti_join(spec_5_3_panel_maj,
                                subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.25, multi_occ_maj_0.5_e, multi_occ_maj_1_e), !(is.na(multi_occ_maj_0.25) & is.na(multi_occ_maj_0.5_e) & is.na(multi_occ_maj_1_e))),
                                by = c("fips_code", "date", "data_series"))

spec_5_3_panel_maj <- merge(pivot_wider(pivot_longer(spec_5_3_panel_maj, cols = starts_with("nr_dis_lag_") | starts_with("perc_maj_"), names_to = "variable", values_to = "value"),
                                        id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
                            select(spec_5_3_panel_maj, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_3_panel_maj[c("fips_code", "date", "data_series")])),],
                            by = c("fips_code", "date", "data_series"), all.x = TRUE)

save(spec_5_3_panel_maj, file = "data/spec_5_3_panel_wide_fg_maj.RData")


time_period_spec <- list("pre_gfc" = c("2000-01-31", "2007-12-31"),
                         "post_gfc" = c("2009-01-31", "2019-12-31"),
                         "post_covid" = c("2021-01-31", "2024-02-29"),
                         "bush" = c("2001-01-31", "2008-12-31"),
                         "obama" = c("2009-01-31", "2016-12-31"),
                         "trump" = c("2017-01-31", "2020-12-31"),
                         "biden" = c("2021-01-31", "2024-02-29"))

spec_6_1n2_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$pre_gfc[1] & date <= time_period_spec$pre_gfc[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "pre_gfc", "spec" = "6.1"),
                              data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$post_gfc[1] & date <= time_period_spec$post_gfc[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "post_gfc", "spec" = "6.1"),
                              data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$post_covid[1] & date <= time_period_spec$post_covid[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "post_covid", "spec" = "6.1"),
                              data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$bush[1] & date <= time_period_spec$bush[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "bush", "spec" = "6.2"),
                              data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$obama[1] & date <= time_period_spec$obama[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "obama", "spec" = "6.2"),
                              data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$trump[1] & date <= time_period_spec$trump[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "trump", "spec" = "6.2"),
                              data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & date >= time_period_spec$biden[1] & date <= time_period_spec$biden[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "biden", "spec" = "6.2"))

openxlsx::write.xlsx(spec_6_1n2_final_maj, file = "results/final final/v2/spec_6_1&2_maj.xlsx")



### Spec 6.3 ----
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

spec_6_3_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_low[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_low", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_medium[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_medium", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_high[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_high", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_low[1] & median_debt_to_income_ratio <= dti_mapping$dti_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_low", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_medium[1] & median_debt_to_income_ratio <= dti_mapping$dti_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_medium", "spec" = "6.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_high[1] & median_debt_to_income_ratio <= dti_mapping$dti_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_high", "spec" = "6.3"))

openxlsx::write.xlsx(spec_6_3_final_maj, file = "results/final final/v2/spec_6_3_maj.xlsx")



### Spec 6.4 ----
spec_6_4_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "stratum" = "all_homes_bottom_tier", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "all_homes_middle_tier", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "stratum" = "all_homes_top_tier", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "stratum" = "single_family_homes", "spec" = "6.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "stratum" = "condo_coop", "spec" = "6.4"))

openxlsx::write.xlsx(spec_6_4_final_maj, file = "results/final final/v2/spec_6_4_maj.xlsx")



#### Spec 6.5 ----
load("data/nri.RData")

nri_composite$fips_code <- fips_pad(nri_composite$state_code, nri_composite$county_code)

spec_5_3_panel_maj <- merge(spec_5_3_panel_maj,
                            unique(select(nri_composite, fips_code, risk_score_composite, eal_score_composite, eal_building_value_composite, alr_building_composite)),
                            by = "fips_code", all.x = TRUE)

# From NRI: https://hazards.fema.gov/nri/data-glossary
#
# RISK_SCORE: Risk value represents the average loss in dollars to buildings, population, and/or agriculture (consequence types) each year to a community due
# to natural hazards adjusted based on the community's Social Vulnerability and Community Resilience.
#
# ALR_VALB: Expected Annual Loss Rate is a measure of relative natural hazard intensities independent of the community's exposure value.
# They represent the average percentage losses to buildings, population, and/or agriculture (consequence types) each year due to natural hazards.


nri_mapping <- list("score_low" = c(0, 33),
                    "score_medium" = c(33, 67),
                    "score_high" = c(67, 100),
                    "alr_low"= c(0, quantile(spec_5_3_panel_maj$alr_building_composite, 0.33, na.rm = T)),
                    "alr_medium"= c(quantile(spec_5_3_panel_maj$alr_building_composite, 0.33, na.rm = T), quantile(spec_5_3_panel_maj$alr_building_composite, 0.67, na.rm = T)),
                    "alr_high"= c(quantile(spec_5_3_panel_maj$alr_building_composite, 0.67, na.rm = T), quantile(spec_5_3_panel_maj$alr_building_composite, 1, na.rm = T)))

spec_6_5_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & risk_score_composite > nri_mapping$score_low[1] & risk_score_composite <= nri_mapping$score_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "risk_score_low", "spec" = "6.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & risk_score_composite > nri_mapping$score_medium[1] & risk_score_composite <= nri_mapping$score_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "risk_score_medium", "spec" = "6.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & risk_score_composite > nri_mapping$score_high[1] & risk_score_composite <= nri_mapping$score_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "risk_score_high", "spec" = "6.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & alr_building_composite > nri_mapping$alr_low[1] & alr_building_composite <= nri_mapping$alr_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "alr_building_low", "spec" = "6.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & alr_building_composite > nri_mapping$alr_medium[1] & alr_building_composite <= nri_mapping$alr_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "alr_building_medium", "spec" = "6.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier" & alr_building_composite > nri_mapping$alr_high[1] & alr_building_composite <= nri_mapping$alr_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "alr_building_high", "spec" = "6.5"))

openxlsx::write.xlsx(spec_6_5_final_maj, file = "results/final final/v2/spec_6_5_maj.xlsx")



### Spec 6.6 ----
load("data/multi_occ_maj.RData")

spec_6_6_panel_maj_0.5 <- anti_join(nr_occ_type_panel_maj,
                                    subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.5), !is.na(multi_occ_maj_0.5)),
                                    by = c("fips_code", "date", "data_series"))

spec_6_6_panel_maj_1 <- anti_join(nr_occ_type_panel_maj,
                                  subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.25, multi_occ_maj_0.5_e, multi_occ_maj_1_e), !(is.na(multi_occ_maj_0.25) & is.na(multi_occ_maj_0.5_e) & is.na(multi_occ_maj_1_e))),
                                  by = c("fips_code", "date", "data_series"))

spec_6_6_panel_maj_3 <- anti_join(nr_occ_type_panel_maj,
                                  subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.25, multi_occ_maj_0.5_e, multi_occ_maj_1_e, multi_occ_maj_2_e, multi_occ_maj_3_e), !(is.na(multi_occ_maj_0.25) & is.na(multi_occ_maj_0.5_e) & is.na(multi_occ_maj_1_e) & is.na(multi_occ_maj_2_e) & is.na(multi_occ_maj_3_e))),
                                  by = c("fips_code", "date", "data_series"))


spec_6_6_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"))

openxlsx::write.xlsx(spec_6_6_final_maj, file = "results/final final/v2/spec_6_6_final_maj.xlsx")
