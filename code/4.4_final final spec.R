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
incident_type_excl <- c("Biological", "Chemical", "Other", "Toxic Substances")
main_db <- dbConnect(DBI::dbDriver("SQLite"), "data/assistance_panels.sqlite")


### Spec 5.1 ----
spec_5_1_panel_maj <- subset(select(nr_occ_type_panel_maj, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)

spec_5_1_panel_maj <- aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series, spec_5_1_panel_maj, sum, na.rm = TRUE)

temp <- nr_occ_type_panel_maj[!(duplicated(nr_occ_type_panel_maj[,c("fips_code", "date", "data_series")])),c("fips_code", "date", "data_series", "zhvi", "unemployment_rate", "avg_wkly_wage", "gdp_value")]
spec_5_1_panel_maj <- merge(spec_5_1_panel_maj,
                            temp,
                            by = c("fips_code", "date", "data_series"), all.x = TRUE)

assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles$total_assistance <- assistance_quartiles$amount_ihp_approved + assistance_quartiles$amount_pa_obligated + assistance_quartiles$amount_hmgp_obligated

assistance_quartiles <- select(assistance_quartiles, place_code, date_incident_begin, date_incident_end, total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_1_panel_maj, fips_code, date))

write_csv(assistance_panel, file = "assistance_panel_5_1.csv")
write_csv(assistance_quartiles, file = "assistance_quartiles_5_1.csv")

assistance_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, aq.total_assistance, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
                               ON ap.fips_code = aq.place_code
                               AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
                               AND ap.ap_date < aq.date_incident_end;")

assistance_panel[is.na(assistance_panel)] <- 0
assistance_panel <- aggregate(cbind(total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated) ~ fips_code + ap_date, assistance_panel, sum, na.rm = TRUE)
assistance_panel <- select(assistance_panel, fips_code, "date" = ap_date, everything())
assistance_panel$total_assistance <- assistance_panel$amount_ihp_approved + assistance_panel$amount_pa_obligated + assistance_panel$amount_hmgp_obligated

assistance_panel <- merge(merge(merge(merge(assistance_panel,
                                            select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
                                            by = "date", all.x = TRUE),
                                      select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.5), date, "p_2" = total_assistance),
                                      by = "date", all.x = TRUE),
                                select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                                by = "date", all.x = TRUE),
                          select(aggregate(total_assistance ~ date, assistance_panel, max), date, "max_val" = total_assistance),
                          by = "date", all.x = TRUE)

save(assistance_panel, file = "data/assistance_panel_max_maj_fg.RData")


spec_5_1_panel_maj <- merge(spec_5_1_panel_maj,
                            assistance_panel,
                            by = c("fips_code", "date"), all.x = TRUE)

spec_5_1_panel_maj$total_assistance[is.na(spec_5_1_panel_maj$total_assistance)] <- 0

spec_5_1_panel_maj$assistance_highest <- ifelse(spec_5_1_panel_maj$total_assistance == spec_5_1_panel_maj$max_val, 1, 0)
spec_5_1_panel_maj$assistance_q4 <- ifelse(spec_5_1_panel_maj$total_assistance >= spec_5_1_panel_maj$p_3, 1, 0)
spec_5_1_panel_maj$assistance_q3 <- ifelse(spec_5_1_panel_maj$total_assistance >= spec_5_1_panel_maj$p_2 & spec_5_1_panel_maj$total_assistance < spec_5_1_panel_maj$p_3, 1, 0)
spec_5_1_panel_maj$assistance_q2 <- ifelse(spec_5_1_panel_maj$total_assistance >= spec_5_1_panel_maj$p_1 & spec_5_1_panel_maj$total_assistance < spec_5_1_panel_maj$p_2, 1, 0)
spec_5_1_panel_maj$assistance_q1 <- ifelse(spec_5_1_panel_maj$total_assistance < spec_5_1_panel_maj$p_1, 1, 0)

spec_5_1_panel_maj$max_val <- NULL
spec_5_1_panel_maj$p_1 <- NULL
spec_5_1_panel_maj$p_2 <- NULL
spec_5_1_panel_maj$p_3 <- NULL

save(spec_5_1_panel_maj, file = "data/spec_5_1_panel_maj_fg.RData")

spec_1 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.2"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.3"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.4"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q1 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.5"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * (assistance_q1 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.6"))

openxlsx::write.xlsx(spec_1, file = "results/final final/spec_1_maj.xlsx")


### Spec 5.1 (a) ----
spec_1_a <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_a"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_a"))

openxlsx::write.xlsx(spec_1_a, file = "results/final final/spec_1_a_maj.xlsx")


### Spec 5.1 (b) ----
spec_5_1_b_panel_maj <- subset(select(nr_occ_type_panel_maj, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)

spec_5_1_b_panel_maj <- aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series, spec_5_1_b_panel_maj, sum, na.rm = TRUE)

temp <- nr_occ_type_panel_maj[!(duplicated(nr_occ_type_panel_maj[,c("fips_code", "date", "data_series")])),c("fips_code", "date", "data_series", "zhvi", "unemployment_rate", "avg_wkly_wage", "gdp_value")]
spec_5_1_b_panel_maj <- merge(spec_5_1_b_panel_maj,
                              temp,
                              by = c("fips_code", "date", "data_series"), all.x = TRUE)

assistance_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, aq.total_assistance, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
                               ON ap.fips_code = aq.place_code
                               AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
                               AND ap.ap_date < aq.date_incident_end;")

assistance_panel[is.na(assistance_panel)] <- 0
assistance_panel <- aggregate(cbind(total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated) ~ fips_code + ap_date, assistance_panel, sum, na.rm = TRUE)
assistance_panel <- select(assistance_panel, fips_code, "date" = ap_date, everything())
assistance_panel$total_assistance <- assistance_panel$amount_ihp_approved + assistance_panel$amount_pa_obligated + assistance_panel$amount_hmgp_obligated

assistance_panel <- merge(merge(merge(merge(assistance_panel,
                                            select(aggregate(total_assistance ~ date, assistance_panel, function(x){quantile(x[x != 0], probs = 0.25)}), date, "p_1" = total_assistance),
                                            by = "date", all.x = TRUE),
                                      select(aggregate(total_assistance ~ date, assistance_panel, function(x){quantile(x[x != 0], probs = 0.5)}), date, "p_2" = total_assistance),
                                      by = "date", all.x = TRUE),
                                select(aggregate(total_assistance ~ date, assistance_panel, function(x){quantile(x[x != 0], probs = 0.75)}), date, "p_3" = total_assistance),
                                by = "date", all.x = TRUE),
                          select(aggregate(total_assistance ~ date, assistance_panel, function(x){quantile(x[x != 0], probs = 1)}), date, "max_val" = total_assistance),
                          by = "date", all.x = TRUE)

assistance_panel$zero <- ifelse(assistance_panel$total_assistance == 0, 1, 0)

save(assistance_panel, file = "data/assistance_panel_max_zeros_maj_fg.RData")


spec_5_1_b_panel_maj <- merge(spec_5_1_b_panel_maj,
                              assistance_panel,
                              by = c("fips_code", "date"), all.x = TRUE)

spec_5_1_b_panel_maj$total_assistance[is.na(spec_5_1_b_panel_maj$total_assistance)] <- 0

spec_5_1_b_panel_maj$assistance_highest <- ifelse(spec_5_1_b_panel_maj$total_assistance == spec_5_1_b_panel_maj$max_val, 1, 0)
spec_5_1_b_panel_maj$assistance_q4 <- ifelse(spec_5_1_b_panel_maj$total_assistance >= spec_5_1_b_panel_maj$p_3, 1, 0)
spec_5_1_b_panel_maj$assistance_q3 <- ifelse(spec_5_1_b_panel_maj$total_assistance >= spec_5_1_b_panel_maj$p_2 & spec_5_1_b_panel_maj$total_assistance < spec_5_1_b_panel_maj$p_3, 1, 0)
spec_5_1_b_panel_maj$assistance_q2 <- ifelse(spec_5_1_b_panel_maj$total_assistance >= spec_5_1_b_panel_maj$p_1 & spec_5_1_b_panel_maj$total_assistance < spec_5_1_b_panel_maj$p_2, 1, 0)
spec_5_1_b_panel_maj$assistance_q1 <- ifelse(spec_5_1_b_panel_maj$total_assistance < spec_5_1_b_panel_maj$p_1 & spec_5_1_b_panel_maj$total_assistance > 0, 1, 0)

spec_5_1_b_panel_maj$max_val <- NULL
spec_5_1_b_panel_maj$p_1 <- NULL
spec_5_1_b_panel_maj$p_2 <- NULL
spec_5_1_b_panel_maj$p_3 <- NULL

save(spec_5_1_b_panel_maj, file = "data/spec_5_1_b_panel_maj_fg.RData")


spec_1_b <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (zero == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (zero == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_q4 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & (assistance_q1 == 1 | assistance_q4 == 1)), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & zero == 0), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier" & zero == 0), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"),
                  data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * (assistance_q2 + assistance_q3 + assistance_q4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_b_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_b"))

openxlsx::write.xlsx(spec_1_b, file = "results/final final/spec_1_b_maj.xlsx")


### Spec 5.2 ----
spec_5_2_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_2_panel <- merge(spec_5_2_panel, fema_incident_mapping_final, by = "incident_type", all.x = TRUE)

assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, incident_type, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles <- merge(assistance_quartiles, fema_incident_mapping_final, by = "incident_type", all.x = TRUE)
assistance_quartiles <- select(assistance_quartiles, place_code, incident_category_man, date_incident_begin, date_incident_end, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_2_panel, fips_code, date, incident_category_man))

write_csv(assistance_panel, file = "assistance_panel_5.2.csv")
write_csv(assistance_quartiles, file = "assistance_quartiles_5.2.csv")

assistance_type_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, ap.incident_category_man, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
                                              ON ap.fips_code = aq.place_code
                                              AND ap.incident_category_man = aq.incident_category_man
                                              AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
                                              AND ap.ap_date < aq.date_incident_end;")

assistance_type_panel[is.na(assistance_type_panel)] <- 0
assistance_type_panel$ap_date <- as.Date(assistance_type_panel$ap_date, format = "%Y-%m-%d")
assistance_type_panel <- aggregate(cbind(amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated) ~ fips_code + ap_date + incident_category_man, assistance_type_panel, sum, na.rm = TRUE) 
assistance_type_panel <- select(assistance_type_panel, fips_code, "date" = ap_date, everything())
assistance_type_panel$total_assistance <- assistance_type_panel$amount_ihp_approved + assistance_type_panel$amount_pa_obligated + assistance_type_panel$amount_hmgp_obligated

assistance_type_panel <- merge(merge(merge(assistance_type_panel,
                                           select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
                                           by = "date", all.x = TRUE),
                                     select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                                     by = "date", all.x = TRUE),
                               select(aggregate(total_assistance ~ date, assistance_type_panel, max), date, "max_val" = total_assistance),
                               by = "date", all.x = TRUE)

save(assistance_type_panel, file = "data/assistance_type_panel_fg.RData")

spec_5_2_panel <- merge(spec_5_2_panel,
                        assistance_type_panel,
                        by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_2_panel$total_assistance[is.na(spec_5_2_panel$total_assistance)] <- 0

spec_5_2_panel$assistance_highest <- ifelse(spec_5_2_panel$total_assistance == spec_5_2_panel$max_val, 1, 0)
spec_5_2_panel$assistance_q_1 <- ifelse(spec_5_2_panel$total_assistance < spec_5_2_panel$p_1, 1, 0)
spec_5_2_panel$assistance_q_4 <- ifelse(spec_5_2_panel$total_assistance >= spec_5_2_panel$p_3, 1, 0)

save(spec_5_2_panel, file = "data/spec_5_2_panel_fg.RData")

# Remove multi-occurrence periods
multi_occ <- subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_999 != 0 & !is.na(nr_dis_lag_999)), function(x){length(unique(na.omit(x)))}), "multi_occ_999" = incident_type, everything()), multi_occ_999 > 1)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_10_e != 0 & !is.na(nr_dis_lag_10_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_10_e" = incident_type, everything()), multi_occ_10_e > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_5_e != 0 & !is.na(nr_dis_lag_5_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_5_e" = incident_type, everything()), multi_occ_5_e > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_3_e != 0 & !is.na(nr_dis_lag_3_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_3_e" = incident_type, everything()), multi_occ_3_e > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_2_e != 0 & !is.na(nr_dis_lag_2_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_2_e" = incident_type, everything()), multi_occ_2_e > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_1_e != 0 & !is.na(nr_dis_lag_1_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_1_e" = incident_type, everything()), multi_occ_1_e > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_1 != 0 & !is.na(nr_dis_lag_1)), function(x){length(unique(na.omit(x)))}), "multi_occ_1" = incident_type, everything()), multi_occ_1 > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_0.5_e != 0 & !is.na(nr_dis_lag_0.5_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_0.5_e" = incident_type, everything()), multi_occ_0.5_e > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_0.5 != 0 & !is.na(nr_dis_lag_0.5)), function(x){length(unique(na.omit(x)))}), "multi_occ_0.5" = incident_type, everything()), multi_occ_0.5 > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ <- merge(multi_occ,
                   subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel, nr_dis_lag_0.25 != 0 & !is.na(nr_dis_lag_0.25)), function(x){length(unique(na.omit(x)))}), "multi_occ_0.25" = incident_type, everything()), multi_occ_0.25 > 1),
                   by = c("fips_code", "date", "data_series"), all = TRUE)

save(multi_occ, file = "data/multi_occ.RData")

# Remove multi-occ
spec_5_2_panel <- anti_join(spec_5_2_panel,
                            subset(select(multi_occ, fips_code, date, data_series, multi_occ_0.25, multi_occ_0.5, multi_occ_0.5_e, multi_occ_1_e), !(is.na(multi_occ_0.25) & is.na(multi_occ_0.5) & is.na(multi_occ_0.5_e) & is.na(multi_occ_1_e))),
                            by = c("fips_code", "date", "data_series"))

# Aggregate panel on incident_category_man
spec_5_2_panel$max_val <- NULL
spec_5_2_panel$p_1 <- NULL
spec_5_2_panel$p_3 <- NULL
spec_5_2_panel$amount_ihp_approved <- NULL
spec_5_2_panel$amount_pa_obligated <- NULL
spec_5_2_panel$amount_hmgp_obligated <- NULL

temp <- select(spec_5_2_panel, -starts_with("nr_dis_lag"), -incident_type)[!(duplicated(spec_5_2_panel[,c("fips_code", "date", "data_series", "incident_category_man")])),]
spec_5_2_panel <- merge(aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series + incident_category_man, spec_5_2_panel, sum, na.rm = TRUE),
                        temp,
                        by = c("fips_code", "date", "data_series", "incident_category_man"), all.x = TRUE);rm(temp)

save(spec_5_2_panel, file = "data/spec_5_2_panel_fg.RData")
load("data/decl_inc_group_type_panel_fg.RData")

spec_5_2_panel_group <- merge(spec_5_2_panel,
                              select(decl_inc_group_type_panel, fips_code, date, incident_category_man, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                              by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)
spec_5_2_panel_group$perc_maj_0.25[is.na(spec_5_2_panel_group$perc_maj_0.25)] <- 0
spec_5_2_panel_group$perc_maj_0.5[is.na(spec_5_2_panel_group$perc_maj_0.5)] <- 0
spec_5_2_panel_group$perc_maj_0.5_e[is.na(spec_5_2_panel_group$perc_maj_0.5_e)] <- 0
spec_5_2_panel_group$perc_maj_1[is.na(spec_5_2_panel_group$perc_maj_1)] <- 0
spec_5_2_panel_group$perc_maj_1_e[is.na(spec_5_2_panel_group$perc_maj_1_e)] <- 0

spec_2 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.1"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.2"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.3"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Cold"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Cold", "effect" = "both", "spec" = "2.4"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.5"),
                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.6"))

openxlsx::write.xlsx(spec_2, file = "results/final final/spec_2.xlsx")



### Spec 5.3 ----
spec_5_3_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_3_panel <- merge(spec_5_3_panel, fema_incident_mapping_final, by = "incident_type", all.x = TRUE)

assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, incident_type, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles <- merge(assistance_quartiles, fema_incident_mapping_final, by = "incident_type", all.x = TRUE)
assistance_quartiles <- select(assistance_quartiles, place_code, incident_category_man, date_incident_begin, date_incident_end, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_3_panel, fips_code, date, incident_category_man))

write_csv(assistance_panel, file = "assistance_panel_5.3.csv")
write_csv(assistance_quartiles, file = "assistance_quartiles_5.3.csv")

assistance_type_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, ap.incident_category_man, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
                                              ON ap.fips_code = aq.place_code
                                              AND ap.incident_category_man = aq.incident_category_man
                                              AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
                                              AND ap.ap_date < aq.date_incident_end;")

assistance_type_panel[is.na(assistance_type_panel)] <- 0
assistance_type_panel$ap_date <- as.Date(assistance_type_panel$ap_date, format = "%Y-%m-%d")
assistance_type_panel <- aggregate(cbind(amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated) ~ fips_code + ap_date + incident_category_man, assistance_type_panel, sum, na.rm = TRUE)
assistance_type_panel <- select(assistance_type_panel, fips_code, "date" = ap_date, everything())
assistance_type_panel$total_assistance <- assistance_type_panel$amount_ihp_approved + assistance_type_panel$amount_pa_obligated + assistance_type_panel$amount_hmgp_obligated

assistance_type_panel <- merge(merge(merge(assistance_type_panel,
                                           select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
                                           by = "date", all.x = TRUE),
                                     select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.5), date, "p_2" = total_assistance),
                                     by = "date", all.x = TRUE),
                               select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                               by = "date", all.x = TRUE)

spec_5_3_panel <- merge(spec_5_3_panel,
                        assistance_type_panel,
                        by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_3_panel$total_assistance[is.na(spec_5_3_panel$total_assistance)] <- 0

spec_5_3_panel$q_1 <- ifelse(spec_5_3_panel$total_assistance < spec_5_3_panel$p_1, 1, 0)
spec_5_3_panel$q_2 <- ifelse(spec_5_3_panel$total_assistance >= spec_5_3_panel$p_1 & spec_5_3_panel$total_assistance < spec_5_3_panel$p_2, 1, 0)
spec_5_3_panel$q_3 <- ifelse(spec_5_3_panel$total_assistance >= spec_5_3_panel$p_2 & spec_5_3_panel$total_assistance < spec_5_3_panel$p_3, 1, 0)
spec_5_3_panel$q_4 <- ifelse(spec_5_3_panel$total_assistance >= spec_5_3_panel$p_3, 1, 0)

spec_5_3_panel <- rename(spec_5_3_panel, "assistance_lowest" = "q_1", "assistance_highest" = "q_4")

save(spec_5_3_panel, file = "data/spec_5_3_panel_fg.RData")

spec_5_3_panel$p_1 <- NULL
spec_5_3_panel$p_2 <- NULL
spec_5_3_panel$p_3 <- NULL
spec_5_3_panel$q_2 <- NULL
spec_5_3_panel$q_3 <- NULL
spec_5_3_panel$amount_ihp_approved <- NULL
spec_5_3_panel$amount_pa_obligated <- NULL
spec_5_3_panel$amount_hmgp_obligated <- NULL

temp <- select(spec_5_3_panel, -starts_with("nr_dis_lag"), -incident_type)[!(duplicated(spec_5_3_panel[,c("fips_code", "date", "data_series", "incident_category_man")])),]
spec_5_3_panel <- merge(aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1) ~ fips_code + date + data_series + incident_category_man, spec_5_3_panel, sum, na.rm = TRUE),
                        temp,
                        by = c("fips_code", "date", "data_series", "incident_category_man"), all.x = TRUE);rm(temp)

save(spec_5_3_panel, file = "data/spec_5_3_panel_fg.RData")

load("data/decl_inc_group_type_panel_fg.RData")

spec_5_3_panel_group <- merge(spec_5_3_panel,
                              select(decl_inc_group_type_panel, fips_code, date, incident_category_man, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                              by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)
spec_5_3_panel_group$perc_maj_0.25[is.na(spec_5_3_panel_group$perc_maj_0.25)] <- 0
spec_5_3_panel_group$perc_maj_0.5[is.na(spec_5_3_panel_group$perc_maj_0.5)] <- 0
spec_5_3_panel_group$perc_maj_0.5_e[is.na(spec_5_3_panel_group$perc_maj_0.5_e)] <- 0
spec_5_3_panel_group$perc_maj_1[is.na(spec_5_3_panel_group$perc_maj_1)] <- 0
spec_5_3_panel_group$perc_maj_1_e[is.na(spec_5_3_panel_group$perc_maj_1_e)] <- 0

spec_5_3_panel_group <- merge(pivot_wider(pivot_longer(spec_5_3_panel_group, cols = starts_with("nr_dis_lag_") | starts_with("perc_maj_"), names_to = "variable", values_to = "value"),
                                          id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
                              select(spec_5_3_panel_group, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_3_panel_group[c("fips_code", "date", "data_series")])),],
                              by = c("fips_code", "date", "data_series"), all.x = TRUE)

save(spec_5_3_panel_group, file = "data/spec_5_3_panel_wide_fg.RData")

spec_3 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_cold + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_cold * perc_maj_0.25_cold + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.1"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.2"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_cold + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * perc_maj_1_water + nr_dis_lag_1_wind * perc_maj_1_wind + nr_dis_lag_1_geological * perc_maj_1_geological + nr_dis_lag_1_cold * perc_maj_1_cold + nr_dis_lag_1_fire * perc_maj_1_fire + nr_dis_lag_1_human_cause * perc_maj_1_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.3"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_cold + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_cold * perc_maj_0.25_cold + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.4"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.5"),
                data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_cold + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * perc_maj_1_water + nr_dis_lag_1_wind * perc_maj_1_wind + nr_dis_lag_1_geological * perc_maj_1_geological + nr_dis_lag_1_cold * perc_maj_1_cold + nr_dis_lag_1_fire * perc_maj_1_fire + nr_dis_lag_1_human_cause * perc_maj_1_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "3.6"))

openxlsx::write.xlsx(spec_3, file = "results/final final/spec_3.xlsx")



### Spec 6.1 & 6.2 ----
incident_type_excl <- c("Biological", "Chemical", "Other", "Toxic Substances")
time_period_spec <- list("pre_gfc" = c("2000-01-31", "2007-12-31"),
                         "post_gfc" = c("2009-01-31", "2019-12-31"),
                         "post_covid" = c("2021-01-31", "2024-02-29"),
                         "bush" = c("2001-01-31", "2008-12-31"),
                         "obama" = c("2009-01-31", "2016-12-31"),
                         "trump" = c("2017-01-31", "2020-12-31"),
                         "biden" = c("2021-01-31", "2024-02-29"))

load("data/spec_5_3_panel_wide_fg.RData")

spec_6_1n2_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$pre_gfc[1] & date <= time_period_spec$pre_gfc[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "pre_gfc", "spec" = "6.1"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$post_gfc[1] & date <= time_period_spec$post_gfc[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "post_gfc", "spec" = "6.1"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$post_covid[1] & date <= time_period_spec$post_covid[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "post_covid", "spec" = "6.1"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$bush[1] & date <= time_period_spec$bush[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "bush", "spec" = "6.2"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$obama[1] & date <= time_period_spec$obama[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "obama", "spec" = "6.2"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$trump[1] & date <= time_period_spec$trump[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "trump", "spec" = "6.2"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & date >= time_period_spec$biden[1] & date <= time_period_spec$biden[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "time_period" = "biden", "spec" = "6.2"))

openxlsx::write.xlsx(spec_6_1n2_final, file = "results/final final/spec_6_1&2.xlsx")



### Spec 6.3 ----
load("data/hmda_panels.RData")

ltv_panel_median <- subset(ltv_panel_median, !is.na(median_loan_to_value_ratio))
dti_panel_median <- subset(dti_panel_median, !is.na(median_debt_to_income_ratio))

spec_5_3_panel_group$year <- lubridate::year(spec_5_3_panel_group$date)
spec_5_3_panel_group <- merge(spec_5_3_panel_group,
                              ltv_panel_median,
                              by.x = c("fips_code", "year"), by.y = c("fips_code", "as_of_year"), all.x = T)
spec_5_3_panel_group <- merge(spec_5_3_panel_group,
                              dti_panel_median,
                              by.x = c("fips_code", "year"), by.y = c("fips_code", "as_of_year"), all.x = T)

ltv_mapping <- list("ltv_low" = c(0, 80),
                    "ltv_medium" = c(80, 95),
                    "ltv_high" = c(95, 100))

dti_mapping <- list("dti_low" = c(0, 35),
                    "dti_medium" = c(35, 50),
                    "dti_high" = c(50, 999))

spec_6_3_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_low[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_low", "spec" = "6.3"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_medium[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_medium", "spec" = "6.3"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & median_loan_to_value_ratio > ltv_mapping$ltv_high[1] & median_loan_to_value_ratio <= ltv_mapping$ltv_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_ltv_high", "spec" = "6.3"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_low[1] & median_debt_to_income_ratio <= dti_mapping$dti_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_low", "spec" = "6.3"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_medium[1] & median_debt_to_income_ratio <= dti_mapping$dti_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_medium", "spec" = "6.3"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & median_debt_to_income_ratio > dti_mapping$dti_high[1] & median_debt_to_income_ratio <= dti_mapping$dti_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "median_dti_high", "spec" = "6.3"))

openxlsx::write.xlsx(spec_6_3_final, file = "results/final final/spec_6_3.xlsx")



### Spec 6.4 ----
spec_6_4_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "stratum" = "all_homes_bottom_tier", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "all_homes_middle_tier", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "stratum" = "all_homes_top_tier", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "stratum" = "single_family_homes", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "stratum" = "condo_coop", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "one_bedroom", "effect" = "both", "stratum" = "one_bedroom", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "two_bedroom", "effect" = "both", "stratum" = "two_bedroom", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "three_bedroom", "effect" = "both", "stratum" = "three_bedroom", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "four_bedroom", "effect" = "both", "stratum" = "four_bedroom", "spec" = "6.4"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "five_plus_bedroom", "effect" = "both", "stratum" = "five_plus_bedroom", "spec" = "6.4"))

openxlsx::write.xlsx(spec_6_4_final, file = "results/final final/spec_6_4.xlsx")



### Spec 6.5 ----
load("data/nri.RData")

nri_composite$fips_code <- fips_pad(nri_composite$state_code, nri_composite$county_code)

spec_5_3_panel_group <- merge(spec_5_3_panel_group,
                              unique(select(nri_composite, fips_code, risk_score_composite, eal_score_composite, eal_building_value_composite, alr_building_composite)),
                              by = "fips_code", all.x = TRUE)

nri_mapping <- list("score_low" = c(0, 33),
                    "score_medium" = c(33, 67),
                    "score_high" = c(67, 100),
                    "alr_low"= c(0, quantile(spec_5_3_panel_group$alr_building_composite, 0.33, na.rm = T)),
                    "alr_medium"= c(quantile(spec_5_3_panel_group$alr_building_composite, 0.33, na.rm = T), quantile(spec_5_3_panel_group$alr_building_composite, 0.67, na.rm = T)),
                    "alr_high"= c(quantile(spec_5_3_panel_group$alr_building_composite, 0.67, na.rm = T), quantile(spec_5_3_panel_group$alr_building_composite, 1, na.rm = T)))

spec_6_5_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & risk_score_composite > nri_mapping$score_low[1] & risk_score_composite <= nri_mapping$score_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "risk_score_low", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & risk_score_composite > nri_mapping$score_medium[1] & risk_score_composite <= nri_mapping$score_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "risk_score_medium", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & risk_score_composite > nri_mapping$score_high[1] & risk_score_composite <= nri_mapping$score_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "risk_score_high", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & eal_score_composite > nri_mapping$score_low[1] & eal_score_composite <= nri_mapping$score_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "eal_score_low", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & eal_score_composite > nri_mapping$score_medium[1] & eal_score_composite <= nri_mapping$score_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "eal_score_medium", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & eal_score_composite > nri_mapping$score_high[1] & eal_score_composite <= nri_mapping$score_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "eal_score_high", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & alr_building_composite > nri_mapping$alr_low[1] & alr_building_composite <= nri_mapping$alr_low[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "alr_building_low", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & alr_building_composite > nri_mapping$alr_medium[1] & alr_building_composite <= nri_mapping$alr_medium[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "alr_building_medium", "spec" = "6.5"),
                        data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_cold + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * perc_maj_0.5_water + nr_dis_lag_0.5_wind * perc_maj_0.5_wind + nr_dis_lag_0.5_geological * perc_maj_0.5_geological + nr_dis_lag_0.5_cold * perc_maj_0.5_cold + nr_dis_lag_0.5_fire * perc_maj_0.5_fire + nr_dis_lag_0.5_human_cause * perc_maj_0.5_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier" & alr_building_composite > nri_mapping$alr_high[1] & alr_building_composite <= nri_mapping$alr_high[2]), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "stratum" = "alr_building_high", "spec" = "6.5"))

openxlsx::write.xlsx(spec_6_5_final, file = "results/final final/spec_6_5.xlsx")



### Spec 6.6 ----
load("data/nr_occ_type_panel.RData")
load("data/nr_occ_type_panel_maj.RData")

multi_occ_maj <- subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_999 != 0 & !is.na(nr_dis_lag_999)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_999" = incident_type, everything()), multi_occ_maj_999 > 1)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_10_e != 0 & !is.na(nr_dis_lag_10_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_10_e" = incident_type, everything()), multi_occ_maj_10_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_5_e != 0 & !is.na(nr_dis_lag_5_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_5_e" = incident_type, everything()), multi_occ_maj_5_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_3_e != 0 & !is.na(nr_dis_lag_3_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_3_e" = incident_type, everything()), multi_occ_maj_3_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_2_e != 0 & !is.na(nr_dis_lag_2_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_2_e" = incident_type, everything()), multi_occ_maj_2_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_1_e != 0 & !is.na(nr_dis_lag_1_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_1_e" = incident_type, everything()), multi_occ_maj_1_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_1 != 0 & !is.na(nr_dis_lag_1)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_1" = incident_type, everything()), multi_occ_maj_1 > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_0.5_e != 0 & !is.na(nr_dis_lag_0.5_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_0.5_e" = incident_type, everything()), multi_occ_maj_0.5_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_0.5 != 0 & !is.na(nr_dis_lag_0.5)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_0.5" = incident_type, everything()), multi_occ_maj_0.5 > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_0.25 != 0 & !is.na(nr_dis_lag_0.25)), function(x){length(unique(na.omit(x)))}), "multi_occ_maj_0.25" = incident_type, everything()), multi_occ_maj_0.25 > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)

# Check cardinality of multi occ not too high
stopifnot(all(aggregate(multi_occ_maj_0.25 ~ fips_code + date, multi_occ_maj, function(x){length(na.omit(unique(x)))})$multi_occ_maj_0.25 == 1))
stopifnot(all(aggregate(multi_occ_maj_0.5 ~ fips_code + date, multi_occ_maj, function(x){length(na.omit(unique(x)))})$multi_occ_maj_0.5 == 1))
stopifnot(all(aggregate(multi_occ_maj_0.5_e ~ fips_code + date, multi_occ_maj, function(x){length(na.omit(unique(x)))})$multi_occ_maj_0.5_e == 1))
stopifnot(all(aggregate(multi_occ_maj_1_e ~ fips_code + date, multi_occ_maj, function(x){length(na.omit(unique(x)))})$multi_occ_maj_1_e == 1))
stopifnot(all(aggregate(multi_occ_maj_2_e ~ fips_code + date, multi_occ_maj, function(x){length(na.omit(unique(x)))})$multi_occ_maj_2_e == 1))
stopifnot(all(aggregate(multi_occ_maj_3_e ~ fips_code + date, multi_occ_maj, function(x){length(na.omit(unique(x)))})$multi_occ_maj_3_e == 1))

save(multi_occ_maj, file = "data/multi_occ_maj.RData")

# Remove multi-occ
spec_6_6_panel_maj_0.5 <- anti_join(nr_occ_type_panel_maj,
                                    subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.5), !is.na(multi_occ_maj_0.5)),
                                    by = c("fips_code", "date", "data_series"))

spec_6_6_panel_maj_1 <- anti_join(nr_occ_type_panel_maj,
                                  subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.25, multi_occ_maj_0.5_e, multi_occ_maj_1_e), !(is.na(multi_occ_maj_0.25) & is.na(multi_occ_maj_0.5_e) & is.na(multi_occ_maj_1_e))),
                                  by = c("fips_code", "date", "data_series"))

spec_6_6_panel_maj_3 <- anti_join(nr_occ_type_panel_maj,
                                  subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_maj_0.25, multi_occ_maj_0.5_e, multi_occ_maj_1_e, multi_occ_maj_2_e, multi_occ_maj_3_e), !(is.na(multi_occ_maj_0.25) & is.na(multi_occ_maj_0.5_e) & is.na(multi_occ_maj_1_e) & is.na(multi_occ_maj_2_e) & is.na(multi_occ_maj_3_e))),
                                  by = c("fips_code", "date", "data_series"))


spec_6_6_final_maj_1 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Chemical"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Chemical", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Chemical"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Chemical", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Chemical"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Chemical", "spec" = "6.6"))

spec_6_6_final_maj_2 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Toxic Substances"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Toxic Substances", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Toxic Substances"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Toxic Substances", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Toxic Substances"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Toxic Substances", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Fishing Losses"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Fishing Losses", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Fishing Losses"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Fishing Losses", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Fishing Losses"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Fishing Losses", "spec" = "6.6"))

spec_6_6_final_maj_3 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Drought"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Drought", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Drought"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Drought", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Drought"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Drought", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_0.5, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_1, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"),
                              data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(spec_6_6_panel_maj_3, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"))

openxlsx::write.xlsx(rbind(spec_6_6_final_maj_1, spec_6_6_final_maj_2, spec_6_6_final_maj_3), file = "results/final final/spec_6_6_final_maj.xlsx")


nr_occ_type_panel_trunc <- subset(nr_occ_type_panel, incident_type %in% c("Biological", "Chemical", "Toxic Substances", "Fishing Losses", "Drought", "Other"))

spec_6_6_final_1 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Biological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Biological", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Chemical"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Chemical", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Chemical"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Chemical", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Chemical"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Chemical", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Toxic Substances"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Toxic Substances", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Toxic Substances"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Toxic Substances", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Toxic Substances"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Toxic Substances", "spec" = "6.6"))

spec_6_6_final_2 <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Fishing Losses"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Fishing Losses", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Fishing Losses"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Fishing Losses", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Fishing Losses"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Fishing Losses", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Drought"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Drought", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Drought"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Drought", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Drought"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Drought", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e + nr_dis_lag_3_e + unemployment_rate + log(avg_wkly_wage), subset(nr_occ_type_panel_trunc, data_series == "all_homes_middle_tier" & incident_type == "Other"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "incident_type" = "Other", "spec" = "6.6"))

openxlsx::write.xlsx(rbind(spec_6_6_final_1, spec_6_6_final_2), file = "results/final final/spec_6_6_final.xlsx")

