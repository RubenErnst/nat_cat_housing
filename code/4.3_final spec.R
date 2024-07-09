rm(list = ls())

library(tidyverse)
library(plm)
library(DBI)
library(RSQLite)

load("data/fema.RData")
load("data/fema_panel.RData")
load("results/fema_multis.RData")
# load("data/prepared_panels.RData")
# load("data/prepared_cost_panel.RData")
load("data/nr_occ_type_panel.RData")
load("data/nr_occ_type_panel_maj.RData")

source("code/1_data merging.R")

# Definitions
incident_type_excl <- c("Biological", "Chemical", "Other", "Toxic Substances")
main_db <- dbConnect(DBI::dbDriver("SQLite"), "data/assistance_panels.sqlite")


### Main spec (Miro ref. 5.1) ----
spec_5_1_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)

spec_5_1_panel <- aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series, spec_5_1_panel, sum, na.rm = TRUE)

temp <- nr_occ_type_panel[!(duplicated(nr_occ_type_panel[,c("fips_code", "date", "data_series")])),c("fips_code", "date", "data_series", "zhvi", "unemployment_rate", "avg_wkly_wage", "gdp_value")]
spec_5_1_panel <- merge(spec_5_1_panel,
                        temp,
                        by = c("fips_code", "date", "data_series"), all.x = TRUE)

# Add assistance panel
assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles$total_assistance <- assistance_quartiles$amount_ihp_approved + assistance_quartiles$amount_pa_obligated + assistance_quartiles$amount_hmgp_obligated

assistance_quartiles <- select(assistance_quartiles, place_code, date_incident_begin, date_incident_end, total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_1_panel, fips_code, date))

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

assistance_panel <- merge(merge(merge(assistance_panel,
                                      select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
                                      by = "date", all.x = TRUE),
                                select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                                by = "date", all.x = TRUE),
                          select(aggregate(total_assistance ~ date, assistance_panel, max), date, "max_val" = total_assistance),
                          by = "date", all.x = TRUE)

save(assistance_panel, file = "data/assistance_panel_max.RData")


spec_5_1_panel <- merge(spec_5_1_panel,
                        assistance_panel,
                        by = c("fips_code", "date"), all.x = TRUE)

spec_5_1_panel$total_assistance[is.na(spec_5_1_panel$total_assistance)] <- 0

spec_5_1_panel$assistance_highest <- ifelse(spec_5_1_panel$total_assistance == spec_5_1_panel$max_val, 1, 0)
spec_5_1_panel$assistance_q_4 <- ifelse(spec_5_1_panel$total_assistance >= spec_5_1_panel$p_3, 1, 0)
spec_5_1_panel$assistance_q_1 <- ifelse(spec_5_1_panel$total_assistance < spec_5_1_panel$p_1, 1, 0)

spec_5_1_panel$max_val <- NULL
spec_5_1_panel$p_1 <- NULL
spec_5_1_panel$p_3 <- NULL

save(spec_5_1_panel, file = "data/spec_5_1_panel.RData")



### Main spec (Miro ref. 5.1) - major disasters ----
spec_5_1_panel_maj <- subset(select(nr_occ_type_panel_maj, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)

spec_5_1_panel_maj <- aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series, spec_5_1_panel_maj, sum, na.rm = TRUE)

temp <- nr_occ_type_panel_maj[!(duplicated(nr_occ_type_panel_maj[,c("fips_code", "date", "data_series")])),c("fips_code", "date", "data_series", "zhvi", "unemployment_rate", "avg_wkly_wage", "gdp_value")]
spec_5_1_panel_maj <- merge(spec_5_1_panel_maj,
                            temp,
                            by = c("fips_code", "date", "data_series"), all.x = TRUE)

# # Add assistance quartiles - normalized by duration
# assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
# assistance_quartiles$total_assistance <- assistance_quartiles$amount_ihp_approved + assistance_quartiles$amount_pa_obligated + assistance_quartiles$amount_hmgp_obligated
# assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- assistance_quartiles$date_closeout_area[is.na(assistance_quartiles$date_incident_end)]
# assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- as.Date("2024-02-29")
# assistance_quartiles <- subset(assistance_quartiles, !is.na(date_incident_begin) & !is.na(date_incident_end))
# 
# assistance_quartiles$duration <- as.integer(assistance_quartiles$date_incident_end - assistance_quartiles$date_incident_begin)
# assistance_quartiles$duration <- DescTools::Winsorize(assistance_quartiles$duration, probs = c(0, 0.99))
# assistance_quartiles$duration[assistance_quartiles$duration == 0] <- 1
# 
# assistance_quartiles$total_assistance <- assistance_quartiles$total_assistance / assistance_quartiles$duration
# assistance_quartiles$amount_ihp_approved <- assistance_quartiles$amount_ihp_approved / assistance_quartiles$duration
# assistance_quartiles$amount_pa_obligated <- assistance_quartiles$amount_pa_obligated / assistance_quartiles$duration
# assistance_quartiles$amount_hmgp_obligated <- assistance_quartiles$amount_hmgp_obligated / assistance_quartiles$duration
# 
# assistance_quartiles <- select(assistance_quartiles, place_code, date_incident_begin, date_incident_end, total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)
# 
# assistance_panel <- unique(select(spec_5_1_panel, fips_code, date))
# 
# # write_csv(assistance_panel, file = "assistance_panel.csv")
# # write_csv(assistance_quartiles, file = "assistance_quartiles.csv")
# 
# assistance_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, aq.total_assistance, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
#                                ON ap.fips_code = aq.place_code
#                                AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
#                                AND ap.ap_date < aq.date_incident_end;")
# 
# assistance_panel[is.na(assistance_panel)] <- 0
# assistance_panel <- aggregate(cbind(total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated) ~ fips_code + ap_date, assistance_panel, sum, na.rm = TRUE) 
# assistance_panel <- select(assistance_panel, fips_code, "date" = ap_date, everything())
# assistance_panel$total_assistance <- assistance_panel$amount_ihp_approved + assistance_panel$amount_pa_obligated + assistance_panel$amount_hmgp_obligated
# 
# assistance_panel <- merge(merge(merge(assistance_panel,
#                                       select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
#                                       by = "date", all.x = TRUE),
#                                 select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.5), date, "p_2" = total_assistance),
#                                 by = "date", all.x = TRUE),
#                           select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
#                           by = "date", all.x = TRUE)
# 
# save(assistance_panel, file = "data/assistance_panel.RData")

# Add assistance panel
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

assistance_panel <- merge(merge(merge(assistance_panel,
                                      select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
                                      by = "date", all.x = TRUE),
                                select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                                by = "date", all.x = TRUE),
                          select(aggregate(total_assistance ~ date, assistance_panel, max), date, "max_val" = total_assistance),
                          by = "date", all.x = TRUE)

save(assistance_panel, file = "data/assistance_panel_max_maj.RData")


spec_5_1_panel_maj <- merge(spec_5_1_panel_maj,
                            assistance_panel,
                            by = c("fips_code", "date"), all.x = TRUE)

spec_5_1_panel_maj$total_assistance[is.na(spec_5_1_panel_maj$total_assistance)] <- 0

spec_5_1_panel_maj$assistance_highest <- ifelse(spec_5_1_panel_maj$total_assistance == spec_5_1_panel_maj$max_val, 1, 0)
spec_5_1_panel_maj$assistance_q4 <- ifelse(spec_5_1_panel_maj$total_assistance >= spec_5_1_panel_maj$p_3, 1, 0)
spec_5_1_panel_maj$assistance_q1 <- ifelse(spec_5_1_panel_maj$total_assistance < spec_5_1_panel_maj$p_1, 1, 0)

spec_5_1_panel_maj$max_val <- NULL
spec_5_1_panel_maj$p_1 <- NULL
spec_5_1_panel_maj$p_3 <- NULL

save(spec_5_1_panel_maj, file = "data/spec_5_1_panel_maj.RData")


# Merge declaration type percentages
load("data/decl_type_panel.RData")
load("data/spec_5_1_panel.RData")
spec_5_1_panel <- merge(spec_5_1_panel,
                        select(decl_type_panel, fips_code, date, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                        by = c("fips_code", "date"), all.x = TRUE)
spec_5_1_panel$perc_maj_0.25[is.na(spec_5_1_panel$perc_maj_0.25)] <- 0
spec_5_1_panel$perc_maj_0.5[is.na(spec_5_1_panel$perc_maj_0.<5)] <- 0
spec_5_1_panel$perc_maj_0.5_e[is.na(spec_5_1_panel$perc_maj_0.5_e)] <- 0
spec_5_1_panel$perc_maj_1_e[is.na(spec_5_1_panel$perc_maj_1_e)] <- 0


spec_1_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.2"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.3"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.4"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_highest, subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.5"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q1 + assistance_q4), subset(spec_5_1_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.6"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_1_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.1_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_1_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.2_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.3_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.4_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * assistance_highest, subset(spec_5_1_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.5_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * (assistance_q1 + assistance_q4), subset(spec_5_1_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.6_sfh"))

openxlsx::write.xlsx(spec_1_final, file = "results/final/spec_1_final_maj.xlsx")


spec_1_extension_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * perc_maj_0.5, subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.7"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * perc_maj_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.8"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e, subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.9"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.10"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * perc_maj_0.5 + nr_dis_lag_0.5 * (assistance_q_1 + assistance_q_4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.11"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * perc_maj_0.5, subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.7_sfh"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * perc_maj_0.5 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.8_sfh"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e, subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.9_sfh"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.10_sfh"),
                                data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + nr_dis_lag_0.5 * perc_maj_0.5 + nr_dis_lag_0.5 * (assistance_q_1 + assistance_q_4) + unemployment_rate + log(avg_wkly_wage), subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.11_sfh"))

openxlsx::write.xlsx(spec_1_extension_final, file = "results/final/spec_1_extension_final.xlsx")



### Main spec (Miro ref. 5.2) ----
spec_5_2_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_2_panel <- merge(spec_5_2_panel, fema_incident_mapping, by = "incident_type", all.x = TRUE)

assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, incident_type, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles <- merge(assistance_quartiles, fema_incident_mapping, by = "incident_type", all.x = TRUE)
assistance_quartiles <- select(assistance_quartiles, place_code, incident_category_man, date_incident_begin, date_incident_end, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_2_panel, fips_code, date, incident_category_man))

write_csv(assistance_panel, file = "assistance_panel.csv")
write_csv(assistance_quartiles, file = "assistance_quartiles.csv")

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

save(assistance_type_panel, file = "data/assistance_type_panel.RData")

spec_5_2_panel <- merge(spec_5_2_panel,
                        assistance_type_panel,
                        by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_2_panel$total_assistance[is.na(spec_5_2_panel$total_assistance)] <- 0

spec_5_2_panel$assistance_highest <- ifelse(spec_5_2_panel$total_assistance == spec_5_2_panel$max_val, 1, 0)
spec_5_2_panel$assistance_q_1 <- ifelse(spec_5_2_panel$total_assistance < spec_5_2_panel$p_1, 1, 0)
spec_5_2_panel$assistance_q_4 <- ifelse(spec_5_2_panel$total_assistance >= spec_5_2_panel$p_3, 1, 0)

save(spec_5_2_panel, file = "data/spec_5_2_panel.RData")

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

save(spec_5_2_panel, file = "data/spec_5_2_panel.RData")



### Main spec (Miro ref. 5.2) - major disasters ----
spec_5_2_panel_maj <- subset(select(nr_occ_type_panel_maj, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_2_panel_maj <- merge(spec_5_2_panel_maj, fema_incident_mapping, by = "incident_type", all.x = TRUE)

assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, incident_type, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
# assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- assistance_quartiles$date_closeout_area[is.na(assistance_quartiles$date_incident_end)]
# assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- as.Date("2024-02-29")
# assistance_quartiles <- subset(assistance_quartiles, !is.na(date_incident_begin) & !is.na(date_incident_end))
# 
# assistance_quartiles$duration <- as.integer(assistance_quartiles$date_incident_end - assistance_quartiles$date_incident_begin)
# assistance_quartiles$duration <- DescTools::Winsorize(assistance_quartiles$duration, probs = c(0, 0.99))
# assistance_quartiles$duration[assistance_quartiles$duration == 0] <- 1
# 
# assistance_quartiles$amount_ihp_approved <- assistance_quartiles$amount_ihp_approved / assistance_quartiles$duration
# assistance_quartiles$amount_pa_obligated <- assistance_quartiles$amount_pa_obligated / assistance_quartiles$duration
# assistance_quartiles$amount_hmgp_obligated <- assistance_quartiles$amount_hmgp_obligated / assistance_quartiles$duration

assistance_quartiles <- merge(assistance_quartiles, fema_incident_mapping, by = "incident_type", all.x = TRUE)

assistance_quartiles <- select(assistance_quartiles, place_code, incident_category_man, date_incident_begin, date_incident_end, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_2_panel_maj, fips_code, date, incident_category_man))

write_csv(assistance_panel, file = "assistance_panel.csv")
write_csv(assistance_quartiles, file = "assistance_quartiles.csv")

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

# assistance_type_panel <- merge(merge(merge(assistance_type_panel,
#                                            select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
#                                            by = "date", all.x = TRUE),
#                                      select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.5), date, "p_2" = total_assistance),
#                                      by = "date", all.x = TRUE),
#                                select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
#                                by = "date", all.x = TRUE)

assistance_type_panel <- merge(merge(merge(assistance_type_panel,
                                           select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
                                           by = "date", all.x = TRUE),
                                     select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                                     by = "date", all.x = TRUE),
                               select(aggregate(total_assistance ~ date, assistance_type_panel, max), date, "max_val" = total_assistance),
                               by = "date", all.x = TRUE)

save(assistance_type_panel, file = "data/assistance_type_panel.RData")

spec_5_2_panel_maj <- merge(spec_5_2_panel_maj,
                            assistance_type_panel,
                            by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_2_panel_maj$total_assistance[is.na(spec_5_2_panel_maj$total_assistance)] <- 0

spec_5_2_panel_maj$assistance_highest <- ifelse(spec_5_2_panel_maj$total_assistance == spec_5_2_panel_maj$max_val, 1, 0)
spec_5_2_panel_maj$assistance_q_1 <- ifelse(spec_5_2_panel_maj$total_assistance < spec_5_2_panel_maj$p_1, 1, 0)
# spec_5_2_panel_maj$q_2 <- ifelse(spec_5_2_panel_maj$total_assistance >= spec_5_2_panel_maj$p_1 & spec_5_2_panel_maj$total_assistance < spec_5_2_panel_maj$p_2, 1, 0)
# spec_5_2_panel_maj$q_3 <- ifelse(spec_5_2_panel_maj$total_assistance >= spec_5_2_panel_maj$p_2 & spec_5_2_panel_maj$total_assistance < spec_5_2_panel_maj$p_3, 1, 0)
spec_5_2_panel_maj$assistance_q_4 <- ifelse(spec_5_2_panel_maj$total_assistance >= spec_5_2_panel_maj$p_3, 1, 0)

# spec_5_2_panel_maj <- rename(spec_5_2_panel_maj, "assistance_lowest" = "q_1", "assistance_highest" = "q_4")

save(spec_5_2_panel_maj, file = "data/spec_5_2_panel_maj.RData")

# Remove multi-occurrence periods
multi_occ_maj <- subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_999 != 0 & !is.na(nr_dis_lag_999)), function(x){length(unique(na.omit(x)))}), "multi_occ_999" = incident_type, everything()), multi_occ_999 > 1)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_10_e != 0 & !is.na(nr_dis_lag_10_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_10_e" = incident_type, everything()), multi_occ_10_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_5_e != 0 & !is.na(nr_dis_lag_5_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_5_e" = incident_type, everything()), multi_occ_5_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_3_e != 0 & !is.na(nr_dis_lag_3_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_3_e" = incident_type, everything()), multi_occ_3_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_2_e != 0 & !is.na(nr_dis_lag_2_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_2_e" = incident_type, everything()), multi_occ_2_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_1_e != 0 & !is.na(nr_dis_lag_1_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_1_e" = incident_type, everything()), multi_occ_1_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_1 != 0 & !is.na(nr_dis_lag_1)), function(x){length(unique(na.omit(x)))}), "multi_occ_1" = incident_type, everything()), multi_occ_1 > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_0.5_e != 0 & !is.na(nr_dis_lag_0.5_e)), function(x){length(unique(na.omit(x)))}), "multi_occ_0.5_e" = incident_type, everything()), multi_occ_0.5_e > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_0.5 != 0 & !is.na(nr_dis_lag_0.5)), function(x){length(unique(na.omit(x)))}), "multi_occ_0.5" = incident_type, everything()), multi_occ_0.5 > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)
multi_occ_maj <- merge(multi_occ_maj,
                       subset(select(aggregate(incident_type ~ fips_code + date + data_series, subset(nr_occ_type_panel_maj, nr_dis_lag_0.25 != 0 & !is.na(nr_dis_lag_0.25)), function(x){length(unique(na.omit(x)))}), "multi_occ_0.25" = incident_type, everything()), multi_occ_0.25 > 1),
                       by = c("fips_code", "date", "data_series"), all = TRUE)

save(multi_occ_maj, file = "data/multi_occ_maj.RData")

# Remove multi-occ
spec_5_2_panel_maj <- anti_join(spec_5_2_panel_maj,
                                subset(select(multi_occ_maj, fips_code, date, data_series, multi_occ_0.25, multi_occ_0.5, multi_occ_0.5_e, multi_occ_1_e), !(is.na(multi_occ_0.25) & is.na(multi_occ_0.5) & is.na(multi_occ_0.5_e) & is.na(multi_occ_1_e))),
                                by = c("fips_code", "date", "data_series"))

# Aggregate panel on incident_category_man
spec_5_2_panel_maj$max_val <- NULL
spec_5_2_panel_maj$p_1 <- NULL
# spec_5_2_panel_maj$p_2 <- NULL
spec_5_2_panel_maj$p_3 <- NULL
# spec_5_2_panel_maj$q_2 <- NULL
# spec_5_2_panel_maj$q_3 <- NULL
spec_5_2_panel_maj$amount_ihp_approved <- NULL
spec_5_2_panel_maj$amount_pa_obligated <- NULL
spec_5_2_panel_maj$amount_hmgp_obligated <- NULL

temp <- select(spec_5_2_panel_maj, -starts_with("nr_dis_lag"), -incident_type)[!(duplicated(spec_5_2_panel_maj[,c("fips_code", "date", "data_series", "incident_category_man")])),]
spec_5_2_panel_maj <- merge(aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series + incident_category_man, spec_5_2_panel_maj, sum, na.rm = TRUE),
                            temp,
                            by = c("fips_code", "date", "data_series", "incident_category_man"), all.x = TRUE);rm(temp)

save(spec_5_2_panel_maj, file = "data/spec_5_2_panel_maj.RData")

# # Pivot wider for all categories + lags
# spec_5_2_panel <- merge(pivot_wider(pivot_longer(spec_5_2_panel, cols = starts_with("nr_dis_lag_"), names_to = "variable", values_to = "value"),
#                                     id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
#                         select(spec_5_2_panel, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_2_panel[c("fips_code", "date", "data_series")])),],
#                         by = c("fips_code", "date", "data_series"), all.x = TRUE)
# 
# save(spec_5_2_panel, file = "data/spec_5_2_panel_wide.RData")

# Merge declaration type percentages
load("data/decl_inc_type_panel.RData")
load("data/spec_5_2_panel.RData")
spec_5_2_panel <- merge(spec_5_2_panel,
                        select(decl_inc_type_panel, fips_code, date, incident_type, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                        by = c("fips_code", "date", "incident_type"), all.x = TRUE)
spec_5_2_panel$perc_maj_0.25[is.na(spec_5_2_panel$perc_maj_0.25)] <- 0
spec_5_2_panel$perc_maj_0.5[is.na(spec_5_2_panel$perc_maj_0.<5)] <- 0
spec_5_2_panel$perc_maj_0.5_e[is.na(spec_5_2_panel$perc_maj_0.5_e)] <- 0
spec_5_2_panel$perc_maj_1_e[is.na(spec_5_2_panel$perc_maj_1_e)] <- 0


load("data/decl_inc_group_type_panel.RData")
load("data/spec_5_2_panel.RData")
spec_5_2_panel_group <- merge(spec_5_2_panel,
                              select(decl_inc_group_type_panel, fips_code, date, incident_category_man, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                              by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)
spec_5_2_panel_group$perc_maj_0.25[is.na(spec_5_2_panel_group$perc_maj_0.25)] <- 0
spec_5_2_panel_group$perc_maj_0.5[is.na(spec_5_2_panel_group$perc_maj_0.<5)] <- 0
spec_5_2_panel_group$perc_maj_0.5_e[is.na(spec_5_2_panel_group$perc_maj_0.5_e)] <- 0
spec_5_2_panel_group$perc_maj_1_e[is.na(spec_5_2_panel_group$perc_maj_1_e)] <- 0


spec_2_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.1"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.2"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.3"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.4"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.5"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.1_sfh"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.2_sfh"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.3_sfh"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.4_sfh"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.5_sfh"),
                          data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.6_sfh"))

openxlsx::write.xlsx(spec_2_final_maj, file = "results/final/spec_2_final_maj.xlsx")

spec_2_1_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.1"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.2"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Extreme Weather", "effect" = "both", "spec" = "2.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.1_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.2_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.3_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Extreme Weather", "effect" = "both", "spec" = "2.4_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.5_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.6_sfh"))

openxlsx::write.xlsx(spec_2_1_final_maj, file = "results/final/spec_2_1_final_maj.xlsx")

spec_2_2_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.1"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.2"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Extreme Weather", "effect" = "both", "spec" = "2.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.1_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.2_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.3_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Extreme Weather", "effect" = "both", "spec" = "2.4_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.5_sfh"),
                            data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_maj, data_series == "single_family_homes" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.6_sfh"))

openxlsx::write.xlsx(spec_2_2_final_maj, file = "results/final/spec_2_2_final_maj.xlsx")

spec_2_extensions <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.7"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.8"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.9"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Extreme Weather", "effect" = "both", "spec" = "2.10"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.11"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.12"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Water"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Water", "effect" = "both", "spec" = "2.13"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Wind"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Wind", "effect" = "both", "spec" = "2.14"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Geological"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Geological", "effect" = "both", "spec" = "2.15"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Extreme Weather"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Extreme Weather", "effect" = "both", "spec" = "2.16"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Fire"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Fire", "effect" = "both", "spec" = "2.17"),
                           data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_0.25 * perc_maj_0.25 + nr_dis_lag_0.5_e * perc_maj_0.5_e + nr_dis_lag_1_e * perc_maj_1_e + (nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e) * assistance_highest + unemployment_rate + log(avg_wkly_wage), subset(spec_5_2_panel_group, data_series == "all_homes_middle_tier" & incident_category_man == "Human Cause"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "incident_category_man" = "Human Cause", "effect" = "both", "spec" = "2.18"))

openxlsx::write.xlsx(spec_2_extensions, file = "results/final/spec_2_extensions.xlsx")



### Main spec (Miro ref. 5.3) ----
spec_5_3_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_3_panel <- merge(spec_5_3_panel, fema_incident_mapping, by = "incident_type", all.x = TRUE)

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

load("data/assistance_type_panel.RData")

spec_5_3_panel <- merge(spec_5_3_panel,
                        assistance_type_panel,
                        by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_3_panel$total_assistance[is.na(spec_5_3_panel$total_assistance)] <- 0

spec_5_3_panel$q_1 <- ifelse(spec_5_3_panel$total_assistance < spec_5_3_panel$p_1, 1, 0)
spec_5_3_panel$q_2 <- ifelse(spec_5_3_panel$total_assistance >= spec_5_3_panel$p_1 & spec_5_3_panel$total_assistance < spec_5_3_panel$p_2, 1, 0)
spec_5_3_panel$q_3 <- ifelse(spec_5_3_panel$total_assistance >= spec_5_3_panel$p_2 & spec_5_3_panel$total_assistance < spec_5_3_panel$p_3, 1, 0)
spec_5_3_panel$q_4 <- ifelse(spec_5_3_panel$total_assistance >= spec_5_3_panel$p_3, 1, 0)

spec_5_3_panel <- rename(spec_5_3_panel, "assistance_lowest" = "q_1", "assistance_highest" = "q_4")

save(spec_5_3_panel, file = "data/spec_5_3_panel.RData")

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

save(spec_5_3_panel, file = "data/spec_5_3_panel.RData")

# Pivot wider for all categories + lags
spec_5_3_panel <- merge(pivot_wider(pivot_longer(spec_5_3_panel, cols = starts_with("nr_dis_lag_"), names_to = "variable", values_to = "value"),
                                    id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
                        select(spec_5_3_panel, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_3_panel[c("fips_code", "date", "data_series")])),],
                        by = c("fips_code", "date", "data_series"), all.x = TRUE)

save(spec_5_3_panel, file = "data/spec_5_3_panel_wide.RData")

# Merge declaration types
load("data/decl_inc_group_type_panel.RData")
load("data/spec_5_3_panel.RData")
spec_5_3_panel_group <- merge(spec_5_3_panel,
                              select(decl_inc_group_type_panel, fips_code, date, incident_type, perc_maj_0.25, perc_maj_0.5, perc_maj_0.5_e, perc_maj_1, perc_maj_1_e),
                              by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)
spec_5_3_panel_group$perc_maj_0.25[is.na(spec_5_3_panel_group$perc_maj_0.25)] <- 0
spec_5_3_panel_group$perc_maj_0.5[is.na(spec_5_3_panel_group$perc_maj_0.<5)] <- 0
spec_5_3_panel_group$perc_maj_0.5_e[is.na(spec_5_3_panel_group$perc_maj_0.5_e)] <- 0
spec_5_3_panel_group$perc_maj_1_e[is.na(spec_5_3_panel_group$perc_maj_1_e)] <- 0

spec_5_3_panel_group <- merge(pivot_wider(pivot_longer(spec_5_3_panel_group, cols = starts_with("nr_dis_lag_") | starts_with("perc_maj_"), names_to = "variable", values_to = "value"),
                                          id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
                              select(spec_5_3_panel_group, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_3_panel_group[c("fips_code", "date", "data_series")])),],
                              by = c("fips_code", "date", "data_series"), all.x = TRUE)


spec_3_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.1"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.2"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.3"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.4"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.5"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.6"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.7"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.8"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.9"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.10"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.11"),
                      data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.12"))

openxlsx::write.xlsx(spec_3_final, file = "results/final/spec_3_final.xlsx")

spec_3_extensions <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_extreme_weather * perc_maj_0.25_extreme_weather + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.7"),
                           data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_extreme_weather * perc_maj_0.25_extreme_weather + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.8"),
                           data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_extreme_weather * perc_maj_0.25_extreme_weather + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.9"),
                           data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_extreme_weather * perc_maj_0.25_extreme_weather + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.10"),
                           data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_extreme_weather * perc_maj_0.25_extreme_weather + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.11"),
                           data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_0.25_water * perc_maj_0.25_water + nr_dis_lag_0.25_wind * perc_maj_0.25_wind + nr_dis_lag_0.25_geological * perc_maj_0.25_geological + nr_dis_lag_0.25_extreme_weather * perc_maj_0.25_extreme_weather + nr_dis_lag_0.25_fire * perc_maj_0.25_fire + nr_dis_lag_0.25_human_cause * perc_maj_0.25_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_group, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.12"))

openxlsx::write.xlsx(spec_3_extensions, file = "results/final/spec_3_extensions.xlsx")


### Main spec (Miro ref. 5.3) - major disasters ----
spec_5_3_panel_maj <- subset(select(nr_occ_type_panel_maj, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_3_panel_maj <- merge(spec_5_3_panel_maj, fema_incident_mapping, by = "incident_type", all.x = TRUE)

# assistance_type_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, ap.incident_category_man, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
#                                               ON ap.fips_code = aq.place_code
#                                               AND ap.incident_category_man = aq.incident_category_man
#                                               AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
#                                               AND ap.ap_date < aq.date_incident_end;")
# 
# assistance_type_panel[is.na(assistance_type_panel)] <- 0
# assistance_type_panel$ap_date <- as.Date(assistance_type_panel$ap_date, format = "%Y-%m-%d")
# assistance_type_panel <- aggregate(cbind(amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated) ~ fips_code + ap_date + incident_category_man, assistance_type_panel, sum, na.rm = TRUE)
# assistance_type_panel <- select(assistance_type_panel, fips_code, "date" = ap_date, everything())
# assistance_type_panel$total_assistance <- assistance_type_panel$amount_ihp_approved + assistance_type_panel$amount_pa_obligated + assistance_type_panel$amount_hmgp_obligated
# 
# assistance_type_panel <- merge(merge(merge(assistance_type_panel,
#                                            select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.25), date, "p_1" = total_assistance),
#                                            by = "date", all.x = TRUE),
#                                      select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.5), date, "p_2" = total_assistance),
#                                      by = "date", all.x = TRUE),
#                                select(aggregate(total_assistance ~ date, assistance_type_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
#                                by = "date", all.x = TRUE)
# 
# save(assistance_type_panel, file = "data/assistance_type_panel.RData")
load("data/assistance_type_panel.RData")

spec_5_3_panel_maj <- merge(spec_5_3_panel_maj,
                            assistance_type_panel,
                            by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_3_panel_maj$total_assistance[is.na(spec_5_3_panel_maj$total_assistance)] <- 0

spec_5_3_panel_maj$assistance_highest <- ifelse(spec_5_3_panel_maj$total_assistance == spec_5_3_panel_maj$max_val, 1, 0)
spec_5_3_panel_maj$assistance_q_1 <- ifelse(spec_5_3_panel_maj$total_assistance < spec_5_3_panel_maj$p_1, 1, 0)
# spec_5_3_panel_maj$q_2 <- ifelse(spec_5_3_panel_maj$total_assistance >= spec_5_3_panel_maj$p_1 & spec_5_3_panel_maj$total_assistance < spec_5_3_panel_maj$p_2, 1, 0)
# spec_5_3_panel_maj$q_3 <- ifelse(spec_5_3_panel_maj$total_assistance >= spec_5_3_panel_maj$p_2 & spec_5_3_panel_maj$total_assistance < spec_5_3_panel_maj$p_3, 1, 0)
spec_5_3_panel_maj$assistance_q_4 <- ifelse(spec_5_3_panel_maj$total_assistance >= spec_5_3_panel_maj$p_3, 1, 0)

# spec_5_3_panel_maj <- rename(spec_5_3_panel_maj, "assistance_lowest" = "q_1", "assistance_highest" = "q_4")

save(spec_5_3_panel_maj, file = "data/spec_5_3_panel_maj.RData")

spec_5_3_panel_maj$max_val <- NULL
spec_5_3_panel_maj$p_1 <- NULL
# spec_5_3_panel_maj$p_2 <- NULL
spec_5_3_panel_maj$p_3 <- NULL
# spec_5_3_panel_maj$q_2 <- NULL
# spec_5_3_panel_maj$q_3 <- NULL
spec_5_3_panel_maj$amount_ihp_approved <- NULL
spec_5_3_panel_maj$amount_pa_obligated <- NULL
spec_5_3_panel_maj$amount_hmgp_obligated <- NULL

temp <- select(spec_5_3_panel_maj, -starts_with("nr_dis_lag"), -incident_type)[!(duplicated(spec_5_3_panel_maj[,c("fips_code", "date", "data_series", "incident_category_man")])),]
spec_5_3_panel_maj <- merge(aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_1) ~ fips_code + date + data_series + incident_category_man, spec_5_3_panel_maj, sum, na.rm = TRUE),
                            temp,
                            by = c("fips_code", "date", "data_series", "incident_category_man"), all.x = TRUE);rm(temp)

save(spec_5_3_panel_maj, file = "data/spec_5_3_panel_maj.RData")

# Pivot wider for all categories + lags
# spec_5_3_panel_maj <- merge(pivot_wider(pivot_longer(spec_5_3_panel_maj, cols = starts_with("nr_dis_lag_"), names_to = "variable", values_to = "value"),
#                                         id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
#                             select(spec_5_3_panel_maj, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_3_panel_maj[,c("fips_code", "date", "data_series")])),],
#                             by = c("fips_code", "date", "data_series"), all.x = TRUE)

temp <- select(spec_5_3_panel_maj, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_3_panel_maj[,c("fips_code", "date", "data_series")])),]

spec_5_3_panel_maj <- merge(pivot_wider(pivot_longer(spec_5_3_panel_maj, cols = starts_with("nr_dis_lag_") | starts_with("assistance_"), names_to = "variable", values_to = "value"),
                                        id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
                            temp,
                            by = c("fips_code", "date", "data_series"), all.x = TRUE)

save(spec_5_3_panel_maj, file = "data/spec_5_3_panel_maj_wide.RData")

spec_3_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.1"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.2"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.3"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.4"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.5"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.6"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.7"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.8"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.9"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.10"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.11"),
                          data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.12"))

openxlsx::write.xlsx(spec_3_final_maj, file = "results/final/spec_3_final_maj.xlsx")


spec_3_1_final_maj <- rbind(data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * assistance_q_1_water + nr_dis_lag_0.25_water * assistance_q_4_water + nr_dis_lag_0.25_wind * assistance_q_1_wind + nr_dis_lag_0.25_wind * assistance_q_4_wind + nr_dis_lag_0.25_geological * assistance_q_1_geological + nr_dis_lag_0.25_geological * assistance_q_4_geological + nr_dis_lag_0.25_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.25_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.25_fire * assistance_q_1_fire + nr_dis_lag_0.25_fire * assistance_q_4_fire + nr_dis_lag_0.25_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.25_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.1"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * assistance_q_1_water + nr_dis_lag_0.5_water * assistance_q_4_water + nr_dis_lag_0.5_wind * assistance_q_1_wind + nr_dis_lag_0.5_wind * assistance_q_4_wind + nr_dis_lag_0.5_geological * assistance_q_1_geological + nr_dis_lag_0.5_geological * assistance_q_4_geological + nr_dis_lag_0.5_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.5_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.5_fire * assistance_q_1_fire + nr_dis_lag_0.5_fire * assistance_q_4_fire + nr_dis_lag_0.5_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.5_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.2"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * assistance_q_1_water + nr_dis_lag_1_water * assistance_q_4_water + nr_dis_lag_1_wind * assistance_q_1_wind + nr_dis_lag_1_wind * assistance_q_4_wind + nr_dis_lag_1_geological * assistance_q_1_geological + nr_dis_lag_1_geological * assistance_q_4_geological + nr_dis_lag_1_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_1_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_1_fire * assistance_q_1_fire + nr_dis_lag_1_fire * assistance_q_4_fire + nr_dis_lag_1_human_cause * assistance_q_1_human_cause + nr_dis_lag_1_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.3"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * assistance_q_1_water + nr_dis_lag_0.25_water * assistance_q_4_water + nr_dis_lag_0.25_wind * assistance_q_1_wind + nr_dis_lag_0.25_wind * assistance_q_4_wind + nr_dis_lag_0.25_geological * assistance_q_1_geological + nr_dis_lag_0.25_geological * assistance_q_4_geological + nr_dis_lag_0.25_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.25_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.25_fire * assistance_q_1_fire + nr_dis_lag_0.25_fire * assistance_q_4_fire + nr_dis_lag_0.25_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.25_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.4"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * assistance_q_1_water + nr_dis_lag_0.5_water * assistance_q_4_water + nr_dis_lag_0.5_wind * assistance_q_1_wind + nr_dis_lag_0.5_wind * assistance_q_4_wind + nr_dis_lag_0.5_geological * assistance_q_1_geological + nr_dis_lag_0.5_geological * assistance_q_4_geological + nr_dis_lag_0.5_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.5_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.5_fire * assistance_q_1_fire + nr_dis_lag_0.5_fire * assistance_q_4_fire + nr_dis_lag_0.5_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.5_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.5"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * assistance_q_1_water + nr_dis_lag_1_water * assistance_q_4_water + nr_dis_lag_1_wind * assistance_q_1_wind + nr_dis_lag_1_wind * assistance_q_4_wind + nr_dis_lag_1_geological * assistance_q_1_geological + nr_dis_lag_1_geological * assistance_q_4_geological + nr_dis_lag_1_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_1_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_1_fire * assistance_q_1_fire + nr_dis_lag_1_fire * assistance_q_4_fire + nr_dis_lag_1_human_cause * assistance_q_1_human_cause + nr_dis_lag_1_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.6"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * assistance_q_1_water + nr_dis_lag_0.25_water * assistance_q_4_water + nr_dis_lag_0.25_wind * assistance_q_1_wind + nr_dis_lag_0.25_wind * assistance_q_4_wind + nr_dis_lag_0.25_geological * assistance_q_1_geological + nr_dis_lag_0.25_geological * assistance_q_4_geological + nr_dis_lag_0.25_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.25_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.25_fire * assistance_q_1_fire + nr_dis_lag_0.25_fire * assistance_q_4_fire + nr_dis_lag_0.25_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.25_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.7"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * assistance_q_1_water + nr_dis_lag_0.5_water * assistance_q_4_water + nr_dis_lag_0.5_wind * assistance_q_1_wind + nr_dis_lag_0.5_wind * assistance_q_4_wind + nr_dis_lag_0.5_geological * assistance_q_1_geological + nr_dis_lag_0.5_geological * assistance_q_4_geological + nr_dis_lag_0.5_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.5_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.5_fire * assistance_q_1_fire + nr_dis_lag_0.5_fire * assistance_q_4_fire + nr_dis_lag_0.5_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.5_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.8"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * assistance_q_1_water + nr_dis_lag_1_water * assistance_q_4_water + nr_dis_lag_1_wind * assistance_q_1_wind + nr_dis_lag_1_wind * assistance_q_4_wind + nr_dis_lag_1_geological * assistance_q_1_geological + nr_dis_lag_1_geological * assistance_q_4_geological + nr_dis_lag_1_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_1_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_1_fire * assistance_q_1_fire + nr_dis_lag_1_fire * assistance_q_4_fire + nr_dis_lag_1_human_cause * assistance_q_1_human_cause + nr_dis_lag_1_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.9"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.25_water + nr_dis_lag_0.25_wind + nr_dis_lag_0.25_geological + nr_dis_lag_0.25_extreme_weather + nr_dis_lag_0.25_fire + nr_dis_lag_0.25_human_cause) ^ 2 + nr_dis_lag_0.25_water * assistance_q_1_water + nr_dis_lag_0.25_water * assistance_q_4_water + nr_dis_lag_0.25_wind * assistance_q_1_wind + nr_dis_lag_0.25_wind * assistance_q_4_wind + nr_dis_lag_0.25_geological * assistance_q_1_geological + nr_dis_lag_0.25_geological * assistance_q_4_geological + nr_dis_lag_0.25_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.25_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.25_fire * assistance_q_1_fire + nr_dis_lag_0.25_fire * assistance_q_4_fire + nr_dis_lag_0.25_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.25_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.10"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_0.5_water + nr_dis_lag_0.5_wind + nr_dis_lag_0.5_geological + nr_dis_lag_0.5_extreme_weather + nr_dis_lag_0.5_fire + nr_dis_lag_0.5_human_cause) ^ 2 + nr_dis_lag_0.5_water * assistance_q_1_water + nr_dis_lag_0.5_water * assistance_q_4_water + nr_dis_lag_0.5_wind * assistance_q_1_wind + nr_dis_lag_0.5_wind * assistance_q_4_wind + nr_dis_lag_0.5_geological * assistance_q_1_geological + nr_dis_lag_0.5_geological * assistance_q_4_geological + nr_dis_lag_0.5_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_0.5_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_0.5_fire * assistance_q_1_fire + nr_dis_lag_0.5_fire * assistance_q_4_fire + nr_dis_lag_0.5_human_cause * assistance_q_1_human_cause + nr_dis_lag_0.5_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.11"),
                            data.frame(plm_results(plm(log(zhvi) ~ (nr_dis_lag_1_water + nr_dis_lag_1_wind + nr_dis_lag_1_geological + nr_dis_lag_1_extreme_weather + nr_dis_lag_1_fire + nr_dis_lag_1_human_cause) ^ 2 + nr_dis_lag_1_water * assistance_q_1_water + nr_dis_lag_1_water * assistance_q_4_water + nr_dis_lag_1_wind * assistance_q_1_wind + nr_dis_lag_1_wind * assistance_q_4_wind + nr_dis_lag_1_geological * assistance_q_1_geological + nr_dis_lag_1_geological * assistance_q_4_geological + nr_dis_lag_1_extreme_weather * assistance_q_1_extreme_weather + nr_dis_lag_1_extreme_weather * assistance_q_4_extreme_weather + nr_dis_lag_1_fire * assistance_q_1_fire + nr_dis_lag_1_fire * assistance_q_4_fire + nr_dis_lag_1_human_cause * assistance_q_1_human_cause + nr_dis_lag_1_human_cause * assistance_q_4_human_cause + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_3_panel_maj, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.12"))

openxlsx::write.xlsx(spec_3_1_final_maj, file = "results/final/spec_3_1_final_maj.xlsx")

