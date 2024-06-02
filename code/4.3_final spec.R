rm(list = ls())

library(tidyverse)
library(plm)

load("data/fema.RData")
load("data/fema_panel.RData")
load("results/fema_multis.RData")
load("data/prepared_panels.RData")
load("data/prepared_cost_panel.RData")

source("code/1_data merging.R")

# Definitions
incident_type_excl <- c("Biological", "Chemical", "Other", "Toxic Substances")


### Main spec (Miro ref. 5.1) ----
spec_5_1_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)

spec_5_1_panel <- aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series, spec_5_1_panel, sum, na.rm = TRUE)

spec_5_1_panel <- merge(spec_5_1_panel,
                        nr_occ_type_panel[!(duplicated(nr_occ_type_panel[c("fips_code", "date", "data_series")])),c("fips_code", "date", "data_series", "zhvi", "unemployment_rate", "avg_wkly_wage", "gdp_value")],
                        by = c("fips_code", "date", "data_series"), all.x = TRUE)

# Add assistance quartiles
assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles$total_assistance <- assistance_quartiles$amount_ihp_approved + assistance_quartiles$amount_pa_obligated + assistance_quartiles$amount_hmgp_obligated
assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- assistance_quartiles$date_closeout_area[is.na(assistance_quartiles$date_incident_end)]
assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- as.Date("2024-02-29")
assistance_quartiles <- subset(assistance_quartiles, !is.na(date_incident_begin) & !is.na(date_incident_end))

assistance_quartiles$duration <- as.integer(assistance_quartiles$date_incident_end - assistance_quartiles$date_incident_begin)
assistance_quartiles$duration <- DescTools::Winsorize(assistance_quartiles$duration, probs = c(0, 0.99))
assistance_quartiles$duration[assistance_quartiles$duration == 0] <- 1

assistance_quartiles$total_assistance <- assistance_quartiles$total_assistance / assistance_quartiles$duration
assistance_quartiles$amount_ihp_approved <- assistance_quartiles$amount_ihp_approved / assistance_quartiles$duration
assistance_quartiles$amount_pa_obligated <- assistance_quartiles$amount_pa_obligated / assistance_quartiles$duration
assistance_quartiles$amount_hmgp_obligated <- assistance_quartiles$amount_hmgp_obligated / assistance_quartiles$duration

assistance_quartiles <- select(assistance_quartiles, place_code, date_incident_begin, date_incident_end, total_assistance, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)
# assistance_quartiles <- aggregate(total_assistance ~ place_code + date_incident_begin, assistance_quartiles, sum, na.rm = T)

assistance_panel <- unique(select(spec_5_1_panel, fips_code, date))

# write_csv(assistance_panel, file = "assistance_panel.csv")
# write_csv(assistance_quartiles, file = "assistance_quartiles.csv")

library(DBI)
library(RSQLite)

main_db <- dbConnect(DBI::dbDriver("SQLite"), "data/assistance_panels.sqlite")

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
                                select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.5), date, "p_2" = total_assistance),
                                by = "date", all.x = TRUE),
                          select(aggregate(total_assistance ~ date, assistance_panel, quantile, probs = 0.75), date, "p_3" = total_assistance),
                          by = "date", all.x = TRUE)

save(assistance_panel, file = "data/assistance_panel.RData")

spec_5_1_panel <- merge(spec_5_1_panel,
                        assistance_panel,
                        by = c("fips_code", "date"), all.x = TRUE)

spec_5_1_panel$total_assistance[is.na(spec_5_1_panel$total_assistance)] <- 0

spec_5_1_panel$q_1 <- ifelse(spec_5_1_panel$total_assistance < spec_5_1_panel$p_1, 1, 0)
spec_5_1_panel$q_2 <- ifelse(spec_5_1_panel$total_assistance >= spec_5_1_panel$p_1 & spec_5_1_panel$total_assistance < spec_5_1_panel$p_2, 1, 0)
spec_5_1_panel$q_3 <- ifelse(spec_5_1_panel$total_assistance >= spec_5_1_panel$p_2 & spec_5_1_panel$total_assistance < spec_5_1_panel$p_3, 1, 0)
spec_5_1_panel$q_4 <- ifelse(spec_5_1_panel$total_assistance >= spec_5_1_panel$p_3, 1, 0)

spec_5_1_panel <- rename(spec_5_1_panel, "assistance_lowest" = "q_1", "assistance_highest" = "q_4")

save(spec_5_1_panel, file = "data/prepared_spec_5_1_panel.RData")



spec_1_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.2"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.3"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.4"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + I(nr_dis_lag_0.25 * assistance_lowest) + I(nr_dis_lag_0.25 * assistance_highest), subset(spec_5_1_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.5"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.1_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.2_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.3_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.4_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + I(nr_dis_lag_0.25 * assistance_lowest) + I(nr_dis_lag_0.25 * assistance_highest), subset(spec_5_1_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.5_sfh"))

openxlsx::write.xlsx(spec_1_final, file = "results/spec_1_final.xlsx")



### Main spec (Miro ref. 5.2) ----
spec_5_2_panel <- subset(select(nr_occ_type_panel, zhvi, fips_code, date, nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage, gdp_value, incident_type, data_series), !incident_type %in% incident_type_excl)
spec_5_2_panel <- merge(spec_5_2_panel, fema_incident_mapping, by = "incident_type", all.x = TRUE)

assistance_quartiles <- unique(select(subset(fema, !incident_type %in% incident_type_excl), disaster_number, place_code, incident_type, date_incident_begin, date_incident_end, date_closeout_area, amount_ihp_approved, amount_ha_approved, amount_ona_approved, amount_pa_obligated, amount_catab_obligated, amount_catc2g_obligated, amount_hmgp_obligated))
assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- assistance_quartiles$date_closeout_area[is.na(assistance_quartiles$date_incident_end)]
assistance_quartiles$date_incident_end[is.na(assistance_quartiles$date_incident_end)] <- as.Date("2024-02-29")
assistance_quartiles <- subset(assistance_quartiles, !is.na(date_incident_begin) & !is.na(date_incident_end))

assistance_quartiles$duration <- as.integer(assistance_quartiles$date_incident_end - assistance_quartiles$date_incident_begin)
assistance_quartiles$duration <- DescTools::Winsorize(assistance_quartiles$duration, probs = c(0, 0.99))
assistance_quartiles$duration[assistance_quartiles$duration == 0] <- 1

assistance_quartiles$amount_ihp_approved <- assistance_quartiles$amount_ihp_approved / assistance_quartiles$duration
assistance_quartiles$amount_pa_obligated <- assistance_quartiles$amount_pa_obligated / assistance_quartiles$duration
assistance_quartiles$amount_hmgp_obligated <- assistance_quartiles$amount_hmgp_obligated / assistance_quartiles$duration

assistance_quartiles <- merge(assistance_quartiles, fema_incident_mapping, by = "incident_type", all.x = TRUE)

assistance_quartiles <- select(assistance_quartiles, place_code, incident_category_man, date_incident_begin, date_incident_end, amount_ihp_approved, amount_pa_obligated, amount_hmgp_obligated)

assistance_panel <- unique(select(spec_5_2_panel, fips_code, date, incident_category_man))

# write_csv(assistance_panel, file = "assistance_panel.csv")
# write_csv(assistance_quartiles, file = "assistance_quartiles.csv")

assistance_type_panel <- dbGetQuery(main_db, "SELECT ap.fips_code, ap.ap_date, ap.incident_category_man, aq.amount_ihp_approved, aq.amount_pa_obligated, aq.amount_hmgp_obligated FROM assistance_panel ap LEFT JOIN assistance_quartiles aq
                                              ON ap.fips_code = aq.place_code
                                              AND ap.incident_category_man = aq.incident_category_man
                                              AND DATE(ap.ap_date, '+30 days') >= aq.date_incident_begin
                                              AND ap.ap_date < aq.date_incident_end;")

assistance_type_panel[is.na(assistance_type_panel)] <- 0
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

save(assistance_type_panel, file = "data/assistance_type_panel.RData")

spec_5_2_panel <- merge(spec_5_2_panel,
                        assistance_type_panel,
                        by = c("fips_code", "date", "incident_category_man"), all.x = TRUE)

spec_5_2_panel$total_assistance[is.na(spec_5_2_panel$total_assistance)] <- 0

spec_5_2_panel$q_1 <- ifelse(spec_5_2_panel$total_assistance < spec_5_2_panel$p_1, 1, 0)
spec_5_2_panel$q_2 <- ifelse(spec_5_2_panel$total_assistance >= spec_5_2_panel$p_1 & spec_5_2_panel$total_assistance < spec_5_2_panel$p_2, 1, 0)
spec_5_2_panel$q_3 <- ifelse(spec_5_2_panel$total_assistance >= spec_5_2_panel$p_2 & spec_5_2_panel$total_assistance < spec_5_2_panel$p_3, 1, 0)
spec_5_2_panel$q_4 <- ifelse(spec_5_2_panel$total_assistance >= spec_5_2_panel$p_3, 1, 0)

spec_5_2_panel <- rename(spec_5_2_panel, "assistance_lowest" = "q_1", "assistance_highest" = "q_4")

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
spec_5_2_panel$p_1 <- NULL
spec_5_2_panel$p_2 <- NULL
spec_5_2_panel$p_3 <- NULL
spec_5_2_panel$q_2 <- NULL
spec_5_2_panel$q_3 <- NULL
spec_5_2_panel$amount_ihp_approved <- NULL
spec_5_2_panel$amount_pa_obligated <- NULL
spec_5_2_panel$amount_hmgp_obligated <- NULL
spec_5_2_panel <- merge(aggregate(cbind(nr_dis_lag_0.25, nr_dis_lag_0.5, nr_dis_lag_0.5_e, nr_dis_lag_1_e) ~ fips_code + date + data_series + incident_category_man, spec_5_2_panel, sum, na.rm = TRUE),
                        select(spec_5_2_panel, -starts_with("nr_dis_lag"), -incident_type)[!(duplicated(spec_5_2_panel[c("fips_code", "date", "data_series", "incident_category_man")])),],
                        by = c("fips_code", "date", "data_series", "incident_category_man"), all.x = TRUE)

save(spec_5_2_panel, file = "data/spec_5_2_panel.RData")

# Pivot wider for all categories + lags
spec_5_2_panel <- merge(pivot_wider(pivot_longer(spec_5_2_panel, cols = starts_with("nr_dis_lag_"), names_to = "variable", values_to = "value"),
                                    id_cols = c("fips_code", "date", "data_series"), names_from = c("variable", "incident_category_man"), names_glue = "{variable}_{str_replace_all(tolower(incident_category_man), ' ', '_')}", values_from = "value", values_fill = 0),
                        select(spec_5_2_panel, -starts_with("nr_dis_lag"))[!(duplicated(spec_5_2_panel[c("fips_code", "date", "data_series")])),],
                        by = c("fips_code", "date", "data_series"), all.x = TRUE)

save(spec_5_2_panel, file = "data/spec_5_2_panel.RData")

spec_2_final <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_2_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "2.1"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_2_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.2"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.3"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.4"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + assistance_lowest * nr_dis_lag_0.5 + assistance_highest * nr_dis_lag_0.5, subset(spec_5_2_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.5"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5, subset(spec_5_2_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "2.1_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(spec_5_2_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.2_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.3_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value), subset(spec_5_2_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.4_sfh"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + assistance_lowest * nr_dis_lag_0.5 + assistance_highest * nr_dis_lag_0.5, subset(spec_5_2_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.5_sfh"))

openxlsx::write.xlsx(spec_2_final, file = "results/spec_2_final.xlsx")


