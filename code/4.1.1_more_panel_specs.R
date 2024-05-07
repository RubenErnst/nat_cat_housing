library(tidyverse)
library(plm)

# Spec 1 log ----
spec_1_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = "1.1_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = "1.2_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "single_family_homes"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "1.3_log"),
                    data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + nr_dis_lag_2_e, data = subset(nr_occ_panel, data_series == "condo_coop"), index = c("fips_code", "date"), model = "random", effect = "twoways")), "data_series" = "condo_coop", "effect" = "both", "spec" = "1.3_log"))

openxlsx::write.xlsx(spec_1_log, file = "results/panel_spec_1_log.xlsx")


# Spec 2 log ----
spec_2_1_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = "2.1_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = "2.1_log"))

spec_2_2_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = "2.2_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = "2.2_log"))


# Add time fixed effects per year
spec_2_3_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = "2.3_log"),
                      data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_panel, fips_code, date, zhvi, data_series, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = "2.3_log"))

openxlsx::write.xlsx(rbind(spec_2_1_log, spec_2_2_log, spec_2_3_log), file = "results/panel_spec_2_log.xlsx")



# Spec 3 log ----
spec_3_1_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_3_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '3.1_log')")))
    } else {
      spec_3_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "3.1_log")
    }
    spec_3_1_log <- rbind(spec_3_1_log, spec_3_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

spec_3_2_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    print(paste0("Started with ", it, " - ", ds))
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_3_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '3.2_log')")))
    } else {
      spec_3_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "3.2_log")
    }
    spec_3_2_log <- rbind(spec_3_2_log, spec_3_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

# Add time fixed effects per year
spec_3_3_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_3_3_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '3.3_log')")))
    } else {
      spec_3_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "3.3_log")
    }
    spec_3_3_log <- rbind(spec_3_3_log, spec_3_3_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}


openxlsx::write.xlsx(rbind(spec_3_1_log, spec_3_2_log, spec_3_3_log), file = "results/panel_spec_3_log.xlsx")



# Spec 4 log ----
spec_4_1_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_4_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '4.1_log')")))
    } else {
      spec_4_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "4.1_log")
    }
    spec_4_1_log <- rbind(spec_4_1_log, spec_4_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

spec_4_2_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_4_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '4.2_log')")))
    } else {
      spec_4_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "4.2_log")
    }
    spec_4_2_log <- rbind(spec_4_2_log, spec_4_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

# Add time fixed effects per year
spec_4_3_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_4_3_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '4.3_log')")))
    } else {
      spec_4_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "4.3_log")
    }
    spec_4_3_log <- rbind(spec_4_3_log, spec_4_3_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}


openxlsx::write.xlsx(rbind(spec_4_1_log, spec_4_2_log, spec_4_3_log), file = "results/panel_spec_4_log.xlsx")


# Spec 5 log ----
spec_5_1_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_5_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '5.1_log')")))
    } else {
      spec_5_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "5.1_log")
    }
    spec_5_1_log <- rbind(spec_5_1_log, spec_5_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

spec_5_2_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_5_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '5.2_log')")))
    } else {
      spec_5_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "5.2_log")
    }
    spec_5_2_log <- rbind(spec_5_2_log, spec_5_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

# Add time fixed effects per year
spec_5_3_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_5_3_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '5.3_log')")))
    } else {
      spec_5_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "5.3_log")
    }
    spec_5_3_log <- rbind(spec_5_3_log, spec_5_3_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

openxlsx::write.xlsx(rbind(spec_5_1_log, spec_5_2_log, spec_5_3_log), file = "results/panel_spec_5_log.xlsx")



# Spec 6 log ----
spec_6_1_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_6_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '6.1_log')")))
    } else {
      spec_6_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "6.1_log")
    }
    spec_6_1_log <- rbind(spec_6_1_log, spec_6_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

spec_6_2_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_6_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '6.2_log')")))
    } else {
      spec_6_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "6.2_log")
    }
    spec_6_2_log <- rbind(spec_6_2_log, spec_6_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

# Add time fixed effects per year
spec_6_3_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_6_3_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '6.3_log')")))
    } else {
      spec_6_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "6.3_log")
    }
    spec_6_3_log <- rbind(spec_6_3_log, spec_6_3_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

openxlsx::write.xlsx(rbind(spec_6_1_log, spec_6_2_log, spec_6_3_log), file = "results/panel_spec_6_log.xlsx")



# Spec 7 log ----
spec_7_1_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_7_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '7.1_log')")))
    } else {
      spec_7_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "7.1_log")
    }
    spec_7_1_log <- rbind(spec_7_1_log, spec_7_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

spec_7_2_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_7_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '7.2_log')")))
    } else {
      spec_7_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "7.2_log")
    }
    spec_7_2_log <- rbind(spec_7_2_log, spec_7_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

# Add time fixed effects per year
spec_7_3_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_7_3_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '7.3_log')")))
    } else {
      spec_7_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "7.3_log")
    }
    spec_7_3_log <- rbind(spec_7_3_log, spec_7_3_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

openxlsx::write.xlsx(rbind(spec_7_1_log, spec_7_2_log, spec_7_3_log), file = "results/panel_spec_7_log.xlsx")


# Spec 8 log ----
spec_8_1_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_8_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '8.1_log')")))
    } else {
      spec_8_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "8.1_log")
    }
    spec_8_1_log <- rbind(spec_8_1_log, spec_8_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

spec_8_2_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_8_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage + gdp_value, subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'entity', 'spec' = '8.2_log')")))
    } else {
      spec_8_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "spec" = "8.2_log")
    }
    spec_8_2_log <- rbind(spec_8_2_log, spec_8_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

# Add time fixed effects per year
spec_8_3_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_8_3_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + unemployment_rate + avg_wkly_wage + gdp_value + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '8.3_log')")))
    } else {
      spec_8_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "8.3_log")
    }
    spec_8_3_log <- rbind(spec_8_3_log, spec_8_3_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

openxlsx::write.xlsx(rbind(spec_8_1_log, spec_8_2_log, spec_8_3_log), file = "results/panel_spec_8_log.xlsx")



### Spec 9 log ----
spec_9_1_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_9_1_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.25, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = '9.1_log')")))
      } else {
        spec_9_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "entity", "spec" = "9.1_log")
      }
      spec_9_1_log <- rbind(spec_9_1_log, spec_9_1_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

spec_9_2_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_9_2_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = '9.2_log')")))
      } else {
        spec_9_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "entity", "spec" = "9.2_log")
      }
      spec_9_2_log <- rbind(spec_9_2_log, spec_9_2_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

# Add time fixed effects per year
spec_9_3_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_9_3_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + factor(lubridate::year(date)), subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'both', 'spec' = '9.3_log')")))
      } else {
        spec_9_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "both", "spec" = "9.3_log")
      }
      spec_9_3_log <- rbind(spec_9_3_log, spec_9_3_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

openxlsx::write.xlsx(rbind(spec_9_1_log, spec_9_2_log, spec_9_3_log), file = "results/panel_spec_9_log.xlsx")



### Spec 10 log ----
spec_10_1_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_10_1_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.25 + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = '10.1_log')")))
      } else {
        spec_10_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "entity", "spec" = "10.1_log")
      }
      spec_10_1_log <- rbind(spec_10_1_log, spec_10_1_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

spec_10_2_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_10_2_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'entity', 'spec' = '10.2_log')")))
      } else {
        spec_10_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "entity", "spec" = "10.2_log")
      }
      spec_10_2_log <- rbind(spec_10_2_log, spec_10_2_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

# Add time fixed effects per year
spec_10_3_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_10_3_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.25 + dis_lag_0.5_e + dis_lag_1_e + gdp_value + unemployment_rate + avg_wkly_wage + factor(lubridate::year(date)), subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, gdp_value, unemployment_rate, avg_wkly_wage, dis_lag_0.25, dis_lag_0.5_e, dis_lag_1_e), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'both', 'spec' = '10.3_log')")))
      } else {
        spec_10_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "both", "spec" = "10.3_log")
      }
      spec_10_3_log <- rbind(spec_10_3_log, spec_10_3_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

openxlsx::write.xlsx(rbind(spec_10_1_log, spec_10_2_log, spec_10_3_log), file = "results/panel_spec_10_log.xlsx")



### Spec 11 log ----
spec_11_1_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = "11.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = "11.1_log"))

spec_11_2_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = "11.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = "11.2_log"))


# Add time fixed effects per year
spec_11_3_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = "11.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)), subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = "11.3_log"))

openxlsx::write.xlsx(rbind(spec_11_1_log, spec_11_2_log, spec_11_3_log), file = "results/panel_spec_11_log.xlsx")



# Spec 12 log ----
spec_12_1_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = "12.1_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = "12.1_log"))

spec_12_2_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_top_tier", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_middle_tier", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "all_homes_bottom_tier", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "single_family_homes", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "condo_coop", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "one_bedroom", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "two_bedroom", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "three_bedroom", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "four_bedroom", "effect" = "entity", "spec" = "12.2_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within", effect = "individual")), "data_series" = "five_plus_bedroom", "effect" = "entity", "spec" = "12.2_log"))


# Add time fixed effects per year
spec_12_3_log <- rbind(data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_top_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_top_tier", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_middle_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_middle_tier", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "all_homes_bottom_tier"), index = c("fips_code", "date"), model = "within")), "data_series" = "all_homes_bottom_tier", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "single_family_homes"), index = c("fips_code", "date"), model = "within")), "data_series" = "single_family_homes", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "condo_coop"), index = c("fips_code", "date"), model = "within")), "data_series" = "condo_coop", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "one_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "one_bedroom", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "two_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "two_bedroom", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "three_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "three_bedroom", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "four_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "four_bedroom", "effect" = "both", "spec" = "12.3_log"),
                       data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.25 + nr_dis_lag_0.5_e + nr_dis_lag_1_e + incident_type + factor(lubridate::year(date)) + gdp_value + unemployment_rate + avg_wkly_wage, subset(select(cost_agg_panel, fips_code, date, zhvi, data_series, incident_type, gdp_value, unemployment_rate, avg_wkly_wage, nr_dis_lag_0.25, nr_dis_lag_0.5_e, nr_dis_lag_1_e), data_series == "five_plus_bedroom"), index = c("fips_code", "date"), model = "within")), "data_series" = "five_plus_bedroom", "effect" = "both", "spec" = "12.3_log"))

openxlsx::write.xlsx(rbind(spec_12_1_log, spec_12_2_log, spec_12_3_log), file = "results/panel_spec_12_log.xlsx")


### Spec 13 log ----
# Nr. dis panel, log(zhvi), log(gdp_value), log(avg_wkly_wage), time FE on year

spec_13_1_log <- data.frame()
for (it in unique(nr_occ_type_panel$incident_type)){
  for (ds in unique(nr_occ_type_panel$data_series)){
    if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_13_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + factor(lubridate::year(date)), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '13.1_log')")))
    } else {
      spec_13_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "13.1_log")
    }
    spec_13_1_log <- rbind(spec_13_1_log, spec_13_1_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

openxlsx::write.xlsx(spec_13_1_log, file = "results/panel_spec_13_1_log.xlsx")

# Dummy dis panel, log(zhvi), log(gdp_value), log(avg_wkly_wage), time FE on year
spec_13_2_log <- data.frame()
for (it in unique(dummy_occ_type_panel$incident_type)){
  for (ds in unique(dummy_occ_type_panel$data_series)){
    if ((nrow(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds)) > 0) & (sum(select(subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds), starts_with("nr_dis")), na.rm = TRUE) > 0)){
      eval(parse(text = paste0("spec_13_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + factor(lubridate::year(date)), subset(select(dummy_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'effect' = 'both', 'spec' = '13.2_log')")))
    } else {
      spec_13_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "both", "spec" = "13.2_log")
    }
    spec_13_2_log <- rbind(spec_13_2_log, spec_13_2_part)
    print(paste0("Done with ", it, " - ", ds))
  }
}

openxlsx::write.xlsx(spec_13_2_log, file = "results/panel_spec_13_2_log.xlsx")

spec_13_3_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (at in unique(cost_panel$assistance_type)){
      if ((nrow(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds & assistance_type == at)) > 0) & (sum(select(subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds & assistance_type == at), starts_with("dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_13_3_part <- data.frame(plm_results(plm(log(zhvi) ~ dis_lag_0.5 + unemployment_rate + log(avg_wkly_wage) + log(gdp_value) + factor(lubridate::year(date)), subset(select(cost_panel, fips_code, date, zhvi, data_series, incident_type, assistance_type, dis_lag_0.5, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "' & assistance_type == '", at, "'), index = c('fips_code', 'date'), model = 'within', effect = 'individual')), 'incident_type' = '", it, "',  'data_series' = '", ds, "', 'assistance_type' = '", at, "', 'effect' = 'both', 'spec' = '13.3_log')")))
      } else {
        spec_13_3_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "assistance_type" = at, "effect" = "both", "spec" = "13.3_log")
      }
      spec_13_3_log <- rbind(spec_13_3_log, spec_13_3_part)
      print(paste0("Done with ", it, " - ", ds, " - ", at))
    }
  }
}

openxlsx::write.xlsx(spec_13_3_log, file = "results/panel_spec_13_3_log.xlsx")

