rm(list = ls())

library(tidyverse)
library(plm)

load("data/zillow_county.RData")
load("data/fema.RData")
load("data/fema_panel.RData")
load("data/prepared_panels.RData")
load("data/prepared_cost_panel.RData")

### First difference spec ----
time_period_spec <- list("pre_gfc" = c("2000-01-31", "2007-12-31"),
                         "post_gfc" = c("2009-01-31", "2019-12-31"),
                         "post_covid" = c("2021-01-31", "2022-12-31"),
                         "bush" = c("2001-01-31", "2008-12-31"),
                         "obama" = c("2009-01-31", "2016-12-31"),
                         "trump" = c("2017-01-31", "2020-12-31"),
                         "biden" = c("2021-01-31", "2022-12-31"))


spec_14_1_log <- data.frame()
for (it in unique(cost_panel$incident_type)){
  for (ds in unique(cost_panel$data_series)){
    for (tp in names(time_period_spec)){
      if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_999, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds & date %in% c(as.Date(time_period_spec[names(time_period_spec) == tp][[1]][1]), as.Date(time_period_spec[names(time_period_spec) == tp][[1]][2])))) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_999, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds & date %in% c(as.Date(time_period_spec[names(time_period_spec) == tp][[1]][1]), as.Date(time_period_spec[names(time_period_spec) == tp][[1]][2]))), starts_with("nr_dis_")), na.rm = TRUE) > 0)){
        eval(parse(text = paste0("spec_14_1_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999 + unemployment_rate + log(avg_wkly_wage), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_999, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "' & date %in% c(as.Date(time_period_spec$", tp, "[1]), as.Date(time_period_spec$", tp, "[2]))), index = c('fips_code', 'date'), model = 'fd', effect = 'individual')), 'incident_type' = '", it, "', 'data_series' = '", ds, "', 'effect' = 'entity', 'model' = 'fd', 'time_period' = '", tp, "', 'spec' = '14.1_log')")))
      } else {
        spec_14_1_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "model" = "fd", "time_period" = tp, "spec" = "14.1_log")
      }
      spec_14_1_log <- rbind(spec_14_1_log, spec_14_1_part)
      print(paste0("Done with ", it, " - ", ds, " - ", tp))
    }
  }
}

# spec_14_2_log <- data.frame()
# for (it in unique(cost_panel$incident_type)){
#   for (ds in unique(cost_panel$data_series)){
#     for (tp in names(time_period_spec)){
#       if ((nrow(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_999, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds & date %in% c(as.Date(time_period_spec[names(time_period_spec) == tp][[1]][1]), as.Date(time_period_spec[names(time_period_spec) == tp][[1]][2])))) > 0) & (sum(select(subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_999, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == it & data_series == ds & date %in% c(as.Date(time_period_spec[names(time_period_spec) == tp][[1]][1]), as.Date(time_period_spec[names(time_period_spec) == tp][[1]][2]))), starts_with("nr_dis_")), na.rm = TRUE) > 0)){
#         eval(parse(text = paste0("spec_14_2_part <- data.frame(plm_results(plm(log(zhvi) ~ nr_dis_lag_999 + unemployment_rate + log(avg_wkly_wage), subset(select(nr_occ_type_panel, fips_code, date, zhvi, data_series, incident_type, nr_dis_lag_999, gdp_value, unemployment_rate, avg_wkly_wage), incident_type == '", it, "' & data_series == '", ds, "' & date %in% c(as.Date(time_period_spec$", tp, "[1]), as.Date(time_period_spec$", tp, "[2]))), index = c('fips_code', 'date'), model = 'fd', effect = 'individual')), 'incident_type' = '", it, "', 'data_series' = '", ds, "', 'effect' = 'entity', 'model' = 'fd', 'time_period' = '", tp, "', 'spec' = '14.2_log')")))
#       } else {
#         spec_14_2_part <- data.frame("variable" = "EMPTY MODEL", "estimate" = NA, "std_error" = NA, "t_value" = NA, "p_value" = NA, "model" = NA, "rsq" = NA, "adj_rsq" = NA, "fstatistic" = NA, "fstat_pvalue" = NA, "nr_obs" = 0, "incident_type" = it,  "data_series" = ds, "effect" = "entity", "model" = "fd", "time_period" = tp, "spec" = "14.2_log")
#       }
#       spec_14_2_log <- rbind(spec_14_2_log, spec_14_2_part)
#       print(paste0("Done with ", it, " - ", ds, " - ", tp))
#     }
#   }
# }





openxlsx::write.xlsx(rbind(spec_14_1_log), file = "results/panel_spec_14_log.xlsx")



