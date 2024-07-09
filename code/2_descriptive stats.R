rm(list = ls())

library(tidyverse)
library(kableExtra)

### Descriptives on Zillow ----
load("data/zillow_county.RData")
source("code/1_data merging.R")

zillow_county_desc <- merge(select(aggregate(date ~ data_series, zillow_county, function(x){length(x)}), data_series, "observations" = date),
                            select(aggregate(date ~ data_series, zillow_county, min), data_series, "min_date" = date),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(date ~ data_series, zillow_county, max), data_series, "max_date" = date),
                            by = "data_series", all.x = TRUE)

zillow_county_desc$date_range <- paste0(zillow_county_desc$min_date, " - ", zillow_county_desc$max_date)
zillow_county_desc$min_date <- NULL
zillow_county_desc$max_date <- NULL

zillow_county$temp_fips <- fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips)
zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(temp_fips ~ data_series, zillow_county, function(x){length(unique(x))}), data_series, "nr_regions" = temp_fips),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            as.data.frame(as.matrix(aggregate(zhvi ~ data_series, zillow_county, function(x){quantile(x, c(0, 0.25, 0.5, 0.75, 1), na.rm = T)}))),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(zhvi ~ data_series, zillow_county, mean), data_series, "mean_zhvi" = zhvi),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(zhvi ~ data_series, zillow_county, function(x_){density(x_, na.rm = T)$x[which.max(density(x_, na.rm = T)$y)]}), data_series, "mode_zhvi" = zhvi),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(zhvi ~ data_series, zillow_county, var), data_series, "var_zhvi" = zhvi),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(zhvi ~ data_series, zillow_county, sd), data_series, "sd_zhvi" = zhvi),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(zhvi ~ data_series, zillow_county, moments::skewness), data_series, "skewness_zhvi" = zhvi),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(zhvi ~ data_series, zillow_county, moments::kurtosis), data_series, "kurtosis_zhvi" = zhvi),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- as.data.frame(t(zillow_county_desc))
names(zillow_county_desc) <- zillow_county_desc[1, ]
zillow_county_desc <- zillow_county_desc[-1,]
zillow_county_desc <- select(zillow_county_desc, all_homes_bottom_tier, all_homes_middle_tier, all_homes_top_tier, single_family_homes, condo_coop, one_bedroom, two_bedroom, three_bedroom, four_bedroom, five_plus_bedroom)
names(zillow_county_desc) <- c("Bottom Tier", "Middle Tier", "Top Tier", "Single Family", "Condo / Co-Op", "1 Bedroom", "2 Bedrooms", "3 Bedrooms", "4 Bedrooms", "5+ Bedrooms")
row.names(zillow_county_desc) <- c("Observations", "Date Range", "Regions", "Minimum", "25% Quantile", "Median", "75% Quantile", "Maximum", "Mean", "Mode", "Variance", "Standard Deviation", "Skewness", "Kurtosis")

# Save as Latex table
zillow_county_desc |> 
  kbl(format = "latex", align = "r") |> 
  kable_classic(full_width = TRUE) |> 
  write_file(file = "tables/zillow_descriptives.tex")


### Stationarity tests on Zillow ----
zillow_county_stat <- data.frame()
for (ds in unique(zillow_county$data_series)){
  z_sub <- subset(zillow_county, data_series == ds & !is.na(zhvi) & fips_code %in% subset(aggregate(date ~ fips_code + data_series, subset(zillow_county, !is.na(zhvi)), function(x){length(unique(x))}), date == 290 & data_series == ds)$fips_code)
  ips_test <- purtest(zhvi ~ trend, data = z_sub, index = c("fips_code", "date"), test = "ips", lags = "SIC")
  zillow_county_stat <- rbind(zillow_county_stat,
                              data.frame("Wtbar" = ips_test$statistic$statistic,
                                         "p_value" = ips_test$statistic$p.value,
                                         "data_series" = ds,
                                         "comment" = "Only counties with 290 observations."))
  print(ds)
}

openxlsx::write.xlsx(zillow_county_stat, file = "tables/zillow_stationarity.xlsx")


### Descriptives on FEMA ----
rm(list = ls())
gc()

load("data/fema.RData")
load("data/zillow_county.RData")
source("code/1_data merging.R")

fema <- merge(fema, fema_incident_mapping, by = "incident_type", all.x = TRUE)

fema_desc <- aggregate(incident_type ~ incident_category_man, fema, function(x){paste(unique(x), collapse = ", ")})
fema_desc$incident_category_man <- factor(fema_desc$incident_category_man, levels = c("Extreme Weather", "Fire", "Geological", "Human Cause", "Water", "Wind", "Other"))

fema_desc <- merge(fema_desc,
                   select(aggregate(disaster_number ~ incident_category_man, subset(fema, date_incident_begin >= as.Date("2000-01-01")), function(x){length(unique(x))}), incident_category_man, "nr_disasters" = disaster_number),
                   by = "incident_category_man", all.x = TRUE)

fema_desc <- merge(fema_desc,
                   select(aggregate(date_declaration ~ incident_category_man, subset(fema, date_incident_begin >= as.Date("2000-01-01")), function(x){paste0(min(x), " - ", max(x))}), incident_category_man, "date_range" = date_declaration),
                   by = "incident_category_man", all.x = TRUE)

zillow_county$fips_code <- fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips)

fema_desc <- merge(fema_desc,
                   select(aggregate(place_code ~ incident_category_man, subset(fema, date_incident_begin >= as.Date("2000-01-01")), function(x){length(unique(x)[unique(x) %in% zillow_county$fips_code])}), incident_category_man, "regions" = place_code),
                   by = "incident_category_man", all.x = TRUE)

# Save as Latex table
fema_desc |> 
  kbl(format = "latex", align = "r") |> 
  kable_classic(full_width = TRUE) |> 
  write_file(file = "tables/fema_descriptives.tex")


### Descriptives on BEA / LAUS ----
load("data/bea_gdp.RData")
load("data/bls_laus.RData")
load("data/bls_qcew.RData")

laus$fips_code <- fips_pad(laus$state_code, laus$county_code)

econ_desc <- merge(merge(subset(select(qcew, "fips_code" = area_fips, agglvl_code, year, qtr, avg_wkly_wage), agglvl_code == 70 & avg_wkly_wage != 0, select = c(fips_code, year, qtr, avg_wkly_wage)),
                         select(laus, fips_code, year, unemployment_rate), by = c("fips_code", "year"), all.x = TRUE),
                   subset(bea, table_name == "CAGDP2" & industry_classification == "...", select = c(fips_code, gdp_value, year)), by = c("fips_code", "year"), all.x = TRUE) |> 
  subset(year >= "2000") |> 
  mutate("unemployment_rate" = as.numeric(unemployment_rate), # NAs introduced through 368 "N.A." strings, checked and okay
         "gdp_value" = as.numeric(gdp_value)) |> 
  pivot_longer(cols = c("avg_wkly_wage", "unemployment_rate", "gdp_value"), names_to = "data_series", values_to = "econ_value") |> 
  mutate("date" = paste0(year, "-", str_pad(qtr, width = 2, side = "left", pad = "0")))

econ_desc_out <- merge(select(aggregate(econ_value ~ data_series, econ_desc, function(x){length(na.omit(x))}), data_series, "observations" = econ_value),
                       select(aggregate(date ~ data_series, subset(econ_desc, !is.na(econ_value)), min), data_series, "min_date" = date),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(date ~ data_series, subset(econ_desc, !is.na(econ_value)), max), data_series, "max_date" = date),
                       by = "data_series", all.x = TRUE)

econ_desc_out$date_range <- paste0(econ_desc_out$min_date, " - ", econ_desc_out$max_date)
econ_desc_out$min_date <- NULL
econ_desc_out$max_date <- NULL

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(fips_code ~ data_series, subset(econ_desc, !is.na(econ_value)), function(x){length(unique(x))}), data_series, "nr_regions" = fips_code),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       as.data.frame(as.matrix(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), function(x){quantile(x, c(0, 0.25, 0.5, 0.75, 1), na.rm = T)}))),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), mean), data_series, "mean_econ_value" = econ_value),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), function(x_){density(x_, na.rm = T)$x[which.max(density(x_, na.rm = T)$y)]}), data_series, "mode_econ_value" = econ_value),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), var), data_series, "var_econ_value" = econ_value),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), sd), data_series, "sd_econ_value" = econ_value),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), moments::skewness), data_series, "skewness_econ_value" = econ_value),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- merge(econ_desc_out,
                       select(aggregate(econ_value ~ data_series, subset(econ_desc, !is.na(econ_value)), moments::kurtosis), data_series, "kurtosis_econ_value" = econ_value),
                       by = "data_series", all.x = TRUE)

econ_desc_out <- as.data.frame(t(econ_desc_out))
names(econ_desc_out) <- econ_desc_out[1, ]
econ_desc_out <- econ_desc_out[-1,]
econ_desc_out <- select(econ_desc_out, unemployment_rate, avg_wkly_wage, gdp_value)
names(econ_desc_out) <- c("Unemployment rate", "Average weekly wage", "GDP")
row.names(econ_desc_out) <- c("Observations", "Date Range", "Regions", "Minimum", "25% Quantile", "Median", "75% Quantile", "Maximum", "Mean", "Mode", "Variance", "Standard Deviation", "Skewness", "Kurtosis")

# Save as Latex table
econ_desc_out |> 
  kbl(format = "latex", align = "r") |> 
  kable_classic(full_width = TRUE) |> 
  write_file(file = "tables/econ_descriptives.tex")
