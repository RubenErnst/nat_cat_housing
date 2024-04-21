rm(list = ls())

library(tidyverse)
library(kableExtra)

### Descriptives on Zillow ----
load("data/zillow_county.RData")

zillow_county_desc <- merge(select(aggregate(date ~ data_series, zillow_county, function(x){length(x)}), data_series, "observations" = date),
                            select(aggregate(date ~ data_series, zillow_county, min), data_series, "min_date" = date),
                            by = "data_series", all.x = TRUE)

zillow_county_desc <- merge(zillow_county_desc,
                            select(aggregate(date ~ data_series, zillow_county, max), data_series, "max_date" = date),
                            by = "data_series", all.x = TRUE)

zillow_county_desc$date_range <- paste0(zillow_county_desc$min_date, " - ", zillow_county_desc$max_date)
zillow_county_desc$min_date <- NULL
zillow_county_desc$max_date <- NULL

zillow_county$temp_fips <- paste0(zillow_county$state_code_fips, zillow_county$municipal_code_fips)
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
  kbl(format = "latex", align="r") |> 
  kable_classic(full_width = TRUE) |> 
  write_file(file = "tables/zillow_descriptives.tex")



### Descriptives on FEMA ----
rm(list = ls())
gc()

load("data/fema.RData")
source("code/1_data merging.R")

fema <- merge(fema, fema_incident_mapping, by = "incident_type", all.x = TRUE)

fema_desc <- aggregate(incident_type ~ incident_category_man, fema, function(x){paste(unique(x), collapse = ", ")})
fema_desc$incident_category_man <- factor(fema_desc$incident_category_man, levels = c("Extreme Weather", "Fire", "Geological", "Human Cause", "Water", "Wind", "Other"))

fema_desc <- merge(fema_desc,
                   select(aggregate(disaster_number ~ incident_category_man, fema, function(x){length(unique(x))}), incident_category_man, "nr_disasters" = disaster_number),
                   by = "incident_category_man", all.x = TRUE)

fema_desc <- merge(fema_desc,
                   select(aggregate(date_declaration ~ incident_category_man, fema, function(x){paste0(min(x), " - ", max(x))}), incident_category_man, "date_range" = date_declaration),
                   by = "incident_category_man", all.x = TRUE)

fema_desc <- merge(fema_desc,
                   select(aggregate(place_code ~ incident_category_man, fema, function(x){length(unique(x))}), incident_category_man, "regions" = place_code),
                   by = "incident_category_man", all.x = TRUE)


