rm(list = ls())

library(tidyverse)
library(ggrepel)


### Data exploration ----
load("data/zillow_county.RData")
zillow_county$fips_code <- fips_pad(zillow_county$state_code_fips, zillow_county$municipal_code_fips)

z_hist_plot <- aggregate(date ~ fips_code + data_series, subset(zillow_county, !is.na(zhvi)), function(x){length(unique(x))}) |> 
  mutate("data_series" = factor(data_series, levels = c("all_homes_bottom_tier", "all_homes_middle_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))) |> 
  ggplot(aes(x = date)) + 
  geom_histogram() +
  scale_x_continuous(name = "Nr. of observations") +
  scale_y_continuous(name = "Nr. of counties") +
  facet_wrap(~data_series) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "plots/0_zillow_nr obs per county.pdf", plot = z_hist_plot, width = 8, height = 6)

# Check for duplicate entity + time + dwelling type observations
stopifnot(any(aggregate(zhvi ~ fips_code + date + data_series, subset(zillow_county, !is.na(zhvi)), length)$zhvi > 1))


# FEMA disasters per type per year
load("data/fema.RData")

f_hist_plot <- aggregate(disaster_number ~ incident_type + lubridate::year(date_incident_begin), fema, function(x){length(unique(x))}) |> 
  select("nr_dis" = disaster_number, incident_type, "year" = "lubridate::year(date_incident_begin)") |> 
  subset(year >= 2000) |> 
  ggplot(aes(x = year, y = nr_dis)) +
  geom_col() +
  facet_wrap(~incident_type) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "plots/0_fema_nr obs per incident type.pdf", plot = f_hist_plot, width = 12, height = 9)


# Observations per county BLS / BEA
load("data/bea_gdp.RData")
load("data/bls_laus.RData")
load("data/bls_qcew.RData")

bea <- subset(bea, industry_id == 1 & (unit == "Millions of current dollars" | unit == "Thousands of dollars"))
bea <- subset(bea, !duplicated(select(bea, fips_code, year)))

# Assimilate units
bea$gdp_value <- as.numeric(bea$gdp_value)
bea$gdp_value[bea$unit == "Thousands of dollars"] <- bea$gdp_value[bea$unit == "Thousands of dollars"] / 1e3
bea$table_name <- "MAGDP2"
bea$unit <- "Millions of current dollars"

# Adjust BEA coded FIPS codes
bea_codes <- subset(bea, fips_code %in% bea_fips_mod$BEA.FIPS)
bea <- subset(bea, !fips_code %in% bea_fips_mod$BEA.FIPS)

# Add number of counties that BEA has aggregated to not inflate GDP numbers after assignment
bea_fips_mod <- merge(bea_fips_mod,
                      select(aggregate(FIPS ~ BEA.FIPS, bea_fips_mod, function(x){length(unique(x))}), BEA.FIPS, "nr_counties" = FIPS),
                      by = "BEA.FIPS", all.x = TRUE)

bea_codes <- merge(splitstackshape::cSplit(data.frame("fips_code" = bea_fips_mod$FIPS,
                                                      "nr_counties" = bea_fips_mod$nr_counties,
                                                      "fips_code_bea" = as.character(bea_fips_mod$BEA.FIPS),
                                                      "year" = paste(min(bea$year):max(bea$year), collapse = ", ")), "year", ", ", "long"),
                   bea_codes,
                   by.x = c("fips_code_bea", "year"), by.y = c("fips_code", "year"), all.x = TRUE)

bea_codes <- subset(bea_codes, !is.na(table_name))
bea_codes$gdp_value <- bea_codes$gdp_value / bea_codes$nr_counties
bea_codes$fips_code_bea <- NULL
bea_codes$nr_counties <- NULL

bea <- rbind(bea, bea_codes); rm(bea_codes)

bea_plot <- aggregate(fips_code ~ year, subset(bea, fips_code %in% fips_pad(fips_counties$state_code, fips_counties$county_code) & substr(fips_code, 3, 5) != "000" & !is.na(gdp_value)), function(x){length(unique(x))})

laus$fips_code <- fips_pad(laus$state_code, laus$county_code)

laus_plot <- aggregate(fips_code ~ year, subset(laus, fips_code %in% fips_pad(fips_counties$state_code, fips_counties$county_code) & substr(fips_code, 3, 5) != "000" & !is.na(unemployment_rate)), function(x){length(unique(x))})

qcew_plot <- aggregate(fips_code ~ year + qtr, subset(select(qcew, "fips_code" = area_fips, year, qtr, avg_wkly_wage), fips_code %in% fips_pad(fips_counties$state_code, fips_counties$county_code) & substr(fips_code, 3, 5) != "000" & !is.na(avg_wkly_wage)), function(x){length(unique(x))})
qcew_plot <- aggregate(fips_code ~ year, qcew_plot, min)

econ_summary <- merge(merge(select(subset(qcew_plot, year >= 2000), "avg_wkly_wage" = fips_code, year),
                            select(subset(laus_plot, year >= 2000), "unemployment_rate" = fips_code, year),
                            by = "year", all.x = TRUE),
                      select(subset(bea_plot, year >= 2000), "gdp_value" = fips_code, year),
                      by = "year", all.x = TRUE)

econ_summary[is.na(econ_summary)] <- 0

openxlsx::write.xlsx(econ_summary, file = "tables/econ_summary.xlsx")


# NRI summary stats
load("data/nri.RData")

nri_counties$fips_code <- fips_pad(nri_counties$state_code, nri_counties$county_code)

nri_plot <- aggregate(fips_code ~ risk_type + value_type, subset(nri_counties, !is.na(value) & value_type %in% c("annualized_freq", "alr_building_value", "eal_building_value", "exposure_building_value", "hlr_buildings", "risk_score", "risk_value")), function(x){length(unique(x))}) |> 
  pivot_wider(id_cols = c("risk_type"), names_from = "value_type", values_from = "fips_code")

nri_plot[is.na(nri_plot)] <- 0

openxlsx::write.xlsx(nri_plot, file = "tables/nri_summary.xlsx")


### Zillow yearly line plot ----
load("data/zillow_county.RData")

# ZHVI median by data series
zhvi_by_data_series_line <- aggregate(zhvi ~ data_series + lubridate::year(date), zillow_county, median, na.rm = T) |>
  select(data_series, "year" = `lubridate::year(date)`, zhvi) |>
  mutate(data_series = factor(data_series, levels = c("all_homes_bottom_tier", "all_homes_middle_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))) |> 
  ggplot(aes(x = year, y = zhvi, linetype = data_series)) +
  geom_line() +
  geom_text_repel(data = aggregate(zhvi ~ data_series + lubridate::year(date), zillow_county, median, na.rm = T) |> select(data_series, "year" = `lubridate::year(date)`, zhvi) |> filter(year == last(year)),
                  aes(label = plyr::revalue(data_series, c("all_homes_bottom_tier" = "Bottom Tier",
                                                           "all_homes_middle_tier" = "Middle Tier",
                                                           "all_homes_top_tier" = "Top Tier",
                                                           "single_family_homes" = "Single Family",
                                                           "condo_coop" = "Condo / Co-Op",
                                                           "one_bedroom" = "1 Bedroom",
                                                           "two_bedroom" = "2 Bedrooms",
                                                           "three_bedroom" = "3 Bedrooms",
                                                           "four_bedroom" = "4 Bedrooms",
                                                           "five_plus_bedroom" = "5+ Bedrooms")),
                      x = year, y = zhvi),
                  hjust = 0.6, vjust = -1.5, size = 3) +
  scale_x_continuous(limits = c(2000, 2024), breaks = seq(2000, 2024, 2)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = "'"), name = "Zillow Home Value Index", limits = c(0, 450000)) +
  scale_linetype(name = "Data Series", labels = c("Bottom Tier", "Middle Tier", "Top Tier", "Single Family", "Condo / Co-Op", "1 Bedroom", "2 Bedrooms", "3 Bedrooms", "4 Bedrooms", "5+ Bedrooms")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "points"),
        panel.spacing = margin(1, 80, 1, 1, "points"))

ggsave(filename = "plots/zhvi_by_data_series_line.pdf", plot = zhvi_by_data_series_line, width = 10, height = 7.5)



# FEMA disaster maps by incident_category ----
load("data/county_shape_plotting.RData")
load("data/fema.RData")
source("code/1_data merging.R")

# Prepare data

# dis_per_region <- aggregate(disaster_number ~ place_code + incident_type, subset(fema, date_incident_begin >= as.Date("2000-01-01")), function(x){length(unique(x))})
# dis_per_region <- subset(dis_per_region, incident_type %in% c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Biological"))
# 
# dis_per_region <- merge(splitstackshape::cSplit(data.frame(subset(county_shape_plotting, substr(fips_code, 1, 2) %in% fips_states$state_code), "incident_type" = paste(c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Biological"), collapse = ", ")), splitCols = "incident_type", sep = ", ", direction = "long"),
#                         dis_per_region,
#                         by.x = c("fips_code", "incident_type"), by.y = c("place_code", "incident_type"), all.x = TRUE)
# 
# ggplot(data = subset(dis_per_region, incident_type == "Fire")) +
#   geom_polygon(aes(x = lon, y = lat, fill = disaster_number, group = fips_code), color = "#494949") +
#   scale_fill_gradient(low = "#fac8a5", high = "#ad4700", na.value = "white") +
#   coord_fixed() +
#   theme_bw() +
#   theme(axis.title = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_blank())

county_shape$fips_code <- fips_pad(county_shape$STATEFP, county_shape$COUNTYFP)
county_shape$incident_type <- paste(c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Biological"), collapse = ", ")

dis_per_region <- aggregate(disaster_number ~ place_code + incident_type, subset(fema, date_incident_begin >= as.Date("2000-01-01")), function(x){length(unique(x))})
dis_per_region <- subset(dis_per_region, incident_type %in% c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Biological"))

dis_per_region <- merge(splitstackshape::cSplit(data.frame(subset(county_shape, substr(fips_code, 1, 2) %in% fips_states$state_code), "incident_type" = paste(c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Biological"), collapse = ", ")), splitCols = "incident_type", sep = ", ", direction = "long"),
                        dis_per_region,
                        by.x = c("fips_code", "incident_type"), by.y = c("place_code", "incident_type"), all.x = TRUE)


fire_map <- ggplot(data = subset(dis_per_region, incident_type == "Fire")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#fac8a5", high = "#ad4700", na.value = "white", name = "Nr. of Fires") +
  coord_sf(xlim = c(-180, -66), ylim = c(15, 71)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

ggsave(filename = "plots/fire_map.pdf", plot = fire_map, width = 10, height = 7.5)

