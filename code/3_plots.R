rm(list = ls())

library(tidyverse)
library(ggrepel)
library(ggpubr)


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



# FEMA disaster maps by incident_type ----
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
county_shape$incident_type <- paste(c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Tornado"), collapse = ", ")

dis_per_region <- aggregate(disaster_number ~ place_code + incident_type, subset(fema, date_incident_begin >= as.Date("2000-01-01")), function(x){length(unique(x))})
dis_per_region <- subset(dis_per_region, incident_type %in% c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Tornado"))

dis_per_region <- merge(splitstackshape::cSplit(data.frame(subset(county_shape, substr(fips_code, 1, 2) %in% fips_states$state_code), "incident_type" = paste(c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Tornado"), collapse = ", ")), splitCols = "incident_type", sep = ", ", direction = "long"),
                        dis_per_region,
                        by.x = c("fips_code", "incident_type"), by.y = c("place_code", "incident_type"), all.x = TRUE)


map_fire <- ggplot(data = subset(dis_per_region, incident_type == "Fire")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#fcdfca", high = "#ad4700", na.value = "white", name = "Nr. of Fires") +
  coord_sf(xlim = c(-180, -66), ylim = c(15, 71), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_storm <- ggplot(data = subset(dis_per_region, incident_type == "Severe Storm")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#ccd0f0", high = "#333859", na.value = "white", name = "Nr. of Severe Storms") +
  coord_sf(xlim = c(-180, -66), ylim = c(15, 71), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_flood <- ggplot(data = subset(dis_per_region, incident_type == "Flood")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#f2e5c9", high = "#a68e5e", na.value = "white", name = "Nr. of Floods") +
  coord_sf(xlim = c(-180, -66), ylim = c(15, 71), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_hurricane <- ggplot(data = subset(dis_per_region, incident_type == "Hurricane")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#b2ccd1", high = "#325961", na.value = "white", name = "Nr. of Hurricanes") +
  coord_sf(xlim = c(-180, -66), ylim = c(15, 71), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_snow <- ggplot(data = subset(dis_per_region, incident_type == "Snowstorm")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#a1e7f0", high = "#32afbf", na.value = "white", name = "Nr. of Snowstorms") +
  coord_sf(xlim = c(-180, -66), ylim = c(15, 71), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_tornado <- ggplot(data = subset(dis_per_region, incident_type == "Tornado")) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#494949", linewidth = 0.2) +
  scale_fill_gradient(low = "#dbba95", high = "#5c442a", na.value = "white", breaks = c(1,2,3), name = "Nr. of Tornados") +
  coord_sf(xlim = c(-178, -66.8), ylim = c(18, 71.4), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

maps_plot <- ggarrange(map_fire, map_storm, map_flood, map_hurricane, map_snow, map_tornado, 
                       labels = c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Tornado"),
                       ncol = 2, nrow = 3)

ggsave(filename = "plots/maps_plot.pdf", plot = maps_plot, width = 12, height = 16)


### FEMA disaster maps by incident_type - contiguous states only ----
map_fire <- ggplot(data = subset(dis_per_region, incident_type == "Fire" & !STATEFP %in% c("02", "15"))) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#fcdfca", high = "#ad4700", na.value = "white", name = "Nr. of Fires") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_storm <- ggplot(data = subset(dis_per_region, incident_type == "Severe Storm" & !STATEFP %in% c("02", "15"))) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#ccd0f0", high = "#333859", na.value = "white", name = "Nr. of Severe Storms") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_flood <- ggplot(data = subset(dis_per_region, incident_type == "Flood" & !STATEFP %in% c("02", "15"))) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#f2e5c9", high = "#a68e5e", na.value = "white", name = "Nr. of Floods") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_hurricane <- ggplot(data = subset(dis_per_region, incident_type == "Hurricane" & !STATEFP %in% c("02", "15"))) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#b2ccd1", high = "#325961", na.value = "white", name = "Nr. of Hurricanes") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_snow <- ggplot(data = subset(dis_per_region, incident_type == "Snowstorm" & !STATEFP %in% c("02", "15"))) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#212121") +
  scale_fill_gradient(low = "#a1e7f0", high = "#32afbf", na.value = "white", name = "Nr. of Snowstorms") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

map_tornado <- ggplot(data = subset(dis_per_region, incident_type == "Tornado" & !STATEFP %in% c("02", "15"))) +
  geom_sf(aes(geometry = geometry, fill = disaster_number), color = "#494949", linewidth = 0.2) +
  scale_fill_gradient(low = "#dbba95", high = "#5c442a", na.value = "white", breaks = c(1,2,3), name = "Nr. of Tornados") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = "WGS84") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        panel.spacing = margin(0,0,0,0))

maps_plot <- ggarrange(map_fire, map_storm, map_flood, map_hurricane, map_snow, map_tornado, 
                       labels = c("Fire", "Severe Storm", "Flood", "Hurricane", "Snowstorm", "Tornado"),
                       ncol = 2, nrow = 3)

ggsave(filename = "plots/maps_plot_contiguous.pdf", plot = maps_plot, width = 12, height = 13)



### Estimator heatmap ----
heatmap_plot <- openxlsx::read.xlsx("results/panel_spec_13_1_log.xlsx")

heatmap_plot <- subset(heatmap_plot, variable == "nr_dis_lag_0.5")

heatmap_plot$data_series <- factor(heatmap_plot$data_series, levels = c("all_homes_bottom_tier", "all_homes_middle_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))
heatmap_plot$p_value_plot <- factor(ifelse(heatmap_plot$p_value < 0.01, "***", ifelse(heatmap_plot$p_value < 0.05, "**", ifelse(heatmap_plot$p_value < 0.1, "*", " "))), levels = c("***", "**", "*", " "))

p_heatmap <- heatmap_plot |> 
  ggplot(aes(x = data_series, y = incident_type, fill = estimate)) +
  geom_tile() +
  geom_text(aes(x = data_series, y = incident_type, label = p_value_plot)) +
  scale_fill_gradient2(name = "Estimates of nr_dis_lag_0.5", low = "#f64931", mid = "white", high = "#4996ab", midpoint = 0, na.value = "grey50") +
  # scale_discrete_manual(name = "Sign.", aesthetics = "label", values = c("p < 0.01" = "***", "p < 0.05" = "**", "p < 0.1" = "*", " " = " ")) +
  annotate("text", x = 11, y = 13, label = "log(zhvi) ~ nr_dis_lag_0.5\n+ unemployment_rate\n+ log(avg_wkly_wage)\n+ log(gdp_value)\n+ D[year]", parse = FALSE, hjust = 0) +
  coord_cartesian(xlim = c(1, 10), ylim = c(1, 15), clip = "off") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave(filename = "plots/heatmap spec_13_1_log.pdf", plot = p_heatmap, width = 10, height = 10)


# Dummy spec
heatmap_plot <- openxlsx::read.xlsx("results/panel_spec_13_2_log.xlsx")

heatmap_plot <- subset(heatmap_plot, variable == "nr_dis_lag_0.5")

heatmap_plot$data_series <- factor(heatmap_plot$data_series, levels = c("all_homes_bottom_tier", "all_homes_middle_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))
heatmap_plot$p_value_plot <- factor(ifelse(heatmap_plot$p_value < 0.01, "***", ifelse(heatmap_plot$p_value < 0.05, "**", ifelse(heatmap_plot$p_value < 0.1, "*", " "))), levels = c("***", "**", "*", " "))

p_heatmap <- heatmap_plot |> 
  ggplot(aes(x = data_series, y = incident_type, fill = estimate)) +
  geom_tile() +
  geom_text(aes(x = data_series, y = incident_type, label = p_value_plot)) +
  scale_fill_gradient2(name = "Estimates of dummy_dis_lag_0.5", low = "#f64931", mid = "white", high = "#4996ab", midpoint = 0, na.value = "grey50") +
  # scale_discrete_manual(name = "Sign.", aesthetics = "label", values = c("p < 0.01" = "***", "p < 0.05" = "**", "p < 0.1" = "*", " " = " ")) +
  annotate("text", x = 11, y = 13, label = "log(zhvi) ~ dummy_dis_lag_0.5\n+ unemployment_rate\n+ log(avg_wkly_wage)\n+ log(gdp_value)\n+ D[year]", parse = FALSE, hjust = 0) +
  coord_cartesian(xlim = c(1, 10), ylim = c(1, 15), clip = "off") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave(filename = "plots/heatmap spec_13_2_log.pdf", plot = p_heatmap, width = 10, height = 10)


# Cost spec
heatmap_plot <- openxlsx::read.xlsx("results/panel_spec_13_3_log.xlsx")

heatmap_plot <- subset(heatmap_plot, variable == "dis_lag_0.5")
heatmap_plot <- subset(heatmap_plot, incident_type != "Other")

heatmap_plot$data_series <- factor(heatmap_plot$data_series, levels = c("all_homes_bottom_tier", "all_homes_middle_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))
heatmap_plot$p_value_plot <- factor(ifelse(heatmap_plot$p_value < 0.01, "***", ifelse(heatmap_plot$p_value < 0.05, "**", ifelse(heatmap_plot$p_value < 0.1, "*", " "))), levels = c("***", "**", "*", " "))

p_heatmap <- heatmap_plot |> 
  ggplot(aes(x = data_series, y = incident_type, fill = estimate)) +
  geom_tile() +
  geom_text(aes(x = data_series, y = incident_type, label = p_value_plot)) +
  scale_fill_gradient2(name = "Estimates of cost_dis_lag_0.5", low = "#f64931", mid = "white", high = "#4996ab", midpoint = 0, na.value = "grey50") +
  # scale_discrete_manual(name = "Sign.", aesthetics = "label", values = c("p < 0.01" = "***", "p < 0.05" = "**", "p < 0.1" = "*", " " = " ")) +
  annotate("text", x = 11, y = 13, label = "log(zhvi) ~ cost_dis_lag_0.5\n+ unemployment_rate\n+ log(avg_wkly_wage)\n+ log(gdp_value)\n+ D[year]", parse = FALSE, hjust = 0) +
  coord_cartesian(xlim = c(1, 10), ylim = c(1, 15), clip = "off") +
  facet_wrap(~assistance_type) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave(filename = "plots/heatmap spec_13_3_log.pdf", plot = p_heatmap, width = 30, height = 10)


# First difference
heatmap_plot <- openxlsx::read.xlsx("results/panel_spec_14_log.xlsx")

heatmap_plot <- subset(heatmap_plot, variable == "nr_dis_lag_999")

heatmap_plot$data_series <- factor(heatmap_plot$data_series, levels = c("all_homes_bottom_tier", "all_homes_middle_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))
heatmap_plot$p_value_plot <- factor(ifelse(heatmap_plot$p_value < 0.01, "***", ifelse(heatmap_plot$p_value < 0.05, "**", ifelse(heatmap_plot$p_value < 0.1, "*", " "))), levels = c("***", "**", "*", " "))

p_heatmap <- heatmap_plot |> 
  ggplot(aes(x = data_series, y = incident_type, fill = estimate)) +
  geom_tile() +
  geom_text(aes(x = data_series, y = incident_type, label = p_value_plot)) +
  scale_fill_gradient2(name = "Estimates of nr_dis_lag_999", low = "#f64931", mid = "white", high = "#4996ab", midpoint = 0, na.value = "grey50") +
  # scale_discrete_manual(name = "Sign.", aesthetics = "label", values = c("p < 0.01" = "***", "p < 0.05" = "**", "p < 0.1" = "*", " " = " ")) +
  annotate("text", x = 11, y = 13, label = "log(zhvi) ~ nr_dis_lag_999\n+ unemployment_rate\n+ log(avg_wkly_wage)", parse = FALSE, hjust = 0) +
  coord_cartesian(xlim = c(1, 10), ylim = c(1, 15), clip = "off") +
  facet_wrap(~time_period, nrow = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        legend.position = "right")

ggsave(filename = "plots/heatmap spec_14_1_log.pdf", plot = p_heatmap, width = 30, height = 20)
