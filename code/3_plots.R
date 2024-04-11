rm(list = ls())

library(tidyverse)


### Descriptive stats ----
load("data/zillow_county.RData")

# ZHVI median by data series
zhvi_by_data_series_line <- aggregate(zhvi ~ data_series + lubridate::year(date), zillow_county, median, na.rm = T) |>
  select(data_series, "year" = `lubridate::year(date)`, zhvi) |>
  mutate(data_series = factor(data_series, levels = c("all_homes_bottom_tier", "all_homes_top_tier", "single_family_homes", "condo_coop", "one_bedroom", "two_bedroom", "three_bedroom", "four_bedroom", "five_plus_bedroom"))) |> 
  ggplot(aes(x = year, y = zhvi, linetype = data_series)) +
  geom_line() +
  geom_text(data = aggregate(zhvi ~ data_series + lubridate::year(date), zillow_county, median, na.rm = T) |> select(data_series, "year" = `lubridate::year(date)`, zhvi) |> filter(year == last(year)),
            aes(label = plyr::revalue(data_series, c("all_homes_bottom_tier" = "Bottom Tier",
                                                     "all_homes_top_tier" = "Top Tier",
                                                     "single_family_homes" = "Single Family",
                                                     "condo_coop" = "Condo / Co-Op",
                                                     "one_bedroom" = "1 Bedroom",
                                                     "two_bedroom" = "2 Bedrooms",
                                                     "three_bedroom" = "3 Bedrooms",
                                                     "four_bedroom" = "4 Bedrooms",
                                                     "five_plus_bedroom" = "5+ Bedrooms")),
                x = year, y = zhvi),
            hjust = 0.6, vjust = -0.5) +
  scale_x_continuous(limits = c(2000, 2024), breaks = seq(2000, 2024, 2)) +
  scale_y_continuous(labels = function(x){format(x, big.mark = "'", scientific = FALSE)}, name = "Zillow Home Value Index") +
  scale_linetype(name = "Data Series", labels = c("Bottom Tier", "Top Tier", "Single Family", "Condo / Co-Op", "1 Bedroom", "2 Bedrooms", "3 Bedrooms", "4 Bedrooms", "5+ Bedrooms")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "points"),
        panel.spacing = margin(1, 80, 1, 1, "points"))

ggsave(filename = "plots/zhvi_by_data_series_line.pdf", plot = zhvi_by_data_series_line, width = 12, height = 9)
