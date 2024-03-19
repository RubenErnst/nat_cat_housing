rm(list = ls())

library(tidyverse)

### Load Zillow data
zillow <- read_delim("data/Metro_median_sale_price_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "median_sales_price")

add_on <- read_delim("data/Metro_mlp_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "median_list_price") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_mean_sale_to_list_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "mean_sale_to_list") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_median_sale_to_list_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "median_sale_to_list") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_pct_sold_above_list_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "pct_sold_above_list") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_pct_sold_below_list_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "pct_sold_below_list") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_invt_fs_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "inventory_for_sale") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_new_listings_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "new_listings") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_new_pending_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "new_pending_listings") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_mean_doz_pending_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "mean_doz_pending") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_med_doz_pending_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "median_doz_pending") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_mean_days_to_close_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "mean_days_to_close") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_median_days_to_close_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "median_days_to_close") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_perc_listings_price_cut_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "pct_listings_price_cut") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_mean_listings_price_cut_amt_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "mean_listings_price_cut") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)

add_on <- read_delim("data/Metro_med_listings_price_cut_amt_uc_sfrcondo_week.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName), names_to = "date", values_to = "median_listings_price_cut") |> 
  select(-SizeRank, -RegionName, -RegionType, -StateName)

zillow <- merge(zillow, add_on, by = c("RegionID", "date"), all = T)


### Clean Zillow data
names(zillow) <- tolower(names(zillow))
zillow <- subset(zillow, regionname != "United States")
zillow$date <- as.Date(zillow$date, format = "%Y-%m-%d")
names(zillow) <- c("region_id", "date", "size_rank", "region_name", "region_type", "state_name", names(zillow)[7:length(names(zillow))])


### Load FEMA data
fema_areas <- read_delim("data/FemaWebDeclarationAreas.csv")
fema_declarations <- read_delim("data/FemaWebDisasterDeclarations.csv")
fema_summaries <- read_delim("data/FemaWebDisasterSummaries.csv")


### Make Zillow / FEMA merge table for regions
region_merge <- unique(select(zillow, region_name, state_name))
names(region_merge) <- c("zillow_region_name", "zillow_state_name")
region_merge$zillow_region_name_no_state <- sapply(region_merge$zillow_region_name, function(x){str_remove(x, ",.*")})

fema_temp <- unique(select(fema_areas, "fema_region_name_no_state" = placeName, "fema_state_name" = stateCode))
fema_temp$fema_region_name <- paste0(sapply(fema_temp$fema_region_name_no_state, function(x){str_remove(x, " \\(.*\\)")}), ", ", fema_temp$fema_state_name)

region_merge <- merge(region_merge, fema_temp, by.x = "zillow_region_name", by.y = "fema_region_name", all.x = T)

# Save for manual match
openxlsx::write.xlsx(region_merge, file = "region_merge.xlsx")

# Load completed match table
region_merge <- openxlsx::read.xlsx("region_merge.xlsx")

# Add statewide declarations
region_merge <- rbind(region_merge,
                      merge(unique(select(region_merge, zillow_region_name, zillow_state_name, zillow_region_name_no_state)),
                            unique(select(fema_areas[grepl("statewide", fema_areas$placeName, ignore.case = T),], "fema_region_name_no_state" = placeName, "fema_state_name" = stateCode)),
                            by.x = "zillow_state_name", by.y = "fema_state_name", all.x = T) |> 
                        subset(!is.na(fema_region_name_no_state)) |> 
                        mutate("fema_state_name" = zillow_state_name))

### Merge Zillow and FEMA
# Merge areas for disaster ID
z_fema_merged <- merge(select(region_merge, zillow_region_name, fema_region_name_no_state, fema_state_name),
                       select(fema_areas, "disaster_number" = disasterNumber, "fema_state_name" = stateCode, "fema_region_name_no_state" = placeName, "designated_date" = designatedDate, "closeout_date" = closeoutDate),
                       by = c("fema_state_name", "fema_region_name_no_state"), all.x = T)

z_fema_merged <- merge(select(z_fema_merged, -fema_state_name, -fema_region_name_no_state),
                       zillow,
                       by.x = "zillow_region_name", by.y = "region_name", all.x = T)

# Save raw cross product for backup
save(z_fema_merged, file = "Zillow_FEMA_merged_raw.RData")

# Filter for disasters declared within 5 years of price date
z_fema_working <- subset(z_fema_merged,
                         designated_date + lubridate::years(5) >= date)


### Preliminary linear regression
# Remove empty / unneeded Zillow metadata variables
z_fema_working <- select(z_fema_working, zillow_region_name, disaster_number, designated_date, closeout_date, region_id, date, size_rank, region_type, state_name, median_sales_price)
z_fema_working <- merge(z_fema_working,
                        select(fema_summaries, -totalNumberIaApproved, -paLoadDate, -iaLoadDate, -totalObligatedAmountHmgp, -hash, -lastRefresh, -id),
                        by.x = "disaster_number", by.y = "disasterNumber", all.x = T)

# Calculate total assistance approved
z_fema_working$total_amount_approved <- rowSums(z_fema_working[,c("totalAmountIhpApproved", "totalAmountHaApproved", "totalAmountOnaApproved", "totalObligatedAmountPa", "totalObligatedAmountCatAb",
                                                                  "totalObligatedAmountCatC2g")], na.rm=TRUE)

z_fema_working <- select(z_fema_working, -totalAmountIhpApproved, -totalAmountHaApproved, -totalAmountOnaApproved, -totalObligatedAmountPa, -totalObligatedAmountCatAb, -totalObligatedAmountCatC2g)

# Calculate number of disasters and total assistance amount in previous 5 years
z_fema_regression <- aggregate(disaster_number ~ zillow_region_name + date, z_fema_working, function(x){length(unique(x))})
z_fema_regression <- merge(z_fema_regression,
                           aggregate(total_amount_approved ~ zillow_region_name + date, z_fema_working, sum, na.rm = T),
                           by = c("zillow_region_name", "date"), all.x = T)