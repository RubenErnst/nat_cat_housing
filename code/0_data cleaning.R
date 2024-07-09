rm(list = ls())

library(tidyverse)
library(DBI)
library(RSQLite)

### Load Zillow data ----
zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_uc_sfrcondo_tier_0.0_0.33_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "all_homes_bottom_tier")

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "all_homes_middle_tier") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_uc_sfrcondo_tier_0.67_1.0_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "all_homes_top_tier") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "single_family_homes") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_uc_condo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "condo_coop") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "one_bedroom") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "two_bedroom") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "three_bedroom") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "four_bedroom") |> 
  rbind(zillow_county)

zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "five_plus_bedroom") |> 
  rbind(zillow_county)

# Save to compressed binary
zillow_county$date <- as.Date(zillow_county$date, format = "%Y-%m-%d")
names(zillow_county) <- c("region_id", "size_rank", "region_name", "region_type", "state_name", "state", "metro", "state_code_fips", "municipal_code_fips", "date", "zhvi", "data_series")
save(zillow_county, file = "data/zillow_county.RData")

# Neighborhood level data
zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "all_homes_smoothed")

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_uc_sfr_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "single_family_homes") |> 
  rbind(zillow_neighborhood)

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_uc_condo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "condo_coop") |> 
  rbind(zillow_neighborhood)

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "one_bedroom") |> 
  rbind(zillow_neighborhood)

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "two_bedroom") |> 
  rbind(zillow_neighborhood)

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "three_bedroom") |> 
  rbind(zillow_neighborhood)

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "four_bedroom") |> 
  rbind(zillow_neighborhood)

zillow_neighborhood <- read_delim("../../3_Data/Zillow/Neighborhood_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -City, -Metro, -CountyName), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "five_plus_bedroom") |> 
  rbind(zillow_neighborhood)

# Save to compressed binary
zillow_neighborhood$date <- as.Date(zillow_neighborhood$date, format = "%Y-%m-%d")
names(zillow_neighborhood) <- c("region_id", "size_rank", "region_name", "region_type", "state_name", "state", "city", "metro", "county_name", "date", "zhvi", "data_series")
save(zillow_neighborhood, file = "data/zillow_neighborhood.RData")



### Load FEMA data ----
fema_areas <- read_delim("../../3_Data/FEMA_Disasters/FemaWebDeclarationAreas.csv")
names(fema_areas) <- c("id", "disaster_number", "program_type_code", "program_type_description", "state_code", "state_name", "place_code", "place_name", "date_designated", "date_entry_area", "date_update_area", "date_closeout_area", "hash", "last_refresh")
fema_areas$date_designated <- as.Date(as.character(fema_areas$date_designated))
fema_areas$date_entry_area <- as.Date(as.character(fema_areas$date_entry_area))
fema_areas$date_update_area <- as.Date(as.character(fema_areas$date_update_area))
fema_areas$date_closeout_area <- as.Date(as.character(fema_areas$date_closeout_area))

fema_declarations <- read_delim("../../3_Data/FEMA_Disasters/FemaWebDisasterDeclarations.csv")
names(fema_declarations) <- c("disaster_number", "date_declaration", "disaster_name", "date_incident_begin", "date_incident_end", "declaration_type", "state_code", "state_name", "incident_type", "date_entry_declaration", "date_update_declaration", "date_closeout_declaration", "region", "program_declared_ih", "program_declared_ia", "program_declared_pa", "program_declared_hm", "id", "hash", "last_refresh")
fema_declarations$date_declaration <- as.Date(as.character(fema_declarations$date_declaration))
fema_declarations$date_incident_begin <- as.Date(as.character(fema_declarations$date_incident_begin))
fema_declarations$date_incident_end <- as.Date(as.character(fema_declarations$date_incident_end))
fema_declarations$date_entry_declaration <- as.Date(as.character(fema_declarations$date_entry_declaration))
fema_declarations$date_update_declaration <- as.Date(as.character(fema_declarations$date_update_declaration))
fema_declarations$date_closeout_declaration <- as.Date(as.character(fema_declarations$date_closeout_declaration))

fema_summaries <- read_delim("../../3_Data/FEMA_Disasters/FemaWebDisasterSummaries.csv", col_types = cols(hash = col_character(), id = col_character(), disasterNumber = col_double(), totalObligatedAmountPa = col_double(), totalObligatedAmountCatAb = col_double(), totalObligatedAmountCatC2g = col_double(), totalObligatedAmountHmgp = col_double(), totalNumberIaApproved = col_double(), totalAmountIhpApproved = col_double(), totalAmountHaApproved = col_double(), totalAmountOnaApproved = col_double(), iaLoadDate = col_datetime(), paLoadDate = col_datetime(), lastRefresh = col_datetime()))
names(fema_summaries) <- c("disaster_number", "number_ia_approved", "amount_ihp_approved", "amount_ha_approved", "amount_ona_approved", "amount_pa_obligated", "amount_catab_obligated", "amount_catc2g_obligated", "date_pa_load", "date_ia_load", "amount_hmgp_obligated", "hash", "last_refresh", "id")
fema_summaries$date_ia_load <- as.Date(as.character(fema_summaries$date_ia_load))
fema_summaries$date_pa_load <- as.Date(as.character(fema_summaries$date_pa_load))

fema <- merge(select(fema_areas, -id, -hash, -last_refresh), select(fema_declarations, -state_name, -id, -hash, -last_refresh), by = c("disaster_number", "state_code"), all.x = TRUE)
fema <- merge(fema, select(fema_summaries, -id, -hash, -last_refresh), by = "disaster_number", all.x = TRUE)

# Fix FIPS codes
source("code/1_data merging.R")
fema <- merge(fema, select(fips_states, state_abbr, state_code), by.x = "state_code", by.y = "state_abbr", all.x = TRUE)
fema <- subset(fema, nchar(place_code) == 5) # Remove special educational areas and tribal zones
fema$temp_county_code <- sapply(fema$place_code, function(x){substr(as.character(x), max(nchar(as.character(x)) - 3 + 1, 1), nchar(as.character(x)))})

fema$place_code <- fips_pad(fema$state_code.y, fema$temp_county_code)
fema$state_code.y <- NULL
fema$temp_county_code <- NULL

# Save to compressed binary
save(fema, file = "data/fema.RData")



### Load HMDA data ----
hmda_db <- dbConnect(drv = RSQLite::SQLite(), "data/hmda_db.sqlite")

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2007_nationwide_all.RData")

# Make agency table, add values
hmda_agencies <- unique(select(hmda_lar_2007, agency_code, agency_abbr, agency_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE agencies (agency_code INT, agency_abbr VARCHAR(5), agency_name TEXT, PRIMARY KEY (agency_code));")
)

for (i in 1:nrow(hmda_agencies)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO agencies VALUES (", hmda_agencies$agency_code[i], ", '",
                                hmda_agencies$agency_abbr[i], "', '",
                                hmda_agencies$agency_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_agencies)))
}

hmda_lar_2007$agency_abbr <- NULL
hmda_lar_2007$agency_name <- NULL
rm(hmda_agencies)


# Make loan_types table, add values
hmda_loan_types <- unique(select(hmda_lar_2007, loan_type, loan_type_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE loan_types (loan_type INT, loan_type_name TEXT, PRIMARY KEY (loan_type));")
)

for (i in 1:nrow(hmda_loan_types)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO loan_types VALUES (", hmda_loan_types$loan_type[i], ", '",
                                hmda_loan_types$loan_type_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_loan_types)))
}

hmda_lar_2007$loan_type_name <- NULL
rm(hmda_loan_types)


# Make property_types table, add values
hmda_property_types <- unique(select(hmda_lar_2007, property_type, property_type_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE property_types (property_type INT, property_type_name TEXT, PRIMARY KEY (property_type));")
)

for (i in 1:nrow(hmda_property_types)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO property_types VALUES (", hmda_property_types$property_type[i], ", '",
                                hmda_property_types$property_type_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_property_types)))
}

hmda_lar_2007$property_type_name <- NULL
rm(hmda_property_types)


# Make loan_purposes table, add values
hmda_loan_purposes <- unique(select(hmda_lar_2007, loan_purpose, loan_purpose_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE loan_purposes (loan_purpose INT, loan_purpose_name TEXT, PRIMARY KEY (loan_purpose));")
)

for (i in 1:nrow(hmda_loan_purposes)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO loan_purposes VALUES (", hmda_loan_purposes$loan_purpose[i], ", '",
                                hmda_loan_purposes$loan_purpose_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_loan_purposes)))
}

hmda_lar_2007$loan_purpose_name <- NULL
rm(hmda_loan_purposes)


# Make owner_occupancies table, add values
hmda_owner_occupancies <- unique(select(hmda_lar_2007, owner_occupancy, owner_occupancy_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE owner_occupancies (owner_occupancy INT, owner_occupancy_name TEXT, PRIMARY KEY (owner_occupancy));")
)

for (i in 1:nrow(hmda_owner_occupancies)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO owner_occupancies VALUES (", hmda_owner_occupancies$owner_occupancy[i], ", '",
                                hmda_owner_occupancies$owner_occupancy_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_owner_occupancies)))
}

hmda_lar_2007$owner_occupancy_name <- NULL
rm(hmda_owner_occupancies)


# Make preapprovals table, add values
hmda_preapprovals <- unique(select(hmda_lar_2007, preapproval, preapproval_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE preapprovals (preapproval INT, preapproval_name TEXT, PRIMARY KEY (preapproval));")
)

for (i in 1:nrow(hmda_preapprovals)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO preapprovals VALUES (", hmda_preapprovals$preapproval[i], ", '",
                                hmda_preapprovals$preapproval_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_preapprovals)))
}

hmda_lar_2007$preapproval_name <- NULL
rm(hmda_preapprovals)


# Make actions_taken table, add values
hmda_actions_taken <- unique(select(hmda_lar_2007, action_taken, action_taken_name))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE actions_taken (action_taken INT, action_taken_name TEXT, PRIMARY KEY (action_taken));")
)

for (i in 1:nrow(hmda_actions_taken)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO actions_taken VALUES (", hmda_actions_taken$action_taken[i], ", '",
                                hmda_actions_taken$action_taken_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_actions_taken)))
}

hmda_lar_2007$action_taken_name <- NULL
rm(hmda_actions_taken)


# Make msamds table, add values
hmda_msamds <- subset(unique(select(hmda_lar_2007, msamd, msamd_name)), !is.na(msamd))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE msamds (msamd INT, msamd_name TEXT, PRIMARY KEY (msamd));")
)

hmda_msamds$msamd_name <- str_replace_all(hmda_msamds$msamd_name, "'", "`")

for (i in 1:nrow(hmda_msamds)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO msamds VALUES (", hmda_msamds$msamd[i], ", '",
                                hmda_msamds$msamd_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_msamds)))
}

hmda_lar_2007$msamd_name <- NULL
rm(hmda_msamds)


# Make states table, add values
hmda_states <- subset(unique(select(hmda_lar_2007, state_code, state_abbr, state_name)), !is.na(state_code))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE states (state_code INT, state_abbr VARCHAR(2), state_name TEXT, PRIMARY KEY (state_code));")
)

for (i in 1:nrow(hmda_states)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO states VALUES (", hmda_states$state_code[i], ", '",
                                hmda_states$state_abbr[i], "', '",
                                hmda_states$state_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_states)))
}

hmda_lar_2007$state_abbr <- NULL
hmda_lar_2007$state_name <- NULL
rm(hmda_states)


# Make counties table, add values
hmda_counties <- subset(unique(select(hmda_lar_2007, state_code, county_code, county_name)), !is.na(county_code) & !is.na(state_code))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE counties (state_code INT, county_code INT, county_name TEXT, PRIMARY KEY (state_code, county_code), FOREIGN KEY (state_code) REFERENCES states(state_code));")
)

hmda_counties$county_name <- str_replace_all(hmda_counties$county_name, "'", "`")

for (i in 1:nrow(hmda_counties)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO counties VALUES (", hmda_counties$state_code[i], ", ",
                                hmda_counties$county_code[i], ", '",
                                hmda_counties$county_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_counties)))
}

hmda_lar_2007$county_name <- NULL
rm(hmda_counties)


# Make ethnicities table, add values
hmda_ethnicities <- subset(unique(rbind(select(hmda_lar_2007, "ethnicity" = applicant_ethnicity, "ethnicity_name" = applicant_ethnicity_name),
                                        select(hmda_lar_2007, "ethnicity" = co_applicant_ethnicity, "ethnicity_name" = co_applicant_ethnicity_name))), !is.na(ethnicity))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE ethnicities (ethnicity INT, ethnicity_name TEXT, PRIMARY KEY (ethnicity));")
)

for (i in 1:nrow(hmda_ethnicities)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO ethnicities VALUES (", hmda_ethnicities$ethnicity[i], ", '",
                                hmda_ethnicities$ethnicity_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_ethnicities)))
}

hmda_lar_2007$applicant_ethnicity_name <- NULL
hmda_lar_2007$co_applicant_ethnicity_name <- NULL
rm(hmda_ethnicities)


# Make races table, add values
hmda_races <- subset(unique(rbind(select(hmda_lar_2007, "race" = applicant_race_1, "race_name" = applicant_race_name_1),
                                  select(hmda_lar_2007, "race" = applicant_race_2, "race_name" = applicant_race_name_2),
                                  select(hmda_lar_2007, "race" = applicant_race_3, "race_name" = applicant_race_name_3),
                                  select(hmda_lar_2007, "race" = applicant_race_4, "race_name" = applicant_race_name_4),
                                  select(hmda_lar_2007, "race" = applicant_race_5, "race_name" = applicant_race_name_5),
                                  select(hmda_lar_2007, "race" = co_applicant_race_1, "race_name" = co_applicant_race_name_1),
                                  select(hmda_lar_2007, "race" = co_applicant_race_2, "race_name" = co_applicant_race_name_2),
                                  select(hmda_lar_2007, "race" = co_applicant_race_3, "race_name" = co_applicant_race_name_3),
                                  select(hmda_lar_2007, "race" = co_applicant_race_4, "race_name" = co_applicant_race_name_4),
                                  select(hmda_lar_2007, "race" = co_applicant_race_5, "race_name" = co_applicant_race_name_5))), !is.na(race))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE races (race INT, race_name TEXT, PRIMARY KEY (race));")
)

for (i in 1:nrow(hmda_races)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO races VALUES (", hmda_races$race[i], ", '",
                                hmda_races$race_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_races)))
}

hmda_lar_2007$applicant_race_name_1 <- NULL
hmda_lar_2007$applicant_race_name_2 <- NULL
hmda_lar_2007$applicant_race_name_3 <- NULL
hmda_lar_2007$applicant_race_name_4 <- NULL
hmda_lar_2007$applicant_race_name_5 <- NULL
hmda_lar_2007$co_applicant_race_name_1 <- NULL
hmda_lar_2007$co_applicant_race_name_2 <- NULL
hmda_lar_2007$co_applicant_race_name_3 <- NULL
hmda_lar_2007$co_applicant_race_name_4 <- NULL
hmda_lar_2007$co_applicant_race_name_5 <- NULL
rm(hmda_races)
gc()


# Make sexes table, add values
hmda_sexes <- subset(unique(rbind(select(hmda_lar_2007, "sex" = applicant_sex, "sex_name" = applicant_sex_name),
                                  select(hmda_lar_2007, "sex" = co_applicant_sex, "sex_name" = co_applicant_sex_name))), !is.na(sex))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE sexes (sex INT, sex_name TEXT, PRIMARY KEY (sex));")
)

for (i in 1:nrow(hmda_sexes)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO sexes VALUES (", hmda_sexes$sex[i], ", '",
                                hmda_sexes$sex_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_sexes)))
}

hmda_lar_2007$applicant_sex_name <- NULL
hmda_lar_2007$co_applicant_sex_name <- NULL
rm(hmda_sexes)
gc()


# Make purchaser_types table, add values
hmda_purchaser_types <- subset(unique(select(hmda_lar_2007, "purchaser_type" = purchaser_type, "purchaser_type_name" = purchaser_type_name)), !is.na(purchaser_type))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE purchaser_types (purchaser_type INT, purchaser_type_name TEXT, PRIMARY KEY (purchaser_type));")
)

for (i in 1:nrow(hmda_purchaser_types)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO purchaser_types VALUES (", hmda_purchaser_types$purchaser_type[i], ", '",
                                hmda_purchaser_types$purchaser_type_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_purchaser_types)))
}

hmda_lar_2007$purchaser_type_name <- NULL
rm(hmda_purchaser_types)
gc()


# Make denial_reasons table, add values
hmda_denial_reasons <- subset(unique(rbind(select(hmda_lar_2007, "denial_reason" = denial_reason_1, "denial_reason_name" = denial_reason_name_1),
                                           select(hmda_lar_2007, "denial_reason" = denial_reason_2, "denial_reason_name" = denial_reason_name_2),
                                           select(hmda_lar_2007, "denial_reason" = denial_reason_3, "denial_reason_name" = denial_reason_name_3))), !is.na(denial_reason))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE denial_reasons (denial_reason INT, denial_reason_name TEXT, PRIMARY KEY (denial_reason));")
)

for (i in 1:nrow(hmda_denial_reasons)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO denial_reasons VALUES (", hmda_denial_reasons$denial_reason[i], ", '",
                                hmda_denial_reasons$denial_reason_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_denial_reasons)))
}

hmda_lar_2007$denial_reason_name_1 <- NULL
hmda_lar_2007$denial_reason_name_2 <- NULL
hmda_lar_2007$denial_reason_name_3 <- NULL
rm(hmda_denial_reasons)
gc()


# Make hoepa_statuses table, add values
hmda_hoepa_statuses <- subset(unique(select(hmda_lar_2007, "hoepa_status" = hoepa_status, "hoepa_status_name" = hoepa_status_name)), !is.na(hoepa_status))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE hoepa_statuses (hoepa_status INT, hoepa_status_name TEXT, PRIMARY KEY (hoepa_status));")
)

for (i in 1:nrow(hmda_hoepa_statuses)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO hoepa_statuses VALUES (", hmda_hoepa_statuses$hoepa_status[i], ", '",
                                hmda_hoepa_statuses$hoepa_status_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_hoepa_statuses)))
}

hmda_lar_2007$hoepa_status_name <- NULL
rm(hmda_hoepa_statuses)
gc()


# Make lien_statuses table, add values
hmda_lien_statuses <- subset(unique(select(hmda_lar_2007, "lien_status" = lien_status, "lien_status_name" = lien_status_name)), !is.na(lien_status))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE lien_statuses (lien_status INT, lien_status_name TEXT, PRIMARY KEY (lien_status));")
)

for (i in 1:nrow(hmda_lien_statuses)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO lien_statuses VALUES (", hmda_lien_statuses$lien_status[i], ", '",
                                hmda_lien_statuses$lien_status_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_lien_statuses)))
}

hmda_lar_2007$lien_status_name <- NULL
rm(hmda_lien_statuses)
gc()


# Make edit_statuses table, add values
hmda_edit_statuses <- subset(unique(select(hmda_lar_2007, "edit_status" = edit_status, "edit_status_name" = edit_status_name)), !is.na(edit_status))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE edit_statuses (edit_status INT, edit_status_name TEXT, PRIMARY KEY (edit_status));")
)

for (i in 1:nrow(hmda_edit_statuses)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO edit_statuses VALUES (", hmda_edit_statuses$edit_status[i], ", '",
                                hmda_edit_statuses$edit_status_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_edit_statuses)))
}

hmda_lar_2007$edit_status_name <- NULL
rm(hmda_edit_statuses)
gc()


# Make application_date_indicators table, add values
hmda_application_date_indicators <- subset(unique(select(hmda_lar_2007, "application_date_indicator" = application_date_indicator)), !is.na(application_date_indicator))
hmda_application_date_indicators$application_date_indicator_name <- c("Application Date >= 01-01-2004", "Application Date = NA (Not Available)", "Application Date < 01-01-2004")

dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE application_date_indicators (application_date_indicator INT, application_date_indicator_name TEXT, PRIMARY KEY (application_date_indicator));")
)

for (i in 1:nrow(hmda_application_date_indicators)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO application_date_indicators VALUES (", hmda_application_date_indicators$application_date_indicator[i], ", '",
                                hmda_application_date_indicators$application_date_indicator_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_application_date_indicators)))
}

rm(hmda_application_date_indicators)
gc()


# Make reverse_mortgages table, add values
hmda_reverse_mortgages <- data.frame("reverse_mortgage" = c(1, 2, 1111, -1),
                                     "reverse_mortgage_name" = c("Reverse mortgage", "Not a reverse mortgage", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE reverse_mortgages (reverse_mortgage INT, reverse_mortgage_name TEXT, PRIMARY KEY (reverse_mortgage));")
)

for (i in 1:nrow(hmda_reverse_mortgages)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO reverse_mortgages VALUES (", hmda_reverse_mortgages$reverse_mortgage[i], ", '",
                                hmda_reverse_mortgages$reverse_mortgage_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_reverse_mortgages)))
}

rm(hmda_reverse_mortgages)
gc()


# Make open_end_locs table, add values
hmda_open_end_locs <- data.frame("open_end_line_of_credit" = c(1, 2, 1111, -1),
                                 "open_end_line_of_credit_name" = c("Open-end line of credit", "Not an open-end line of credit", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE open_end_locs (open_end_line_of_credit INT, open_end_line_of_credit_name TEXT, PRIMARY KEY (open_end_line_of_credit));")
)

for (i in 1:nrow(hmda_open_end_locs)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO open_end_locs VALUES (", hmda_open_end_locs$open_end_line_of_credit[i], ", '",
                                hmda_open_end_locs$open_end_line_of_credit_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_open_end_locs)))
}

rm(hmda_open_end_locs)
gc()


# Make business_or_commercial_purposes table, add values
hmda_business_or_commercial_purposes <- data.frame("business_or_commercial_purpose" = c(1, 2, 1111, -1),
                                                   "business_or_commercial_purpose_name" = c("Primarily for a business or commercial purpose", "Not primarily for a business or commercial purpose", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE business_or_commercial_purposes (business_or_commercial_purpose INT, business_or_commercial_purpose_name TEXT, PRIMARY KEY (business_or_commercial_purpose));")
)

for (i in 1:nrow(hmda_business_or_commercial_purposes)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO business_or_commercial_purposes VALUES (", hmda_business_or_commercial_purposes$business_or_commercial_purpose[i], ", '",
                                hmda_business_or_commercial_purposes$business_or_commercial_purpose_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_business_or_commercial_purposes)))
}

rm(hmda_business_or_commercial_purposes)
gc()


# Make negative_amortizations table, add values
hmda_negative_amortizations <- data.frame("negative_amortization" = c(1, 2, 1111, -1),
                                          "negative_amortization_name" = c("Negative amortization", "No negative amortization", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE negative_amortizations (negative_amortization INT, negative_amortization_name TEXT, PRIMARY KEY (negative_amortization));")
)

for (i in 1:nrow(hmda_negative_amortizations)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO negative_amortizations VALUES (", hmda_negative_amortizations$negative_amortization[i], ", '",
                                hmda_negative_amortizations$negative_amortization_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_negative_amortizations)))
}

rm(hmda_negative_amortizations)
gc()


# Make interest_only_payments table, add values
hmda_interest_only_payments <- data.frame("interest_only_payment" = c(1, 2, 1111, -1),
                                          "interest_only_payment_name" = c("Interest-only payments", "No interest-only payments", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE interest_only_payments (interest_only_payment INT, interest_only_payment_name TEXT, PRIMARY KEY (interest_only_payment));")
)

for (i in 1:nrow(hmda_interest_only_payments)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO interest_only_payments VALUES (", hmda_interest_only_payments$interest_only_payment[i], ", '",
                                hmda_interest_only_payments$interest_only_payment_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_interest_only_payments)))
}

rm(hmda_interest_only_payments)
gc()


# Make balloon_payments table, add values
hmda_balloon_payments <- data.frame("balloon_payment" = c(1, 2, 1111, -1),
                                    "balloon_payment_name" = c("Balloon payment", "No balloon payment", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE balloon_payments (balloon_payment INT, balloon_payment_name TEXT, PRIMARY KEY (balloon_payment));")
)

for (i in 1:nrow(hmda_balloon_payments)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO balloon_payments VALUES (", hmda_balloon_payments$balloon_payment[i], ", '",
                                hmda_balloon_payments$balloon_payment_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_balloon_payments)))
}

rm(hmda_balloon_payments)
gc()


# Make other_nonamortizing_features table, add values
hmda_other_nonamortizing_features <- data.frame("other_nonamortizing_features" = c(1, 2, 1111, -1),
                                                "other_nonamortizing_features_name" = c("Other non-fully amortizing features", "No other non-fully amortizing features", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE other_nonamortizing_features (other_nonamortizing_features INT, other_nonamortizing_features_name TEXT, PRIMARY KEY (other_nonamortizing_features));")
)

for (i in 1:nrow(hmda_other_nonamortizing_features)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO other_nonamortizing_features VALUES (", hmda_other_nonamortizing_features$other_nonamortizing_features[i], ", '",
                                hmda_other_nonamortizing_features$other_nonamortizing_features_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_other_nonamortizing_features)))
}

rm(hmda_other_nonamortizing_features)
gc()


# Make construction_methods table, add values
hmda_construction_methods <- data.frame("construction_method" = c(1, 2, -1),
                                        "construction_method_name" = c("Site-built", "Manufactured home", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE construction_methods (construction_method INT, construction_method_name TEXT, PRIMARY KEY (construction_method));")
)

for (i in 1:nrow(hmda_construction_methods)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO construction_methods VALUES (", hmda_construction_methods$construction_method[i], ", '",
                                hmda_construction_methods$construction_method_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_construction_methods)))
}

rm(hmda_construction_methods)
gc()


# Make occupancy_types table, add values
hmda_occupancy_types <- data.frame("occupancy_type" = c(1, 2, 3, -1),
                                   "occupancy_type_name" = c("Principal residence", "Second residence", "Investment property", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE occupancy_types (occupancy_type INT, occupancy_type_name TEXT, PRIMARY KEY (occupancy_type));")
)

for (i in 1:nrow(hmda_occupancy_types)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO occupancy_types VALUES (", hmda_occupancy_types$occupancy_type[i], ", '",
                                hmda_occupancy_types$occupancy_type_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_occupancy_types)))
}

rm(hmda_occupancy_types)
gc()


# Make credit_score_types table, add values
hmda_credit_score_types <- data.frame("credit_score_type" = c(1:10, 1111, -1),
                                      "credit_score_type_name" = c("Equifax Beacon 5.0", "Experian Fair Isaac", "FICO Risk Score Classic 04", "FICO Risk Score Classic 98", "VantageScore 2.0", "VantageScore 3.0", "More than one credit scoring model", "Other credit scoring model", "Not applicable", "No co-applicant", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE credit_score_types (credit_score_type INT, credit_score_type_name TEXT, PRIMARY KEY (credit_score_type));")
)

for (i in 1:nrow(hmda_credit_score_types)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO credit_score_types VALUES (", hmda_credit_score_types$credit_score_type[i], ", '",
                                hmda_credit_score_types$credit_score_type_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_credit_score_types)))
}

rm(hmda_credit_score_types)
gc()


# Make submission_of_applications table, add values
hmda_submission_of_applications <- data.frame("submission_of_application" = c(1, 2, 3, 1111, -1),
                                              "submission_of_application_name" = c("Submitted directly to your institution", "Not submitted directly to your institution", "Not applicable", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE submission_of_applications (submission_of_application INT, submission_of_application_name TEXT, PRIMARY KEY (submission_of_application));")
)

for (i in 1:nrow(hmda_submission_of_applications)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO submission_of_applications VALUES (", hmda_submission_of_applications$submission_of_application[i], ", '",
                                hmda_submission_of_applications$submission_of_application_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_submission_of_applications)))
}

rm(hmda_submission_of_applications)
gc()


# Make initially_payable_to_institutions table, add values
hmda_initially_payable_to_institutions <- data.frame("initially_payable_to_institution" = c(1, 2, 3, 1111, -1),
                                                     "initially_payable_to_institution_name" = c("Initially payable to your institution", "Not initially payable to your institution", "Not applicable", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE initially_payable_to_institutions (initially_payable_to_institution INT, initially_payable_to_institution_name TEXT, PRIMARY KEY (initially_payable_to_institution));")
)

for (i in 1:nrow(hmda_initially_payable_to_institutions)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO initially_payable_to_institutions VALUES (", hmda_initially_payable_to_institutions$initially_payable_to_institution[i], ", '",
                                hmda_initially_payable_to_institutions$initially_payable_to_institution_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_initially_payable_to_institutions)))
}

rm(hmda_initially_payable_to_institutions)
gc()


# Make auss table, add values
hmda_auss <- data.frame("aus" = c(1:7, 1111, -1),
                        "aus_name" = c("Desktop Underwriter (DU)", "Loan Prospector (LP) or Loan Product Advisor", "Technology Open to Approved Lenders (TOTAL) Scorecard", "Guaranteed Underwriting System (GUS)", "Other", "Not applicable", "Internal Proprietary System", "Exempt", "N/A"))
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE auss (aus INT, aus_name TEXT, PRIMARY KEY (aus));")
)

for (i in 1:nrow(hmda_auss)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO auss VALUES (", hmda_auss$aus[i], ", '",
                                hmda_auss$aus_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_auss)))
}

rm(hmda_auss)
gc()





# Amend existing tables
dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO purchaser_types VALUES (71, 'Credit union, mortgage company, or finance company');")
)
dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO purchaser_types VALUES (72, 'Life insurance company');")
)

dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO loan_purposes VALUES (31, 'Refinancing');")
)
dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO loan_purposes VALUES (32, 'Cash-out refinancing');")
)
dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO loan_purposes VALUES (4, 'Other purpose');")
)
dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO loan_purposes VALUES (5, 'Not applicable');")
)

dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO hoepa_statuses VALUES (3, 'Not applicable');")
)

dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO hoepa_statuses VALUES (3, 'Not applicable');")
)

hmda_add_ethnicities <- data.frame("ethnicity" = c(11, 12, 13, 14),
                                   "ethnicity_name" = c("Mexican", "Puerto Rican", "Cuban", "Other Hispanic or Latino"))
for (i in 1:nrow(hmda_add_ethnicities)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO ethnicities VALUES (", hmda_add_ethnicities$ethnicity[i], ", '",
                                hmda_add_ethnicities$ethnicity_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_add_ethnicities)))
}
rm(hmda_add_ethnicities)

hmda_add_races <- data.frame("race" = c(21, 22, 23, 24, 25, 26, 27, 41, 42, 43, 44),
                             "race_name" = c("Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Guamanian or Chamorro", "Samoan", "Other Pacific Islander"))
for (i in 1:nrow(hmda_add_races)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO races VALUES (", hmda_add_races$race[i], ", '",
                                hmda_add_races$race_name[i], "');"))
  )
  print(paste0(i, "/", nrow(hmda_add_races)))
}
rm(hmda_add_races)

dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO sexes VALUES (6, '(Co-)Applicant selected both male and female')")
)

dbClearResult(
  dbSendQuery(hmda_db, "INSERT INTO denial_reasons VALUES (10, 'Not applicable')")
)



# Make main table, add values
dbClearResult(
  dbSendQuery(hmda_db, paste0("CREATE TABLE main (
                              as_of_year INT,
                              respondent_id VARCHAR(10),
                              agency_code INT,
                              conforming_loan_limit VARCHAR(2),
                              reverse_mortgage INT,
                              open_end_line_of_credit INT,
                              business_or_commercial_purpose INT,
                              loan_type INT,
                              property_type INT,
                              loan_purpose INT,
                              owner_occupancy INT,
                              loan_amount_000s INT,
                              loan_to_value_ratio NUMERIC,
                              interest_rate NUMERIC,
                              rate_spread NUMERIC,
                              total_loan_costs NUMERIC,
                              total_points_and_fees NUMERIC,
                              origination_charges NUMERIC,
                              discount_points NUMERIC,
                              lender_credits NUMERIC,
                              loan_term INT,
                              prepayment_penalty_term INT,
                              intro_rate_period INT,
                              negative_amortization INT,
                              interest_only_payment INT,
                              balloon_payment INT,
                              other_nonamortizing_features INT,
                              property_value INT,
                              construction_method INT,
                              occupancy_type INT,
                              total_units TEXT,
                              debt_to_income_ratio TEXT,
                              applicant_credit_score_type INT,
                              co_applicant_credit_score_type INT,
                              preapproval INT,
                              action_taken INT,
                              msamd INT,
                              state_code INT,
                              county_code INT,
                              census_tract_number VARCHAR(7),
                              applicant_ethnicity INT,
                              applicant_ethnicity_2 INT,
                              applicant_ethnicity_3 INT,
                              applicant_ethnicity_4 INT,
                              applicant_ethnicity_5 INT,
                              co_applicant_ethnicity INT,
                              co_applicant_ethnicity_2 INT,
                              co_applicant_ethnicity_3 INT,
                              co_applicant_ethnicity_4 INT,
                              co_applicant_ethnicity_5 INT,
                              applicant_race_1 INT,
                              applicant_race_2 INT,
                              applicant_race_3 INT,
                              applicant_race_4 INT,
                              applicant_race_5 INT,
                              co_applicant_race_1 INT,
                              co_applicant_race_2 INT,
                              co_applicant_race_3 INT,
                              co_applicant_race_4 INT,
                              co_applicant_race_5 INT,
                              applicant_sex INT,
                              co_applicant_sex INT,
                              applicant_age TEXT,
                              co_applicant_age TEXT,
                              applicant_age_above_62 VARCHAR(3),
                              co_applicant_age_above_62 VARCHAR(3),
                              submission_of_application INT,
                              initially_payable_to_institution INT,
                              aus_1 INT,
                              aus_2 INT,
                              aus_3 INT,
                              aus_4 INT,
                              aus_5 INT,
                              applicant_income_000s INT,
                              purchaser_type INT,
                              denial_reason_1 INT,
                              denial_reason_2 INT,
                              denial_reason_3 INT,
                              denial_reason_4 INT,
                              hoepa_status INT,
                              lien_status INT,
                              edit_status INT,
                              sequence_number VARCHAR(7),
                              population INT,
                              minority_population NUMERIC,
                              hud_median_family_income INT,
                              tract_to_msamd_income NUMERIC,
                              number_of_owner_occupied_units INT,
                              number_of_1_to_4_family_units INT,
                              tract_median_age_of_housing_units INT,
                              application_date_indicator INT,
                              FOREIGN KEY (action_taken) REFERENCES actions_taken(action_taken),
                              FOREIGN KEY (agency_code) REFERENCES agencies(agency_code),
                              FOREIGN KEY (application_date_indicator) REFERENCES application_date_indicators(application_date_indicator),
                              FOREIGN KEY (aus_1) REFERENCES auss(aus),
                              FOREIGN KEY (aus_2) REFERENCES auss(aus),
                              FOREIGN KEY (aus_3) REFERENCES auss(aus),
                              FOREIGN KEY (aus_4) REFERENCES auss(aus),
                              FOREIGN KEY (aus_5) REFERENCES auss(aus),
                              FOREIGN KEY (balloon_payment) REFERENCES balloon_payments(balloon_payment),
                              FOREIGN KEY (business_or_commercial_purpose) REFERENCES business_or_commercial_purposes(business_or_commercial_purpose),
                              FOREIGN KEY (construction_method) REFERENCES construction_methods(construction_method),
                              FOREIGN KEY (applicant_credit_score_type) REFERENCES credit_score_types(credit_score_type),
                              FOREIGN KEY (co_applicant_credit_score_type) REFERENCES credit_score_types(credit_score_type),
                              FOREIGN KEY (state_code, county_code) REFERENCES counties(state_code, county_code),
                              FOREIGN KEY (denial_reason_1) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (denial_reason_2) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (denial_reason_3) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (denial_reason_4) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (edit_status) REFERENCES edit_statuses(edit_status),
                              FOREIGN KEY (applicant_ethnicity) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (applicant_ethnicity_2) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (applicant_ethnicity_3) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (applicant_ethnicity_4) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (applicant_ethnicity_5) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (co_applicant_ethnicity) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (co_applicant_ethnicity_2) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (co_applicant_ethnicity_3) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (co_applicant_ethnicity_4) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (co_applicant_ethnicity_5) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (hoepa_status) REFERENCES hoepa_statuses(hoepa_status),
                              FOREIGN KEY (initially_payable_to_institution) REFERENCES initially_payable_to_institutions(initially_payable_to_institution),
                              FOREIGN KEY (interest_only_payment) REFERENCES interest_only_payments(interest_only_payment),
                              FOREIGN KEY (lien_status) REFERENCES lien_statuses(lien_status),
                              FOREIGN KEY (loan_purpose) REFERENCES loan_purposes(loan_purpose),
                              FOREIGN KEY (loan_type) REFERENCES loan_types(loan_type),
                              FOREIGN KEY (msamd) REFERENCES msamds(msamd),
                              FOREIGN KEY (negative_amortization) REFERENCES negative_amortizations(negative_amortization),
                              FOREIGN KEY (occupancy_type) REFERENCES occupancy_types(occupancy_type),
                              FOREIGN KEY (open_end_line_of_credit) REFERENCES open_end_locs(open_end_line_of_credit),
                              FOREIGN KEY (other_nonamortizing_features) REFERENCES other_nonamortizing_features(other_nonamortizing_features),
                              FOREIGN KEY (owner_occupancy) REFERENCES owner_occupancies(owner_occupancy),
                              FOREIGN KEY (preapproval) REFERENCES preapprovals(preapproval),
                              FOREIGN KEY (property_type) REFERENCES property_types(property_type),
                              FOREIGN KEY (purchaser_type) REFERENCES purchaser_types(purchaser_type),
                              FOREIGN KEY (applicant_race_1) REFERENCES races(race),
                              FOREIGN KEY (applicant_race_2) REFERENCES races(race),
                              FOREIGN KEY (applicant_race_3) REFERENCES races(race),
                              FOREIGN KEY (applicant_race_4) REFERENCES races(race),
                              FOREIGN KEY (applicant_race_5) REFERENCES races(race),
                              FOREIGN KEY (co_applicant_race_1) REFERENCES races(race),
                              FOREIGN KEY (co_applicant_race_2) REFERENCES races(race),
                              FOREIGN KEY (co_applicant_race_3) REFERENCES races(race),
                              FOREIGN KEY (co_applicant_race_4) REFERENCES races(race),
                              FOREIGN KEY (co_applicant_race_5) REFERENCES races(race),
                              FOREIGN KEY (reverse_mortgage) REFERENCES reverse_mortgages(reverse_mortgage),
                              FOREIGN KEY (applicant_sex) REFERENCES sexes(sex),
                              FOREIGN KEY (co_applicant_sex) REFERENCES sexes(sex),
                              FOREIGN KEY (state_code) REFERENCES states(state_code),
                              FOREIGN KEY (submission_of_application) REFERENCES submission_of_applications(submission_of_application));")
  )
)


# Add empty columns to pre-2017
pre_2017_adj <- function(df){
  eval(parse(text = paste0(
    df, "$conforming_loan_limit <<- NA;",
    df, "$reverse_mortgage <<- NA;",
    df, "$open_end_line_of_credit <<- NA;",
    df, "$business_or_commercial_purpose <<- NA;",
    df, "$loan_to_value_ratio <<- NA;",
    df, "$interest_rate <<- NA;",
    df, "$total_loan_costs <<- NA;",
    df, "$total_points_and_fees <<- NA;",
    df, "$origination_charges <<- NA;",
    df, "$discount_points <<- NA;",
    df, "$lender_credits <<- NA;",
    df, "$loan_term <<- NA;",
    df, "$prepayment_penalty_term <<- NA;",
    df, "$intro_rate_period <<- NA;",
    df, "$negative_amortization <<- NA;",
    df, "$interest_only_payment <<- NA;",
    df, "$balloon_payment <<- NA;",
    df, "$other_nonamortizing_features <<- NA;",
    df, "$property_value <<- NA;",
    df, "$construction_method <<- NA;",
    df, "$occupancy_type <<- NA;",
    df, "$total_units <<- NA;",
    df, "$debt_to_income_ratio <<- NA;",
    df, "$applicant_credit_score_type <<- NA;",
    df, "$co_applicant_credit_score_type <<- NA;",
    df, "$applicant_age <<- NA;",
    df, "$co_applicant_age <<- NA;",
    df, "$applicant_age_above_62 <<- NA;",
    df, "$co_applicant_age_above_62 <<- NA;",
    df, "$submission_of_application <<- NA;",
    df, "$initially_payable_to_institution <<- NA;",
    df, "$aus_1 <<- NA;",
    df, "$aus_2 <<- NA;",
    df, "$aus_3 <<- NA;",
    df, "$aus_4 <<- NA;",
    df, "$aus_5 <<- NA;",
    df, "$denial_reason_4 <<- NA;",
    df, "$tract_median_age_of_housing_units <<- NA;"
  )))
}


# Adjust post-2017 to pre-2017 conventions
post_2017_adj <- function(df){
  eval(parse(text = paste0(
    df, " <<- rename(", df, ", 'as_of_year' = 'activity_year', 'respondent_id' = 'lei', 'msamd' = 'derived_msa-md', 'state_abbr' = 'state_code', 'census_tract_number' = 'census_tract', 'open_end_line_of_credit' = 'open-end_line_of_credit', 'loan_amount_000s' = 'loan_amount', 'applicant_income_000s' = 'income', 'co_applicant_credit_score_type' = 'co-applicant_credit_score_type', 'applicant_ethnicity' = 'applicant_ethnicity-1', 'applicant_ethnicity_2' = 'applicant_ethnicity-2', 'applicant_ethnicity_3' = 'applicant_ethnicity-3', 'applicant_ethnicity_4' = 'applicant_ethnicity-4', 'applicant_ethnicity_5' = 'applicant_ethnicity-5', 'co_applicant_ethnicity' = 'co-applicant_ethnicity-1', 'co_applicant_ethnicity_2' = 'co-applicant_ethnicity-2', 'co_applicant_ethnicity_3' = 'co-applicant_ethnicity-3', 'co_applicant_ethnicity_4' = 'co-applicant_ethnicity-4', 'co_applicant_ethnicity_5' = 'co-applicant_ethnicity-5', 'co_applicant_ethnicity_observed' = 'co-applicant_ethnicity_observed', 'applicant_race_1' = 'applicant_race-1', 'applicant_race_2' = 'applicant_race-2', 'applicant_race_3' = 'applicant_race-3', 'applicant_race_4' = 'applicant_race-4', 'applicant_race_5' = 'applicant_race-5', 'co_applicant_race_1' = 'co-applicant_race-1', 'co_applicant_race_2' = 'co-applicant_race-2', 'co_applicant_race_3' = 'co-applicant_race-3', 'co_applicant_race_4' = 'co-applicant_race-4', 'co_applicant_race_5' = 'co-applicant_race-5', 'co_applicant_race_observed' = 'co-applicant_race_observed', 'co_applicant_sex' = 'co-applicant_sex', 'co_applicant_sex_observed' = 'co-applicant_sex_observed', 'co_applicant_age' = 'co-applicant_age', 'co_applicant_age_above_62' = 'co-applicant_age_above_62', 'aus_1' = 'aus-1', 'aus_2' = 'aus-2', 'aus_3' = 'aus-3', 'aus_4' = 'aus-4', 'aus_5' = 'aus-5', 'denial_reason_1' = 'denial_reason-1', 'denial_reason_2' = 'denial_reason-2', 'denial_reason_3' = 'denial_reason-3', 'denial_reason_4' = 'denial_reason-4', 'population' = 'tract_population', 'minority_population' = 'tract_minority_population_percent', 'hud_median_family_income' = 'ffiec_msa_md_median_family_income', 'tract_to_msamd_income' = 'tract_to_msa_income_percentage', 'number_of_owner_occupied_units' = 'tract_owner_occupied_units', 'number_of_1_to_4_family_units' = 'tract_one_to_four_family_homes');",
    
    df, "$loan_amount_000s <<- ", df, "$loan_amount_000s / 1e3;",
    df, "$applicant_income_000s <<- ", df, "$applicant_income_000s / 1e3;",
    
    df, "$state_code <<- as.integer(substr(", df, "$county_code, 1, 2));",
    df, "$county_code <<- as.integer(substr(", df, "$county_code, 3, 5));",
    df, "$state_abbr <<- NULL;",
    
    df, "$property_type <<- ifelse(", df, "$derived_dwelling_category == 'Single Family (1-4 Units):Site-Built', 1, ifelse(", df, "$derived_dwelling_category %in% c('Single Family (1-4 Units):Manufactured', 'Multifamily:Manufactured (5+ Units)'), 2, ifelse(", df, "$derived_dwelling_category == 'Multifamily:Site-Built (5+ Units)', 3, NA)));",
    
    df, "$owner_occupancy <<- ifelse(", df, "$occupancy_type == 1, 1, ifelse(", df, "$occupancy_type %in% c(2, 3), 2, NA));",
    
    df, "$agency_code <<- NA;",
    
    df, "$edit_status <<- NA;",
    
    df, "$sequence_number <<- NA;",
    
    df, "$application_date_indicator <<- 0;",
    
    df, "$loan_to_value_ratio <<- ifelse(", df, "$loan_to_value_ratio == 'Exempt', -1111, as.numeric(", df, "$loan_to_value_ratio));",
    df, "$interest_rate <<- ifelse(", df, "$interest_rate == 'Exempt', -1111, as.numeric(", df, "$interest_rate));",
    df, "$rate_spread <<- ifelse(", df, "$rate_spread == 'Exempt', -1111, as.numeric(", df, "$rate_spread));",
    df, "$total_loan_costs <<- ifelse(", df, "$total_loan_costs == 'Exempt', -1111, as.numeric(", df, "$total_loan_costs));",
    df, "$total_points_and_fees <<- ifelse(", df, "$total_points_and_fees == 'Exempt', -1111, as.numeric(", df, "$total_points_and_fees));",
    df, "$origination_charges <<- ifelse(", df, "$origination_charges == 'Exempt', -1111, as.numeric(", df, "$origination_charges));",
    df, "$discount_points <<- ifelse(", df, "$discount_points == 'Exempt', -1111, as.numeric(", df, "$discount_points));",
    df, "$lender_credits <<- ifelse(", df, "$lender_credits == 'Exempt', -1111, as.numeric(", df, "$lender_credits));",
    
    df, "$loan_term <<- ifelse(", df, "$loan_term == 'Exempt', -1111, as.integer(", df, "$loan_term));",
    df, "$prepayment_penalty_term <<- ifelse(", df, "$prepayment_penalty_term == 'Exempt', -1111, as.integer(", df, "$prepayment_penalty_term));",
    df, "$intro_rate_period <<- ifelse(", df, "$intro_rate_period == 'Exempt', -1111, as.integer(", df, "$intro_rate_period));",
    df, "$property_value <<- ifelse(", df, "$property_value == 'Exempt', -1111, as.integer(", df, "$property_value));"
  )))
}




# Ingest data
hmda_ingest <- function(ingest_row){
  ingest_row[as.vector(sapply(ingest_row, function(x){typeof(x) == "character"}))] <- as.list(paste0("'", ingest_row[as.vector(sapply(ingest_row, function(x){typeof(x) == "character"}))], "'"))
  ingest_row[as.vector(is.na(ingest_row))] <- "NULL"
  
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO main VALUES (",
                                ingest_row$as_of_year, ", ",
                                ingest_row$respondent_id, ", ",
                                ingest_row$agency_code, ", ",
                                ingest_row$conforming_loan_limit, ", ",
                                ingest_row$reverse_mortgage, ", ",
                                ingest_row$open_end_line_of_credit, ", ",
                                ingest_row$business_or_commercial_purpose, ", ",
                                ingest_row$loan_type, ", ",
                                ingest_row$property_type, ", ",
                                ingest_row$loan_purpose, ", ",
                                ingest_row$owner_occupancy, ", ",
                                ingest_row$loan_amount_000s, ", ",
                                ingest_row$loan_to_value_ratio, ", ",
                                ingest_row$interest_rate, ", ",
                                ingest_row$rate_spread, ", ",
                                ingest_row$total_loan_costs, ", ",
                                ingest_row$total_points_and_fees, ", ",
                                ingest_row$origination_charges, ", ",
                                ingest_row$discount_points, ", ",
                                ingest_row$lender_credits, ", ",
                                ingest_row$loan_term, ", ",
                                ingest_row$prepayment_penalty_term, ", ",
                                ingest_row$intro_rate_period, ", ",
                                ingest_row$negative_amortization, ", ",
                                ingest_row$interest_only_payment, ", ",
                                ingest_row$balloon_payment, ", ",
                                ingest_row$other_nonamortizing_features, ", ",
                                ingest_row$property_value, ", ",
                                ingest_row$construction_method, ", ",
                                ingest_row$occupancy_type, ", ",
                                ingest_row$total_units, ", ",
                                ingest_row$debt_to_income_ratio, ", ",
                                ingest_row$applicant_credit_score_type, ", ",
                                ingest_row$co_applicant_credit_score_type, ", ",
                                ingest_row$preapproval, ", ",
                                ingest_row$action_taken, ", ",
                                ingest_row$msamd, ", ",
                                ingest_row$state_code, ", ",
                                ingest_row$county_code, ", ",
                                ingest_row$census_tract_number, ", ",
                                ingest_row$applicant_ethnicity, ", ",
                                ingest_row$co_applicant_ethnicity, ", ",
                                ingest_row$applicant_race_1, ", ",
                                ingest_row$applicant_race_2, ", ",
                                ingest_row$applicant_race_3, ", ",
                                ingest_row$applicant_race_4, ", ",
                                ingest_row$applicant_race_5, ", ",
                                ingest_row$co_applicant_race_1, ", ",
                                ingest_row$co_applicant_race_2, ", ",
                                ingest_row$co_applicant_race_3, ", ",
                                ingest_row$co_applicant_race_4, ", ",
                                ingest_row$co_applicant_race_5, ", ",
                                ingest_row$applicant_sex, ", ",
                                ingest_row$co_applicant_sex, ", ",
                                ingest_row$applicant_age, ", ",
                                ingest_row$co_applicant_age, ", ",
                                ingest_row$applicant_age_above_62, ", ",
                                ingest_row$co_applicant_age_above_62, ", ",
                                ingest_row$submission_of_application, ", ",
                                ingest_row$initially_payable_to_institution, ", ",
                                ingest_row$aus_1, ", ",
                                ingest_row$aus_2, ", ",
                                ingest_row$aus_3, ", ",
                                ingest_row$aus_4, ", ",
                                ingest_row$aus_5, ", ",
                                ingest_row$applicant_income_000s, ", ",
                                ingest_row$purchaser_type, ", ",
                                ingest_row$denial_reason_1, ", ",
                                ingest_row$denial_reason_2, ", ",
                                ingest_row$denial_reason_3, ", ",
                                ingest_row$denial_reason_4, ", ",
                                ingest_row$hoepa_status, ", ",
                                ingest_row$lien_status, ", ",
                                ingest_row$edit_status, ", ",
                                ingest_row$sequence_number, ", ",
                                ingest_row$population, ", ",
                                ingest_row$minority_population, ", ",
                                ingest_row$hud_median_family_income, ", ",
                                ingest_row$tract_to_msamd_income, ", ",
                                ingest_row$number_of_owner_occupied_units, ", ",
                                ingest_row$number_of_1_to_4_family_units, ", ",
                                ingest_row$tract_median_age_of_housing_units, ", ",
                                ingest_row$application_date_indicator,
                                ");"))
  )
}


# Run ingest
load("../../3_Data/HMDA LAR/Nationwide records/hmda_2007_nationwide_all.RData")
pre_2017_adj("hmda_lar_2007")
for (i in 1:nrow(hmda_lar_2007)){
  hmda_ingest(hmda_lar_2007[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2007)))
  }
}
rm(hmda_lar_2007)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2008_nationwide_all.RData")
pre_2017_adj("hmda_lar_2008")
for (i in 1:nrow(hmda_lar_2008)){
  hmda_ingest(hmda_lar_2008[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2008)))
  }
}
rm(hmda_lar_2008)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2009_nationwide_all.RData")
pre_2017_adj("hmda_lar_2009")
for (i in 1:nrow(hmda_lar_2009)){
  hmda_ingest(hmda_lar_2009[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2009)))
  }
}
rm(hmda_lar_2009)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2010_nationwide_all.RData")
pre_2017_adj("hmda_lar_2010")
for (i in 1:nrow(hmda_lar_2010)){
  hmda_ingest(hmda_lar_2010[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2010)))
  }
}
rm(hmda_lar_2010)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2011_nationwide_all.RData")
pre_2017_adj("hmda_lar_2011")
for (i in 1:nrow(hmda_lar_2011)){
  hmda_ingest(hmda_lar_2011[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2011)))
  }
}
rm(hmda_lar_2011)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2012_nationwide_all.RData")
pre_2017_adj("hmda_lar_2012")
for (i in 1:nrow(hmda_lar_2012)){
  hmda_ingest(hmda_lar_2012[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2012)))
  }
}
rm(hmda_lar_2012)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2013_nationwide_all.RData")
pre_2017_adj("hmda_lar_2013")
for (i in 1:nrow(hmda_lar_2013)){
  hmda_ingest(hmda_lar_2013[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2013)))
  }
}
rm(hmda_lar_2013)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2014_nationwide_all.RData")
pre_2017_adj("hmda_lar_2014")
for (i in 1:nrow(hmda_lar_2014)){
  hmda_ingest(hmda_lar_2014[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2014)))
  }
}
rm(hmda_lar_2014)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2015_nationwide_all.RData")
pre_2017_adj("hmda_lar_2015")
for (i in 1:nrow(hmda_lar_2015)){
  hmda_ingest(hmda_lar_2015[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2015)))
  }
}
rm(hmda_lar_2015)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2016_nationwide_all.RData")
pre_2017_adj("hmda_lar_2016")
for (i in 1:nrow(hmda_lar_2016)){
  hmda_ingest(hmda_lar_2016[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2016)))
  }
}
rm(hmda_lar_2016)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2017_nationwide_all.RData")
pre_2017_adj("hmda_lar_2017")
for (i in 1:nrow(hmda_lar_2017)){
  hmda_ingest(hmda_lar_2017[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2017)))
  }
}
rm(hmda_lar_2017)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2018_nationwide_all.RData")
post_2017_adj("hmda_lar_2018")
for (i in 1:nrow(hmda_lar_2018)){
  hmda_ingest(hmda_lar_2018[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2018)))
  }
}
rm(hmda_lar_2018)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2019_nationwide_all.RData")
post_2017_adj("hmda_lar_2019")
for (i in 1:nrow(hmda_lar_2019)){
  hmda_ingest(hmda_lar_2019[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2019)))
  }
}
rm(hmda_lar_2019)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2020_nationwide_all.RData")
post_2017_adj("hmda_lar_2020")
for (i in 1:nrow(hmda_lar_2020)){
  hmda_ingest(hmda_lar_2020[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2020)))
  }
}
rm(hmda_lar_2020)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2021_nationwide_all.RData")
post_2017_adj("hmda_lar_2021")
for (i in 1:nrow(hmda_lar_2021)){
  hmda_ingest(hmda_lar_2021[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2021)))
  }
}
rm(hmda_lar_2021)
gc()

load("../../3_Data/HMDA LAR/Nationwide records/hmda_2022_nationwide_all.RData")
post_2017_adj("hmda_lar_2022")
for (i in 1:nrow(hmda_lar_2022)){
  hmda_ingest(hmda_lar_2022[i, ])
  if (i %% 500 == 0){
    print(paste0(i, "/", nrow(hmda_lar_2022)))
  }
}
rm(hmda_lar_2022)
gc()

# Add FIPS mapping
source("code/1_data merging.R")
dbClearResult(
  dbSendQuery(hmda_db, "CREATE TABLE fips_codes (state_code INT, county_code INT, fips_code TEXT, PRIMARY KEY (state_code, county_code), FOREIGN KEY (state_code) REFERENCES counties(county_code), FOREIGN KEY (county_code) REFERENCES counties(county_code));")
)

fips_mapping <- subset(fips_counties, county_code != "000")
fips_mapping$fips_code <- fips_pad(fips_mapping$state_code, fips_mapping$county_code)
fips_mapping$state_code <- as.integer(fips_mapping$state_code)
fips_mapping$county_code <- as.integer(fips_mapping$county_code)
for (i in 1:nrow(fips_mapping)){
  dbClearResult(
    dbSendQuery(hmda_db, paste0("INSERT INTO fips_codes VALUES(", fips_mapping$state_code[i], ", ", fips_mapping$county_code[i], ", '", fips_mapping$fips_code[i], "');"))
  )
}

dbDisconnect(hmda_db)



### Load NRI data ----
nri_counties <- read_csv("../../3_Data/NRI_Table_Counties/NRI_Table_Counties.csv")
nri_tribal <- read_csv("../../3_Data/NRI_Table_Tribal_Counties/NRI_Table_Tribal_Counties.csv")
nri_tribal <- select(nri_tribal, -names(nri_tribal)[!names(nri_tribal) %in% names(nri_counties)])

# Adjust column names for pivot with automatic name coercion
names(nri_counties)[sapply(names(nri_counties), function(x){length(unlist(str_extract_all(x, "_"))) > 1})] <- str_replace(str_replace(names(nri_counties)[sapply(names(nri_counties), function(x){length(unlist(str_extract_all(x, "_"))) > 1})], "EXP_AREA", "EXPAREA"), "ALR_NPCTL", "ALRNPCTL")
names(nri_tribal)[sapply(names(nri_tribal), function(x){length(unlist(str_extract_all(x, "_"))) > 1})] <- str_replace(str_replace(names(nri_tribal)[sapply(names(nri_tribal), function(x){length(unlist(str_extract_all(x, "_"))) > 1})], "EXP_AREA", "EXPAREA"), "ALR_NPCTL", "ALRNPCTL")


nri_composite <- rbind(nri_counties, nri_tribal) |> 
  select(-STATE, -STATEABBRV, -COUNTY, -STCOFIPS, -NRI_VER) |> 
  rename("object_id" = "OID_",
         "nri_id" = "NRI_ID",
         "state_code" = "STATEFIPS",
         "county_code" = "COUNTYFIPS",
         "county_type" = "COUNTYTYPE",
         "population" = "POPULATION",
         "building_value" = "BUILDVALUE",
         "agriculture_value" = "AGRIVALUE",
         "area" = "AREA",
         "risk_value_composite" = "RISK_VALUE",
         "risk_score_composite" = "RISK_SCORE",
         "risk_rating_composite" = "RISK_RATNG",
         "risk_stateperc_composite" = "RISK_SPCTL",
         "eal_score_composite" = "EAL_SCORE",
         "eal_rating_composite" = "EAL_RATNG",
         "eal_stateperc_composite" = "EAL_SPCTL",
         "eal_total_composite" = "EAL_VALT",
         "eal_building_value_composite" = "EAL_VALB",
         "eal_population_composite" = "EAL_VALP",
         "eal_population_equivalence_composite" = "EAL_VALPE",
         "eal_agriculture_value_composite" = "EAL_VALA",
         "alr_building_composite" = "ALR_VALB",
         "alr_population_composite" = "ALR_VALP",
         "alr_agriculture_composite" = "ALR_VALA",
         "alr_nationalperc_composite" = "ALR_NPCTL",
         "alr_sovi_adj_composite" = "ALR_VRA_NPCTL",
         "sovi_score_composite" = "SOVI_SCORE",
         "sovi_rating_composite" = "SOVI_RATNG",
         "sovi_stateperc_composite" = "SOVI_SPCTL",
         "resl_score_composite" = "RESL_SCORE",
         "resl_rating_composite" = "RESL_RATNG",
         "resl_stateperc_composite" = "RESL_SPCTL",
         "resl_value_composite" = "RESL_VALUE",
         "comm_risk_factor_composite" = "CRF_VALUE") |> 
  select(object_id, nri_id, state_code, county_code, county_type, population, building_value, agriculture_value, area, risk_value_composite, risk_score_composite, risk_rating_composite,
         risk_stateperc_composite, eal_score_composite, eal_rating_composite, eal_stateperc_composite, eal_total_composite, eal_building_value_composite, eal_population_composite,
         eal_population_equivalence_composite, eal_agriculture_value_composite, alr_building_composite, alr_population_composite, alr_agriculture_composite, alr_nationalperc_composite,
         alr_sovi_adj_composite, sovi_score_composite, sovi_rating_composite, sovi_stateperc_composite, resl_score_composite, resl_rating_composite, resl_stateperc_composite, resl_value_composite,
         comm_risk_factor_composite)

nri_counties <- rbind(nri_counties, nri_tribal) |> 
  select(-STATE, -STATEABBRV, -COUNTY, -STCOFIPS, -NRI_VER) |> 
  rename("object_id" = "OID_",
         "nri_id" = "NRI_ID",
         "state_code" = "STATEFIPS",
         "county_code" = "COUNTYFIPS",
         "county_type" = "COUNTYTYPE",
         "population" = "POPULATION",
         "building_value" = "BUILDVALUE",
         "agriculture_value" = "AGRIVALUE",
         "area" = "AREA",
         "risk_value_composite" = "RISK_VALUE",
         "risk_score_composite" = "RISK_SCORE",
         "risk_rating_composite" = "RISK_RATNG",
         "risk_stateperc_composite" = "RISK_SPCTL",
         "eal_score_composite" = "EAL_SCORE",
         "eal_rating_composite" = "EAL_RATNG",
         "eal_stateperc_composite" = "EAL_SPCTL",
         "eal_total_composite" = "EAL_VALT",
         "eal_building_value_composite" = "EAL_VALB",
         "eal_population_composite" = "EAL_VALP",
         "eal_population_equivalence_composite" = "EAL_VALPE",
         "eal_agriculture_value_composite" = "EAL_VALA",
         "alr_building_composite" = "ALR_VALB",
         "alr_population_composite" = "ALR_VALP",
         "alr_agriculture_composite" = "ALR_VALA",
         "alr_nationalperc_composite" = "ALR_NPCTL",
         "alr_sovi_adj_composite" = "ALR_VRA_NPCTL",
         "sovi_score_composite" = "SOVI_SCORE",
         "sovi_rating_composite" = "SOVI_RATNG",
         "sovi_stateperc_composite" = "SOVI_SPCTL",
         "resl_score_composite" = "RESL_SCORE",
         "resl_rating_composite" = "RESL_RATNG",
         "resl_stateperc_composite" = "RESL_SPCTL",
         "resl_value_composite" = "RESL_VALUE",
         "comm_risk_factor_composite" = "CRF_VALUE",
         ) |> 
  select(-population, -building_value, -agriculture_value, -area, -risk_value_composite, -risk_score_composite, -risk_rating_composite, -risk_stateperc_composite, -eal_score_composite, -eal_rating_composite,
         -eal_stateperc_composite, -eal_total_composite, -eal_building_value_composite, -eal_population_composite, -eal_population_equivalence_composite, -eal_agriculture_value_composite, -alr_building_composite,
         -alr_population_composite, -alr_agriculture_composite, -alr_nationalperc_composite, -alr_sovi_adj_composite, -sovi_score_composite, -sovi_rating_composite, -sovi_stateperc_composite, -resl_score_composite,
         -resl_rating_composite, -resl_stateperc_composite, -resl_value_composite, -comm_risk_factor_composite) |> 
  pivot_longer(cols = c(-object_id, -nri_id, -state_code, -county_code, -county_type),
               names_to = c("risk_type_abbr", "value_type_abbr"), names_sep = "_", values_to = "value", values_transform = as.character)

# Make risk types more readable
nri_counties <- merge(nri_counties, data.frame("risk_type_abbr" = c("AVLN", "CFLD", "CWAV", "DRGT", "ERQK", "HAIL", "HWAV", "HRCN", "ISTM", "LNDS", "LTNG", "RFLD", "SWND", "TRND", "TSUN", "VLCN", "WFIR", "WNTW"),
                                               "risk_type" = c("avalanche", "coastal_flooding", "cold_wave", "drought", "earthquake", "hail", "heat_wave", "hurricane", "ice_storm", "landslide", "lightning", "riverine_flood", "strong_wind", "tornado", "tsunami", "volcanic_activity", "wildfire", "winter_weather")),
                      by = "risk_type_abbr", all.x = TRUE)

# Make value types more readable
nri_counties <- merge(nri_counties, data.frame("value_type_abbr" = c("EVNTS", "AFREQ", "EXPAREA", "EXPB", "EXPP", "EXPPE", "EXPT", "HLRB", "HLRP", "HLRR", "EALB", "EALP", "EALPE", "EALT", "EALS", "EALR", "ALRB", "ALRP", "ALRNPCTL", "RISKV", "RISKS", "RISKR", "ALRA", "EXPA", "HLRA", "EALA"),
                                               "value_type" = c("nr_events", "annualized_freq", "exposure_impacted_area", "exposure_building_value", "exposure_population", "exposure_population_equivalence", "exposure_total", "hlr_buildings", "hlr_population", "hlr_rating", "eal_building_value", "eal_population", "eal_population_equivalence", "eal_total", "eal_score", "eal_rating", "alr_building_value", "alr_population", "alr_nationalperc", "risk_value", "risk_score", "risk_rating", "alr_agriculture", "exposure_agriculture", "hlr_agriculture", "eal_agriculture")),
                      by = "value_type_abbr", all.x = TRUE)

# Save to binary
save(nri_counties, nri_composite, file = "data/nri.RData")




### BLS data ----

qcew_year <- do.call(rbind, lapply(list.files("../../3_Data/BLS/QCEW/2023.q1-q3.by_area/", pattern = ".csv", recursive = TRUE, full.names = TRUE), function(x){read_csv(x, show_col_types = FALSE)}))
qcew <- subset(qcew_year, agglvl_title %in% c("County, Total Covered", "County, Total -- by ownership sector", "County, by Domain -- by ownership sector", "County, by Supersector -- by ownership sector", "County, NAICS Sector -- by ownership sector"))

qcew_data_dict <- openxlsx::read.xlsx("../../3_Data/BLS/QCEW/QCEW data dict.xlsx")

# Check if column naming is consistent for row aggregation
qcew_name_check <- data.frame(t(qcew_data_dict$field_name))
names(qcew_name_check) <- qcew_data_dict$field_name
qcew_name_check$check_year <- NA

for (y in 1990:2023){
  suppressMessages(eval(parse(text = paste0("qcew_name_check_year <- do.call(rbind, lapply(list.files('../../3_Data/BLS/QCEW/", y, ".q1-q4.by_area/', pattern = '.csv', recursive = TRUE, full.names = TRUE), function(x){names(read_csv(x, show_col_types = FALSE, n_max = 1))}))"))))
  qcew_name_check_year <- data.frame(qcew_name_check_year)
  names(qcew_name_check_year) <- qcew_data_dict$field_name
  qcew_name_check_year$check_year <- y
  qcew_name_check <- rbind(qcew_name_check, qcew_name_check_year)
}

qcew_name_check <- qcew_name_check |> 
  pivot_longer(cols = c(-check_year), names_to = "data_dict_name", values_to = "effective_name")

qcew_name_check <- aggregate(effective_name ~ data_dict_name, qcew_name_check, function(x){paste(unique(x), collapse = ", ")})
View(qcew_name_check)

# Name check looks fine, enforce data dict naming
names(qcew) <- qcew_data_dict$field_name

# Read data
for (y in 2022:1990){
  start_time <- Sys.time()
  suppressMessages(eval(parse(text = paste0("qcew_year <- do.call(rbind, lapply(list.files('../../3_Data/BLS/QCEW/", y, ".q1-q4.by_area/', pattern = '.csv', recursive = TRUE, full.names = TRUE), function(x){read_csv(x, show_col_types = FALSE)}))"))))
  names(qcew_year) <- qcew_data_dict$field_name
  qcew <- rbind(qcew,
                subset(qcew_year, agglvl_title %in% c("County, Total Covered", "County, Total -- by ownership sector", "County, by Domain -- by ownership sector", "County, by Supersector -- by ownership sector", "County, NAICS Sector -- by ownership sector")))
  gc()
  print(paste0(y, " -- elapsed time: ", Sys.time() - start_time))
}

# Save binary
save(qcew, file = "data/bls_qcew.RData")



### BLS LAUS ----
laus <- do.call(rbind, lapply(paste0("../../3_Data/BLS/LAUS/LAUS_county_", 1990:2022, ".xlsx"), function(x){openxlsx::read.xlsx(x, colNames = FALSE, startRow = 7)}))
names(laus) <- c("laus_code", "state_code", "county_code", "county_name", "year", "labor_force", "employed", "unemployed", "unemployment_rate")

# Save binary
save(laus, file = "data/bls_laus.RData")



### BEA local GDP data ----
bea_2017_files <- list.files("../../3_Data/BEA/GDP by County and Metropolitan Area/gdpmetro0918/", pattern = ".csv")
bea_2017 <- do.call(rbind, lapply(bea_2017_files, function(x){read_csv(file = paste0("../../3_Data/BEA/GDP by County and Metropolitan Area/gdpmetro0918/", x), show_col_types = FALSE)}))

# Clean out redacted, unavailable, and non-meaningful values
bea_2017 <- pivot_longer(bea_2017, cols = as.character(2001:2017), names_to = "year", names_transform = as.integer, values_to = "gdp_value")
bea_2017 <- subset(bea_2017, !(is.na(IndustryClassification) & is.na(Description)))
bea_2001_2017_industries <- unique(select(bea_2017, "industry_id" = "IndustryId", "industry_classification" = "IndustryClassification", "description" = "Description"))
bea_2017 <- select(bea_2017, "fips_code" = "GeoFIPS", "table_name" = "TableName", "component_name" = "ComponentName", "unit" = "Unit", "industry_id" = "IndustryId", "industry_classification" = "IndustryClassification", "gdp_value", "year")
bea_2017$gdp_value[bea_2017$gdp_value %in% c("(D)", "(L)", "(NA)", "(NM)")] <- NA


bea_2022 <- read_csv(file = "../../3_Data/BEA/GDP by County and Metropolitan Area/CAGDP2/CAGDP2__ALL_AREAS_2017_2022.csv", n_max = 108052)

# Clean out redacted, unavailable, and non-meaningful values
bea_2022 <- pivot_longer(bea_2022, cols = as.character(2017:2022), names_to = "year", names_transform = as.integer, values_to = "gdp_value")
bea_2017_2022_industries <- unique(select(bea_2022, "industry_id" = "LineCode", "industry_classification" = "IndustryClassification", "description" = "Description"))
bea_2022$component_name <- "Gross domestic product (GDP) by county and metropolitan area"
bea_2022 <- select(bea_2022, "fips_code" = "GeoFIPS", "table_name" = "TableName", "component_name", "unit" = "Unit", "industry_id" = "LineCode", "industry_classification" = "IndustryClassification", "gdp_value", "year")
bea_2022$gdp_value[bea_2022$gdp_value %in% c("(D)", "(L)", "(NA)", "(NM)")] <- NA

bea_2017 <- subset(bea_2017, year != 2017)
bea <- rbind(bea_2017, bea_2022)

# Save binary
save(bea, bea_2001_2017_industries, bea_2017_2022_industries, file = "data/bea_gdp.RData")



### Shape data per county ----
county_shape <- sf::read_sf(dsn = "../../3_Data/CENSUS_Shapefiles/tl_2023_us_county/")
county_shape <- sf::st_transform(county_shape, crs = sp::CRS("+proj=longlat"))

# Save binary
save(county_shape, file = "data/county_shape.RData")


# Prepare data for plotting maps
source("code/1_data merging.R")

county_shape_plotting <- data.frame()
for (i in 1:nrow(county_shape)){
  county_part <- data.frame(sf::st_coordinates(county_shape[i,]))[,1:2]
  names(county_part) <- c("lon", "lat")
  county_part$fips_code <- fips_pad(county_shape$STATEFP[i], county_shape$COUNTYFP[i])
  county_shape_plotting <- rbind(county_shape_plotting, county_part)
  print(paste0(i, "/", nrow(county_shape)))
}

# Fix coordinates that cross the date line
county_shape_plotting$lon[county_shape_plotting$lon > 0] <- county_shape_plotting$lon[county_shape_plotting$lon > 0] - 360

save(county_shape_plotting, file = "data/county_shape_plotting.RData")



### Population estimate per county ----
county_population <- read_csv("../../3_Data/CENSUS_Population/co-est00int-tot.csv", show_col_types = FALSE)
names(county_population) <- tolower(names(county_population))

county_population$fips_code <- fips_pad(county_population$state, county_population$county)
county_population <- pivot_longer(select(county_population, fips_code, "state_name" = stname, "county_name" = ctyname, starts_with("popestimate")), cols = starts_with("popestimate"), names_to = "year", names_prefix = "popestimate", values_to = "population_estimate")
county_population <- subset(county_population, year != 2010)

population_temp <- read_csv("../../3_Data/CENSUS_Population/co-est2020-alldata.csv", show_col_types = FALSE)
names(population_temp) <- tolower(names(population_temp))

population_temp$fips_code <- fips_pad(population_temp$state, population_temp$county)
population_temp <- pivot_longer(select(population_temp, fips_code, "state_name" = stname, "county_name" = ctyname, starts_with("popestimate")), cols = starts_with("popestimate"), names_to = "year", names_prefix = "popestimate", values_to = "population_estimate")
population_temp <- subset(population_temp, year != 2020)

county_population <- rbind(county_population, population_temp)

population_temp <- read_csv("../../3_Data/CENSUS_Population/co-est2023-alldata.csv", show_col_types = FALSE)
names(population_temp) <- tolower(names(population_temp))

population_temp$fips_code <- fips_pad(population_temp$state, population_temp$county)
population_temp <- pivot_longer(select(population_temp, fips_code, "state_name" = stname, "county_name" = ctyname, starts_with("popestimate")), cols = starts_with("popestimate"), names_to = "year", names_prefix = "popestimate", values_to = "population_estimate")

county_population <- rbind(county_population, population_temp); rm(population_temp)

# Save binary
save(county_population, file = "data/county_population_estimate.RData")
