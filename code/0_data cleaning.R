rm(list = ls())

library(tidyverse)
library(DBI)
library(RSQLite)

### Load Zillow data ----
zillow_county <- read_delim("../../3_Data/Zillow/County_zhvi_uc_sfrcondo_tier_0.0_0.33_sm_sa_month.csv") |> 
  pivot_longer(cols = c(-RegionID, -SizeRank, -RegionName, -RegionType, -StateName, -State, -Metro, -StateCodeFIPS, -MunicipalCodeFIPS), names_to = "date", values_to = "zhvi") |> 
  mutate("data_series" = "all_homes_bottom_tier")

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

fema <- merge(select(fema_areas, -id, -hash, -last_refresh), select(fema_declarations, -state_name, -id, -hash, -last_refresh), by = c("disaster_number", "state_code"), all.x = T)
fema <- merge(fema, select(fema_summaries, -id, -hash, -last_refresh), by = "disaster_number", all.x = T)

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



















dbClearResult(
  dbSendQuery(hmda_db, paste0("CREATE TABLE hmda (",
                              as_of_year = int,
                              respondent_id = int,
                              agency_name = character,
                              agency_abbr = character,
                              agency_code = int,
                              loan_type_name = character,
                              loan_type = int,
                              property_type_name = character,
                              property_type = int,
                              loan_purpose_name = character,
                              loan_purpose = int,
                              owner_occupancy_name = character,
                              owner_occupancy = numeric,
                              loan_amount_000s = numeric,
                              preapproval_name = character,
                              preapproval = numeric,
                              action_taken_name = character,
                              action_taken = numeric,
                              msamd_name = character,
                              msamd = numeric,
                              state_name = character,
                              state_abbr = character,
                              state_code = numeric,
                              county_name = character,
                              county_code = numeric,
                              census_tract_number = character,
                              applicant_ethnicity_name = character,
                              applicant_ethnicity = numeric,
                              co_applicant_ethnicity_name = character,
                              co_applicant_ethnicity = numeric,
                              applicant_race_name_1 = character,
                              applicant_race_1 = numeric,
                              applicant_race_name_2 = character,
                              applicant_race_2 = numeric,
                              applicant_race_name_3 = logical,
                              applicant_race_3 = logical,
                              applicant_race_name_4 = logical,
                              applicant_race_4 = logical,
                              applicant_race_name_5 = logical,
                              applicant_race_5 = logical,
                              co_applicant_race_name_1 = character,
                              co_applicant_race_1 = numeric,
                              co_applicant_race_name_2 = character,
                              co_applicant_race_2 = numeric,
                              co_applicant_race_name_3 = logical,
                              co_applicant_race_3 = logical,
                              co_applicant_race_name_4 = logical,
                              co_applicant_race_4 = logical,
                              co_applicant_race_name_5 = logical,
                              co_applicant_race_5 = logical,
                              applicant_sex_name = character,
                              applicant_sex = numeric,
                              co_applicant_sex_name = character,
                              co_applicant_sex = numeric,
                              applicant_income_000s = numeric,
                              purchaser_type_name = character,
                              purchaser_type = numeric,
                              denial_reason_name_1 = character,
                              denial_reason_1 = numeric,
                              denial_reason_name_2 = character,
                              denial_reason_2 = numeric,
                              denial_reason_name_3 = character,
                              denial_reason_3 = numeric,
                              rate_spread = character,
                              hoepa_status_name = character,
                              hoepa_status = numeric,
                              lien_status_name = character,
                              lien_status = numeric,
                              edit_status_name = character,
                              edit_status = numeric,
                              sequence_number = character,
                              population = numeric,
                              minority_population = numeric,
                              hud_median_family_income = numeric,
                              tract_to_msamd_income = numeric,
                              number_of_owner_occupied_units = numeric,
                              number_of_1_to_4_family_units = numeric,
                              application_date_indicator = numeric)
                              
                              
                      
                      
bDisconnect(hmda_db)
