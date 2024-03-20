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


# Make main table, add values
dbClearResult(
  dbSendQuery(hmda_db, paste0("CREATE TABLE hmda (
                              as_of_year INT,
                              respondent_id VARCHAR(10),
                              agency_code INT,
                              loan_type INT,
                              property_type INT,
                              loan_purpose INT,
                              owner_occupancy INT,
                              loan_amount_000s INT,
                              preapproval INT,
                              action_taken INT,
                              msamd INT,
                              state_code INT,
                              county_code INT,
                              census_tract_number VARCHAR(7),
                              applicant_ethnicity INT,
                              co_applicant_ethnicity INT,
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
                              applicant_income_000s INT,
                              purchaser_type INT,
                              denial_reason_1 INT,
                              denial_reason_2 INT,
                              denial_reason_3 INT,
                              rate_spread NUMERIC(3, 2),
                              hoepa_status INT,
                              lien_status INT,
                              edit_status INT,
                              sequence_number VARCHAR(7),
                              population INT,
                              minority_population NUMERIC(3, 2),
                              hud_median_family_income INT,
                              tract_to_msamd_income NUMERIC(3, 2),
                              number_of_owner_occupied_units INT,
                              number_of_1_to_4_family_units INT,
                              application_date_indicator INT,
                              FOREIGN KEY (action_taken) REFERENCES actions_taken(action_taken),
                              FOREIGN KEY (agency_code) REFERENCES agencies(agency_code),
                              FOREIGN KEY (application_date_indicator) REFERENCES application_date_indicators(application_date_indicator),
                              FOREIGN KEY (state_code, county_code) REFERENCES counties(state_code, county_code),
                              FOREIGN KEY (denial_reason_1) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (denial_reason_2) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (denial_reason_3) REFERENCES denial_reasons(denial_reason),
                              FOREIGN KEY (edit_status) REFERENCES edit_statuses(edit_status),
                              FOREIGN KEY (applicant_ethnicity) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (co_applicant_ethnicity) REFERENCES ethnicities(ethnicity),
                              FOREIGN KEY (hoepa_status) REFERENCES hoepa_statuses(hoepa_status),
                              FOREIGN KEY (lien_status) REFERENCES lien_statuses(lien_status),
                              FOREIGN KEY (loan_purpose) REFERENCES loan_purposes(loan_purpose),
                              FOREIGN KEY (loan_type) REFERENCES loan_types(loan_type),
                              FOREIGN KEY (msamd) REFERENCES msamds(msamd),
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
                              FOREIGN KEY (applicant_sex) REFERENCES sexes(sex),
                              FOREIGN KEY (co_applicant_sex) REFERENCES sexes(sex),
                              FOREIGN KEY (state_code) REFERENCES states(state_code));")
  )
)

# TODO: Add columns, foreign keys, (primary key) from 2018+ format
# TODO: Ingest data -> replace NA with NULL


dbDisconnect(hmda_db)
