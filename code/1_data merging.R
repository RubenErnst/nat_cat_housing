library(tidyverse)

### Load FIPS mapping ----
fips_states <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/fips.xlsx", startRow = 1, rows = c(1:52)) |> 
  mutate(state_name = str_to_title(state_name))

fips_counties <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/fips.xlsx", startRow = 53)

bea_fips_mod <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/BEA_FIPS_Modifications.xlsx", startRow = 2)
bea_fips_mod$county_code_bea <- substr(bea_fips_mod$BEA.FIPS, 3, 5)
bea_fips_mod$county_code <- substr(bea_fips_mod$FIPS, 3, 5)
bea_fips_mod$state_code <- substr(bea_fips_mod$FIPS, 1, 2)
fips_counties <- merge(fips_counties,
                       select(bea_fips_mod, county_code_bea, county_code, state_code),
                       by = c("state_code", "county_code"), all.x = T)
fips_counties$county_code_bea[is.na(fips_counties$county_code_bea)] <- fips_counties$county_code[is.na(fips_counties$county_code_bea)]

### FEMA incident mapping ----
fema_incident_mapping <- data.frame("incident_type" = c("Flood", "Tornado", "Earthquake", "Severe Storm", "Drought", "Hurricane", "Typhoon", "Fire", "Severe Ice Storm", "Freezing",
                                                        "Coastal Storm", "Snowstorm", "Fishing Losses", "Dam/Levee Break", "Mud/Landslide", "Volcanic Eruption", "Toxic Substances",
                                                        "Human Cause", "Terrorist", "Tsunami", "Other", "Chemical", "Biological", "Tropical Storm", "Winter Storm"),
                                    "incident_category_man" = c("Water", "Wind", "Geological", "Wind", "Extreme Weather", "Wind", "Wind", "Fire", "Extreme Weather", "Extreme Weather",
                                                                "Wind", "Extreme Weather", "Human Cause", "Water", "Geological", "Geological", "Other",
                                                                "Human Cause", "Human Cause", "Water", "Other", "Other", "Other", "Wind", "Extreme Weather"))


### Utility functions ----
fips_pad <- function(state_code, county_code){
  return(paste0(str_pad(state_code, width = 2, side = "left", pad = "0"),
                str_pad(county_code, width = 3, side = "left", pad = "0")))
}

fips_unpad <- function(fips_code){
  if (nchar(fips_code) != 5){stop("FIPS must be of length 5.")}
  return(c(as.integer(substr(fips_code, 1, 2)), as.integer(substr(fips_code, 3, 5))))
}

plm_results <- function(plm_obj){
  stopifnot(class(plm_obj)[1] == "plm")
  summ <- summary(plm_obj)
  out <- data.frame("variable" = row.names(summ$coefficients),
                    "estimate" = data.frame(summ$coefficients)[,1],
                    "std_error" = data.frame(summ$coefficients)[,2],
                    "t_value" = data.frame(summ$coefficients)[,3],
                    "p_value" = data.frame(summ$coefficients)[,4])
  out$model <- plm_obj$call$model
  out$rsq <- summary(plm_obj)$r.squared[1]
  out$adj_rsq <- summary(plm_obj)$r.squared[2]
  out$fstatistic <- summary(plm_obj)$fstatistic$statistic
  out$fstat_pvalue <- summary(plm_obj)$fstatistic$p.value
  out$nr_obs <- nrow(plm_obj$model)
  return(out)
}

f_test_results <- function(plm_1, plm_2){
  stopifnot(class(plm_1)[1] == "plm")
  stopifnot(class(plm_2)[1] == "plm")
  hm <- pFtest(plm_1, plm_2)
  out <- data.frame("f_stat" = hm$statistic,
                    "df_1" = hm$parameter[1],
                    "df_2" = hm$parameter[2],
                    "p_value" = hm$p.value)
  return(out)
}

hausman_results <- function(plm_fe, plm_re){
  stopifnot(class(plm_fe)[1] == "plm")
  stopifnot(class(plm_re)[1] == "plm")
  hm <- phtest(plm_fe, plm_re)
  out <- data.frame("chi_sq" = hm$statistic,
                    "df" = hm$parameter,
                    "p_value" = hm$p.value)
  return(out)
}
