library(tidyverse)

### Load FIPS mapping ----
fips_states <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/fips.xlsx", startRow = 1, rows = c(1:52)) |> 
  mutate(state_name = str_to_title(state_name))

fips_counties <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/fips.xlsx", startRow = 53)


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

