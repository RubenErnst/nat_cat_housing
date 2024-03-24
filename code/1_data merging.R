rm(list = ls())

library(tidyverse)

# Load FIPS mapping
fips_states <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/fips.xlsx", startRow = 1, rows = c(1:52)) |> 
  mutate(state_name = str_to_title(state_name))

fips_counties <- openxlsx::read.xlsx("../../3_Data/FCC_FIPS/fips.xlsx", startRow = 53)


# Utility functions
fips_pad <- function(state_code, county_code){
  return(paste0(str_pad(state_code, width = 2, side = "left", pad = "0"),
                str_pad(county_code, width = 3, side = "left", pad = "0")))
}

fips_unpad <- function(fips_code){
  if (nchar(fips_code) != 5){stop("FIPS must be of length 5.")}
  return(c(as.integer(substr(fips_code, 1, 2)), as.integer(substr(fips_code, 3, 5))))
}
