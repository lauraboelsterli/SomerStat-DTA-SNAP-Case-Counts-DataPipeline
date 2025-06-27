# derek's census api example...
# using census api to graph eligibility 
if(!require("pacman")) install.packages('pacman')
p_load(tidyverse, readxl, dplyr, stringr, tidycensus, magrittr, purrr)


# CENSUS API KEY FROM https://api.census.gov/data/key_signup.html


# ADD SOMERSTAT'S CENSUS API KEY 
api_key <- ""



# Load API key
# census_api_key(api_key, install = TRUE)

# Run this if needed to produce a list of all variable names
# variables <- load_variables(year = acs_year, dataset = "acs5")
# variables <- load_variables(year = 2022, dataset = "acs1/subject")
variables <- load_variables(2022, "acs1/subject")
# filter(vars_2022_acs1_subject, name == "S1701_C01_001")
# vars <- variables %>% 
#   filter(str_detect(name, "S1701_C01")) 
vars <- variables %>% 
  filter(str_detect(name, "S2704"))

# want 2018-2024
# theres no 2024 acs1 release yet...
years <- 2018:2023

acs_data <- data.frame()

# Pull BG-level variables
# pull number under 200% of fpl
census_200_fpl <- map_dfr(years, function(y){
  message("Pulling year: ", y)
  tryCatch({
    census_200_fpl <- get_acs(
      survey = "acs1",
      geography = "state",
      variables = "S1701_C01_042",
      state = "MA",
      year = y,
      output = "wide"
    ) %>%
      select(-ends_with("M")) %>%
      rename(fpl_200 = S1701_C01_042E) %>%
      mutate(year = y)
  }, error = function(e) {
    message("Failed for year ", y, ": ", e$message)
    NULL
  })
})
acs_data <- bind_rows(acs_data, census_200_fpl)


census_200_fpl_somerville <- map_dfr(years, function(y) {
  # if (y == 2020) return(NULL)  # skip 2020, not available via API
  
  message("Pulling year: ", y)
  tryCatch({
    get_acs(
      survey = "acs1",  
      geography = "place",
      variables = "S1701_C01_042",
      state = "MA",
      year = y,
      output = "wide"
    ) %>%
      filter(NAME == "Somerville city, Massachusetts") %>%
      select(-ends_with("M")) %>%
      rename(fpl_200 = S1701_C01_042E) %>%
      mutate(year = y)
  }, error = function(e) {
    message("Failed for year ", y, ": ", e$message)
    NULL
  })
})

acs_data <- bind_rows(acs_data, census_200_fpl_somerville)


# 	S2704_C02_002
census_medicaid_somerville <- map_dfr(years, function(y) {
  # if (y == 2020) return(NULL)  # skip 2020, not available via API
  
  message("Pulling year: ", y)
  tryCatch({
    get_acs(
      survey = "acs1",  
      geography = "place",
      variables = "S2704_C02_006",
      state = "MA",
      year = y,
      output = "wide"
    ) %>%
      filter(NAME == "Somerville city, Massachusetts") %>%
      select(-ends_with("M")) %>%
      rename(medicaid = S2704_C02_006E) %>%
      mutate(year = y)
  }, error = function(e) {
    message("Failed for year ", y, ": ", e$message)
    NULL
  })
})

acs_data <- bind_rows(acs_data, census_medicaid_somerville)


# 	S2704_C02_002
census_medicaid_mass <- map_dfr(years, function(y) {
  # if (y == 2020) return(NULL)  # skip 2020, not available via API
  
  message("Pulling year: ", y)
  tryCatch({
    get_acs(
      survey = "acs1",  
      geography = "state",
      variables = "S2704_C02_006",
      state = "MA",
      year = y,
      output = "wide"
    ) %>%
      select(-ends_with("M")) %>%
      rename(medicaid = S2704_C02_006E) %>%
      mutate(year = y)
  }, error = function(e) {
    message("Failed for year ", y, ": ", e$message)
    NULL
  })
})

acs_data <- bind_rows(acs_data, census_medicaid_mass)

# write_csv(acs_data, "census_data.csv")

# This will produce a table of the selected variables for the selected geographies and state.
# Each variable will have two columns, "E" for estimates and "M" for margins of error.
# It's a good idea to rename columns intuitively so you don't lose track of what's what.

# Delete margin of error columns


# Discard the block group NAME from income table
income %<>% select(-c("NAME"))

# Rename estimate columns
income %<>% rename(hh = "B19001_001E", i10 = "B19001_002E", i10_15 = "B19001_003E", 
                   i15_20 = "B19001_004E", i20_25 = "B19001_005E", i25_30 = "B19001_006E", 
                   i30_35 = "B19001_007E", i35_40 = "B19001_008E", i40_45 = "B19001_009E",
                   i45_50 = "B19001_010E", i50_60 = "B19001_011E", i60_75 = "B19001_012E",
                   i75_100 = "B19001_013E", i100_125 = "B19001_014E", i125_150 = "B19001_015E",
                   i150_200 = "B19001_016E", i200 = "B19001_017E"
)

# This produces a table of households in each 2016-2020 ACS income bucket for every block group in MA.
# Easy!
