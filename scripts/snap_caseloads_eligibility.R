# get yearly snap caseload data and census denominators 

# get the monthly caseload data 

# render the plot 
if(!require("pacman")) install.packages('pacman')
p_load(tidyverse, readxl, dplyr, stringr, tidycensus, magrittr, purrr, ggplot2, scales,lubridate)

# CENSUS API KEY - make your .Renviron file to add it there 
readRenviron(".Renviron")
api_key <- Sys.getenv("CENSUS_API_KEY")

# initializing final df i will append all info to for final dataset
acs_data <- data.frame()

# ------------------Census data-----------------------------------------------
# function for census api calls 
get_census_metric <- function(variable, geography, acs_type, years, state = "MA", place_name = NULL) {
  map_dfr(years, function(y) {
    message("Pulling ", acs_type, " for ", geography, " in ", y)
    tryCatch({
      data <- get_acs(
        survey = "acs1",
        geography = geography,
        variables = variable,
        state = state,
        year = y,
        output = "wide"
      ) %>%
        select(-ends_with("M")) %>%
        rename(acs_value = !!paste0(variable, "E")) %>%
        mutate(year = y, acs_type = acs_type)
      
      if (!is.null(place_name)) {
        data <- data %>%
          filter(NAME == place_name)
      }
      
      return(data)
    }, error = function(e) {
      message("Failed for ", geography, " ", y, ": ", e$message)
      NULL
    })
  })
}

# there's no 2024 acs1 release yet... so change the range once new data is released here!
years <- 2018:2024
# pull denominators for snap eligibility 
acs_data <- bind_rows(
  # pulling the under 200% of fpl metric here
  get_census_metric("S1701_C01_042", "state", "200% FPL", years), 
  get_census_metric("S1701_C01_042", "place", "200% FPL", years, place_name = "Somerville city, Massachusetts"),
  # now pulling the medicaid metrics for MA
  get_census_metric("S2704_C02_006", "state", "Medicaid", years),
  get_census_metric("S2704_C02_006", "place", "Medicaid", years, place_name = "Somerville city, Massachusetts")
)
# could write to csv here if we just wanted census data 
# write_csv(acs_data, "census_data.csv")


# --------------SNAP data-------------------
# Folder containing SNAP files scraped from DTA web
folder_path <- "data/raw_files"
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)


# intilaize df for all the monthly snap caseload data (household and individual levels)
# will also later be used to aggregate to year level to compare with the census metrics 
snap_combined <- data.frame()

# they changed their files structures after 2021, so there are two separate
#functions to extract the data from the dta files
# this is the newer file structure and includes households and individuals in
# a filterable way (in comparison to the old structure- you'll see below what i mean...)
process_new_snap_sheet <- function(file, sheet_name, count_col, type_label) {
  read_excel(file, sheet = sheet_name) %>%
    rename(city = CITY, Date = CYCLE_MONTH, Count = !!sym(count_col)) %>%
    filter(AU_PGM_CD == "SNAP", MEMB_STAT_CD != "CLOSED") %>%
    mutate(
      # make count numeric for merging
      Count = as.numeric(Count),
      # make date a date for merging
      Date = as.Date(Date),
      # to make things filterable under type col
      Type = type_label,
      # not all zip codes have 5 digits and are missing the initial 0, so this 
      # aims to standardize 
      ZIP_CODE = ifelse(nchar(ZIP_CODE) == 4, paste0("0", ZIP_CODE), ZIP_CODE)
    ) %>%
    filter(
      # take out nulls
      !is.na(Count),
      # take out all non numeric zip codes in the zip col (found a couple)
      grepl("^\\d{5}$", ZIP_CODE),
      # this was messing things up as well, this removes the ones the 
      # rhode island and NH zips lol 
      city != "(blank)"
    ) %>%
    select(city, ZIP_CODE, Date, Count, Type)
}

# these are the older dta files, where the data of the file is not in 
# the name of the file or in a column (for the 2018-2021 snap file structure)
# since the file name doesn't actually mean anything
# identify which row has the date info based on the heading (the date is
# one col next to the heading) lol
process_old_snap_sheet_weird_date_extraction <- function(file, sheet_name, city_col, count_col, zip_col, type_label) {
  df <- read_excel(file, sheet = sheet_name)
  
  row_index <- which(df[[1]] == "Data represents caseload during month of:")

  excel_date <- df[[2]][row_index]
  converted_date <- as.Date(as.numeric(excel_date), origin = "1899-12-30")
  
  df_cleaned <- df %>%
    rename(
      city = all_of(city_col),
      Count = all_of(count_col),
      ZIP_CODE = all_of(zip_col)
    ) %>%
    mutate(
      # make count numeric for merging
      Count = as.numeric(Count),
      # make date a date for merging
      Date = as.Date(converted_date),
      Type = type_label,
      # not all zip codes have 5 digits and are missing the initial 0, so this 
      # aims to standardize 
      ZIP_CODE = ifelse(nchar(ZIP_CODE) == 4, paste0("0", ZIP_CODE), ZIP_CODE)
    ) %>%
    filter(
      !is.na(Count),
      grepl("^\\d{5}$", ZIP_CODE),
      # (blank) was keeping some unwanted rows (aka aka zip codes in ri and nh) 
      # in dataset in the newer files so filtering 
      # out here as well just in case 
      city != "(blank)"
    ) %>%
    select(city, ZIP_CODE, Date, Count, Type)
  
  return(df_cleaned)
}

for (file in file_list) {
  cat("Looking into:", file, "\n")
  sheet_names <- excel_sheets(file)
  
  # get individual and household data from the newer files
  # some have a trailing space
  if (any(grepl("^Reported Month Caseload Data", str_squish(sheet_names)))) {
    sheet_name <- sheet_names[grep("^Reported Month Caseload Data", sheet_names)]
    
    snap_combined <- bind_rows(
      snap_combined,
      process_new_snap_sheet(file, sheet_name, "CLIENTS", "Recipients"),
      process_new_snap_sheet(file, sheet_name, "CASES", "Households")
    )
  }

  
  if ("SNAP_RECIPIENTS" %in% sheet_names) {
    result <- process_old_snap_sheet_weird_date_extraction(
      file = file,
      sheet_name = "SNAP_RECIPIENTS",
      city_col = "...2",
      count_col = "...3",
      zip_col = "SNAP Recipients By Zipcode",
      type_label = "Recipients"
    )
    snap_combined <- bind_rows(snap_combined, result)
  }
  
  if ("SNAP_AU's" %in% sheet_names) {
    result <- process_old_snap_sheet_weird_date_extraction(
      file = file,
      sheet_name = "SNAP_AU's",
      city_col = "...2",
      count_col = "...3",
      zip_col = "SNAP AU's By Zipcode",
      type_label = "Households"
    )
    snap_combined <- bind_rows(snap_combined, result)
  }
  
}

# filter out any date later than dec 2024 
snap_combined <- snap_combined %>%
  filter(as.Date(Date) <= as.Date("2024-12-31") & as.Date(Date) > as.Date("2017-12-31"))

# write to csv if you want all monthly snap data for Mass or just Somerville (w city col. filtering) 
# write_csv(snap_combined, "monthly_snap_values.csv")




# ------creating the datasets with yearly acs and snap recipients counts------ 
# make table for  monthly somerville snap recipients
# add counts for all cities per month
snap_monthly_somerville <- snap_combined %>%
  filter(city == "SOMERVILLE", Type == "Recipients") %>%
  mutate(year = year(Date)) %>%
  group_by(year, Date) %>%
  summarize(monthly_total = sum(Count, na.rm = TRUE), .groups = "drop")

# get df of avg annual somerville snap recipients for acs metric table
somerville_snap_recipients_year <- snap_monthly_somerville %>%
  group_by(year) %>%
  summarize(
    snap_sum_total = sum(monthly_total),
    num_months = n(),
    snap_mean = round(snap_sum_total / num_months),
    snap_range = max(monthly_total) - min(monthly_total),
    .groups = "drop"
  ) %>%
  mutate(NAME = "Somerville city, Massachusetts")

# do same thing for all of Massachusetts 
snap_monthly_mass <- snap_combined %>%
  filter(Type == "Recipients") %>%
  mutate(year = year(Date)) %>%
  group_by(year, Date) %>%  # group to one row per month
  summarize(monthly_total = sum(Count, na.rm = TRUE), .groups = "drop")

massachusetts_snap_recipients_year <- snap_monthly_mass %>%
  group_by(year) %>%
  summarize(
    snap_sum_total = sum(monthly_total),
    num_months = n(),
    snap_mean = round(snap_sum_total / num_months),
    snap_range = max(monthly_total) - min(monthly_total),
    .groups = "drop"
  ) %>%
  mutate(NAME = "Massachusetts")

snap_recipients_year <- data.frame()
# add yearly somerville and mass counts into one df 
snap_recipients_year <- bind_rows(
  somerville_snap_recipients_year,
  massachusetts_snap_recipients_year
)
# now add the yearly acs eligibility estimates to the same df for one complete 
# yearly counts table
acs_data_with_snap <- acs_data %>%
  left_join(snap_recipients_year, by = c("NAME", "year"))
# removing totals column to avoid confusion when looking for yearly values 
# we decided on (average of total recipients over each year (calculated above))
acs_data_with_snap <- acs_data_with_snap %>% select(-snap_sum_total)  # drops the column

# create a file that includes yearly snap recipient count for Mass., Somerville, and fpl and medicaid metrics from the ACS (2018-2024)
# write_csv(acs_data_with_snap, "yearly_snap_acs_values.csv", quote = "all")



# ------plotting-------
# plot of monthly somerville snap recipients (monthly)
ggplot(snap_monthly_somerville, aes(x = Date, y = monthly_total)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Somerville SNAP Recipients",
    x = "Month",
    y = "Number of Recipients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma, limits = c(0, NA)) # fixing the labels 


# plot of avg. annual somerville snap recipients (yearly)
ggplot(somerville_snap_recipients_year, aes(x = year, y = snap_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yearly Average of Total Somerville SNAP Recipients",
    x = "Year",
    y = "Number of Recipients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma, limits = c(0, NA)) # fixing the labels 

# plot of monthly mass. snap recipients (monthly)
ggplot(snap_monthly_mass, aes(x = Date, y = monthly_total)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Massachusetts SNAP Recipients",
    x = "Month",
    y = "Number of Recipients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma, limits = c(0, NA)) # fixing the labels 

# plot of avg. annual mass, snap recipients (yearly)
ggplot(massachusetts_snap_recipients_year, aes(x = year, y = snap_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Yearly Average of Total Massachusetts SNAP Recipients",
    x = "Year",
    y = "Number of Recipients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma, limits = c(0, NA)) # fixing the labels 

# plot of avg. annual mass/somerville snap recipients and acs estimates (yearly)
ggplot(acs_data_with_snap, aes(x = year)) +
  geom_line(aes(y = acs_value, color = "ACS Estimated Eligible")) +
  geom_line(aes(y = snap_mean, color = "Average SNAP Recipients")) +
  facet_wrap(~NAME, scales = "free_y") +
  labs(
    title = "ACS Estimated SNAP Eligibility vs. Actual SNAP Recipients",
    x = "Year",
    y = "Number of People",
    color = "Source"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)