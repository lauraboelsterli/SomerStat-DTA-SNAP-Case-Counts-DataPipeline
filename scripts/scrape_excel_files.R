library(httr)
library(fs)  # For file handling
library(lubridate)  # For date manipulation

download_file <- function(url, destfile) {
  # Download to a temporary file first
  temp_file <- tempfile(fileext = ".xlsx")
  response <- GET(url, write_disk(temp_file, overwrite = TRUE))
  
  # Verify if the download was successful before moving the file
  if (http_status(response)$category == "Success" && file_exists(temp_file)) {
    file_move(temp_file, destfile)  # Move only if successful
    message("Download successful: ", destfile)
    return(TRUE) ## return TRUE when the download is successful
  } else {
    message("Download failed: ", url)
    file_delete(temp_file)  # Clean up failed downloads
    return(FALSE) ## return FALSE when there is a download fail
  }
  
}

# Define the base URL pattern
base_url <- "https://www.mass.gov/doc/caseload-by-zip-code-report-%s-%d/download"
## for some reason some of the urls add "-0" at the end so I try that if the normal one fails
backup_url <- "https://www.mass.gov/doc/caseload-by-zip-code-report-%s-%d-0/download"
## you can easily add more formats if it changes in the future again!

# Define the directory to save the files
dir_create("data/raw_files")

# Generate a sequence of months from November 2017 to December 2024
start_date <- ymd("2017-11-01")
# end_date <- ymd("2024-12-01")  # Ensure we stop at December 2024
end_date <- ymd("2025-03-30")  # Ensure we stop at December 2024
dates <- seq(start_date, end_date, by = "months")

# Download each file only if it does not already exist
for (date in dates) {
  # Convert to lowercase month name and year
  date <- as.Date(date)
  month_str <- tolower(format(date, "%B"))
  year_num <- year(date)
  
  # Construct the final file path
  destfile <- path("data/raw_files/", sprintf("caseload_by_zipcode_%s_%d.xlsx", month_str, year_num))
  
  # Skip download if file already exists
  if (file_exists(destfile)) {
    message("File already exists, skipping: ", destfile)
    next
  }
  
  # Construct the URL
  url <- sprintf(base_url, month_str, year_num)
  fallback <- sprintf(backup_url, month_str, year_num)
  
  ## you could continue to string this with more fallback urls
  if (!download_file(url, destfile)) { ## try to download at url, if that fails try the backup (-0 at end)
    download_file(fallback, destfile)
  }
  
  ## clean data and send to Socrata (only for new data)
  
}


  
