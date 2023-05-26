##--libraries----
library(tidyverse)

##--Creating Function for reading and cleaning data----
read_and_clean_data <- function(folder_path) {
  # Get a list of all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$")
  
  # Initialize a list to store the cleaned dataframes
  cleaned_data <- list()
  
  # Loop over each CSV file
  for (i in seq_along(csv_files)) {
    # Read in the data
    df <- read.csv(file.path(folder_path, csv_files[i]))
    
    # Get the name of the current CSV file (without the .csv extension)
    csv_name <- tools::file_path_sans_ext(csv_files[i])
    
    # Extract the year from the CSV file name
    year <- as.numeric(sub(".*FY(\\d+).*", "\\1", csv_name))
    
    # Add a year column
    df$year <- year
    
    # Your cleaning code here
    
    ##--filtering for counties in Greater Philadelphia----
    df <- df %>% 
      filter((recipient_county_name == "MONTGOMERY" & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "BUCKS" & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "CHESTER" & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "DELAWARE"  & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "BURLINGTON" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "CAMDEN" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "GLOUCESTER" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "NEW CASTLE" & recipient_state_name == "DELAWARE") |
               (recipient_county_name == "CECIL" & recipient_state_name == "MARYLAND") | 
               (recipient_county_name == "SALEM" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "PHILADELPHIA" & recipient_state_name == "PENNSYLVANIA")
      ) 
    
    ##--filtering for infrastructure related government departments----
    df <- df %>% 
      filter(awarding_agency_name == "Department of Transportation" |
               awarding_agency_name == "Department of Energy" |
               awarding_agency_name == "Department of Housing and Urban Development" |
               awarding_agency_name == "Department of Defense" |
               awarding_agency_name == "Environmental Protection Agency" 
      )
    
    ##--filtering relevant variables----
    # df <- df %>% 
    #   select(recipient_state_name, recipient_county_name, recipient_name, recipient_address_line_1, 
    #          period_of_performance_start_date, period_of_performance_current_end_date,
    #          awarding_office_name, funding_office_name, recipient_city_code, awarding_agency_name, federal_action_obligation, 
    #          total_obligated_amount, cfda_title, assistance_type_description, 
    #          business_types_description, year)
    
    ##--summing ammounts by data filters
    df <- df %>% 
      mutate(region = ifelse(recipient_county_name == "PHILADELPHIA", "Philadelphia", "Greater Philadelphia")) 
    
    ##--calculating duration of grants 
    df$period_of_performance_start_date <- as.Date(df$period_of_performance_start_date)
    df$period_of_performance_current_end_date <- as.Date(df$period_of_performance_current_end_date)
    
    df$grant_duration <- as.numeric(difftime(df$period_of_performance_current_end_date, df$period_of_performance_start_date, units = "weeks")) 
    
    ##--calculating time left in funding
    current_date <- Sys.Date()
    df$months_left <- as.numeric(difftime(df$period_of_performance_current_end_date, current_date, units = "weeks")) 
    
    
    # Add the cleaned dataframe to the list
    cleaned_data[[i]] <- df
  }
  
  return(cleaned_data)
}

##--Running the function on all the datasets----
cleaned_data <- read_and_clean_data("Data/Federal_Assistance")

##--Putting dataframe in R's global environment----
names(cleaned_data) <- paste0("df", seq_along(cleaned_data), "_clean")
list2env(cleaned_data, envir = .GlobalEnv)

##--Binding datasets together---- 

# Initialize an empty dataframe to store the combined data
df_grphl <- data.frame()

# Loop over the names of dataframes
for (i in 1:68) {
  # Get the name of the current dataframe
  df_name <- paste0("df", i, "_clean")
  
  # Retrieve the current dataframe from R environment
  df <- get(df_name)
  
  # Bind the current dataframe to the combined data
  df_grphl <- rbind(df_grphl, df)
}


#--saving data set----
write.csv(df_grphl, "Data/clean_fed_funds.csv")
