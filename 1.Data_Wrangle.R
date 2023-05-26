##--libraries-------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(readxl)
library(ipumsr)
library(survey)
library(readr)
options(scipen=999)

##--1a. CES data clean-----------------------------------------------------------------

# define a cleaning function
ces_clean <- function(filename) {
  
  data <- read.csv(file = filename)

  data <- data %>% 
    mutate(sector = Col_2[8],
           data_type = Col_2[10])
  
  data$sector[13] <- "sector"
  data$data_type[13] <- "data_type"
  
  data <- data[-c(1,2,3,4,5,6,7,8,9,10,11,12),] # removing empty rows in the dataa
  colnames(data) <- as.character(unlist(data[1, ])) # set column names based on first row values
  data <- data[-1,]
  
  data <- data %>% 
    gather(month, values, Jan:Dec) %>% 
    mutate(values = as.numeric(values)
           ) %>% 
    select(Year, sector, values)
}

# creating filenames for the datasets
filenames <- list.files(path = "Data/CES",
                        pattern = ".csv",
                        full.names = TRUE)

# passing ces_clean function through all the data files
cleaned_data <- lapply(filenames, function(x) {
  
  # Apply your custom cleaning function to the data
  cleaned_data <- ces_clean(x)
  
  # Return the cleaned data
  return(cleaned_data)
})

# Combine the cleaned data frames into a single master data frame
master_ces_data <- do.call(rbind, cleaned_data)

##--1b. CES data: creating sector historical proportions--------------------------------

# calculating annual employment in each sector
master_ces_data <- master_ces_data %>% 
  group_by(Year, sector) %>% 
  mutate(annual_employment = mean(values, na.rm = TRUE)) %>% 
  distinct(annual_employment, .keep_all = TRUE)

# calculating proportional employment in each sector per year
master_ces_data <- master_ces_data %>% 
  group_by(Year) %>%
  mutate(prop_employment = annual_employment/sum(annual_employment))

# calculating average proportional employment in each sector for the entire time period
master_ces_data <- master_ces_data %>% 
  group_by(sector) %>% 
  mutate(avg_prop_employment = mean(prop_employment, na.rm = TRUE))  



##--1c. CES data: filter data for visuals---------------------------------------

# data for pie chart proportions
ces_proportions <- master_ces_data %>% 
  distinct(avg_prop_employment, .keep_all = TRUE)
  
# data for indexed growth 
ces_growth <- master_ces_data %>% 
  mutate(construction_indicator = ifelse(sector == "Mining, Logging, and Construction", "Construction", "Overall Economy")) %>% 
  group_by(Year, construction_indicator) %>% 
  mutate(avg_indicator_employment = mean(annual_employment, na.rm = TRUE)) %>% 
  distinct(avg_indicator_employment, .keep_all = TRUE) 

ces_growth <- ces_growth %>% 
  mutate(Year = as.character(Year)) %>% 
  group_by(construction_indicator) %>% 
  mutate(indx_growth = avg_indicator_employment/(avg_indicator_employment[which(Year == "1990.0")]))

  
##--2a. ABS data import-----------------------------------------------------------------

abs_data <- read_csv("Data/abs_data.csv")
abs_data <- abs_data[-1,]

##--3a. IPUMS data----------------------------------------------------------------------

# Reading the IPUMS metadata
ddi <- read_ipums_ddi("Data/usa_00024.xml")
ipums_view(ddi)

# The dat.gz is the dataset downloaded from the IPUMS website
ipums_data <- ipumsr::read_ipums_micro(
  ddi = "Data/usa_00024.xml",
  data_file = "Data/usa_00024.dat.gz"
)

##--3b. IPUMS data filters----------------------------------------------------------------------

ipums_data <- ipums_data %>% 
  filter(!is.na(MET2013)) # removing years that do not have Metropolitan areas defined 

ipums_data <- ipums_data %>% 
  mutate(
    SEX = as_factor(SEX),
    EDUC = as_factor(EDUC), 
    
    RACE = as_factor(RACE),
    RACED = as_factor(RACED),
    
    HISPAN = as_factor(HISPAN),
    HISPAND = as_factor(HISPAND), 
       
    EMPSTAT = as_factor(EMPSTAT),
    CLASSWKR = as_factor(CLASSWKR),
    IND = as_factor(IND),
    OCC = as_factor(OCC),
    
    OCC = recode(OCC,
                    `10` = "Chief executives and legislators",
                    `20` = "General and operations managers",
                    `51` = "Marketing managers",
                    `52` = "Sales managers",
                    `102` = "Facilities managers",
                    `110` = "Computer and information systems managers",
                    `120` = "Financial managers",
                    `136` = "Human resources managers",
                    `140` = "Industrial production managers",
                    `150` = "Purchasing managers",
                    `160` = "Transportation, storage, and distribution managers",
                    `220` = "Construction managers",
                    `230` = "Education and childcare administrators",
                    `310` = "Food service managers",
                    `335` = "Entertainment and recreation managers",
                    `350` = "Medical and health services managers",
                    `360` = "Natural sciences managers",
                    `410` = "Property, real estate, and community association managers",
                    `420` = "Social and community service managers",
                    `425` = "Emergency management directors",
                    `440` = "Other managers",
                    `530` = "Purchasing agents, except wholesale, retail, and farm products",
                    `540` = "Claims adjusters, appraisers, examiners, and investigators",
                    `565` = "Compliance officers",
                    `600` = "Cost estimators",
                    `630` = "Human resources workers",
                    `650` = "Training and development specialists",
                    `700` = "Logisticians",
                    `705` = "Project management specialists",
                    `710` = "Management analysts",
                    `726` = "Fundraisers",
                    `735` = "Market research analysts and marketing specialists",
                    `750` = "Business operations specialists, all other",
                    `800` = "Accountants and auditors",
                    `810` = "Property appraisers and assessors",
                    `845` = "Financial and investment analysts",
                    `850` = "Personal financial advisors",
                    `860` = "Insurance underwriters",
                    `930` = "Tax examiners and collectors, and revenue agents",
                    `940` = "Tax preparers",
                    
                    `1006` = "Computer systems analysts",
                    `1010` = "Computer programmers",
                    `1021` = "Software developers",
                    `1032` = "Web and digital interface designers",
                    `1050` = "Computer support specialists",
                    `1108` ="Computer occupations, all other",
                    `1220` ="Operations research analysts",
                    `1240` ="Other mathematical science occupations",
                    `1320` ="Aerospace engineers",
                    `1360` ="Civil engineers",
                    `1420` = "Environmental engineers",
                    `1430` = "Industrial engineers, including health and safety",
                    `1530` = "Other engineers",
                    `1610` = "Biological scientists",
                    `1650` = "Other life scientists",
                    `1720` = "Chemists and materials scientists",
                    `1745` = "Environmental scientists and specialists, including health",
                    `1760` = "Physical scientists, all other",
                    `1860` = "Other social scientists",
                    `1910` = "Biological technicians",
                    `1920` = "Chemical technicians",
                    `1970` = "Other life, physical, and social science technicians",
                    `2001` = "Substance abuse and behavioral disorder counselors",
                    `2002` = "Educational, guidance, and career counselors and advisors",
                    `2004` = "Mental health counselors",
                    `2014` = "Social workers, all other",
                    `2015` = "Probation officers and correctional treatment specialists",
                    `2016` = "Social and human service assistants",
                    `2025` =  "Other community and social service specialists",
                    `2040` = "Clergy",
                    `2050` = "Directors, religious activities and education",
                    `2060` = "Religious workers, all other",
                    `2100` = "Lawyers, and judges, magistrates, and other judicial workers",
                    `2145` = "Paralegals and legal assistants",
                    `2170` = "Title examiners, abstractors, and searchers",
                    `2180` = "Legal support workers, all other",
                    `2205` = "Postsecondary teachers",
                    `2300` = "Preschool and kindergarten teachers",
                    `2310` = "Elementary and middle school teachers",
                    `2320` = "Secondary school teachers",
                    `2330` = "Special education teachers",
                    `2350` = "Tutors",
                    `2360` = "Other teachers and instructors",
                    `2400` = "Archivists, curators, and museum technicians",
                    `2545` = "Teaching assistants", 
                    `2555` = "Other educational instruction and library workers", 
                    `2600` = "Artists and related workers",
                    `2634` = "Graphic designers",
                    `2635` = "Interior designers",
                    `2640` = "Other designers",
                    `2710` = "Producers and directors",
                    `2722` = "Coaches and scouts",
                    `2740` = "Dancers and choreographers",
                    `2752` = "Musicians and singers",
                    `2810` = "News analysts, reporters, and journalists", 
                    `2825` = "Public relations specialists",
                    `2830` = "Editors",
                    `2840` = "Technical writers",
                    `2850` = "Writers and authors",
                    `2861` = "Interpreters and translators",
                    `2910` = "Photographers",
                    `2920` = "Television, video, and film camera operators and editors",
                    `3010` = "Dentists",
                    `3050` = "Pharmacists",
                    `3090` = "Physicians",
                    `3100` = "Surgeons",
                    `3110` = "Physician assistants", 
                    `3140` = "Audiologists",
                    `3150` = "Occupational therapists",
                    `3160` = "Physical therapists",
                    `3230` = "Speech-language pathologists",
                    `3245` = "Other therapists",
                    `3250` = "Veterinarians",
                    `3255` = "Registered nurses",
                    `3256` = "Nurse anesthetists",
                    `3258` = "Nurse practitioners, and nurse midwives",
                    `3300` = "Clinical laboratory technologists and technicians",
                    `3322` = "Diagnostic medical sonographers",
                    `3323` = "Radiologic technologists and technicians",
                    `3421` = "Pharmacy technicians",
                    `3500` = "Licensed practical and licensed vocational nurses", 
                    `3515` = "Medical records specialists",
                    `3550` = "Other healthcare practitioners and technical occupations",
                    `3601` = "Home health aides",
                    `3602` = "Personal care aides",
                    `3603` = "Nursing assistants",
                    `3620` = "Physical therapist assistants and aides",
                    `3630` = "Massage therapists",
                    `3640` = "Dental assistants",
                    `3646` = "Medical transcriptionists",
                    `3649` = "Phlebotomists",
                    `3802` = "Correctional officers and jailers",
                    `3870` = "Police officers",
                    `3930` = "Security guards and gaming surveillance officers",
                    `3960` = "Other protective service workers",
                    `4000` = "Chefs and head cooks",
                    `4010` = "First-line supervisors of food preparation and serving workers",
                    `4020` = "Cooks",
                    `4030` = "Food preparation workers",
                    `4040` = "Bartenders",
                    `4055` = "Fast food and counter workers",
                    `4110` = "Waiters and waitresses",
                    `4130` = "Dining room and cafeteria attendants and bartender helpers",
                    `4140` = "Dishwashers",
                    `4210` = "First-line supervisors of landscaping, lawn service, and groundskeeping workers",
                    `4220` = "Janitors and building cleaners",
                    `4230` = "Maids and housekeeping cleaners",
                    `4255` = "Other grounds maintenance workers",
                    `4350` = "Animal caretakers",
                    `4435` = "Other entertainment attendants and related workers",
                    `4521` = "Manicurists and pedicurists",
                    `4540` = "Tour and travel guides",
                    `4600` = "Childcare workers",
                    `4621` = "Exercise trainers and group fitness instructors",
                    `4622` = "Recreation workers",
                    `4640` = "Residential advisors",
                    `4655` = "Personal care and service workers, all other", 
                    `4700` = "First-line supervisors of retail sales workers",
                    `4710` = "First-line supervisors of non-retail sales workers",
                    `4720` = "Cashiers",
                    `4760` = "Retail salespersons",
                    `4810` = "Insurance sales agents",
                    `4820` = "Securities, commodities, and financial services sales agents", 
                    `4850` = "Sales representatives, wholesale and manufacturing",
                    `4920` = "Real estate brokers and sales agents",
                    `4965` = "Sales and related workers, all other",
                    `5000` = "First-line supervisors of office and administrative support workers",
                    `5100` = "Bill and account collectors",
                    `5120` = "Bookkeeping, accounting, and auditing clerks",
                    `5220` = "Court, municipal, and license clerks",
                    `5230` = "Credit authorizers, checkers, and clerks",
                    `5240` = "Customer service representatives",
                    `5300` = "Hotel, motel, and resort desk clerks", 
                    `5320` = "Library assistants, clerical",
                    `5400` = "Receptionists and information clerks",
                    `5410` = "Reservation and transportation ticket agents and travel clerks",
                    `5510` = "Couriers and messengers",
                    `5730` = "Medical secretaries and administrative assistants", 
                    `5740` = "Secretaries and administrative assistants, except legal, medical, and executive",
                    `5810` = "Data entry keyers",
                    `5820` = "Word processors and typists",
                    `5860` = "Office clerks, general",
                    `5920` = "Statistical assistants",
                    `5940` = "Other office and administrative support workers",
                    `6050` = "Other agricultural workers",
                    `6230` = "Carpenters",
                    `6260` = "Construction laborers",
                    `6355` = "Electricians",
                    `6800` = "Derrick, rotary drill, and service unit operators, and roustabouts, oil, gas, and mining",
                    `7200` = "Automotive service technicians and mechanics",
                    `7315` = "Heating, air conditioning, and refrigeration mechanics and installers",
                    `7330` = "Industrial and refractory machinery mechanics",
                    `7340` = "Maintenance and repair workers, general",
                    `7540` = "Locksmiths and safe repairers",
                    `7610` = "Helpers--installation, maintenance, and repair workers",
                    `7750` = "Other assemblers and fabricators",
                    `7800` = "Bakers",
                    `7810` = "Butchers and other meat, poultry, and fish processing workers",
                    `7840` = "Food batchmakers",
                    `8030` = "Machinists",
                    `8140` = "Welding, soldering, and brazing workers",
                    `8255` = "Printing press operators",
                    `8300` = "Laundry and dry-cleaning workers",
                    `8350` = "Tailors, dressmakers, and sewers",
                    `8610` = "Stationary engineers and boiler operators",
                    `8740` = "Inspectors, testers, sorters, samplers, and weighers",
                    `8760` = "Dental and ophthalmic laboratory technicians and medical appliance technicians",
                    `8800` = "Packaging and filling machine operators and tenders",
                    `9050` = "Flight attendants",
                    `9130` = "Driver/sales workers and truck drivers",
                    `9142` = "Taxi drivers",
                    `9300` = "Sailors and marine oilers, and ship engineers",
                    `9310` = "Ship and boat captains and operators", 
                    `9415` = "Passenger attendants",
                    `9610` = "Cleaners of vehicles and equipment", 
                    `9620` = "Laborers and freight, stock, and material movers, hand",
                    `9640` = "Packers and packagers, hand",
                    `9645` = "Stockers and order fillers",
                    `9720` = "Refuse and recyclable material collectors",
                    `9830` = "Military, rank not specified"
       ))


ipums_phillymetro <- ipums_data %>% 
  filter(MET2013 == 37980) # filtering for just Philadelphia metro

ipums_philly <- ipums_data %>% 
  filter(STATEFIP == 42 & COUNTYFIP == 101) # filtering for just Philadelphia county

ipums_phillyprime <- ipums_data %>% 
  filter((STATEFIP == 42 & COUNTYFIP == 91) | # montgomery, pa
           (STATEFIP == 42 & COUNTYFIP == 17) | # buck, pa
            (STATEFIP == 42 & COUNTYFIP == 29) | # chester, pa
              (STATEFIP == 42 & COUNTYFIP == 45) | # delaware, pa
                 (STATEFIP == 34 & COUNTYFIP == 5) | # burlington, nj
                    (STATEFIP == 34 & COUNTYFIP == 7) | # camden, nj
                       (STATEFIP == 34 & COUNTYFIP == 15) | # gloucester, nj
                          (STATEFIP == 34 & COUNTYFIP == 33) | # salem, nj
                             (STATEFIP == 10 & COUNTYFIP == 3) | # new castle, de
                                (STATEFIP == 24 & COUNTYFIP == 15) # cecil, md
         )


##--3c. IPUMS Survey weights--------------------------------------------------------------------------
dtaDesign_philly <- svydesign(id   = ~CLUSTER,
                           strata  = ~STRATA,
                           weights = ~PERWT,
                           nest    = TRUE,
                           data    = ipums_philly)

dtaDesign_phillyprime <- svydesign(id = ~CLUSTER,
                                   strata  = ~STRATA,
                                   weights = ~PERWT,
                                   nest    = TRUE,
                                   data    = ipums_phillyprime)

##--3d. IPUMS Cross tabulations - Philadelphia and GRPHL: RACE -------------------------------------------------------

# JUST PHILADELPHIA 
occ_table <- svytable(~YEAR+RACE+HISPAN+IND+CLASSWKR, design = dtaDesign_philly)
occ_data <- as.data.frame(occ_table)

ipums_clean <- function(filename) {
  dat <- filename
  
  dat <- dat %>% 
    filter(IND == 770 | IND == 3080) %>% # filtering for construction industry 
    mutate(RACE_adj = ifelse((RACE == "White" & HISPAN == "Not Hispanic"), "White", 
                             ifelse((RACE == "Black/African American" & HISPAN == "Not Hispanic"), "Black/African American",
                                    ifelse((RACE == "Other Asian or Pacific Islander" & HISPAN == "Not Hispanic"), "Other Asian or Pacific Islander",
                                           ifelse((RACE == "Other race, nec" & HISPAN == "Not Hispanic"), "Other race, nec",
                                                  ifelse((RACE == "Two major races" & HISPAN == "Not Hispanic"), "Two major races",
                                                         ifelse((RACE == "American Indian or Alaska Native" & HISPAN == "Not Hispanic"), "American Indian or Alaska Native",
                                                                ifelse((RACE == "Chinese" & HISPAN == "Not Hispanic"), "Chinese",
                                                                       ifelse((RACE == "Japanese" & HISPAN == "Not Hispanic"), "Japanese",
                                                                              ifelse((RACE == "Three or more major races" & HISPAN == "Not Hispanic"), "Three or more major races", "Hispanic")))))))))) %>% 
    filter(HISPAN != "Not Reported")
}

ipums_construction_philly <- ipums_clean(occ_data) 
ipums_construction_philly$region <- "Philadelphia"

# GRPHL WITHOUT PHILADELPHIA 
occ_table <- svytable(~YEAR+RACE+HISPAN+IND+CLASSWKR, design = dtaDesign_phillyprime)
occ_data <- as.data.frame(occ_table)

ipums_construction_phillyprime <- ipums_clean(occ_data)
ipums_construction_phillyprime$region <- "Rest of Greater Philadelphia"

# MERGING IPUMS DATASETS TOGETHER 
ipums_construction <- rbind(ipums_construction_philly, ipums_construction_phillyprime)

  
##--3e. IPUMS Cross tabulations: AGE -------------------------------------------------------

# # Identifying distribution of construction employers by age 
# occ_table <- svytable(~YEAR+AGE+IND+CLASSWKR, design = dtaDesign)
# occ_data <- as.data.frame(occ_table)
# 
# # creating age brackets 
# ipums_age <- occ_data %>% 
#   mutate(AGE = as.numeric(AGE),
#     age_bracket = ifelse(AGE < 20, "Less than 20 years", 
#                               ifelse((AGE >= 20 & AGE < 40), "20-39",
#                                      ifelse((AGE >=40 & AGE < 60), "40-59",
#                                             ifelse((AGE >= 60 & AGE < 80), "60-79",
#                                                     ifelse(AGE >= 80, "80-100", AGE
#                                      )))))) %>% 
#   filter(IND == 770 & # filtering for construction industry employers
#          CLASSWKR == "Self-employed") %>% 
#   group_by(YEAR, age_bracket) %>% 
#   mutate(Freq_sum = sum(Freq)) %>% 
#   distinct(Freq_sum, .keep_all = TRUE) %>% 
#   group_by(YEAR) %>% 
#   mutate(age_prop = Freq_sum/sum(Freq_sum) * 100)
# 
# # 

##--3f. IPUMS Cross tabulations: GENDER -------------------------------------------------------

# JUST PHILADELPHIA 
occ_table <- svytable(~YEAR+SEX+IND+CLASSWKR, design = dtaDesign_philly)
occ_data <- as.data.frame(occ_table)
ipums_gender_philly <- occ_data %>% 
  mutate(region = "Philadelphia")

# GRPHL WITHOUT PHILADELPHIA 
occ_table <- svytable(~YEAR+SEX+IND+CLASSWKR, design = dtaDesign_phillyprime)
occ_data <- as.data.frame(occ_table)
ipums_gender_phillyprime <- occ_data %>% 
  mutate(region = "Rest of Greater Philadelphia")

# MERGING DATASETS
ipums_gender <- rbind(ipums_gender_philly, ipums_gender_phillyprime)

# creating age brackets 
ipums_gender <- ipums_gender %>% 
  filter(IND == 770 & # filtering for construction industry employers
         CLASSWKR == "Self-employed") %>% 
  group_by(YEAR, region) %>% 
  mutate(gender_prop = Freq/sum(Freq) * 100) 

##--4a. ACS data: Setting variable and year list for ACS-----------------------------------------
varlist19 <- load_variables(year = 2019, dataset = "acs1")

varlist <- c(
             total_white = "B02001_002",
             total_black = "B02001_003",
             total_native = "B02001_004",
             total_asian = "B02001_005",
             total_hawaii = "B02001_006",
             total_otherrace = "B02001_007",
             total_multirace = "B02001_008",
             total_hispanic = "B03001_003",
             
                
             total_employees = "B24060_031", # Total Construction Workers
             
             total_male = "B24010_117", # Male 
             total_female = "B24010_268", # Female 
             
             fulltime_male = "B24020_117", # Full time Male
             fulltime_female = "B24020_268", # Full time Female
            
             white_male = "B24010A_032", # White Male 
             white_female = "B24010A_068", # White Female 
             
             black_male = "B24010B_032", # Black Male 
             black_female = "B24010B_068", # Black Female 
             
             native_male = "B24010C_032", # Native Male
             native_female = "B24010C_068", # Native Female
             
             asian_male = "B24010D_032", # Asian Male
             asian_female = "B24010D_068", # Asian Female
             
             hawaii_male = "B24010E_032", # Hawaii Male
             hawaii_female = "B24010E_068", # Hawaii Female
             
             otherrace_male = "B24010F_032", # Other race Male
             otherrace_female = "B24010F_068", # Other race Female
             
             multirace_male = "B24010G_032", # 2 or more races Male
             multirace_female = "B24010G_068", # 2 or more races Female
             
             whiteNONHISPANIC_male = "B24010H_032", # White alone, Not Hispanic Male
             whiteNONHISPANIC_female  = "B24010H_068", # White alone, Not Hispanic Female
             
             hispanic_male = "B24010I_032", # Hispanic Male
             hispanic_female = "B24010I_068" # Hispanic Female
             )

# setting years 
years <- lst(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021)

##--4b. ACS data Extract---------------------------------------------------------
acs_metro_dta <- map_dfr(
  years,
  ~ get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    variables = varlist,
    year = .x,
    survey = "acs1",
    geometry = FALSE
  ),
  .id = "Year"  
) %>%
  select(-moe) %>%
  arrange(variable, NAME) 

# geo-mapping data for just 2021
acs_map_dta <- get_acs(
    geography = "county",
    variables = "B01003_001",
    year = 2021,
    survey = "acs1",
    geometry = TRUE
  ) %>% 
  mutate(GEOID = as.integer(GEOID))
  
# county level data for the same variables 
acs_county_dta <- map_dfr(
  years,
  ~ get_acs(
    geography = "county",
    variables = varlist,
    year = .x,
    survey = "acs1",
    geometry = FALSE
  ),
  .id = "Year"  
) %>%
  select(-moe) %>%
  arrange(variable, NAME) 

##--5a. OES data import-------------------------------------------------------------

# Extract combined OES file from the zip archive to a temporary directory
oes_data <- read_csv("Data/oes1221_allmetrosoccs_20220425.csv.zip", show_col_types = FALSE)

# Adding OES 2022 data
oes_2022 <- read_excel("Data/MSA_M2022_dl.xlsx")

# adding year to the oes_2022 data to match with master dataset
oes_2022 <- oes_2022 %>% 
  mutate(year = 2022)

# Identify common columns
common_cols <- intersect(names(oes_data), names(oes_2022))

# Bind data frames based on common columns
oes_master <- rbind(oes_data[,common_cols], oes_2022[,common_cols])

##--5b. OES data inflation adjustment-------------------------------------------

# Read the CPI data 
cpi_data <- read_excel("Data/cpi_data.xlsx")
cpi_data <- cpi_data[-c(1,2,3,4,5,6,7,8,9,10),]
names(cpi_data) <- cpi_data[1,]
cpi_data <- cpi_data[-1,]
cpi_data$Annual <- as.numeric(cpi_data$Annual)
cpi_data$Year <- as.numeric(cpi_data$Year)
cpi_data <- cpi_data %>% 
  rename(year = Year)
cpi_2022 <- cpi_data$Annual[cpi_data$year == 2022]

#**** NOTE: THIS CPI IS FOR GREATER PHILADELPHIA ONLY 

# merging cpi data and oes data by year
oes_philly <- oes_master %>% 
  filter(AREA == "37980") %>% 
  left_join(cpi_data, by = "year")

# Applying the inflation factor to wage numbers in OES data 
oes_philly <- oes_philly %>% 
  mutate(H_MEAN = as.numeric(H_MEAN),
         H_MEDIAN = as.numeric(H_MEDIAN),
         A_MEAN = as.numeric(A_MEAN),
         A_MEDIAN = as.numeric(A_MEDIAN),
         
         H_MEAN_real = (H_MEAN / Annual) * cpi_2022,
         H_MEDIAN_real = (H_MEDIAN / Annual) * cpi_2022,
         A_MEAN_real = (A_MEAN / Annual) * cpi_2022,
         A_MEDIAN_real = (A_MEDIAN / Annual) * cpi_2022
         
         )

##--6a. ACS + OES Mapping Data 2021---------------------------------------------

# filtering relevant information for mapping
oes_21 <- oes_data %>% 
  filter(year == 2021 & OCC_CODE == "47-0000") %>% 
  rename(GEOID = AREA)

# merging with the acs map data 
GIS_data <- acs_map_dta %>% 
  inner_join(oes_21, by = "GEOID") %>% 
  select(GEOID, NAME, OCC_TITLE, H_MEAN, H_MEDIAN, A_MEAN, A_MEDIAN)

# storing cpi value for 2021
cpi_2021 <- cpi_data$Annual[cpi_data$year == 2021]

# adjusting wage numbers for inflation 
GIS_data <- GIS_data %>% 
  mutate(H_MEAN = (H_MEAN / cpi_2021) * cpi_2022,
         H_MEDIAN = (H_MEDIAN / cpi_2021) * cpi_2022,
         A_MEAN = (A_MEAN / cpi_2021) * cpi_2022,
         A_MEDIAN = (A_MEDIAN / cpi_2021) * cpi_2022
         )
  


##--7a. QCEW data clean---------------------------------------------------------
# define a cleaning function

qcew_clean <- function(filename) {
  
  data <- read.csv(file = filename)
  
  data <- data %>% 
    mutate(sector = X[7],
           data_type = X[10])
  
  data$sector[13] <- "sector"
  data$data_type[13] <- "data_type"
  
  data <- data[-c(1,2,3,4,5,6,7,8,9,10,11,12),] # removing empty rows in the data
  colnames(data) <- as.character(unlist(data[1, ])) # set column names based on first row values
  data <- data[-1,]
  
  data <- data %>% 
    gather(month, values, Jan:Dec) %>% 
    mutate(values = as.numeric(values)
    ) %>% 
    select(Year, month, sector, values)
}

# applying clean function to philadelphia construction industry
philly_construction <- qcew_clean("Data/QCEW/Philadelphia_County/SeriesReport-20230508173256_1d0869.csv")

# calculating average number of employees in construction sector in Philadelphia
philly_construction <- philly_construction %>% 
  mutate(avg_emp_prop = mean(values, na.rm = TRUE))















