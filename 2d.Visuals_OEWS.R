##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sf)
library(waffle)
library(sfheaders)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--4a. OES Data: Identifying Construction Occupations----------------------------------------

# Occupation titles 
occupation_titles <- oes_philly$OCC_TITLE

# Define regular expressions to match Construction sector occupation titles
construction_regex <- "(?i)(Architect|Building Contractor|Building Inspector|Building Maintenance Technician|Building Surveyor|Carpenter|Concrete Finisher|Construction Equipment Operator|Construction Laborer|Construction Manager|Construction Project Manager|Construction Superintendent|Crane Operator|Drywall|Electrician|Environmental Engineer|Estimator|Fire Sprinkler Installer|Floor Installer|General Contractor|Glazier|Heavy Equipment Operator|HVAC Technician|Interior Designer|Ironworker|Landscape Architect|Landscaper|Mason|Painter|Pipefitter|Plumber|Project Engineer|Real Estate Agent|Roofing Contractor|Roofer|Scaffolder|Sheet Metal Worker|Structural Engineer|Surveyor|Tiler|Welder|Boilermakers|Carpet Installers|Elevator|Fence|Floor Layers|Floor Sanders and Finishers|Hazardous Materials Removal Workers|Highway Maintenance Workers|Insulation Workers|Paving, Surfacing, and Tamping Equipment Operators|Paperhangers|Pile Driver Operators|Pipelayers|Rail-Track Laying and Maintenance Equipment Operators|Reinforcing Iron and Rebar Workers|Rock Splitters, Quarry|Septic Tank Servicers and Sewer Pipe Cleaners|Solar Photovoltaic Installers|Structural Iron and Steel Workers|Tapers|Terrazzo Workers and Finishers|Tile and Marble Setters|Tile and Stone Setters)"

# Use grep function to extract occupation titles that match the Construction sector regex
construction_occupations <- unique(grep(construction_regex, occupation_titles, value = TRUE))

##--4b. OES Data: Function that creates a dummy variable for matching construction occupations-------------

create_construction_dummy <- function(data, data_var_name, matched_occupations) {
  # Initialize a vector of zeros for the dummy variable
  construction_dummy <- rep(0, nrow(data))
  
  # Loop through each occupation in construction_occupations
  for (occupation in construction_occupations) {
    # Check if the data variable matches the occupation using grepl
    occupation_matches <- grepl(occupation, data[[data_var_name]])
    
    # Set the corresponding element of the dummy variable to 1 if there is a match
    construction_dummy[occupation_matches] <- 1
  }
  
  # Add the dummy variable to the data dataframe
  data$construction_dummy <- construction_dummy
  
  # Return the modified data dataframe
  return(data)
}

# Assume you have the 'data' dataframe and 'construction_occupations' vector defined
oes_philly_dummy <- create_construction_dummy(oes_philly, "OCC_TITLE", construction_occupations)

oes_philly_dummy <- oes_philly_dummy %>% 
  select(PRIM_STATE, AREA, OCC_CODE, OCC_TITLE, construction_dummy, year, 
         H_MEAN_real, H_MEDIAN_real, A_MEAN_real, A_MEDIAN_real)

##--4c. OES Data: Filters for Construction Wages and Overall Wages------------------
oes_philly_construction <- oes_philly_dummy %>% 
  filter(construction_dummy == 1 | OCC_TITLE == "All Occupations") %>% 
  group_by(year, construction_dummy) %>% 
  mutate(avg_hwage_mean = mean(H_MEAN_real, na.rm = TRUE),
         avg_hwage_median = mean(H_MEDIAN_real, na.rm = TRUE),
         avg_awage_mean = mean(A_MEAN_real, na.rm = TRUE),
         avg_awage_median = mean(A_MEDIAN_real, na.rm = TRUE)
  ) %>% 
  distinct(avg_hwage_mean, .keep_all = TRUE) %>% 
  gather(wage_type, amount, avg_hwage_mean:avg_awage_median) %>% 
  mutate(year_date = as.Date(paste0(year, "-01-01")),
         wage_type = ifelse(wage_type == "avg_awage_mean", "Mean Annual Wage",
                            ifelse(wage_type == "avg_awage_median", "Median Annual Wage",
                                   ifelse(wage_type == "avg_hwage_mean", "Mean Hourly Wage",
                                          ifelse(wage_type == "avg_hwage_median", "Median Hourly Wage", wage_type))))
  ) 

##--4d. OES Data: Line Plots of Wages--------------------------------------------

oes_philly_construction %>% 
  mutate(construction_dummy = ifelse(construction_dummy == 1, "Construction Sector", "All Occupations")) %>% 
  ggplot(aes(x=year_date, y = amount, color = as.factor(construction_dummy))) + 
  geom_line() +  
  facet_wrap(~wage_type, scale = "free_y") + 
  scale_color_manual(values = c("#FB3640", "#0A2463")) +
  scale_y_continuous(labels = function(x) paste0("$", x)) + 
  labs(y = "Wages", x = "", 
       title = "Wages in Greater Philadelphia for the Construction Sector",
       caption = "Note: All wages are inflation-adjusted to 2022 dollars. \nSource: OES Data") +
  theme_light() + 
  theme(axis.title = element_blank(),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.direction="horizontal",
        legend.text = element_text(),
        text = element_text(family = "Georgia"),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 15, margin = margin(b = 10, t = 5), color = "darkslategrey", hjust = 0),
        plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey50", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))




##--4e. OES Data: Bar Plots of Average Wages------------------------------------

oes_barplot <- oes_philly_construction %>% 
  group_by(construction_dummy, wage_type) %>% 
  mutate(average_amount = mean(amount)) %>% 
  distinct(average_amount, .keep_all = TRUE) 

# BARPLOT OF AVERAGE WAGES

oes_barplot %>% 
  filter(wage_type == "Median Hourly Wage" | wage_type == "Median Annual Wage") %>% 
  mutate(construction_dummy = ifelse(construction_dummy == 1, "Construction Sector", "All Occupations")) %>% 
  ggplot(aes(x=as.factor(construction_dummy), y=average_amount, fill = as.factor(construction_dummy))) +
  geom_col(width = 0.7) +
  facet_wrap(~wage_type, scale = "free_y") + 
  scale_fill_manual(values = c("#0A2463", "#FB3640")) +
  scale_y_continuous(labels = function(x) paste0("$", x)) + 
  labs(y = "Wages", x = "", 
       title = "Median Wages in Greater Philadelphia for the Construction Sector (2012 - 2022)",
       caption = "Note: All wages are inflation-adjusted to 2022 dollars. \nSource: OES Data") +
  theme_light() + 
  theme(axis.title = element_blank(),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.direction="horizontal",
        legend.text = element_text(),
        text = element_text(family = "Georgia"),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 15, margin = margin(b = 10, t = 5), color = "darkslategrey", hjust = 0),
        plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey50", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))



##--4f. OES Data: Cleveland Plot of Wage differences-----------------------

oes_philly_construction <- oes_philly_dummy %>% 
  filter(construction_dummy == 1 | OCC_TITLE == "All Occupations") %>% 
  group_by(year, construction_dummy) %>% 
  mutate(avg_hwage_mean = mean(H_MEAN_real, na.rm = TRUE),
         avg_hwage_median = mean(H_MEDIAN_real, na.rm = TRUE),
         avg_awage_mean = mean(A_MEAN_real, na.rm = TRUE),
         avg_awage_median = mean(A_MEDIAN_real, na.rm = TRUE)
  ) %>% 
  distinct(avg_hwage_mean, .keep_all = TRUE) %>% 
  mutate(year_date = as.Date(paste0(year, "-01-01"))
  ) 

# ANNUAL WAGES 
oes_annualwage <- oes_philly_construction %>% 
  select(PRIM_STATE, AREA, OCC_CODE, OCC_TITLE, construction_dummy, year, year_date, avg_awage_median) %>% 
  group_by(year) %>% 
  mutate(value1 = avg_awage_median[1],
         value2 = avg_awage_median[2]) %>% 
  distinct(value1, .keep_all = TRUE) 

# LOLLIPOP PLOT OF ANNUAL WAGES
ggplot(oes_annualwage) +
  geom_segment( aes(x=year_date, xend=year_date, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=year_date, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=year_date, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")








