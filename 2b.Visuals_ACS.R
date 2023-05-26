##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sf)
library(waffle)
library(sfheaders)
library(patchwork)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--2a.1. ACS Data: Ethnic Proportions at County level----------------------------------------------

# filtering for just race variables 
acs_race_employees <- acs_county_dta %>% 
  filter(variable != "total_employees" & 
           variable != "total_female" & 
           variable != "total_male" & 
           variable != "fulltime_female" & 
           variable != "fulltime_male" & 
           variable != "total_white" &
           variable != "total_black" &
           variable != "total_native" &
           variable != "total_asian" &
           variable != "total_hawaii" &
           variable != "total_otherrace" &
           variable != "total_multirace" &
           variable != "total_hispanic"
  )

# Separate ethnicity column into race and gender columns
acs_race_employees <- separate(acs_race_employees, variable, into = c("race", "gender"), sep = "_")

# Summing by ethnicity 
acs_race_employees <- acs_race_employees %>% 
  group_by(Year, GEOID, race) %>% 
  mutate(total_estimate = sum(estimate, na.rm = TRUE)) %>% 
  distinct(total_estimate, .keep_all = TRUE) %>% 
  select(-c(gender, estimate))

# Removing whiteNONHISPANICS 
acs_race_employees <- acs_race_employees %>% 
  filter(race != "whiteNONHISPANIC") %>% 
  mutate(Year = as.factor(Year),
         race = as.factor(race)) 

# Generating proportional employment by race
acs_race_employees <- acs_race_employees %>% 
  group_by(Year, GEOID) %>% 
  mutate(emp_prop = (total_estimate/sum(total_estimate, na.rm = TRUE)) * 100) %>% 
  distinct(emp_prop, .keep_all = TRUE) %>% 
  select(-total_estimate)

##--2a.2. ACS Data: Ethnic Proportions AND Population at County level------------

acs_race_pop <- acs_county_dta %>% 
  filter(variable == "total_white" |
           variable == "total_black" |
           variable == "total_native" |
           variable == "total_asian" |
           variable == "total_hawaii" |
           variable == "total_otherrace" |
           variable == "total_multirace" |
           variable == "total_hispanic")

# creating race variable for merging
acs_race_pop$variable <- sub("total_", "", acs_race_pop$variable)

# renaming race variable 
acs_race_pop <- acs_race_pop %>% 
  rename(race = variable)

# calculating proportions for each race 
acs_race_pop <- acs_race_pop %>% 
  group_by(Year, GEOID) %>% 
  mutate(ethnic_prop = (estimate/sum(estimate, na.rm = TRUE)) * 100) %>% 
  select(-estimate)


##--2a.3. ACS Data: Combining County Ethnic Data and Overall Ethnic Data at County Level-------

# calculating proportions of employees by race in the employee data

ethnic_merge <- acs_race_employees %>% 
  inner_join(acs_race_pop, by = c("Year", "GEOID", "NAME", "race"))

# calculating over/under representation in construction industry by ethnicity 
ethnic_merge <- ethnic_merge %>%
  group_by(Year, GEOID) %>% 
  mutate(diff_prop = emp_prop - ethnic_prop) 

# calculating average difference in representation by county
ethnic_merge <- ethnic_merge %>% 
  group_by(GEOID, race) %>% 
  mutate(avg_diff_prop = mean(diff_prop, na.rm = TRUE)) %>% 
  distinct(avg_diff_prop, .keep_all = TRUE)

##--2a.4. ACS Data: Philadelphia vs. Rest of Greater Philadelphia---------------

# filtering counties in Greater Philadelphia excluding Philadelphia 
philly_prime <- ethnic_merge %>% 
  filter(NAME == "Montgomery County, Pennsylvania" | 
           NAME == "Bucks County, Pennsylvania" |
           NAME == "Chester County, Pennsylvania" |
           NAME == "Delaware County, Pennsylvania" |
           NAME == "Burlington County, New Jersey" |
           NAME == "Camden County, New Jersey" |
           NAME == "Gloucester County, New Jersey" |
           NAME == "New Castle County, Delaware" |
           NAME == "Cecil County, Maryland" | 
           NAME == "Salem County, New Jersey") %>% 
  mutate(region = "Rest of Greater Philadelphia") %>% 
  group_by(race) %>% 
  mutate(region_emp_prop = mean(avg_diff_prop)) # creating average differences across counties excluding Philadelphia 

# filtering Philadelphia 
philly <- ethnic_merge %>% 
  filter(NAME == "Philadelphia County, Pennsylvania") %>% 
  mutate(region = "Philadelphia",
         region_emp_prop = avg_diff_prop)
  
region_employment <- rbind(philly, philly_prime) 
region_employment <- region_employment %>% 
  mutate(race = paste0(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race))))

##--2a.5. ACS Data: Barplot Plot of Representation at County Level----

region_employment %>%
  filter(race != "Otherrace" & race != "Hawaii") %>% 
  ggplot(aes(x=reorder(race, region_emp_prop),  y=region_emp_prop, fill = region)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#3E92CC", "#D8315B")) +
  labs(x = "", y = "Employment Proportion - Population Proportion \n(% points)",
       title = "Proportional Representation in Construction Sector \nin Greater Philadelphia (2010 - 2021)",
       subtitle = "This graph shows the representation of employees in the Construction sector by race and ethnicity. \nPositive numbers mean the ethnicity is over-represented in the construction sector compared to \ntheir population proportion in the same region. Negative numbers mean vice versa. The bars are \nalso separated by Philadelphia and rest of Greater Philadelphia excluding Philadelphia.",
       caption = "Source: American Community Survey") +
  theme_minimal() + 
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
        plot.caption = element_text(size = 10, margin = margin(t = 10), color = "grey50", hjust = 0),
        axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


##--2b.1. ACS Data: Gender Proportions at County level---------------------------------

acs_gender_employees <- acs_county_dta %>% 
  filter(variable == "total_male" | variable == "total_female") %>% 
  group_by(Year, GEOID) %>% 
  mutate(gender_prop = estimate/sum(estimate, na.rm = TRUE)) # proportion of construction workers by gender 


acs_gender_employees <- acs_gender_employees %>% 
  group_by(GEOID, variable) %>% 
  mutate(avg_gender_prop = mean(gender_prop, na.rm = TRUE)) %>% 
  distinct(avg_gender_prop, .keep_all = TRUE)

##--2b.2. ACS Data: Gender Proportions in philadelphia and rest of GRPHL---------------------------------

acs_philly <- acs_gender_employees %>% 
  filter(NAME == "Philadelphia County, Pennsylvania")

acs_phillyprime <- acs_gender_employees %>% 
  filter(NAME == "Montgomery County, Pennsylvania" | 
           NAME == "Bucks County, Pennsylvania" |
           NAME == "Chester County, Pennsylvania" |
           NAME == "Delaware County, Pennsylvania" |
           NAME == "Burlington County, New Jersey" |
           NAME == "Camden County, New Jersey" |
           NAME == "Gloucester County, New Jersey" |
           NAME == "New Castle County, Delaware" |
           NAME == "Cecil County, Maryland" | 
           NAME == "Salem County, New Jersey") %>% 
  group_by(variable) %>% 
  mutate(avg_gender_grphl = mean(avg_gender_prop, na.rm = TRUE))

##--2b.3. ACS Data: Waffle Plot of Gender Proportions---------------------------------

# Create a named vector of data
gender_philly <- c(`% Females`=4, `% Males`= 96)
gender_phillyprime <- c(`% Females`=2, `% Males`= 98)

# Create a waffle plot
gr1 <- waffle(gender_philly, rows=10,
               colors = c("#D8315B", "#3E92CC"),
               legend_pos = "bottom") + 
       labs(title = "Philadelphia",
            caption = "Source: American Community Survey") + 
       theme(
            text = element_text(family = "Georgia", color = "darkslategrey"),
            plot.title = element_text(size = 12, hjust = 0),
            plot.caption = element_text(size = 8, hjust = 0, color = "grey50"))

gr2 <- waffle(gender_phillyprime, rows=10,
            colors = c("#D8315B", "#3E92CC"),
            legend_pos = "bottom") + 
        labs(title = "Rest of Greater Philadelphia") +
        theme(
            text = element_text(family = "Georgia", color = "darkslategrey"),
            plot.title = element_text(size = 12, hjust = 0),
            plot.caption = element_text(size = 8, hjust = 0, color = "grey50"))

gr1 + gr2 + 
  plot_annotation(title = "Proportion of Employees in Construction Sector (%) by Gender",
                  subtitle = "This graph shows the number of people employed in the Construction sector by proportion \nacross male and female employees. Each box represents one percentage of the entire \nconstruction industry in each region and the estimates are 16-year average of employee \nproportions by gender.",
                  theme = theme(plot.title = element_text(size = 14, color = "darkslategrey", family = "Georgia"),
                                plot.subtitle = element_text(size = 10, color = "grey40", family = "Georgia"))) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.direction = "horizontal")
  


