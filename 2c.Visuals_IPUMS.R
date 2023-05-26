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

##--3a. IPUMS Data: filtering for Self-Employed Respondents-----------------------

ipums_construction <- ipums_construction %>% 
  filter(CLASSWKR == "Self-employed" & 
           IND != 3080) # removing this industry code because of almost all 0 values across years

# combining asian race classification
ipums_construction <- ipums_construction %>% 
  mutate(RACE_adj = as.factor(as.character(RACE_adj)),
         RACE_adj = ifelse(RACE_adj == "Chinese", "Asian", 
                           ifelse(RACE_adj == "Japanese", "Asian",
                                  ifelse(RACE_adj == "Other Asian or Pacific Islander", "Asian",
                                         ifelse(RACE_adj == "Other race, nec", "Other",
                                                ifelse(RACE_adj == "Two major races", "Multi-Ethnic",
                                                       ifelse(RACE_adj == "Three or more major races", "Multi-Ethnic",
                                                              ifelse(RACE_adj == "White", "White",
                                                                     ifelse(RACE_adj == "Black/African American", "Black/African American",
                                                                            ifelse(RACE_adj == "American Indian or Alaska Native", "American Indian or Alaska Native",
                                                                                   ifelse(RACE_adj == "Hispanic", "Hispanic", RACE_adj)))))))))))
# summing employers by race 
ipums_construction <- ipums_construction %>% 
  group_by(YEAR, RACE_adj, region) %>% 
  mutate(Freq_adj = sum(Freq)) %>% 
  distinct(Freq_adj, .keep_all = TRUE)

# calculating proportions of employers by race
ipums_construction <- ipums_construction %>% 
  group_by(YEAR, region) %>% 
  mutate(prop_employers = (Freq_adj/sum(Freq_adj, na.rm = TRUE)) *100)

# changing data to match with ACS data 
ipums_construction <- ipums_construction %>% 
  mutate(RACE_adj = tolower(RACE_adj),
         RACE_adj =ifelse(RACE_adj == "black/african american", "black",
                          ifelse(RACE_adj == "multi-ethnic", "multirace",
                                 ifelse(RACE_adj == "american indian or alaska native", "native",
                                        ifelse(RACE_adj == "other", "otherrace", RACE_adj))))
  ) %>% 
  rename(race = RACE_adj, 
         Year = YEAR) %>% 
  select(Year, race, region, prop_employers)

##--3c. IPUMS Data + ACS Data: Mapping Employer and Ethnic Distributions------------------------

# calculating proportions in acs population data 
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

# calculating proportions for each race 
acs_race_pop <- acs_race_pop %>% 
  group_by(Year, GEOID) %>% 
  mutate(ethnic_prop = (estimate/sum(estimate, na.rm = TRUE)) * 100) %>% 
  select(-estimate)

# filtering for GRPHL
philly_prime <- acs_race_pop %>% 
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
  mutate(region = "Rest of Greater Philadelphia")

# filtering for Philadelphia
philly <- acs_race_pop %>% 
  filter(NAME == "Philadelphia County, Pennsylvania") %>% 
  mutate(region = "Philadelphia")

# binding datasets 
acs_race_pop <- rbind(philly, philly_prime)
acs_race_pop <- acs_race_pop %>% 
  rename(race = variable) %>% 
  group_by(Year, race, region) %>% 
  mutate(avg_ethnic_prop = mean(ethnic_prop, na.rm = TRUE)) %>% 
  distinct(avg_ethnic_prop, .keep_all = TRUE) %>% 
  select(Year, race, region, avg_ethnic_prop)

# merging datasets
ethnic_merge <- ipums_construction %>% 
  inner_join(acs_race_pop, by = c("Year", "race", "region"))

# calculating average difference in race and employer proportions
ethnic_merge <- ethnic_merge %>% 
  mutate(diff_prop = prop_employers - avg_ethnic_prop) %>% 
  group_by(race, region) %>% 
  mutate(avg_diff_prop = mean(diff_prop, na.rm = TRUE)) %>% 
  distinct(avg_diff_prop, .keep_all = TRUE)

##--3d. Barplot Plot of Employer Representation at County Level----

ethnic_merge %>%
  mutate(race = paste0(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race)))) %>% 
  filter(race != "Otherrace" & race != "Native") %>% 
  ggplot(aes(x=reorder(race, avg_diff_prop),  y=avg_diff_prop, fill = region)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#FF8811", "#392F5A")) +
  labs(x = "", y = "Employer Proportion - Population Proportion \n(% points)",
       title = "Employer Representation in Construction Sector \nin Greater Philadelphia (2010 - 2021)",
       subtitle = "This graph shows the representation of employers in the Construction sector by race and ethnicity. \nPositive numbers mean the ethnicity is over-represented in the construction sector compared to \ntheir population proportion in the same region. Negative numbers mean vice versa. The bars are \nalso separated by Philadelphia and rest of Greater Philadelphia excluding Philadelphia.",
       caption = "Source: American Community Survey, IPUMS") +
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



# generating average employer and ethnic distributions
employer_map <- employer_map %>% 
  group_by(race) %>% 
  mutate(avg_ethnicity = mean(ethnic_prop, na.rm = TRUE),
         avg_employer = mean(prop_employers, na.rm = TRUE)
  ) %>% 
  distinct(avg_ethnicity, .keep_all = TRUE) %>% 
  select(-c(ethnic_prop, prop_employers)) %>% 
  arrange(avg_employer)

##--3e. IPUMS Data: Employer Distribution by Age--------------------------------

# Generate color palette
my_colors <- c("#FF9200", "#1097FF", "#FF4900", 
               "#7915FF", "#FFBF00")  

# bar graph of ethnic proportions of employers over time 
ipums_age_fig <- ipums_age %>% 
  plot_ly(
    type = 'bar',
    x =~ YEAR, 
    y =~ age_prop,
    color =~ age_bracket,
    colors = my_colors,
    text =~ age_bracket,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y:.1f}%")
  ) %>% 
  layout(barmode = 'stack',
         title = list(text="<br>      Employer Trends in the Construction Industry<br>      in Greater Philadelphia by Age",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Proportion of Employers (%)"),
         xaxis = list(title = ""),
         legend = list(
           orientation = "h",
           xanchor = "center",
           x = 0.5,
           yanchor = "top",
           y = -0.1
         ),
         annotations = list(
           x = 1.05, # X position of the caption (right side of the plot)
           y = 1.1, # Y position of the caption (top of the plot)
           text = "Source: IPUMS USA Data Estimates", # The text of the caption
           showarrow = FALSE, # Don't show an arrow pointing to the caption
           xref = "paper", # Set the X position reference to the plot area
           yref = "paper", # Set the Y position reference to the plot area
           font = list(size = 9, color = "grey80"), # Set the font size of the caption
           align = "right", # Align the caption to the right
           xanchor = "right", # Anchor the caption to the right side of the plot
           yanchor = "top" # Anchor the caption to the top of the plot
         ),
         margin = list(l = 70, r = 70, b = 50, t = 70)
  )

ipums_age_fig
##--3f. IPUMS Data: Employer Distribution by Gender--------------------------------

# calculating average proportions of male and female employers
ipums_gender <- ipums_gender %>% 
  group_by(SEX, region) %>% 
  mutate(avg_gender_prop = mean(gender_prop, na.rm = TRUE)) %>% 
  distinct(avg_gender_prop, .keep_all = TRUE)

# Create a named vector of data
gender_philly <- c(`% Females`= 4, `% Males`= 96)
gender_phillyprime <- c(`% Females`= 5, `% Males`= 95)

# Create a waffle plot
gr1 <- waffle(gender_philly, rows=10,
              colors = c("#392F5A", "#FF8811"), 
              legend_pos = "bottom") + 
  labs(title = "Philadelphia",
       caption = "Source: Integrated Public Use Microdata Series") + 
  theme(
    text = element_text(family = "Georgia", color = "darkslategrey"),
    plot.title = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 0, color = "grey50"))

gr2 <- waffle(gender_phillyprime, rows=10,
              colors = c("#392F5A", "#FF8811"),
              legend_pos = "bottom") + 
  labs(title = "Rest of Greater Philadelphia") +
  theme(
    text = element_text(family = "Georgia", color = "darkslategrey"),
    plot.title = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 0, color = "grey50"))

gr1 + gr2 + 
  plot_annotation(title = "Proportion of Employers in Construction Sector (%) by Gender",
                  subtitle = "This graph shows the number of employers in the Construction sector by proportion \nacross male and female employers. Each box represents one percentage of the entire \nconstruction industry in each region and the estimates are 16-year average of employer \nproportions by gender.",
                  theme = theme(plot.title = element_text(size = 14, color = "darkslategrey", family = "Georgia"),
                                plot.subtitle = element_text(size = 10, color = "grey40", family = "Georgia"))) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.direction = "horizontal")
 



