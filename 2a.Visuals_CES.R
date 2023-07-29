##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(patchwork)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

ces_growth2 <- ces_growth %>%
  filter(sector == "Mining, Logging, and Construction") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(Year >= 2010) %>% 
  # first sort by year
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         Diff_growth = avg_indicator_employment - lag(avg_indicator_employment), # Difference in employment between years
         AARG = ((Diff_growth / Diff_year)/lag(avg_indicator_employment)) * 100)  

ces_growth2 <- ces_growth2 %>% 
  mutate(avg_AARG = mean(AARG, na.rm = TRUE))


##--1. CES Data: Indexed Growth of Construction----------------------------------------

ces_growth2 %>% 
  ggplot(aes(x=Year, y = avg_indicator_employment)) + 
  # geom_rect(xmin = 1990, xmax = 1991, ymin = 0.78, ymax = 1.2, color = 'grey90', fill = 'grey90', 
  #           alpha = 0.2) +
  # geom_rect(xmin = 2001, xmax = 2002, ymin = 0.78, ymax = 1.2, color = 'grey90', fill = 'grey90', 
  #           alpha = 0.2) +
  # geom_rect(xmin = 2007, xmax = 2009, ymin = 0.78, ymax = 1.2, color = 'grey90', fill = 'grey90', 
  #           alpha = 0.2) +
  # geom_rect(xmin = 2020, xmax = 2020, ymin = 0.78, ymax = 1.2, color = 'grey90', fill = 'grey90', 
  #           alpha = 0.2) +
  geom_line(color = "#FF4900") +
  #geom_hline(yintercept = 80, color = "grey40") +
  scale_y_continuous(limits = c(80,125)) +
  labs(x = "", y = "Number of Employees (1000s)",
    title = "Construction Employment (in thousands) in Greater Philadelphia \nbetween 2010 and 2023",
    subtitle = "This graph shows the level of employment in the Construction, Mining and Extraction industry \nin Greater Philadelphia between 2010 and 2023.",
    caption = "Source: Current Employment Statistics") + 
  # geom_vline(xintercept = 2001, color = "grey80", linetype = "dashed") +
  # geom_vline(xintercept = 2008, color = "grey80", linetype = "dashed") +
  # geom_vline(xintercept = 2020, color = "grey80", linetype = "dashed") +
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
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey50", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))


##--2. CES Data: Industry Size by Employment in Greater Philadelphia------------------- 

# Hole size
hsize <- 5

# donut chart 
ces_proportions %>% 
  ggplot(aes(x = hsize, y = avg_prop_employment, fill = sector)) +
  geom_col(color = NA) +
  scale_fill_manual(values = c("grey80", "grey80", "grey80", "grey80", "grey80",
                               "grey80", "grey80", "grey80", "#1097FF", "grey80",
                               "grey80", "grey80", "grey80", "grey80", "grey80",
                               "grey80", "grey80", "grey80")) +
  geom_text(x = -5.5, y = -28, label = "1.4%", size = 3.5, family = "Georgia", color = "darkslategrey") +
  geom_label(x = 0.5, y = 3, label = " \nConstruction Sector \n 115,480 employees \n ",
             size = 7, family = "Georgia", color = "darkslategrey", fill = "#9dceff") +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  labs(title = "Average Size of Construction Sector in Greater Philadelphia \n(1990-2023)",
       subtitle = "This graph shows the size of the construction sector as a proportion of \nall economic sectors in Greater Philadelphia. Size of each economic sector \nis measured by the number of people employed in that sector. The proportion \nand number of employees represent 33-year average from 1990-2023.",
       caption = "Source: Current Employment Statistics") +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Georgia", color = "darkslategrey"),
        plot.title = element_text(size = 15, margin = margin(b = 10, t = 5), hjust = 0),
        plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 0)),
        plot.caption = element_text(size =    , margin = margin(t = 10), color = "grey50", hjust = 0))

