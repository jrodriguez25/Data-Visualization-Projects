## Data Visualization (GOVT16-QSS17) Spring 2023
## R Language
## Lab 3, R Language
## Name: Jeremy Rodriguez



library(tidyverse)
library(ggthemes)
library(usmap)
library(patchwork)


# Load Data ---------------------------------------------------------------


entries <- read_csv("data/entries.csv")
meets <- read_csv("data/meets.csv") 


# Functions ---------------------------------------------------------------


clean_data <- function ( tested){
  entries %>% right_join(meets, by = "MeetID") %>%
    mutate(Year =year(as.Date(Date))) %>% 
    select(-Deadlift4Kg, -Squat4Kg, -Bench4Kg) %>% 
    drop_na((contains("Deadlift") | contains("Squat") | contains("Bench") | TotalKg| Age)) %>% 
    filter(Equipment == "Raw" , Tested == tested, MeetCountry== "USA")
}



assign_divisions <- function(data){
  data %>% 
  mutate(Division = case_when(
                              Age >= 20 & Age <24 ~ "Junior",
                              Age >=24 & Age <40 ~ "Open",
                              ))
}

# Create df and clean Divisions --------------------------------------------------------

raw_tested <- clean_data("Yes") %>% assign_divisions()

# Data Wrangling ----------------------------------------------------------

allSexWilksState <- raw_tested %>%
  filter(MeetState != "AR") %>%
  filter(Division =="Open"| Division == "Junior") %>% 
  group_by(MeetState) %>% 
  summarize(median = median(Wilks, na.rm =TRUE))


top_5 <- allSexWilksState %>% 
  top_n(5, median ) %>% 
  arrange((median)) %>% 
  select(MeetState, median)



# GGPLOT ------------------------------------------------------------------
map <- us_map()

state_locations <- allSexWilksState %>% left_join(map, by= c("MeetState"= "abbr"))

med_min_value <- state_locations %>%
  summarize(min_value = min(median, na.rm = TRUE)) %>%
  pull(min_value)

med_max_value <- state_locations %>%
  summarize(max_value = max(median, na.rm = TRUE)) %>%
  pull(max_value)

g1 <- ggplot() +
  geom_polygon(data = state_locations,
               aes(x = x, 
                   y = y,
                   group = group,
                   fill = median), 
               color = "black") +
  scale_fill_gradient(low ='white',
                      high= "blue",
                      na.value = "grey",
                      limits = c(med_min_value, med_max_value))+
  
  theme_map() +
  coord_fixed()+
  theme(legend.position = "bottom")+
  labs(title = "States With the Strongest Powerlifters (By Wilks Score)", 
       fill = "Median Wilks Score")


g2 <- ggplot(data = top_5, aes(x= fct_reorder(MeetState,median), y = median)) +
  geom_col(fill ="darkorchid4") +
  labs(x = "State", y= "Median Wilks Score") +
  coord_flip()+
  geom_text(aes(label= median),color="white",position = position_stack(vjust = 0.5), size = 3)+
  theme_few() 


min_year <-as.character(raw_tested %>% pull((Year)) %>% min())
max_year <- as.character(raw_tested %>% pull((Year)) %>% max())


g3 <- g1+g2 +plot_layout(ncol = 1, heights = c(3, 1)) +
  plot_annotation(title = "What is the Strongest Powerlifting State Determined by Wilks Score?", subtitle = paste("Data Taken from", min_year, "to", max_year, "ages 20-40")) +labs(caption = "Source: OpenPowerlifting.org")
g3




