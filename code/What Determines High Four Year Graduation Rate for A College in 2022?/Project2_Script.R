## Data Visualization (GOVT16-QSS17) Spring 2023
## Lab_Session02
##
## Name: Jeremy Rodriguez
## Date: May 16, 2023


# Load packages -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(usmap)
library(maps)
library(patchwork)
library(ggthemes)


folder <- "data/"

fig_h <- 8
fig_w <- 11



# Load Dictionary and Data Frames -----------------------------------------

df1 <- read_csv(str_c(folder, "Most-Recent-Cohorts-Institution.csv"), na = "NULL")         
dic <- read_xlsx(str_c(folder,"CollegeScorecardDataDictionary.xlsx"), 
                 sheet = 4)

#Functions -------------------------------------------------------

#Quartiles Used to categorize
calculate_quartiles <- function(df, var){
  values <- c(min(df[[var]], na.rm = TRUE), max(df[[var]], na.rm = TRUE))
  quartiles <- quantile(as.numeric(values), probs = c(0.25, 0.5, 0.75))
  
  return(quartiles)
}

#categorize into 4 bins
categorize<- function(var, quartiles){
  ifelse(var < quartiles[1], 1,
         ifelse(var < quartiles[2], 2,
                ifelse(var < quartiles[3], 3, 4)))
}


#Plot Function 

#Citation for Double Curly Brackets: https://thomasadventure.blog/posts/turning-your-ggplot2-code-into-a-function/ 
#Double Curly Brackets for being able to access the x variable in the dataframe

color_bars <- c("red", "lightblue")

plot_func <- function(data, x){
  ggplot(data, aes(x = {{x}}, y=avg_grad, fill =ifelse(avg_grad == max(avg_grad), "highest", "not")))+
    geom_col()+ ylim(0,100)+
    scale_fill_manual(values = color_bars) +
    theme_few()+
    ylab("Average Graduation Rate (%)")+
    theme(legend.position = 'none')
}

# Types of Institutions ------------------------------------------

grad_rate_inst <- df1 %>% 
  mutate(institution_type = case_when(HBCU ==1 ~ "HBCU",
                                      str_detect(INSTNM, "^Harvard University$|^Yale University$|^Princeton University$|^Columbia University$|^University of Pennsylvania$|^Dartmouth College$|^Brown University$|^Cornell University$")~ "Ivy League",
                                      CONTROL ==1 ~ "Public",
                                      CONTROL ==2 ~"Private")) %>% 
           select(INSTNM, institution_type, C150_4, CONTROL) %>% 
           drop_na() %>% 
  group_by(institution_type) %>% 
  summarize(avg_grad = mean(C150_4)*100)


g1 <- plot_func(grad_rate_inst, institution_type)+
  labs(title = "Type of Institution", x = "Institution Type")




# Average Income of Students code -----------------------------------------
#Basis of this code from the sample.R

income_vars <- dic %>% 
  filter(str_detect(`NAME OF DATA ELEMENT`, "household income")) %>% 
  select(`VARIABLE NAME`) %>% 
  pull()

#Check Variables
income_vars

df_income <- df1 %>% 
  mutate(MEDIAN_HH_INC = if_else(MEDIAN_HH_INC == "PrivacySuppressed", NA, MEDIAN_HH_INC)) %>% 
  select(INSTNM, MEDIAN_HH_INC, C150_4) %>% 
  drop_na()

income_quartiles <- calculate_quartiles(df_income, "MEDIAN_HH_INC")


grad_rate_inc <- df_income %>% 
  mutate(med_inc_category = categorize(MEDIAN_HH_INC, income_quartiles)) %>% 
  group_by(med_inc_category) %>% 
  summarize(avg_grad = mean(C150_4)*100)


g2 <- plot_func(grad_rate_inc, med_inc_category)+
  scale_x_continuous(breaks= c(1,2,3,4), labels= c("25th Percentile", 
                                                   "50th Percentile", 
                                                   "75th Percentile", 
                                                   "100th Percentile")) +
  labs(title = "Income", x = "Income Level") 


# Endowment of Institution ------------------------------------------------

endow_vars <- dic %>% 
  filter(str_detect(`NAME OF DATA ELEMENT`, "endowment")) %>% 
  select(`VARIABLE NAME`) %>% 
  pull()

endow_vars

df_endowment <- df1 %>% 
  select(INSTNM, ENDOWBEGIN, C150_4) %>% 
  drop_na()

endowment_quartiles <- calculate_quartiles(df_endowment, "ENDOWBEGIN")


grad_rate_end <- df_endowment %>% 
  mutate(end_category = categorize(ENDOWBEGIN, endowment_quartiles)) %>% 
  group_by(end_category) %>% 
  summarize(avg_grad= mean(C150_4)*100)




g3 <- plot_func(grad_rate_end, end_category)+
  scale_x_continuous(breaks= c(1,2,3,4), labels= c("25th Percentile", "50th Percentile", "75th Percentile", "100th Percentile")) +
  labs(title = "Endowment", x = "Endowment Level") 


# Location of Institution -------------------------------------------------

regions <- read_csv("data/hate-crimes.csv")
states <- us_map()

#Use the hate crime data that has regions to determine region for each state
regions <- states %>% left_join(regions, by = c("full" = "state")) %>% 
  select(region, abbr)

#check for differences
anti_join(states,regions, by = "abbr")

loc_vars <- dic %>% 
  filter(str_detect(`NAME OF DATA ELEMENT`, "State")) %>% 
  select(`VARIABLE NAME`) %>% 
  pull()
#Check variables
loc_vars

#get the region by matching the state abbreviations
grad_rate_loc <- df1 %>% 
  select(INSTNM, STABBR, C150_4) %>% 
  left_join(regions, by = c("STABBR" = "abbr")) %>% 
  drop_na() %>% 
  group_by(region) %>% 
  summarize(avg_grad = mean(C150_4)*100)

g4 <- plot_func(grad_rate_loc, region)+
  labs(title = "Region", x = "Region in the United States")

# Sort plots together with patchwork-----------------------------------------------------

overall_plot <- g1 +g3 +g4 +g2 +
  plot_annotation(title = "What Determines High Four Year Graduation Rate for A College in 2022?", 
                  subtitle = "Colleges with Prestige, High Family Wealth, Large Endowment, and Located in the Northeast tend to have the Highest Graduation Rates",
                  caption= "Source: U.S. Department of Education College Scorecard")

overall_plot

# Save the Plot -----------------------------------------------------------



ggsave("figures/final_fig.pdf", height = fig_h, width = fig_w)

