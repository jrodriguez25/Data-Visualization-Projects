## Data Visualization (GOVT16-QSS17) Spring 2023
## Lab 01
## Name: Jeremy Rodriguez
## Date: May 9, 2023




# Notes -------------------------------------------------------------------

#I originally wanted to explore how income inequality affected covid cases and
#or deaths but found that the correlation was not as strong as GDP. I decided to 
#keep my wrangling of SWIID since it had helped me limit and specify the 
#countries that will be included in the final figure**


# Load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(modelr)
library(ggrepel)

fig_h <- 8
fig_w <- 11

# Loading and Reorganizing Data----------------------------------------------------------

#Swiid
load('data/swiid9_2.rda')

#Coordinates Map
world_map<- map_data("world") %>% 
  select(!subregion)

#Extract max gini
max_gini_by_country<- swiid_summary %>% 
  group_by(country) %>% 
  summarise(max_gini_disp= max(gini_disp, na.rm = TRUE))

#Extract max deaths and cases by country
owid<- read_csv("data/owid-covid-data.csv") %>% 
  filter(!str_detect(iso_code, "OWID")) %>% 
  group_by(location) %>% 
  summarize(max_deaths = max(total_deaths, na.rm = TRUE),
            max_cases = max(total_cases, na.rm = TRUE))

#Population in 2021 by country
country_population<- read_excel("data/API_SP.POP.TOTL_DS2_en_excel_v2_5447786.xls", 
                                skip = 3) %>% 
  select(`Country Name`, `2021`) %>% rename("country"=`Country Name`, 
                                            "pop" = `2021`)
#GDP per capita by country 2021
gdp <- read_excel("data/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_5358450.xls", skip = 3) %>% 
  select(`Country Name`, `2021`) %>% rename("country"=`Country Name`,
                                            "gdp" = `2021`) 



# Data Wrangling ----------------------------------------------------------

#check which don't match
anti_join(max_gini_by_country, owid, by = c("country" = "location"))

#Try to understand differences
owid %>% filter(str_detect(location, "Cze")) %>% select(location) %>% distinct()

#Used Google to figure out the which Congo's were the democatic republic and the republic
owid %>% filter(str_detect(location, "Con")) %>% select(location) %>% distinct()

owid %>% filter(str_detect(location, "Cot")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Kor")) %>% select(location) %>% distinct()
#Kosovo not in gini
owid %>% filter(str_detect(location, "Kos")) %>% select(location) %>% distinct()

owid %>% filter(str_detect(location, "Micro")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Pal")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Cot")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Sov")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Kitt")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Lucia")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Vinc")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Pri")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Timor")) %>% select(location) %>% distinct()
owid %>% filter(str_detect(location, "Yug")) %>% select(location) %>% distinct()

#Change location names to match
owid <- owid %>% 
  mutate(location = case_when(location == "Czechia" ~ "Czech Republic",
                              location == "Congo" ~ "Congo-Brazzaville", 
                              location == "Democratic Republic of Congo"~ "Congo-Kinshasa",
                              location == "Cote d'Ivoire"~"Côte d'Ivoire",
                              location == "South Korea" ~ "Korea",
                              location == "Micronesia (country)"~ "Micronesia",
                              location == "Palestine" ~ "Palestinian Territories",
                              location == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
                              location == "Saint Lucia" ~ "St. Lucia",
                              location == "St. Vincent and the Grenadines" ~ "St. Vincent and Grenadines",
                              location == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
                              location == "Timor" ~ "Timor-Leste",
                             TRUE ~ location))
#Check again
anti_join(max_gini_by_country, owid, by = c("country" = "location"))

#Merge the gini and covid data sets now
gini_covid <- max_gini_by_country %>%
  left_join(owid, by = c('country' = 'location')) %>% 
  mutate(max_deaths= ifelse(is.infinite(max_deaths), NA, max_deaths)) %>% 
  drop_na()

#Check which locations don't match
anti_join(gini_covid, world_map, by =c("country" = "region")) 

#Check differences for ones I don't already know
world_map %>% filter(str_detect(region, "Ant")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Con")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Coast")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Swa")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Kor")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Pal")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Saint")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Sao")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "Trin")) %>% select(region) %>% distinct()
world_map %>% filter(str_detect(region, "UK")) %>% select(region) %>% distinct()



#Change locations to match
world_map <- world_map %>% 
  mutate(region = case_when(region == "Antigua" ~ "Antigua and Barbuda",
                              region == "Democratic Republic of the Congo"~ "Congo-Kinshasa",
                              region == "Republic of Congo" ~ "Congo-Brazzaville", 
                              region == "Ivory Coast"~"Côte d'Ivoire",
                              region == "Swaziland"~"Eswatini",
                              region == "South Korea" ~ "Korea",
                              region == "Palestine"~  "Palestinian Territories",
                              region == "Saint Kitts" ~ "St. Kitts and Nevis",
                              region == "Saint Lucia" ~"St. Lucia",
                              region == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
                              region == "Trinidad" ~ "Trinidad and Tobago",
                              region == "UK" ~"United Kingdom",
                              region == "USA" ~ "United States",
                              TRUE ~region
  ))

#Check Again 
anti_join(gini_covid, world_map, by =c("country" = "region")) 

#Merge GDP and POP. Similar datasets
gdp_pop <- gdp %>% left_join(country_population, by= c("country"))

#Check for GDP/pop 
anti_join(gini_covid, gdp_pop, by = c("country"))

#Fix it
gdp_pop %>% filter(str_detect(country, "Ang")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Baha")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Bru")) %>% select(country) %>% distinct
gdp_pop %>% filter(str_detect(country, "Ver")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Con")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Cz")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Ivo")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Eg")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Gam")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Ira")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Kore")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Kyrg")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "La")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "nesia")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "est")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Rus")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Slo")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Syr")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Sao")) %>% select(country) %>% distinct
gdp_pop %>% filter(str_detect(country, "Tur")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Ven")) %>% select(country) %>% distinct()
gdp_pop %>% filter(str_detect(country, "Yem")) %>% select(country) %>% distinct()

#Match locations
gdp_pop <- gdp_pop %>% 
  mutate(country = case_when(country == "Bahamas, The"~ "Bahamas",
                             country == "Brunei Darussalam"~ "Brunei",
                             country == "Cabo Verde"~ "Cape Verde",
                             country == "Congo, Rep." ~ "Congo-Brazzaville",
                             country == "Congo, Dem. Rep."~ "Congo-Kinshasa",
                             country == "Czechia" ~ "Czech Republic",
                             country == "Cote d'Ivoire"~"Côte d'Ivoire",
                             country =="Egypt, Arab Rep."~ "Egypt",
                             country == "Gambia, The"~ "Gambia",
                             country == "Iran, Islamic Rep."~"Iran",
                             country == "Korea, Rep."~ "Korea",
                             country == "Kyrgyz Republic"~ "Kyrgyzstan",
                             country == "Lao PDR"~ "Laos",
                             country == "Micronesia, Fed. Sts."~ "Micronesia",
                             country == "West Bank and Gaza"~ "Palestinian Territories",
                             country == "Russian Federation"~"Russia",
                             country == "Slovak Republic" ~"Slovakia",
                             country == "Syrian Arab Republic"~ "Syria",
                             country == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
                             country == "Turkiye" ~ "Turkey",
                             country == "Venezuela, RB"~ "Venezuela",
                             country == "Yemen, Rep."~ "Yemen",
                             TRUE ~ country))

#Check again 
anti_join(gini_covid, gdp_pop, by = c("country")) 

#Merge them
gini_covid_pop <- gini_covid %>% left_join(gdp_pop, by = c("country")) %>% 
  drop_na()

#Merge with coordinate info
df_coords <- gini_covid_pop %>% left_join(world_map, by= c("country" = "region"))



#Check range
df_coords %>%
  summarise(gdp_range = range(gdp),
            max_gini_disp_range = range(max_gini_disp))

#Create breaks based off range. Also Create cases and deaths per 100k to standardize
divisions_gdp <- quantile(221:134000, probs = c(0, 0.25, 0.5, 0.75, 1))
division_gini_disp <- quantile(24.8:66.4, probs = c(0, 0.25, 0.5, 0.75, 1))

df_coords <- df_coords %>% 
  mutate(deaths_per_100k = max_deaths/ pop * 100000,
         cases_per_100k = max_cases/pop *10000,
         gdp_category = case_when(
           gdp <33665.75 ~"Low Income",
           gdp <67110.05 ~"Medium-Low Income",
           gdp < 100555.25 ~ "Medium-High Income",
           TRUE ~ "High Income"
         ),
         gini_category = case_when(
           max_gini_disp <35.0 ~"Low Inequality",
           max_gini_disp <45.30 ~"Medium-Low Inequality",
           max_gini_disp < 55.55 ~ "Medium High Inequality",
           TRUE~ "High"))

# PLOTTING ----------------------------------------------------------------


g1 <- ggplot()+
  geom_point(data = df_coords, aes(x= gdp, y = cases_per_100k, color =gdp_category )) +
  theme_few()+
  geom_smooth(data= df_coords,mapping = aes(x = gdp, y = cases_per_100k), method= 'lm') + 
  scale_x_continuous(breaks = c(221, 33665, 67110, 100555, 134000),
                     labels = c("$221", "$33,665", "$67,110", "$100,555", "$134,000")) +
  scale_color_discrete(limits = c("High Income", "Medium-High Income", "Medium-Low Income", "Low Income"))+
  labs(x = "GDP Per Capita (USD)", 
       y = "Cases (Per 100,000 People)", 
       title= "How Did Wealth Impact the Number of Covid Cases for Nations?", 
       subtitle = "Wealthier Countries Suffered Higher Case Rates Per 100k People Despite Wealth",
       color = "Wealth Category",
       caption = "Source: Our World In Data, The World Bank ")+
  geom_text(data = df_coords %>% 
              filter(gdp_category %in% c('High Income', 'Medium-High Income')),
            aes(x = gdp - 2000, y = cases_per_100k - 5, label = country),
            size = 3, color = 'black', hjust = 1, vjust = 1) +
  theme(legend.justification = "center") #This last line was copied from class
      
g1


# Save figure -------------------------------------------------------------

ggsave("figures/project_figure.pdf", height = fig_h, width = fig_w)





