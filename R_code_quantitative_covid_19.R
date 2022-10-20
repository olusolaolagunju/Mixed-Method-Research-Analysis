library(tidyverse)
library(skimr)
library(forcats) # to re-order bar graphs (increasing/decreasing)
library(dplyr)
install.packages("ggpmisc") # to create table inside ggplot
library(ggpmisc)
library(readxl)
install.packages("hablar")
library(hablar) # to tackle Inf and NA
library(ggplot2)
# read data
covid_ethiopia <- read_excel("C:/Users/DELL/OneDrive/COURSERA COURSES/GOOGLE ANALYTIC/R/Google Data Analytic/dataset/covid_ethiopia.xlsx")

# view data
colnames(covid_ethiopia)
skim_without_charts(covid_ethiopia)


#removing outliers
clean_data <- covid_ethiopia %>% 
  select(iso_code, continent, location, date, population,  new_cases,total_cases,
         new_deaths, total_deaths, people_vaccinated, people_fully_vaccinated) %>% 
  filter(continent !=c("World", "Asia", "Lower middle income, Upper middle income", 
                       "High income", "Low income"))


#Infection rate (cases/population)
IFR <- clean_data %>% 
  group_by(continent,location, population) %>% 
  summarise(total_case = sum(new_cases, na.rm = TRUE)) %>% 
  mutate(cases_per_pop = total_case/population,
         IFR_rate =  paste0(sprintf("%4.1f", cases_per_pop * 100), "%")) 

# Case Fatality rate  (Cases/Deaths)
CFR <- clean_data %>% 
  group_by(continent,location, population) %>% 
  summarise(total_case = sum(new_cases, na.rm = TRUE), 
            total_death = sum(new_deaths, na.rm = TRUE)) %>% 
  mutate(CFR = total_death/total_case,
         CFR_rate =  paste0(sprintf("%4.1f", CFR * 100), "%")) 


# Singl_dose: number of people that have recieved at least a single dose
# of covid vaccine per population
# max_ of hablar package was used to remove -inf drugery 
single_dose_world <- clean_data %>% 
  group_by(continent,location, population) %>% 
  summarise(vacc = max_(people_vaccinated)) %>% 
  mutate(vacc_per_pop = vacc/population,
         vacc_per_pop_rate = paste0(sprintf("%4.1f", vacc_per_pop * 100), "%")) %>% 
  arrange(vacc_per_pop_rate)

# calculating average figure of IFR, CFR and Single dose per contienent ( Africa, Europe..)

# Average cfr per contienent 
CFR_avg <- CFR %>% 
  group_by(continent) %>% 
  summarise(average_CFR = mean(CFR, na.rm = TRUE)) %>% 
  mutate(average_CFR_rate =  paste0(sprintf("%4.1f", average_CFR * 100), "%")) %>% 
  arrange(desc(average_CFR_rate))

#Average IFR 

IFR_avg <- IFR %>% 
  group_by(continent) %>% 
  summarise(average_IFR = mean(cases_per_pop, na.rm = TRUE)) %>% 
  mutate(average_IFR_rate =  paste0(sprintf("%4.1f", average_IFR * 100), "%")) %>% 
  arrange(desc(average_IFR_rate))

Single_dose_avg <- single_dose_world %>% 
  group_by(continent) %>% 
  summarise(average_dose = mean(vacc_per_pop, na.rm = TRUE)) %>% 
  mutate(average_dose_rate =  paste0(sprintf("%4.1f", average_dose * 100), "%")) %>% 
  arrange(desc(average_dose_rate))


# plots

#1. Infection rate 

ggplot(IFR_avg, aes(x= fct_reorder(continent, average_IFR_rate), average_IFR_rate))+
  geom_col(fill = "lightblue")+
  labs(x = "continent")+
  theme_classic()+
  geom_text(aes(label = average_IFR_rate))+
  labs(title = "Global Infection Rate for Sept 2022",
       subtitle = " Africa has the lowest infection rate across all continents",
       x = "Country",
       y = "Infection rate",
       caption = "Data collected by Mathieu et al., 2021 \n Visualized by Olusola Olagunju")




#2. Case Fatality rate

ggplot(CFR_avg, aes(x= fct_reorder(continent, average_CFR_rate), average_CFR_rate))+
  geom_col(fill = "lightblue")+
  labs(x = "continent")+
  theme_classic()+
  geom_text(aes(label = average_CFR_rate))+
  labs(title = "Global Case Fatality Rate for Sept 2022",
       subtitle = " Africa and Asia recorded high deaths per cases despite\n  having a low infection rate, 3.2% and 10.5% respectively",
       x = "Country",
       y = "Case fatality rate",
       caption = "Data collected by Mathieu et al., 2022 \n Visualized by Olusola Olagunju")


#3 Vaccination rate 

ggplot(Single_dose_avg, aes(x= fct_reorder(continent, average_dose_rate), average_dose_rate))+
  geom_col(fill = "lightblue")+
  labs(x = "continent")+
  theme_classic()+
  geom_text(aes(label = average_dose_rate))+
  labs(title = "Global Vaccination Rate of Covid-19 Vaccine for Sept 2022",
       subtitle = " Percentage of the number of people that have recieved at least a dose \n of Covid-19 vaccine per population.   \n Africa has the least vaccination rate",
       x = "Country",
       y = "Vaccination rate",
       caption = "Data collected by Mathieu et al., 2022 \n Visualized by Olusola Olagunju")




# Adding Median age and Gdp/population to each plots above 

#creating table using install.packages("ggpmisc")
install.packages("ggpmisc")
library(ggpmisc)

age_gdp <-  global_median %>% 
  rename(Continent = "continent",
         Median_age = "median age",
         Gdp_pop = "Gdp (USD trillion)")


#1. Adding age_gdp to IFR plot

IFR_plot <-  ggplot(IFR_avg, aes(x= fct_reorder(continent, average_IFR_rate), average_IFR_rate))+
  geom_col(fill = "lightblue")+
  labs(x = "continent")+
  theme_classic()+
  geom_text(aes(label = average_IFR_rate))+
  labs(title = "Global Infection Rate for Sept 2022",
       subtitle = " Africa had the lowest infection rate across all continents. \n The continent's median age significantly influenced the infection rate.",
       x = "Country",
       y = "Infection rate",
       caption = "Data collected by Mathieu et al., 2022 \n Visualized by Olusola Olagunju")


IFR_plot

IFR_plot + annotate(geom = "table",
                     x = "Africa",
                     y = "36.0%",
                     label = list(age_gdp))

ggsave("IFR_average_plot.jpg", width = 6, height = 6)


#2. Adding age_gdp tableto CFR plot
CFR_plot <- ggplot(CFR_avg, aes(x= fct_reorder(continent, average_CFR_rate), average_CFR_rate))+
  geom_col(fill = "lightblue")+
  labs(x = "continent")+
  theme_classic()+
  geom_text(aes(label = average_CFR_rate))+
  labs(title = "Global Case Fatality Rate for Sept 2022",
       subtitle = " Africa and Asia recorded higher deaths per case despite having the least \n infection rate at 3.2% and 10.5% respectively. The median age and Gdp/Pop \n had no significant effect on the case fatality rate",
       x = "Country",
       y = "Case fatality rate",
       caption = "Data collected by Mathieu et al., 2021 \n Visualized by Olusola Olagunju")
CFR_plot

CFR_plot + annotate(geom = "table",
                    x = "Oceania",
                    y = "14.0%",
                    label = list(age_gdp))

ggsave("CFR_average_plot.jpg", width = 6, height = 6)


#3. Adding age_gdp to Single dose (vaccination rate)plot
Vacc_plot <- ggplot(Single_dose_avg, aes(x= fct_reorder(continent, average_dose_rate), average_dose_rate))+
  geom_col(fill = "lightblue")+
  labs(x = "continent")+
  theme_classic()+
  geom_text(aes(label = average_dose_rate))+
  labs(title = "Global Vaccination Rate of Covid-19 Vaccine for Sept 2022",
       subtitle = " Africa had the least vaccination rate. The median age and Gdp/Pop had \n strong correlations with the vaccination rate",
       x = "Country",
       y = "Vaccination rate",
       caption = "Data collected by Mathieu et al., 2022 \n Visualized by Olusola Olagunju")

Vacc_plot

Vacc_plot + annotate(geom = "table",
                    x = "Africa",
                    y = "77.0%",
                    label = list(age_gdp))

ggsave("Vacc_average_plot.jpg", width = 6, height = 6)
