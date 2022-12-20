library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(colorspace)
library(here)

library(readr)
Electric_Vehicle_Population <- read_csv("C:/Users/pc/Desktop/my project raw file/Electric_Vehicle_Population.csv")
View(Electric_Vehicle_Population)

ggplot(data=Electric_Vehicle_Population)+
  geom_count(mapping = aes(x=Make, y=Model_Year))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Electric cars by makers", 
       subtitle = "Companies with highest number of E.cars made over the years")
  
Electric_Vehicle_Range <- Electric_Vehicle_Population %>%  
  group_by(Electric_Range,Model_Year,Make,Model,Electric_Vehicle_Type,Clean_Alternative_Fuel_Vehicle_Eligibility) %>% 
  drop_na() %>% 
  summarise(Electric_Range=max(Electric_Range))
View(Electric_Vehicle_Range)

Electric_Vehicle_Range2 <- Electric_Vehicle_Range %>% 
  tail(30)
View(Electric_Vehicle_Range2)

ggplot(data = Electric_Vehicle_Range2)+
  geom_point(mapping = aes(x=Model_Year, y=Electric_Range, color=Model))+
  theme(axis.text.x = element_text(angle = 60))+
  facet_wrap(~Make)+
  labs(title = "E.V,Makers and Miles Covered", 
       subtitle = "Total Distance Covered by Electric Vehicles (E.V)")

Electric_Vehicle_Range3 <- Electric_Vehicle_Population %>%  
  group_by(Model_Year,Electric_Range,Make,Model,Electric_Vehicle_Type,Clean_Alternative_Fuel_Vehicle_Eligibility) %>% 
  drop_na() %>% 
  summarise(Electric_Range=max(Electric_Range))
View(Electric_Vehicle_Range3)

Electric_Vehicle_Range4 <- Electric_Vehicle_Range3 %>% 
  head(5)
View(Electric_Vehicle_Range4)

ggplot(data = Electric_Vehicle_Range4)+
  geom_point(mapping = aes(x=Make, y=Electric_Range, color=Model))+
  theme(axis.text.x = element_text(angle = 60))+
  facet_wrap(~Model_Year)+
  labs(title = "First E.V Made And Makers", 
       subtitle = "The First Electric Vehicles (E.V) Made by Car Companies")

ggplot(data= Electric_Vehicle_Population)+
  geom_bar(mapping = aes(x=Clean_Alternative_Fuel_Vehicle_Eligibility, fill=Electric_Vehicle_Type))+
  theme(axis.text.x = element_text(angle = 45))+
  facet_wrap(~Electric_Vehicle_Type)
  labs(title = "Vehicle Fuel Eligibility", subtitle = "Number of that Runs on Clean Energy")

ggplot(data= Electric_Vehicle_Population)+
  geom_bar(mapping = aes(x=Electric_Vehicle_Type))+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Vehicle Fuel Eligibility", subtitle = "Number of that Runs on Clean Energy")

