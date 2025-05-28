library(tidyverse)
library(janitor)
library(lubridate)
read.csv("INDIA-AQI-DATA-2015-2020.csv")->df
clean_names(df)->df1
view(df1)
df1 %>% #Changing the column to row
  mutate(year=date %>% year(),
       month=date %>% month(),
       day=date %>% day(),
       week=date %>% week(),
       weekday=date %>% wday(label=T))->df2
colnames(df2)
unique(df2$city)
df2 %>% 
pivot_longer(3:15, names_to = "parameter", values_to = "values") ->df3

#Yearwise pollutants trend
df3 %>% 
  group_by(year,parameter) %>% 
  summarise(mean_value=mean(values,na.rm=T))->dfyear

dfyear %>% 
  ggplot(aes(x=year,y=mean_value))+
  geom_line(colour="red")+
  facet_wrap(~parameter,scales = "free_y")+
  labs(title = "Air pollutants trend",
       subtitle = "From 2015 to 2020",
       x=NULL,
       y="Pollutants value",
       caption = "Source:AQI India Dataset")+
  theme_linedraw()->plot1
plot1

ggsave("Air pollutants trends.pdf",
       plot = plot1,
       unit="in",
       width=10,
       height=6)

#Air quality trends for bengaluru
#co trends for all city
#air quality trends for bngl,chennai,mum,hyd
#pm2.5 trend for bnglr 2015-2020

df3 %>% 
  filter(city== "Bengaluru") %>% 
  group_by(year,parameter) %>% 
  summarise(mean_value=mean(values,na.rm=T)) %>% 
    
ggplot(aes(x=year,y=mean_value))+
    geom_line(colour="black")+
  facet_wrap(~parameter,scales = "free_y")+
  labs(title = "Bengaluru Air pollutants trend",
       subtitle = "From 2015 to 2020",
       x=NULL,
       y="Pollutants value",
       caption = "Source:AQI India Dataset")+
  theme_linedraw()->plot2
plot2()

#Heat map
df3 %>% 
  filter(parameter=="co") %>% 
  group_by(week,weekday,month) %>% 
  summarise(mean_value=mean(values,na.rm=T)) %>% 
  ggplot(aes(x=week,
             y=weekday,
             fill=mean_value))+
  geom_tile()+
  facet_wrap(~month, scales = "free_x") +
  # scale_fill_gradient(low = "yellow", high = "red") +
  scale_fill_gradientn(colours = c("darkgreen", "orange", "purple")) +
  theme_minimal() +
  labs(title = "CO heat map",
       subtitle = "For all cities",
       x = NULL,
       y = NULL)
