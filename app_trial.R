library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(janitor)
library(plotly)
library(zoo)
library(lubridate)
df <- read.csv2("./2018_EconomicFreedom.csv")
df <- df[,1:9]
#df <- df[df$Year==2016,]

colnames(df) <- str_remove_all(colnames(df),'X[:digit:][:punct:]{2}')
colnames(df) <- str_replace_all(colnames(df),'[:punct:]'," ")
colnames(df) <- str_replace_all(colnames(df),'   ',' and ')
colnames(df) <- gsub('\\s+', '', colnames(df))
df <- remove_empty(df, which = c("rows", "cols"))

Continents <- read.csv("~/GitHub/Coursera_DevelopingDataProducts/Rshine/myFirstShiny/Continents.txt")
Continents <- Continents[,c(3,6:8)]

df <- merge(df,Continents,by.x="ISOCode",by.y="alpha.3")
df$ISOCode <- df$ISOCode %>% as.factor()
df$Countries <- df$Countries %>% as.factor()
df$region <- df$region %>% as.factor()
df$sub.region <- df$sub.region %>% as.factor()
#df$Year <- as.Date(as.yearmon(df$Year) + 11/12, frac = 1)

choices_country <- unique(as.character(df$Countries))
choices_country <- setNames(choices_country,choices_country)
choices_year <- df$Year %>% unique() %>% sort() 
choices_year <- setNames(choices_year,choices_year)

pal <- c("red", "blue", "green")

 <- choices_country[1:10]
dataset <- df %>% filter(Countries==input) %>% group_by(Countries) %>% filter(Year==max(Year))

plot_geo(dataset, locationmode = 'world') %>% 
    add_trace(
      z = ~SUMMARYINDEX, locations = ~ISOCode,
      color = ~SUMMARYINDEX, colors = c("green","blue") 
    )


plot_ly(dataset, x = ~Year  , y = ~SizeofGovernment  ,symbols = ~ Countries) %>% add_lines()
dataset %>% 
  mutate(onlyyear = year(Year)) %>% 
  group_by(Countries) %>% 
  plot_ly(x = ~ onlyyear ) %>% 
  add_lines(y = ~ SUMMARYINDEX , 
            color = ~ factor(Countries)
  ) 