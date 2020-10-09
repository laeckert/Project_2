library(tidyverse)
library(readr)

getwd()
data.path <- "C:/Users/leckert/Documents/NCSU/ST558/Project_2"
day <- read_csv(paste0(data.path,"/day.csv"))
Monday <- day %>% filter(weekday==1) %>% select(-c(casual,registered, instant, dteday))

