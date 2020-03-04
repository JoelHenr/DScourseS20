library(rvest)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(magrittr) 

df <- read_csv("GDP.csv")
summary(df)

mean(df$GDP, na.rm = TRUE)

df %>%
  mutate(GDP = as.factor(GDP)) 
df %>% 
  mutate(DATE=as.factor(DATE))


df$diff <- df$GDP - lag(df$GDP)
df$perchnge <- (df$diff/df$GDP)*100
ggplot(data = df, aes(x=df$DATE,y=perchnge))+ggtitle("Change in US GDP Over Time")+ 
  xlab("Time") + ylab("US GDP") + geom_line()+theme_clean()

ggplot(data = df, aes(x=df$DATE,y=perchnge))+ggtitle("Change in US GDP Over Time")+ 
  xlab("Time") + ylab("US GDP") + geom_point()+theme_stata()

ggplot(data = df, aes(x=df$DATE,y=perchnge))+ggtitle("Change in US GDP Over Time")+ 
  xlab("Time") + ylab("US GDP") + geom_line()+theme_economist_white()


  

  




