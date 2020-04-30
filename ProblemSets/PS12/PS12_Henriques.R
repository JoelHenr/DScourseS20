library(sampleSelection)
library(tidyverse)
library(stargazer)
library(httr)
library(mice)
library(dplyr)
library(foreign)
library(plm)
library(mlogit)

# Set the Working directory 
setwd("~/DScourseS20/Structural")

# Read in the data into data frame
df <-read.csv("wages12.csv")
df1 <-read.csv("wages12.csv")
df2 <-read.csv("wages12.csv")
df3 <-read.csv("wages12.csv")

# Making variables into factor variables 
df1$college <- as.factor("college")
df1$married <-as.factor("married")
df1$union <-as.factor("union")

# Summary of data 
stargazer(df, type = 'text')

# listwise deletion 
complete.cases(df$logwage)
df[complete.cases(df$logwage),]
df_comp <- df[complete.cases(df$logwage),]
est_comp <- lm(logwage ~ hgc + union + college + exper, data = df_comp)

# Mean imputation for Lwage
df$logwage[is.na(df$logwage)]<-mean(df$logwage, na.rm = TRUE)

# Now imputing missing lwages
est <- lm(logwage ~ hgc + union + college + exper, data = df)
stargazer(est, type = 'text')

# Flag variable and invalid observations 
df1 <- df1%>%mutate(valid = ifelse(is.na(logwage),0,1)) #might work? omg it worked! 
df2 <- df2%>%mutate(valid = df$logwage > 0) 
df3 <- df2%>%mutate(valid = df$logwage > 0) 

# Heckman selection 
hk <- selection(selection = valid ~ hgc + union + college + married + kids, 
          outcome = logwage ~ hgc + union + college + exper, 
          data = df2)

# Results of all three models
stargazer(hk,est_comp,est, type = 'text')

# Probability model 
estim <- glm(valid ~ hgc + union + college + married + kids,
             family = binomial(link='logit'), data = df2)
print(summary(estim))

# Make Married and Kids equal zero 
df3$married <-(df3$married = 0)
df3$kids <- (df3$kids = 0)

#New Parameters 
estim2 <- glm(valid ~ hgc + union + college + married + kids,
             family = binomial(link='logit'), data = df3)
print(summary(estim2))

#Compare Results 
stargazer(estim, estim2, type = 'text')


