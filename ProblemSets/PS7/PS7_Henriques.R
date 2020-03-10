library(mice)
library(stargazer)
library(tidyverse)

#reading in data
df<-read.csv("https://raw.githubusercontent.com/tyleransom/DScourseS20/master/ModelingOptimization/wages.csv")

#Dropping observations where hgc and tenure are missing 
df_drop<- subset(df, (!is.na(df$hgc)) & (!is.na(df$tenure)))

#view data 
df_drop %>% as.data.frame %>% stargazer(type="text")

#listwise deletion
complete.cases(df)
df[complete.cases(df), ] # Keep only the complete rows
df_complete <- df[complete.cases(df), ] # Store the complete cases subset in a new data frame
est_complete <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data = df_complete)


#Mean Imputation for log wages
df$logwage[is.na(df$logwage)] <-mean(df$logwage, na.rm = TRUE)

#imputing missing logwages
est <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data = df)
summary(est)


#mice package multiple imputation regression model
md.pattern(df)
df.imp = mice(df, seed = 12345)
fit = with(df.imp, lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data = df))
summary(pool(fit))

#comparing data results
stargazer(est_complete,est,type="text") 
summary(pool(fit))

mean(df$logwage)
mean(df_complete$logwage)

#Experimental codes
#For Oscar df<-read.csv("wages.csv", stringsAsFactors = FALSE)
#summary(df)
#x <-df[complete.cases(df),]
#str(x)
#x <-na.omit(df)
#str(x)
#df$hgc<-as.numeric(df$hgc)
#df$college<-as.numeric(df$college)
#df$married<-as.numeric(df$married)
#df$age<-as.numeric(df$age)


