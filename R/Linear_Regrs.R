house <- read.csv("Housing_data.csv",header=TRUE)
str(house)
sel <- sample.split(house$LivingArea,SplitRatio = 0.7)
install.packages("caTools")
library("caTools")

house_train <- subset(house,sel==TRUE)
house_model <- lm(Price ~ LivingArea,data = house_train)
summary(house_model)

multivar_model <- lm(Price ~ LivingArea+Bathrooms+Age+Fireplace,data = house_train)
summary(multivar_model)

nydf <- read.csv("nyc.csv",header = TRUE)
sel2 <- sample.split(nydf$Food,)
str(nydf)
nydf$East <- as.factor(nydf$East)
str(nydf)
ny_model <- lm(Price ~ Food,data = nydf)
summary(ny_model)

Class <- read.csv("Class.csv",header = TRUE)
str(Class)
Class$Sex <- as.factor(Class$Sex)
library(plyr)
Class <- rename(Class,c("Name   "="Name"))
Class$Name <- as.factor(Class$Name)
summary(Class)
 
Class_model <- lm(Weight ~ Height, data= Class)
summary(Class_model) 



