food <- read.table("fastfood-1.txt",sep=" ",header = TRUE)

#ANOVA ONE WAY
r <- as.matrix(food)
r <- c(t(r))
r

f=c("Item1","Item2","Item3")
k=3 #No of treatment levels becoz marketing 3 new products
n=6 #Obs per treatment

tm <- gl(k,1,length = n*k,factor(f))
tm

av <- aov(r~tm)
av

summary(av)

library(MASS)
View(survey)
head(survey)
tb1<-table(survey$Smoke,survey$Exer)
tb1

chisq.test(tb1)


 