# logistic regression

# import the admission.csv
adm <- read.csv('admission.csv')
View(adm)

str(adm)

# step 1 - supervised learning :

# train and test 

library('caTools')
sel <- sample.split(adm$GRE,SplitRatio = 0.7)
sel

# training dataset
adm_train <- subset(adm,sel==TRUE)

# test datatset
adm_test <- subset(adm,sel==FALSE)

# step 2 -
# train the model on the training data

# glm - generalized linear models
#glm(Y ~ x1+x2+...,data=,family='gaussian/binomial')


# dependent variable - admit
logistic_model <- glm(ADMIT ~ .,data=adm_train,family='binomial')

# output of the model 
summary(logistic_model)

# Checking AIC - aikake information criterion, the lower the better


# h0 : the coeffcient for the variable =0
# ha: b =<>0
logistic_model <- glm(ADMIT ~ GPA+RANK,data=adm_train,family='binomial')

# output of the model 
summary(logistic_model)

# predict the values on the test data
# type='response' gives the probabilites of occurence of the event

adm_test$pred_admit <- predict(logistic_model,newdata=adm_test,type='response')
View(adm_test)

# putting a threshold and then convert the continuous value to a categorical val
adm_test$pred_class <- ifelse(adm_test$pred_admit>=0.4,1,0)

library('caret')
# confusionMatrix(actual value, predicted value, positive class)
confusionMatrix(table(adm_test$ADMIT,adm_test$pred_class),positive='1')

# threshold for 50% prob - 75.8%
# threshold for 60% accuracy - 74%
# threshold for 40% accuracy - 65 %
#SO the first one was the BEST
