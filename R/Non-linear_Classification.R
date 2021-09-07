install.packages('rpart')
library('rpart')

crdt <- read.csv("credit_data.csv",header = TRUE)
str(crdt)

crdt$Creditability <- as.factor(crdt$Creditability)

#Cross-validation
summary(crdt)

library('caTools')
apply(crdt, MARGIN = 2, var)
sel <- sample.split(crdt$Credit.Amount, SplitRatio = 0.7)

crdt_train <- subset(crdt, sel==TRUE)
crdt_test <- subset(crdt,sel==FALSE)

#Classification and Regression
#If dependent var is numeric then Regression
#Else for categorical , Classification

#--------------Decision Tree----------------
dtree <- rpart(formula = Creditability ~ ., data= crdt_train, method = 'class')

#Visualization of decision tree
install.packages('rpart.plot')
library('rpart.plot')

prp(dtree)

#Prediction of model on test data
crdt_test$dtree_pred <- predict(dtree, crdt_test,type = 'class')

#Checking Accuracy
library('caret')
confusionMatrix(table(crdt_test$Creditability,crdt_test$dtree_pred), positive = '1')

#------------Random forest-------------
install.packages('randomForest')
library('randomForest')

#Model-training
rf_model <- randomForest(Creditability ~ ., data = crdt_train)

#Out of bag error : A entry which is not seen in the test data..and that is predicted in
#train data and comes out to be wrong

#Model tuning
mtry <- tuneRF(crdt_train[,-1], crdt_train$Creditability, ntreeTry = 100, 
               stepFactor = 1.5, improve = 0.01, plot = TRUE, trace = TRUE)

rf_model <- randomForest(Creditability ~ ., data = crdt_train, mtry=2, ntree=100)

#Predict
crdt_test$rf_pred <- predict(rf_model, crdt_test)

#Accuracy
confusionMatrix(table(crdt_test$Creditability,crdt_test$rf_pred),positive = '1')

#Importance variable in random forest
importance(rf_model)
varImpPlot(rf_model)

#Naive Bayes
library('e1071')
nb_model <- naiveBayes(Creditability ~ . , data = crdt_train)

#Predicting
crdt_test$nb_pred <- predict(nb_model,crdt_test)

#Accuracy
confusionMatrix(table(crdt_test$Creditability,crdt_test$nb_pred),positive = '1')

#---------K-nearest neighbour------------ 

##on IRIS dataset
View(iris)
str(iris)

#Data scaling to normalize 
iris_scale <- data.frame(scale(iris[,-5]))
#Adding the species column for modelling 
iris_scale <- cbind(iris_scale,Species = iris$Species)

#Cross-validation
s <- sample.split(iris_scale$Petal.Length,SplitRatio = 0.7)

iris_train <- subset(iris_scale,s==TRUE)
iris_test <- subset(iris_scale,s==FALSE)

#KNN model

library('class')
knn_predict <- knn(iris_train[,-5],iris_test[,-5],iris_train[,5],k=13)

#accuracy
table(iris_test$Species,knn_predict)
table(iris_test$Species)

#---------------Association Rules------------
install.packages('arules')
library('arules')
data('Groceries')
inspect(Groceries[1:8])

#Which objects are ordered together
rules <- apriori(Groceries, parameter = list(support=0.01,confidence=0.5))
inspect(rules)





