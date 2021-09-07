View(iris)
str(iris)

#Distribution
table(iris$Species)

#Cross-Validation
library('caTools')
sel <- sample.split(iris$Petal.Length,SplitRatio = 0.7)
iris_train <- subset(iris,sel==TRUE)
iris_test <- subset(iris,sel==FALSE)

#SUPPORT VECTOR MACHINE FUNCTION(SVM)
install.packages('e1071')
library('e1071')
svm_linear <- svm(Species ~ Petal.Length+Petal.Width,data = iris_train, kernel ='linear')
svm_radial <- svm(Species ~ Petal.Length+Petal.Width,data = iris_train, kernel ='radial')

par(mfrow=c(2,1))

plot(svm_linear,iris_train[,c(5,3,4)])

plot(svm_radial,iris_train[,c(5,3,4)])

#Predict procedure
iris_test$Species_pred <- predict(svm_linear, newdata = iris_test)

#Accuracy testing
library('caret')
confusionMatrix(table(iris_test$Species,iris_test$Species_pred))

iris_train[,c(5,3,4)]

