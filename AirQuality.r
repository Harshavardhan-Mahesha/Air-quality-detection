----------------------------------------- Load Dataset --------------------------------------------
#load library to use
library(animation)
library(ggplot2)
library(tree)

#read data from file
airqual <- read.csv("C:/Users/lenovo/Desktop/project/New folders/air_data.csv")
View(airqual)
set.seed(20)
---------------------------------------- Data Preparation----------------------------------------------
#Prepare data
data <- airqual
head(data)
str(data)
data$Class <- factor(data$Class)
str(data)

# split data into test and train
ind <- sample(2, nrow(data), replace= TRUE, prob= c(0.70,0.30))
train <- data[ind==1,]
test <- data[ind==2,]
View(train)
View(test)
---------------------------------------- Multinomial Logistic Regression----------------------------------------------
#develop multinomial logistic regression model
library(nnet) #nnet: Neural Network

mlr_dt_model <- multinom(Class~NO+NOx+CO+NO2+O3+PM10+PM2.5+SO2,data= train)

summary(mlr_dt_model)

#predict for all 
mlr_pred <- predict(mlr_dt_model,test[,1:8],type="class")
mlr_pred

# To print Confusion Matrix
lr_cm <- table(mlr_pred,test$Class)
lr_cm

# Classification Report
#Error
lr_error <-(1- sum(diag(lr_cm))/sum(lr_cm))
lr_error
#Accuracy
lr_accuracy <- (sum(diag(lr_cm))/sum(lr_cm))
lr_accuracy

#---------------------------------------- Decision Tree -------------------------------------------
# develop decision tree model
dt_model= tree(Class~NO+NOx+CO+NO2+O3+PM10+PM2.5+SO2,train)

# predict 
summary(dt_model)
print(dt_model)
dt_pred <- predict(dt_model,test[,1:8],type="class")


# plot the dt_model
plot(dt_model)
text(dt_model)

# To print Confusion Matrix
dt_cm <- table(dt_pred,test$Class)
dt_cm

# Classification Report
# Error
dt_error <-(1- sum(diag(dt_cm))/sum(dt_cm))
dt_error
# Accuracy
dt_accuracy <- (sum(diag(dt_cm))/sum(dt_cm))
dt_accuracy

#---------------------------------------- Random Forest -------------------------------------------
# develop random forest tree model
library(randomForest)

rf_dt_model =randomForest(Class~NO+NOx+CO+NO2+O3+PM10+PM2.5+SO2,data=train,mtry=2,importance=TRUE,proximity=TRUE)
print(rf_dt_model)
summary(rf_dt_model)

# predict 
rf_pred = predict(rf_dt_model,test[,1:8],type="class")
rf_pred

# To print Confusion Matrix
rf_cm <- table(rf_pred,test$Class)
rf_cm

# Classification Report
# Error
rf_error <-(1- sum(diag(rf_cm))/sum(rf_cm))
rf_error
# Accuracy
rf_accuracy <- (sum(diag(rf_cm))/sum(rf_cm))
rf_accuracy

#---------------------------------------- Statistics --------------------------------
algo <- c("MLR","DT","RF")
algo
error <- c(lr_error,dt_error,rf_error)
error
accuracy <- c(lr_accuracy,dt_accuracy,rf_accuracy)
accuracy

errorInfo=data.frame(name= algo,  value=error)
View(errorInfo)
accuracyInfo=data.frame(name= algo,  value=accuracy)
View(accuracyInfo)

ggplot(errorInfo, aes(x=algo , y=error)) + geom_bar(stat = "identity")
ggplot(accuracyInfo, aes(x=algo , y=accuracy)) + geom_bar(stat = "identity")


#-----------------------------------------Test Model on New Data -------------------------------------------
test_data = test[1:10,1:8]
View(test_data)
result_mlr <- predict(mlr_dt_model,test_data,type="class")
result_mlr
result_dt <- predict(dt_model,test_data,type="class")
result_dt
result_rf <- predict(rf_dt_model,test_data,type="class")
result_rf


