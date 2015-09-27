setwd("D:\\Repo\\pml-project")
rm(list = ls())

require(caret)
require(dplyr)
data <- read.csv("pml-training.csv", na.strings = c("", "#DIV/0!", "NA"))
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
data <- data[, which(na_count == 0)]
data <- data %>% tbl_df
data <- filter(data, new_window == "no")
data <- data[, -(1:7)]

inTrain <- createDataPartition(data$classe, p = .6, list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]
str(training)

pre <- preProcess(training[, -53], method = "pca", thresh = 0.9)
trainPCA <- predict(pre, newdata = training[, -53])
trainPCA$classe <- training$classe
testPCA <- predict(pre, newdata = testing[, -53])
m <- randomForest(classe ~ ., data = trainPCA)
m
pred <- predict(m, newdata = testPCA)
table(pred, testing$classe)
