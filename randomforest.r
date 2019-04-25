donorData=read.csv("https://raw.githubusercontent.com/painel55/UCI-Blood-Transfusion-Service-Center-Data-Set-/master/transfusion.data.txt")
dim(donorData)
str(donorData)
donorData$whether.he.she.donated.blood.in.March.2007 <- as.factor(donorData$whether.he.she.donated.blood.in.March.2007)
n <- c("Recency", "Frequency", "Monetary", "Time", "donatedMar07")
colnames(donorData) <- n
summary(donorData)
set.seed(112718)
library(caret)
library(caTools)
sample <- sample.split(donorData, SplitRatio = 0.8)
donor_train <- subset(donorData, sample == TRUE)
donor_test  <- subset(donorData, sample == FALSE)
library(randomForest)
model.rf <- randomForest::randomForest(donatedMar07 ~ Recency + Monetary, data = donor_test)
y_hat4 <- predict(model.rf, donor_test)
predictedY_hatValues4 <- y_hat4
predictedY_hatValues4[predictedY_hatValues4 > 0.4] = 1
predictedY_hatValues4[predictedY_hatValues4 <= 0.4] = 0
library(e1071)
confusionMatrix(
  factor(donor_test$donatedMar07, levels=0:1), 
  factor(predictedY_hatValues4, levels=0:1)
)
cat("Accuracy = ", confusionMatrix(
  factor(donor_test$donatedMar07, levels=0:1), 
  factor(predictedY_hatValues4, levels=0:1)
)$overall[[1]] *100 ,"%")
  
