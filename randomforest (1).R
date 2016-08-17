library(randomForest)
library(caret)
library(e1071)
aaa1 <- read.csv(file = "F:/bank-full.csv" , header=TRUE , sep=";")
set.seed(100)
index <- sample(1:nrow(aaa1),round(0.75*nrow(aaa1)))
xxx<-aaa1[index,]


set.seed(100)
aaa1.rf <- randomForest(y ~ ., data = xxx,
                        mtry = 2, importance = TRUE,ntree=500,
                        do.trace = 100)


plot(aaa1.rf)
print(aaa1.rf)
aaa1.rf$predicted.response <- predict(aaa1.rf,xxx)
confusionMatrix(data=aaa1.rf$predicted.response,
                reference=xxx$y,
                positive="yes")