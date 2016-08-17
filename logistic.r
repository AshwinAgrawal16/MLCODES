#Reading the dataset
bank<-read.csv("E:/bank-additional-full.csv", sep=";")
head(bank)
str(bank)
View(bank)
bank1<-data.matrix(bank)
View(bank1)

#to count yes no
a <- table(bank$y)
a

# data preparation
maxs<- apply(bank1,2,max)
mins<- apply(bank1,2,min)
bank_norm<- as.data.frame(scale(bank1,center=mins,scale=maxs-mins))
View(bank_norm)

#Data splitting in test and train data
set.seed(2)
ind = sample(2, nrow(bank_norm), replace = TRUE, prob=c(0.7,0.3)) 
trainset = bank_norm[ind == 1,]
testset = bank_norm[ind == 2,]


View(trainset)
View(testset)

dim(trainset)
dim(testset)

model <- glm(y ~.,family=binomial(link='logit'),data=trainset)
summary(model)
anova(model, test="Chisq")
library(pscl)
pR2(model)
fitted.results <- predict(model,newdata=subset(testset,select=c(1:20)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
misClasificError <- mean(fitted.results != testset$y)
misClasificError
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model, newdata=subset(testset,select=c(1:20)), type="response")
pr <- prediction(p, testset$y)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc





