library(mxnet)
aaa1 <- read.csv(file = "F:/bank-full.csv" , header=TRUE , sep=";")
set.seed(100)
index <- sample(1:nrow(aaa1),round(0.75*nrow(aaa1)))
aaaa1<-data.matrix((aaa1))

maxs <- apply(aaaa1, 2, max) 
mins <- apply(aaaa1, 2, min)

scaled <- as.data.frame(scale(aaaa1, center = mins, scale = maxs - mins))
train<-data.matrix(scaled[index,1:16])
test<-scaled[index,17]
View(scaled)

mx.set.seed(0)
model <- mx.mlp(train, test, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=10, array.batch.size=10, learning.rate=0.03, momentum=0.9, 
                eval.metric=mx.metric.accuracy)


preds = predict(model, train)
## Auto detect layout of input matrix, use rowmajor..
pred.label = max.col(t(preds))-1
table(pred.label, test)