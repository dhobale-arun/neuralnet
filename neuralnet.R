#neuralnet

nn <- neuralnet(Concrete.Strength ~ FM.of.Sand + W.C.Ratio + Water + Cement + Fine.Aggregate + Coarse.Aggregate + CA.Size.Ratio + A.C.Ratio + F.A.Ratio, data=btp, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)
nn.results <- compute(nn, test)
temp_test <- subset(test, select = c("FM.of.Sand", "W.C.Ratio","Water","Cement","Fine.Aggregate","Coarse.Aggregate","A.C.Ratio","F.A.Ratio"))
nn.results <- compute(nn, temp_test)
#MAX-MIN NORMALIZATION
normalize <- function(x) {
 return ((x - min(x)) / (max(x) - min(x)))
   }
maxmindf <- as.data.frame(lapply(btp, normalize))

trainset <- maxmindf[1:80, ]
testset <- maxmindf[81:114, ]

nn <- neuralnet(Concrete.Strength ~ FM.of.Sand + W.C.Ratio + Water + Cement + Fine.Aggregate + Coarse.Aggregate + CA.Size.Ratio + A.C.Ratio + F.A.Ratio, data=trainset, hidden=c(5,2), linear.output=TRUE, threshold=0.01)
nna <- neuralnet(Concrete.Strength ~ FM.of.Sand + W.C.Ratio + Water + Cement + Fine.Aggregate + Coarse.Aggregate + CA.Size.Ratio + A.C.Ratio + F.A.Ratio, data=trainset, act.fct = "tanh", linear.output=TRUE, threshold=0.01)
nnb <- neuralnet(Concrete.Strength ~ FM.of.Sand + W.C.Ratio + Water + Cement + Fine.Aggregate + Coarse.Aggregate + CA.Size.Ratio + A.C.Ratio + F.A.Ratio, data=trainset, hidden=c(3,1),act.fct = "tanh", linear.output=TRUE, threshold=0.01)

nna$result.matrix
plot(nn)
temp_test <- subset(testset, select = c("FM.of.Sand", "W.C.Ratio","Water","Cement","Fine.Aggregate","Coarse.Aggregate","CA.Size.Ratio","A.C.Ratio","F.A.Ratio"))
head(temp_test)
nn.resultsc <- compute(nnc, temp_test)
resultsc <- data.frame(actual = testset$Concrete.Strength, prediction = nn.resultsc$net.result)

predictedc=resultsc$prediction *86.97 + 19.53
actualc=resultsc$actual *86.97 + 19.53
comparisonc=data.frame(predictedc,actualc)
deviationc=((actualc-predictedc)/actualc)
comparisonc=data.frame(predictedc,actualc,deviationc)
accuracyc=1-abs(mean(deviationc))
accuracyc

nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$Concrete.Strength, prediction = nn.results$net.result)
predicted=resultsc$prediction *86.97 + 19.53
actual=resultsc$actual *86.97 + 19.53
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy
actual = testset$Concrete.Strength
prediction = nn.results$net.result
RMSE.NN = (sum((actual - predicted)^2) / nrow(testset)) ^ 0.5
RMSE.NN
R2 <- 1-RMSE.NN
R2

nn.resultsc <- compute(nnc, temp_test)
resultsc <- data.frame(actual = testset$Concrete.Strength, prediction = nn.resultsc$net.result)
predictedc=resultsc$prediction *21.9 + 23.80
actualc=resultsc$actual *21.9 + 23.8
comparisonc=data.frame(predictedc,actualc)
deviationc=((actualc-predictedc)/actualc)
comparisonc=data.frame(predictedc,actualc,deviationc)
accuracyc=1-abs(mean(deviationc))
accuracyc
RMSE.NNc = (sum((actualc - predictedc)^2) / nrow(testset)) ^ 0.5
RMSE.NN
R2c <- 1-RMSE.NNc
R2c

nn.resultsc <- compute(nnc, temp_test)
resultsc <- data.frame(actual = testset$Concrete.Strength, prediction = nn.resultsc$net.result)
predictedc=resultsc$prediction *21.9 + 23.80
actualc=resultsc$actual *21.9 + 23.8
comparisonc=data.frame(predictedc,actualc)
deviationc=((actualc-predictedc)/actualc)
comparisonc=data.frame(predictedc,actualc,deviationc)
accuracyc=1-abs(mean(deviationc))
accuracyc
RMSE.NNc = (sum((actualc - predictedc)^2) / nrow(testset)) ^ 0.5
RMSE.NN
R2c <- 1-RMSE.NNc
R2c

nn.resultsc <- compute(nnc, temp_test)
resultsc <- data.frame(actual = testset$Concrete.Strength, prediction = nn.resultsc$net.result)
predictedc=resultsc$prediction *21.9 + 23.80
actualc=resultsc$actual *21.9 + 23.8
comparisonc=data.frame(predictedc,actualc)
deviationc=((actualc-predictedc)/actualc)
comparisonc=data.frame(predictedc,actualc,deviationc)
accuracyc=1-abs(mean(deviationc))
accuracyc
RMSE.NNc = (sum((actualc - predictedc)^2) / nrow(testset)) ^ 0.5
RMSE.NN
R2c <- 1-RMSE.NNc
R2c

plot(actuala, predicteda, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
abline(0,1)
#rmse

RMSE.NNc = (sum((actualc - predictedc)^2) / nrow(testset)) ^ 0.5
RMSE.NN
R2c <- 1-RMSE.NNc
R2c

rsq <- function (actualc, predictedc) cor(actualc,predictedc) ^ 2
rsqb <- function(x, y) summary(lm(y~x))$r.squared
rsqb <- rsq(actualc,predictedc)
rsqb
