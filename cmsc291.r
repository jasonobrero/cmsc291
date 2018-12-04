#install.packages(c("plyr", "neuralnet"))

require(plyr)
require(neuralnet)
library(plyr)
library(neuralnet)
data = read.csv("data_new.csv", header = TRUE)
data.numeric = data.frame(sapply(data, function(x) as.numeric(as.character(x))))

data.train = data.numeric[,"Dataset"] == 0
data.valid = data.numeric[,"Dataset"] == 1

data.numeric$Dataset = NULL

max = apply(data.numeric, 2, max)
min = apply(data.numeric, 2, min)

data.scaled = as.data.frame(scale(data.numeric, center = min, scale = max - min))
data.NNtrain = data.scaled[data.train == TRUE,]
data.NNtest = data.scaled[data.train == FALSE,]

mat = model.matrix(
    ~ ID + Size + Volume + WedgeCount + ClawCount + MaxDegree + AveDeg + Fill,
    data = data.NNtrain
)

set.seed(2)
results = list()
for (testcase in testcases) {
  print(testcase[[1]])
  NN = neuralnet(
    ID ~ + Size + Volume + WedgeCount + ClawCount + MaxDegree + AveDeg + Fill,
    mat,
    hidden = testcase[[1]],
    linear.output = FALSE,
    stepmax = 1e+7
  )
  
  results[[testcase[[3]]]] = NN
  
  predict_testNN = compute(NN, data.NNtest[c(1:7)])
  predict_testNN = (predict_testNN$net.result * (max(data.scaled$ID) - min(data.scaled$ID))) + min(data.scaled$ID)
  
  png(testcase[[2]])
  plot(data.NNtest$ID, predict_testNN, col = 'blue', pch = 16, ylab = "Predicted", xlab = "Real")
  dev.off()
}