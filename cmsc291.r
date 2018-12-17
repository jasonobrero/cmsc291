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
    algorithm = "slr",
    threshold = 0.05,
    hidden = testcase[[1]],
    linear.output = FALSE,
    lifesign = "full",
    stepmax = 1e+8
  )
  
  results[[testcase[[3]]]] = NN
  
  predict_testNN = compute(NN, data.NNtest[c(1:7)])
  predict_testNN = (predict_testNN$net.result * (max(data.scaled$ID) - min(data.scaled$ID))) + min(data.scaled$ID)
  
  max = apply(data.numeric, 2, max)
  min = apply(data.numeric, 2, min)
  scale = within(data.NNtest, s.xdata <- scale(data.NNtest, center = min, scale = max - min))
  scale = scale$s.xdata
  
  predict_testNN = as.data.frame(predict_testNN)
  max_predict = apply(predict_testNN, 2, max)
  min_predict = apply(predict_testNN, 2, min)
  scale_predict = within(predict_testNN, s.predict <- scale(predict_testNN, center = min_predict, scale = max_predict - min_predict))
  scale_predict = scale_predict$s.predict
  
  ydata = scale_predict * attr(scale, 'scaled:scale')[1] + attr(scale, 'scaled:center')[1]
  #ydata = predict_testNN * attr(scale, 'scaled:scale')[1] + attr(scale, 'scaled:center')[1]
  ydata = apply(ydata, 2, floor)
  
  confusion = (sum(data$ID[data.valid] == ydata) / length(ydata)) * 100
  main = paste("Neural Network Test Set for ", as.character(testcase[[2]]), " Hidden Layers", sep = "")
  sub = paste("Correct: ", confusion, "% ", "Incorrect: ", 100 - confusion, "%", sep = "")
        
  png(testcase[[3]])
  #plot(data.scaled$ID[data.valid], predict_testNN, col = 'blue', pch = 16, ylab = "Predicted", xlab = "Real", main = main, sub = sub)
  plot(data$ID[data.valid], ydata, col = 'blue', pch = 16, ylab = "Predicted", xlab = "Real", main = main, sub = sub)
  dev.off()
}

tc = 1
for (testcase in testcases[c(1:38,40:length(testcases))]) {
  png(testcase[[4]])
  plot(results[[tc]], rep = "best")
  dev.off()
  tc = tc + 1
}