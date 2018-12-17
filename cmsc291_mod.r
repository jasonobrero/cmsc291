#install.packages(c("plyr", "neuralnet"))

require(plyr)
require(neuralnet)
library(plyr)
library(neuralnet)
data = read.csv("data_mod.csv", header = TRUE)
data.numeric = data.frame(sapply(data, function(x) as.numeric(as.character(x))))

data.train = data.numeric[,"Dataset"] == 0
data.valid = data.numeric[,"Dataset"] == 1

data.numeric$Dataset = NULL

max = apply(data.numeric, 2, max)
min = apply(data.numeric, 2, min)

data.scaled = as.data.frame(scale(data.numeric, center = min, scale = max - min))
data.NNtrain = data.scaled[data.train == TRUE,]
data.NNtest = data.scaled[data.train == FALSE,]

dependent = ""
colnames = c()
xdat = c()
for (class in c(1:50)) {
    if(class != 50){
      dependent = paste(dependent, "Class", as.character(class), " + ", sep = "")  
    }else{
      dependent = paste(dependent, "Class", as.character(class), sep = "")
    }
    colnames = c(colnames, class)
    xdat = c(xdat, class, class, class, class)
}
formula = paste("~", dependent, "+ Size + Volume + WedgeCount + ClawCount + MaxDegree + AveDeg + Fill")
formula = as.formula(formula)

formula.nn = paste(dependent, "~", "+ Size + Volume + WedgeCount + ClawCount + MaxDegree + AveDeg + Fill")
formula.nn = as.formula(formula.nn)

mat = model.matrix(
    formula,
    data = data.NNtrain
)

set.seed(2)
results = list()
for (testcase in testcases) {
  print(testcase[[1]])
  NN = neuralnet(
    formula.nn,
    mat,
    lifesign = "full",
    algorithm = "rprop+",
    threshold = 0.08,
    hidden = testcase[[1]],
    linear.output = FALSE,
    stepmax = 1e+8
  )

  results[[testcase[[2]]]] = NN

  predict_testNN = compute(NN, data.NNtest[c(51:57)])
  predict_testNN = (predict_testNN$net.result * (max(data.scaled[data.valid,c(1:50)]) - min(data.scaled[data.valid,c(1:50)]))) + min(data.scaled[data.valid,c(1:50)])
  
  colnames(predict_testNN) = colnames
  predict_testNN = as.data.frame(predict_testNN)
  # ydat = as.numeric(colnames(predict_testNN)[max.col(predict_testNN, ties.method = "random")])
  ydat = apply(predict_testNN, 1, which.min)

  png(testcase[[3]])
  plot(xdat, ydat, col = 'blue', pch = 16, ylab = "Predicted", xlab = "Real")
  dev.off()
}

tc = 1
for (testcase in testcases) {
  png(testcase[[4]])
  plot(results[[tc]], rep = "best")
  dev.off()
  tc = tc + 1
}

saveRDS(results, file = "neuralnets.rds")