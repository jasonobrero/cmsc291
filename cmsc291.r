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
data.NNtrain = data.scaled[data.train,]
data.NNtest = data.scaled[-data.train,]

mat = model.matrix(
    ~ ID + Size + Volume + WedgeCount + ClawCount + MaxDegree + AveDeg + Fill,
    data = data.NNtrain
)

set.seed(2)
NN = neuralnet(
    ID ~ + Size + Volume + WedgeCount + ClawCount + MaxDegree + AveDeg + Fill,
    mat,
    hidden = c(5, 3),
    linear.output = FALSE,
    stepmax = 1e+7
)