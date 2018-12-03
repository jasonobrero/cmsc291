#install.packages(c("plyr", "neuralnet"))

require(plyr)
require(neuralnet)
library(plyr)
library(neuralnet)
data = read.csv("data.csv", header = TRUE)

data$FILE = NULL
data$BMT = NULL
data$REPLICATE = NULL
data$CLASS2 = NULL

data.train = data[data[,'DATA.SET'] == "Training",]
data.valid = data[data[,'DATA.SET'] == "Validation",]
data.cross = data[data[,'DATA.SET'] == "Cross-validation",]

data$DATA.SET = NULL
data.train$DATA.SET = NULL
data.valid$DATA.SET = NULL
data.cross$DATA.SET = NULL

factors = as.factor(unique(data$CLASS))
data$CLASS = mapvalues(x = data$CLASS, from=factors, to=as.numeric(factors))
data = as.data.frame(data.matrix(data))

max = apply(data, 2, max)
min = apply(data, 2, min)

data.scaled = as.data.frame(scale(data, center = min, scale = max - min))
data.NNtrain = data.scaled[as.numeric(row.names(data.train)),]
data.NNtest = data.scaled[-as.numeric(row.names(data.train)),]

mat = model.matrix(
    ~ CLASS + C.LEVEL + M.LEVEL + VERTICES + CONNECTED.EDGES + WEDGE.COUNT +
          CLAW.COUNT + MAX.DEGREE, 
    data = data.NNtrain
)

set.seed(2)
NN = neuralnet(
    CLASS ~ 
        C.LEVEL + M.LEVEL + VERTICES + CONNECTED.EDGES + WEDGE.COUNT +
        CLAW.COUNT + MAX.DEGREE,
    mat,
    hidden = c(5, 3),
    stepmax = 1e+7
)