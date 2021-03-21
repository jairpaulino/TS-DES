rm(list = ls()); graphics.off()

# Libraries
library(GA) # Genetic algorithm
library(neuralnet) # ANN-MLP
library(keras)
library(tensorflow)

# Importing functions
source("Codes/preProcessing.R")
source("Codes/optimalTfMLP.R")
source("Codes/performanceMetrics.R")

# Step 1 - preprocessing ----
# Importing data
# Brazil_210319.csv; 
data_df_all = read.csv("Data/US_210319.csv", sep = ";")
countryNames = 'US_icAcu'
data_df = data_df_all$inc_acu ; data_df = na.omit(data_df)
plot.ts(data_df) #View(data_df_all)

# Step 1 - Data preprocessing
# Data splitting
perc = 0.8
train_df = data_df[1:round(length(data_df)*perc,0)]
test_df = data_df[(round(length(data_df)*perc,0)+1):length(data_df)]
#length(data_df); length(train_df); length(test_df)

# Normalizing data
trainNorm_df = getNormalizedTS(train_df, 0.2, 0.8, max(train_df), min(train_df))
testNorm_df = getNormalizedTS(test_df, 0.2, 0.8, max(train_df), min(train_df))
allData_df = c(trainNorm_df, testNorm_df)
#trainNorm_df; testNorm_df

# Getting sliding windows matrix
x = getSlidingWindowMatrix(trainNorm_df, ar = 5)
X_trainData = x[,-1] %>% as.matrix()
y_trainData =  x[,1] 
#length(y_trainData)

y = getSlidingWindowMatrix(allData_df, ar = 5)
X_allData = y[,-1] %>% as.matrix()
y_allData =  y[,1] 
#length(y_allData)

# Step 2 - Modelling and forecasting 

# Tf MLP
#source("Codes/optimalTfMLP.R")
#modelTfMLP = getTfMLP(X_trainData, y_trainData, ar=5)
#predict = getPrediction(modelTfMLP, X_allData)
# plot.ts(y_allData, lwd = 2)
# lines(predict, col = 2, lwd = 2)
# abline(v=length(y_trainData), col='gray', lwd = 2,lty = 2)
# legend('topleft', c('TS', 'Tf-MLP'), col=c(1,2), lty=1, lwd = c(2,2), cex = 0.8)
# plot(y_allData~predict)

resultsAll = generatePoolOfMLP()
#View(resultsAll)

# Plots
# plot.ts(resultsAll$target, ylim=c(0.2,0.9))
# for (i in 2:31){#i=6
#   lines(resultsAll[[i]], col = 'gray')
# }
# lines(resultsAll$target, lwd=2)
# abline(v=242, col=2, lwd=2, lty=2)
# 
# library(corrplot)
#resultsAll_w = read.csv(file = 'Results/test_210319_Brazil_icAcu.csv', sep = ';')
# res = resultsAll_w
# res$target = NULL
# cor = cor(res)
# corrplot(cor)
# View(round(cor, 4))
# 
# plot.ts(resultsAll_w$target, ylim=c(0.2,1.2))
# for (i in 2:31){#i=6
#   lines(resultsAll_w[[i]], col = 'gray')
# }
# lines(resultsAll_w$target, lwd=2)
# abline(v=242, col=2, lwd=2, lty=2)

