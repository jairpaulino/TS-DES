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
data_df_all = read.csv("Data/pe_covid.csv", sep = ";")
data_df = data_df_all$conf_mm7 ; data_df = na.omit(data_df)
plot.ts(data_df)

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

y = getSlidingWindowMatrix(allData_df, ar = 5)
X_allData = y[,-1] %>% as.matrix()
y_allData =  y[,1] 

# Step 2 - Modelling and forecasting 
# Tf MLP
#source("Codes/optimalTfMLP.R")
modelTfMLP = getTfMLP(X_trainData, y_trainData, ar=5)
predict = getPrediction(modelTfMLP, X_allData)

plot.ts(y_allData, lwd = 2)
lines(predict, col = 2, lwd = 2)
abline(v=length(y_trainData), col='gray', lwd = 2,lty = 2)
legend('topleft', c('TS', 'Tf-MLP'), col=c(1,2), lty=1, lwd = c(2,2), cex = 0.8)
plot(y_allData~predict)

