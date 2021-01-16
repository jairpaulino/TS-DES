rm(list = ls()); graphics.off()

library(GA) # Genetic algorithm
library(neuralnet) # ANN-MLP

source("Codes/preProcessing.R")
source("Codes/optimalMLP.R")
source("Codes/performanceMetrics.R")

# Importing data
data_df_all = read.csv("Data/pe_covid.csv", sep = ";")
data_df = data_df_all$conf_inc

# Step 1 - Data preprocessing
# Data splitting
perc = 0.8 
train_df = data_df[1:round(length(data_df)*perc,0)]
test_df = data_df[(round(length(data_df)*perc,0)+1):length(data_df)]
#length(data_df); length(train_df); length(test_df)

trainNorm_df = normalize(train_df, 0.2, 0.8, max(train_df), min(train_df))
testNorm_df = normalize(test_df, 0.2, 0.8, max(train_df), min(train_df))
#View(trainNorm_df); View(testNorm_df)

# Step 2 - Modelling and forecasting 
# MLP GA
source("Codes/optimalMLP.R")
mlpGA = mlpGA_results(trainNorm_df, 2)



