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
source('Codes/oracleModel.R')
source('Codes/desbedModel.R')

# Step 1 - preprocessing ----
# Importing data
# Brazil_icDia; Brazil_icMm7; Brazil_icAcu
data_df_all = read.csv("Data/UK_210319.csv", sep = ";")
countryNames = 'UK_icAcu'
data_df = data_df_all$inc_acu; data_df = na.omit(data_df)
plot.ts(data_df) #View(data_df_all)
#head(data_df, 10)


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

m = length(y_trainData)
n = length(y_allData)

# Step 2 - Modelling and forecasting ----

# Get Tf-MLP pool of predictors
#resultsAll = generatePoolOfMLP()

# Ready results
#countryNames = 'Brazil_icDia'
resultsAll_w = read.csv(file = paste('Results/test_210319_', countryNames, '.csv', sep=""), sep = ';')

# Calculate SA, SM, Oracle and DESBED models
saModel = getSaAndSmModels(resultsAll_w)$saModel
smModel = getSaAndSmModels(resultsAll_w)$smModel
oracleModel = getOracleModel(resultsAll_w)$oracleModel
DESBED = getDesbedModel(y_trainData, y_allData, resultsAll_w)

# Calculate metrics
metricsMatrix = data.frame(matrix(ncol=5, nrow = 8))
colnames(metricsMatrix) = c("MSE", "MAE", "MAPE", "THEIL", "ARV")
rownames(metricsMatrix) = c("Oracle", "SA", "SM", "k_1", "k_2", "k_3", "k_4", "k_5")

metricsMatrix[1,] = getMetrics(resultsAll_w$target[(m+1):n], oracleModel[(m+1):n])
metricsMatrix[2,] = getMetrics(resultsAll_w$target[(m+1):n], saModel[(m+1):n])
metricsMatrix[3,] = getMetrics(resultsAll_w$target[(m+1):n], smModel[(m+1):n])
metricsMatrix[4,] = getMetrics(resultsAll_w$target[(m+1):n], DESBED$DESBED_01)
metricsMatrix[5,] = getMetrics(resultsAll_w$target[(m+1):n], DESBED$DESBED_02_c)
metricsMatrix[6,] = getMetrics(resultsAll_w$target[(m+1):n], DESBED$DESBED_03_c)
metricsMatrix[7,] = getMetrics(resultsAll_w$target[(m+1):n], DESBED$DESBED_04_c)
metricsMatrix[8,] = getMetrics(resultsAll_w$target[(m+1):n], DESBED$DESBED_05_c)

write.table(metricsMatrix, file = paste('Results/k_metricsMatrix_', countryNames, '.csv', sep = ''), sep = ";")

resultsAll_w = na.omit(resultsAll_w)
oracleModel = na.omit(oracleModel)
saModel = na.omit(saModel)
smModel = na.omit(smModel)
resultsAll_w = na.omit(resultsAll_w)
# DESBED$DESBED_01 = na.omit(DESBED$DESBED_01)
# DESBED$DESBED_02_c = na.omit(DESBED$DESBED_02_c)
# DESBED$DESBED_03_c = na.omit(DESBED$DESBED_03_c)
# DESBED$DESBED_04_c = na.omit(DESBED$DESBED_04_c)
# DESBED$DESBED_05_c = na.omit(DESBED$DESBED_05_c)

library(RColorBrewer)
jpeg(file = paste("Results/Figure/",'K_' ,countryNames, '.jpeg', sep = ""), width = 1200, height = 800, res = 150, quality=100 )
col = c(1, 'gray', brewer.pal(6, "Set1")) # "Set2", 'Dark2'
lwd = 2; lty = c(1, 1, 1, 1, 1, 1, 1, 1) #c(NA, NA, NA, 2, 2, 2)
pch = c(NA, NA, 15, 16, 17,18, 19, 20)

length(resultsAll_w$target[(m+1):n])

plot.ts(resultsAll_w$target[(m+1):n], 
        ylim = c(min(resultsAll_w[(m+1):n,]), max(resultsAll_w[(m+1):n,])*1.1),
        xlab = 'Index (test set)', ylab = countryNames)
for (i in 2:31){#i=6
  lines(resultsAll_w[[i]][(m+1):n], col = col[2], lwd=lwd)
}
lines(resultsAll_w$target[(m+1):n], lwd = lwd)
lines(oracleModel[(m+1):n], col=col[3], lwd = lwd)
points(oracleModel[(m+1):n], col=col[3], pch=pch[3])
lines(DESBED$DESBED_01, col=col[4], lwd = lwd,  lty = lty[4])
points(DESBED$DESBED_01, col=col[4], pch=pch[4])
lines(DESBED$DESBED_02_c, col = col[5], lwd = lwd,  lty = lty[6])
points(DESBED$DESBED_02_c, col = col[5], lwd = lwd, pch = pch[5])
lines(DESBED$DESBED_03_c, col = col[6], lwd = lwd,  lty = lty[6])
points(DESBED$DESBED_03_c, col = col[6], lwd = lwd, pch = pch[6])
lines(DESBED$DESBED_04_c, col = col[7], lwd = lwd,  lty = lty[6])
points(DESBED$DESBED_04_c, col = col[7], lwd = lwd, pch = pch[7])
lines(DESBED$DESBED_05_c, col = col[8], lwd = lwd,  lty = lty[6])
points(DESBED$DESBED_05_c, col = col[8], lwd = lwd, pch = pch[8])

legend = c('TS', 'MLPs', 'Oracle', 'k_1', 'k_2', 'k_3', 'k_4', 'k_5')
legend('top', legend =  legend, horiz = T, cex = 0.8, col = col, 
       lty=1, lwd=2, pch = pch, inset = 0.02)
dev.off()


