rm(list = ls()); graphics.off()

library(GA)
library(neuralnet)

source("Codes/preProcessing.R")
source("Codes/optimalMLP.R")
source("Codes/performanceMetrics.R")

# Importing data
data_df = as.numeric(AirPassengers)

# Step 1 - data preprocessing ----
# Data splitting
perc = 0.8 
train_df = data_df[1:round(length(data_df)*perc,0)]
test_df = data_df[(round(length(data_df)*perc,0)+1):length(data_df)]
#length(data_df); length(train_df); length(test_df)

trainNorm_df = normalize(train_df, 0.2, 0.8, max(train_df), min(train_df))
testNorm_df = normalize(test_df, 0.2, 0.8, max(train_df), min(train_df))
#View(trainNorm_df); View(testNorm_df)

# Step 2 - Modelling ----

#OptGAParameters = getOptGAParameters(); OptGAParameters

optGAParameters_df = as.data.frame(matrix(ncol = 4, nrow = 50))
names(optGAParameters_df) = c("ar", "nh1", "lr", "af")

modelResult = NULL
for(i in 1:2){ #i=1
  OptGAParameters = getOptGAParameters(i); OptGAParameters
  model = getMLP(trainNorm_df = trainNorm_df, 
                 optGAParameters = OptGAParameters)
  
  modelResult[[i]] = model
  
  optGAParameters_df[i, 1] = OptGAParameters[1]
  optGAParameters_df[i, 2] = OptGAParameters[2]
  optGAParameters_df[i, 3] = OptGAParameters[3]
  optGAParameters_df[i, 4] = OptGAParameters[4]
} #View(optGAParameters_df); View(modelResult)

plot.ts(testNorm_df[11:29], lwd = 2, ylim = c(0.5, 1.05))
lines(modelResult[[1]], col = 2, lwd = 2)
lines(modelResult[[2]], col = 3, lwd = 2)



write.csv(x = optGAParameters_df, file = "Results/optGAParameters_df.csv")
write.csv(x = modelResult, file = "Results/modelResult.csv")

#data = read.csv("Results/optGAParameters_df.csv", sep = ";")
