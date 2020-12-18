rm(list = ls()); graphics.off()

library(GA) # Genetic algorithm
library(neuralnet) # ANN-MLP

source("Codes/preProcessing.R")
source("Codes/optimalMLP.R")
source("Codes/performanceMetrics.R")

# Importing data
data_df = as.numeric(AirPassengers)

# Step 1 - Data preprocessing ----
# Data splitting
perc = 0.8 
train_df = data_df[1:round(length(data_df)*perc,0)]
test_df = data_df[(round(length(data_df)*perc,0)+1):length(data_df)]
#length(data_df); length(train_df); length(test_df)

trainNorm_df = normalize(train_df, 0.2, 0.8, max(train_df), min(train_df))
testNorm_df = normalize(test_df, 0.2, 0.8, max(train_df), min(train_df))
#View(trainNorm_df); View(testNorm_df)

# Step 2 - Modelling and forecasting ----

#OptGAParameters = getOptGAParameters(); OptGAParameters

optGAParameters_df = as.data.frame(matrix(ncol = 4, nrow = 50))
names(optGAParameters_df) = c("ar", "nh1", "lr", "af")

modelResult = as.data.frame(matrix(ncol = 50, nrow = length(trainNorm_df)))

begin = proc.time()

for(i in 1:50){ #i=1
  OptGAParameters = getOptGAParameters(i); OptGAParameters
  
  trainMLP_df = getMLP(trainNorm_df = trainNorm_df, 
                 optGAParameters = OptGAParameters)
  
  length(trainNorm_df)
  
  modelResult[(OptGAParameters[1]+1):length(trainNorm_df),i] = trainMLP_df

  optGAParameters_df[i, 1] = OptGAParameters[1]
  optGAParameters_df[i, 2] = OptGAParameters[2]
  optGAParameters_df[i, 3] = OptGAParameters[3]
  optGAParameters_df[i, 4] = OptGAParameters[4]
} #View(optGAParameters_df); View(modelResult)

end = proc.time() - begin

#plot.ts(testNorm_df[11:29], lwd = 2, ylim = c(0.5, 1.05))
#lines(modelResult[[1]], col = 2, lwd = 2)
#lines(modelResult[[2]], col = 3, lwd = 2)
#legend("topleft", c("TS", "MLP_1", "MLP_2"), 
#       lty = c(1,1,1), lwd = c(2),
#       col = c(1, 2, 3), horiz = T, cex = 0.8)

write.csv(x = optGAParameters_df, file = "Results/optGAParameters_df.csv")
write.csv(x = modelResult, file = "Results/modelResult.csv")
#View(optGAParameters_df); View(modelResult)

modelResult = read.csv("Results/modelResult_02.csv", sep = ",")
modelResult[[1]] = NULL

# plot.ts(trainNorm_df, lwd = 3, ylim=c(0.2, 0.8))
# for (i in 1:50) {
#   lines(lines(modelResult[[i]], lwd = 2,
#               col = "gray", lty = 2))  
#   Sys.sleep(0.75)
# }

metrics = as.data.frame(matrix(nrow = 50, ncol = 4))

for(i in 1:50){ #i=1
  metrics[i,1] = getMSE(target = trainNorm_df, 
                      forecast = modelResult[[i]])
  metrics[i,2] = getMAPE(target = trainNorm_df, 
                        forecast = modelResult[[i]])
  metrics[i,3] = getARV(target = trainNorm_df, 
                         forecast = modelResult[[i]])
  #metrics[i,4] = getTheil(target = trainNorm_df, 
  #                      forecast = modelResult[[i]])
} 
View(metrics)

#km = kmeans(na.omit(modelResult),3)
#plot(modelResult, col =(km$cluster +1), 
#     main="K-Means result with 3 clusters", pch=20, cex=1)



#dormir <- function(seg = 1) {
#  Sys.sleep(seg)
#  return(seg)
#}

#plot.ts(trainNorm_df, lwd = 2)
#lines(modelResult$V6, col = 2, lwd = 2, lty = 2)
#lines(modelResult$V7, col = 3, lwd = 2, lty = 3)
#lines(modelResult$V8, col = 4, lwd = 2, lty = 4)


#data = read.csv("Results/optGAParameters_df.csv", sep = ";")
