#SA 

modelSA = NULL

for (i in 1:length(modelResult$V1)) {
  modelSA[i] = sum(modelResult[i,])/7
}  

modelSM = NULL

for (i in 1:length(modelResult$V1)){ #i=1
  modelSM[i] = median(as.numeric(modelResult[i,-8]))
}  

modelResult$SM = modelSM

metrics = as.data.frame(matrix(nrow = 9, ncol = 3))
names(metrics) = c("MSE", "MAPE", "ARV")
for(i in 1:9){ #i=1
  metrics[i,1] = getMSE(target = trainNorm_df, 
                        forecast = modelResult[[i]])
  metrics[i,2] = getMAPE(target = trainNorm_df, 
                         forecast = modelResult[[i]])
  metrics[i,3] = getARV(target = trainNorm_df, 
                        forecast = modelResult[[i]])
  #metrics[i,4] = getTheil(target = trainNorm_df, 
  #                      forecast = modelResult[[i]])
} 
metrics
