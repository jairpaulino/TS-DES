# Create DESBED model
getDesbedModel = function(y_trainData, y_allData, resultsAll_w){
  
  DESBED = NULL; DESBED_01 = NULL; 
  DESBED_02 = NULL; DESBED_03 = NULL
  DESBED_04 = NULL; DESBED_05 = NULL
  
  m = length(y_trainData)
  n = length(y_allData)
  for (j in (m+1):n){#j=m+1
    
    allCandidates = X_allData
    
    # calcula a distMin 
    distVector = NULL
    distMin = 1e10
    distMinIndex = NULL
    for (i in 1:m){ #i=1
      a = allCandidates[i,]
      b = allCandidates[j,]
      
      distVector = dist(rbind(a, b), method = 'euclidean')
      # "euclidean", "maximum", "manhattan", 
      # "canberra", "binary" or "minkowski"
      if(distVector < distMin){
        distMin = distVector
        distMinIndex = i 
      }
    }
    
    print(paste(distMinIndex, round(distMin, 4)))
    
    # seleciona o PRIMEIRO melhor candidato 
    candidate = resultsAll_w[distMinIndex,]
    plot(as.numeric(candidate)[2:31])
    
    metricsCandidate = NULL
    for(k in 2:31){#k=2
      metricsCandidate[k] = getMSE(candidate$target, as.numeric(candidate[k]))
    }
    
    plot(metricsCandidate[2:30])
    modelCand_01 = which.min(metricsCandidate)
    
    DESBED_01[j-m] = as.numeric(resultsAll_w[j, c(modelCand_01)])
    DESBED[j-m] = DESBED_01[j-m] #+ 0.2*DESBED_02[j-m] + 0.15*DESBED_03[j-m] + 0.1*DESBED_04[j-m] +  0.05*DESBED_05[j-m]
  }
  
  
  return(list('DESBED' = DESBED))
  
}

getMetrics = function(target, forecast){
  
  #target = resultsAll_w$target[(m+1):n]
  #forecast = smModel[(m+1):n]
  
  target = na.omit(target)
  forecast = na.omit(forecast)
  metricsResults = data.frame(matrix(ncol=5, nrow = 1))
  colnames(metricsResults) = c("MSE", "MAE", "MAPE", "THEIL", "ARV")

  metricsResults$MSE = getMSE(target, forecast)
  metricsResults$MAE = getMAE(target, forecast)
  metricsResults$MAPE = getMAPE(target, forecast)
  metricsResults$THEIL = getTheil(target,forecast)
  metricsResults$ARV = getARV(target, forecast)
  
  return(metricsResults)
}

# plot.ts(resultsAll_w$target[(m+1):n], ylim=c(0.6, 1.2))
# for (i in 2:31){#i=6
#   lines(resultsAll_w[[i]][(m+1):n], col = 'gray', lwd=2)
# }
# lines(resultsAll_w$target[(m+1):n], lwd=3)
# lines(oracleModel[(m+1):n], col=2, lwd=3)
# lines(saModel[(m+1):n], col=3, lwd=3)
# lines(smModel[(m+1):n], col=4, lwd=3)
# lines(DESBED, col = 6, lwd = 3, lty=3)

# getMSE(oracleModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMSE(saModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMSE(smModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMSE(DESBED, resultsAll_w$target[(m+1):n])
# 
# getTheil(oracleModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getTheil(saModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getTheil(smModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getTheil(resultsAll_w$target[(m+1):n], DESBED)
# 
# getMAE(oracleModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMAE(saModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMAE(smModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMAE(DESBED, resultsAll_w$target[(m+1):n])
# 
# getMAPE(oracleModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMAPE(saModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMAPE(smModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getMAPE(DESBED, resultsAll_w$target[(m+1):n])
# 
# getARV(oracleModel[(m+1):n], resultsAll_w$target[(m+1):n])
# getARV(saModel[(m+1):n], resultsAll_w$target[(m+1):n])
# # getARV(smModel[(m+1):n], resultsAll_w$target[(m+1):n])
# # getARV(DESBED, resultsAll_w$target[(m+1):n])
# 
# plot.ts(resultsAll_w$target)#, ylim=c(0.2,0.9))
# for (i in 2:31){#i=6
#   lines(resultsAll_w[[i]], col = 'gray')
# }
# lines(resultsAll_w$target[(m+1):n], lwd=3)
# lines(oracleModel[(m+1):n], col=2, lwd=3)
# lines(resultsAll_w$model_24, col=4, lwd=3)
# lines(smModel, col=3, lwd=3)
# #lines(DESBED, col = 6, lwd = 2)
# #lines(resultsAll_w$model_16[(m+1):n], col=4, lwd=3)
# abline(v=238, col=2, lwd=2, lty=2)
# 

