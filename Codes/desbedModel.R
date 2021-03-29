# Create DESBED model
getDesbedModel = function(y_trainData, y_allData, resultsAll_w){
  
  DESBED = NULL; DESBED_01 = NULL; 
  DESBED_02 = NULL; DESBED_02_c = NULL;
  DESBED_03 = NULL; DESBED_03_c = NULL;
  DESBED_04 = NULL; DESBED_04_c = NULL;
  DESBED_05 = NULL; DESBED_05_c = NULL;
  
  m = length(y_trainData)
  n = length(y_allData)
  for (j in (m+1):n){#j=m+1
    
    allCandidates = X_allData
    
    # Calcula o DESBED_01
    # Calcula a distMin_df e seleciona a menor 
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
    
    # Calcula o DESBED_02
    allCandidates[distMinIndex, ] = c(rep(10, 5))
  
    # Calcula a distMin_df e seleciona a SEGUNDA menor
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
  
  # seleciona o SEGUNDO melhor candidato 
  candidate = resultsAll_w[distMinIndex,]
  plot(as.numeric(candidate)[2:31])
  
  metricsCandidate = NULL
  for(k in 2:31){#k=2
    metricsCandidate[k] = getMSE(candidate$target, as.numeric(candidate[k]))
  }
  
  plot(metricsCandidate[2:30])
  modelCand_02 = which.min(metricsCandidate)
  
  DESBED_02[j-m] = as.numeric(resultsAll_w[j, c(modelCand_02)])
  
  p = 0.5
  DESBED_02_c[j-m] = p*DESBED_01[j-m] + p*DESBED_02[j-m]
  
  # Calcula o DESBED_03
  allCandidates[distMinIndex, ] = c(rep(10, 5))
  
  # Calcula a distMin_df e seleciona a TERCEIRA menor
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
  
  # seleciona o SEGUNDO melhor candidato 
  candidate = resultsAll_w[distMinIndex,]
  plot(as.numeric(candidate)[2:31])
  
  metricsCandidate = NULL
  for(k in 2:31){#k=2
    metricsCandidate[k] = getMSE(candidate$target, as.numeric(candidate[k]))
  }
  
  plot(metricsCandidate[2:30])
  modelCand_03 = which.min(metricsCandidate)
  
  DESBED_03[j-m] = as.numeric(resultsAll_w[j, c(modelCand_03)])
  
  p = 0.33
  DESBED_03_c[j-m] = p*DESBED_01[j-m] + p*DESBED_02[j-m] + p*DESBED_03[j-m]
  
  # Calcula o DESBED_04
  allCandidates[distMinIndex, ] = c(rep(10, 5))
  
  # Calcula a distMin_df e seleciona a QUARTA menor
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
  
  # seleciona o QUARTO melhor candidato 
  candidate = resultsAll_w[distMinIndex,]
  plot(as.numeric(candidate)[2:31])
  
  metricsCandidate = NULL
  for(k in 2:31){#k=2
    metricsCandidate[k] = getMSE(candidate$target, as.numeric(candidate[k]))
  }
  
  plot(metricsCandidate[2:30])
  modelCand_04 = which.min(metricsCandidate)
  
  DESBED_04[j-m] = as.numeric(resultsAll_w[j, c(modelCand_04)])
  
  p = 0.25
  DESBED_04_c[j-m] = p*DESBED_01[j-m] + p*DESBED_02[j-m] + p*DESBED_03[j-m] + p*DESBED_04[j-m]
  
  
  # Calcula o DESBED_05
  allCandidates[distMinIndex, ] = c(rep(10, 5))
  
  # Calcula a distMin_df e seleciona a QUINTO menor
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
  
  # seleciona o QUINTO melhor candidato 
  candidate = resultsAll_w[distMinIndex,]
  plot(as.numeric(candidate)[2:31])
  
  metricsCandidate = NULL
  for(k in 2:31){#k=2
    metricsCandidate[k] = getMSE(candidate$target, as.numeric(candidate[k]))
  }
  
  plot(metricsCandidate[2:30])
  modelCand_05 = which.min(metricsCandidate)
  
  DESBED_05[j-m] = as.numeric(resultsAll_w[j, c(modelCand_05)])
  
  p = 0.2
  DESBED_05_c[j-m] = p*DESBED_01[j-m] + p*DESBED_02[j-m] + p*DESBED_03[j-m] + p*DESBED_04[j-m] + p*DESBED_05[j-m]
  }
  
  
  #DESBED_04_c == DESBED_05_c
  
  return(list('DESBED_01' = DESBED_01,
              'DESBED_02' = DESBED_02,
              'DESBED_02_c' = DESBED_02_c,
              'DESBED_03' = DESBED_03,
              'DESBED_03_c' = DESBED_03_c,
              'DESBED_04' = DESBED_04,
              'DESBED_04_c' = DESBED_04_c,
              'DESBED_05' = DESBED_05,
              'DESBED_05_c' = DESBED_05_c))
  
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

# 
# library(rgl)
# plot3d(x = X_trainData[1:150,1],
#        y = X_trainData[1:150,2],
#        z = X_trainData[1:150,3], 
#        size = 5,
#        xlab = "x_0",  ylab = "x_1,", zlab = "x_2,")
# plot3d(x = X_trainData[160,1],
#        y = X_trainData[160,2],
#        z = X_trainData[150,3], 
#        size = 50, add = T, col = 2)

