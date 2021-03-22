# get SA and SM models
getSaAndSmModels = function(resultsAll_w){
  # Create SA model
  saModel = NULL
  for (i in 2:length(resultsAll_w$target)){#i=6
    saModel[i] = mean(as.numeric(resultsAll_w[i,c(2:31)]))
  }
  
  # Create SM model
  smModel = NULL
  for (i in 2:length(resultsAll_w$target)){#i=6
    smModel[i] = median(as.numeric(resultsAll_w[i,c(2:31)]))
  }
  return(list('saModel' = saModel,
              'smModel' = smModel))
}


getOracleModel = function(resultsAll_w){
  # Calculate distanceToTarget
  distanceToTarget = as.data.frame(matrix(ncol=30, nrow=length(resultsAll_w$target)))
  names(distanceToTarget) = names(resultsAll_w[c(-1)])
  for (i in 1:30){#i=2
    distanceToTarget[,(i)] = abs(resultsAll_w[,1] - resultsAll_w[,i+1])
  } #View()
  
  posMin = max.col(-distanceToTarget) # column position of minimum
  
  # Create ORACLE model
  oracleModel = NULL
  for (i in 1:length(resultsAll_w$target)){#i=4
    oracleModel[i] = resultsAll_w[i,(posMin[i]+1)]
  }
  return(list('oracleModel' = oracleModel))
}
