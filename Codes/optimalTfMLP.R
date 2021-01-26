#' getSlidingWindowMatrix
#' 
#' @description Creates a matrix (sliding window) from a time series
#' @param ar An intenger that specifies the autoregressive length
#' @param series A time series (array)
#'
#' @return Sliding window data.frame 
#' @export
#'
#' @examples
#' ok
getSlidingWindowMatrix = function(ar, series){
  
  #series = 1:30; ar = 3; ss = 12; sar = 2
  matrix.sliding.window.ar = as.data.frame(matrix(nrow = length(series), ncol = (ar+1)))
  c = 0
  for(j in  1:(ar+1)){
    for(i in 1:length(series)){
      matrix.sliding.window.ar[(i+c),j] = series[i]
    }  
    c = c + 1
  } #head(matriz.sliding.window.ar)
  
  names_ar = NULL
  for(i in 1:(ar+1)){
    names_ar[i] = paste("t_", (i-1), sep = "")
  }
  names(matrix.sliding.window.ar) = names_ar
  
  #View(matriz.sliding.window.ar)
  return(na.omit(matriz.sliding.window.ar))
}

annMLPModel = function(trainingData, nhl){
  trainingData = na.omit(trainingData)
  set.seed(123)
  mlpModel = neuralnet(t_0 ~ .,
                       data = trainingData,
                       learningrate = 0.01,
                       stepmax = 1e+05,
                       algorithm = "rprop+" ,
                       act.fct = 'logistic',
                       hidden = c(nhl),
                       rep = 10,
  )
  return(mlpModel)
}

oneStepANN = function(model, testData){
  testData = na.omit(testData)
  oneStepANN = compute(model, testData, rep = 5)
  return(oneStepANN)
}

# Cria funcao fitness - GA
fitnessGA = function(ar, nh1, lr, af, time_series = train_df){
  #time_series = trainingSetNorm; ar = 2.7; sar = 1; ss = 15.7; nh1 = 5
  
  ar = floor(ar) #ss = floor(ss);
  lr = floor(lr) #sar = floor(sar);  la=1
  nh1 = floor(nh1) #nh2 = floor(nh2);
  af = floor(af)

  # ar = round(ar) #ss = floor(ss);
  # lr = round(lr) #sar = floor(sar);  la=1
  # nh1 = round(nh1) #nh2 = floor(nh2);
  # af = round(af)
  
  learningR = c(0.01, 0.005, 0.001)
  actFunc = c("tanh", "logistic")
  
  matriz = getAnnMatrix(time_series, ar = ar)
  matriz = na.omit(matriz)
  #View(matriz)
  
  set.seed(123)
  model_mlp = neuralnet(t_0 ~ .,
                        data = matriz,
                        learningrate = lr[learningR],
                        act.fct = actFunc[af], #"logistic",
                        algorithm = "rprop+", #learningAlg[la],
                        hidden = c(nh1),#, nh2),
                        rep = 5)
  #plot(model_mlp)
  
  #length(model_mlp$net.result[[1]])
  tamanho = length(model_mlp$data[[1]])
  
  matriz.previsao = as.data.frame(matrix(nrow = tamanho, ncol = 2))
  names(matriz.previsao) = c("obs", "forecast")
  matriz.previsao$obs = model_mlp$data[[1]]
  matriz.previsao$forecast = model_mlp$net.result[[1]]
  matriz.previsao = na.omit(matriz.previsao)
  minMSE = getMSE(matriz.previsao$obs, matriz.previsao$forecast)
  return(minMSE)  
}

# Calcula os parametros - GA
getOptGAParameters = function(seed, time_series){
  #time_series = 1:50; seed = 1
  #ar, ss, sar, nh1
  
  popSize = 20
  
  # c() - ar, nh1, lr, acf
  lower = c(02, 02, 01, 01)
  upper = c(12, 20, 04, 03)
  
  parGA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA (x[1], x[2], x[3], x[4]),
           lower = lower, upper = upper, 
           pcrossover = 0.85,
           pmutation = 0.15,
           popSize = 10,
           elitism = base::max(1, round(popSize*0.5)),
           maxiter = 200,
           parallel = T,
           run = 1,
           seed = seed)
  
  #parGA@solution
  parGA@solution
  tamanho = length(summary(parGA)$solution)
  ar = round(summary(parGA)$solution[tamanho/4,1], 0)
  #ss = round(summary(parGA)$solution[2], 0)
  #ss = round(summary(parGA)$solution[tamanho,][2], 0)
  #sar = round(summary(parGA)$solution[3], 0)
  nh1 = round(summary(parGA)$solution[tamanho/4,2], 0)
  lr = round(summary(parGA)$solution[tamanho/4,3], 0)
  af = round(summary(parGA)$solution[tamanho/4, 4], 0)

  #plot(parGA)
  
  result = c(ar, nh1, lr, af)
  return(result)
}

getMLP = function(trainNorm_df, optGAParameters){
  #OptGAParameters
  #train = normalized.data$training_set; head(train)
  #test = normalized.data$test_set; View(test)
  MLPTrain_df =  getAnnMatrix(ar = OptGAParameters[1], time_series = trainNorm_df)
  #MLPTest_df =  getAnnMatrix(ar = OptGAParameters[1], time_series = testNorm_df)
  
  #View(MLP_df)
  
  learningR = c(0.01, 0.005, 0.001)
  actFunc = c("tanh", "logistic")
  
  #beginTrain = proc.time()
  set.seed(123)
  mlpModel = neuralnet(t_0 ~ .,
                       data = MLPTrain_df,
                       learningrate = learningR[OptGAParameters[3]],
                       algorithm = "rprop+",
                       act.fct = actFunc[OptGAParameters[4]],
                       hidden = c(OptGAParameters[2]),
                       rep = 1,
                       )
  #procTimeTrain = proc.time() - beginTrain
  #beginTest = proc.time()
  #onestepMLP = compute(mlpModel, MLPTest_df, rep = 5)
  onestepMLP = as.numeric(mlpModel$net.result[[1]])
  #procTimeTest = proc.time() - beginTest
  
  #result = list()
  #result$train = (mlpModel$net.result[[1]] + mlpModel$net.result[[2]] + mlpModel$net.result[[3]])/3
  #result$test = onestepMLP$net.result
  #result$proc_time_train = procTimeTrain
  #result$proc_time_test = procTimeTest
  #names(result) = c("ar", "nh1", "lr", "af")
  return(onestepMLP)
}

mlpGA_results = function(trainSet, nModel = 5){
  #trainSet = trainNorm_df; nModel = 5
  optGAParameters_df = as.data.frame(matrix(ncol = 4, nrow = nModel))
  names(optGAParameters_df) = c("ar", "nh1", "lr", "af")

  modelResult = as.data.frame(matrix(ncol = nModel, nrow = length(trainSet)))
  
  begin = proc.time()
  for(i in 1:nModel){ #i=1
    OptGAParameters = getOptGAParameters(seed = i, trainSet)
    #OptGAParameters
    
    trainMLP_df = getMLP(trainSet, OptGAParameters)
    
    length(trainSet)
    
    modelResult[(OptGAParameters[1]+1):length(trainSet),i] = trainMLP_df
    
    optGAParameters_df[i, 1] = OptGAParameters[1]
    optGAParameters_df[i, 2] = OptGAParameters[2]
    optGAParameters_df[i, 3] = OptGAParameters[3]
    optGAParameters_df[i, 4] = OptGAParameters[4]
    
    print(optGAParameters_df)
  } #View(optGAParameters_df); View(modelResult)
  end = proc.time() - begin
  
  write.csv(x = optGAParameters_df, file = "Results/optGAParameters_pe_mlp.csv")
  write.csv(x = modelResult, file = "Results/modelResult_pe_mlp.csv")
  #View(optGAParameters_df); View(modelResult)
  
  optGAParameters_df = na.omit(optGAParameters_df)
  #View(optGAParameters_df)
  
  modelResult = read.csv("Results/modelResult_pe_mlp.csv", sep = ",")
  modelResult[[1]] = NULL; modelResult = na.omit(modelResult)
  #View(modelResult)
  
  a = length(trainSet) - length(modelResult[[1]])
  
  plot.ts(trainSet[a:length(trainNorm_df)], 
          lwd = 3, ylim=c(0.2, 0.8))
  for (i in 1:7){
    lines(lines(modelResult[[i]], lwd = 2,
                col = "gray", lty = 2))
    #Sys.sleep(0.75)
  }
  
  metrics = as.data.frame(matrix(nrow = nModel, ncol = 3))
  names(metrics) = c("MSE", "MAPE", "ARV")
  for(i in 1:nModel){ #i=1
    metrics[i,1] = getMSE(target = trainNorm_df, 
                          forecast = modelResult[[i]])
    metrics[i,2] = getMAPE(target = trainNorm_df, 
                           forecast = modelResult[[i]])
    metrics[i,3] = getARV(target = trainNorm_df, 
                          forecast = modelResult[[i]])
    #metrics[i,4] = getTheil(target = trainNorm_df, 
    #                      forecast = modelResult[[i]])
  } #metrics 
  
  write.csv(x = metrics, file = "Results/metrics_pe_mlp.csv")
  return(c(modelResult, end[[3]]))
}

