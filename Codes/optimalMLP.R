# Criar matrix a partir da ST
getAnnMatrix = function(ar, ss, sar, time_series=trainNorm_df){
  
  #time_series = 1:30; ar = 3; ss = 12; sar = 2
  matriz.sliding.window.ar = as.data.frame(matrix(nrow = length(time_series), ncol = (ar+1)))
  c = 0
  for(j in  1:(ar+1)){
    for(i in 1:length(time_series)){
      matriz.sliding.window.ar[(i+c),j] = time_series[i]
    }  
    c = c + 1
  } #head(matriz.sliding.window.ar)
  
  names_ar = NULL
  for(i in 1:(ar+1)){
    names_ar[i] = paste("t_", (i-1), sep = "")
  }
  names(matriz.sliding.window.ar) = names_ar
  
  #View(matriz.sliding.window.ar)
  return(na.omit(matriz.sliding.window.ar))
  
}

annMLPModel = function(trainingData, nhl){
  trainingData = na.omit(trainingData)
  set.seed(i)
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
fitnessGA = function(ar, nh1, lr, af, serie_temporal = trainNorm_df){
  #time_series = trainingSetNorm; ar = 2.7; sar = 1; ss = 15.7; nh1 = 5
  
  ar = floor(ar); #ss = floor(ss);
  lr = floor(lr); #sar = floor(sar);  la=1
  nh1 = floor(nh1) #nh2 = floor(nh2);
  af = floor(af)
  
  learningR = c(0.01, 0.005, 0.,001)
  actFunc = c("tanh", "logistic")
  
  matriz = getAnnMatrix(trainNorm_df, ar = ar)
  matriz = na.omit(matriz)
  #View(matriz)
  
  set.seed(i)
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
getOptGAParameters = function(i){
  #time_series = train.set; ar, ss, sar, nh1
  
  popSize = 20
  
  # c() - ar, ss, sar, nh1
  #lower = c(01, 10, 01, 01)
  #upper = c(06, 20, 05, 20)

  # c() - ar, nh1, lr, acf
  lower = c(02, 02, 01, 01)
  upper = c(12, 20, 04, 03)
  
  parGA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA (x[1], x[2], x[3], x[4]),
           lower = lower, upper = upper, 
           pcrossover = 0.85,
           pmutation = 0.15,
           popSize = 30,
           elitism = base::max(1, round(popSize*0.5)),
           maxiter = 200,
           parallel = T,
           run = 5,
           seed = i)
  
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

  plot(parGA)
  
  result = c(ar, nh1, lr, af)
  return(result)
}

getMLP = function(trainNorm_df, optGAParameters){
  #OptGAParameters
  #train = normalized.data$training_set; head(train)
  #test = normalized.data$test_set; View(test)
  MLPTrain_df =  getAnnMatrix(ar = OptGAParameters[1], time_series = trainNorm_df)
  MLPTest_df =  getAnnMatrix(ar = OptGAParameters[1], time_series = testNorm_df)
  
  #View(MLP_df)
  
  learningR = c(0.01, 0.005, 0.,001)
  actFunc = c("tanh", "logistic")
  
  #beginTrain = proc.time()
  set.seed(i)
  mlpModel = neuralnet(t_0 ~ .,
                       data = MLPTrain_df,
                       learningrate = learningR[OptGAParameters[3]],
                       algorithm = "rprop+",
                       act.fct = actFunc[OptGAParameters[4]],
                       hidden = c(OptGAParameters[2]),
                       rep = 5,
                       )
  #procTimeTrain = proc.time() - beginTrain
  #beginTest = proc.time()
  onestepMLP = compute(mlpModel, MLPTest_df, rep = 5)
  #procTimeTest = proc.time() - beginTest
  
  #result = list()
  #result$train = (mlpModel$net.result[[1]] + mlpModel$net.result[[2]] + mlpModel$net.result[[3]])/3
  #result$test = onestepMLP$net.result
  #result$proc_time_train = procTimeTrain
  #result$proc_time_test = procTimeTest
  #names(result) = c("ar", "nh1", "lr", "af")
  return(onestepMLP$net.result)
}

