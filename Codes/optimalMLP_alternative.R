library(forecast)
library(GA)
library(doParallel)

getSE = function(target,forecast){
  SE=(target-forecast)^2
  return(SE)
}
getMSE = function(target,forecast){
  SE = getSE(target,forecast)
  MSE=mean(SE, na.rm=TRUE)
  MSE
}

nCores = detectCores()
cl = makePSOCKcluster(nCores-1)
registerDoParallel(cl)

nCores = detectCores()
dataTrain = rexp(100, 0.7)
dataTrain = AirPassengers

fitnessGA_ARIMA = function(p, d, q, data = dataTrain){
  #p = 1; ic = 3.11; d = 2.5; q = 5.3
  #ic = floor(ic)
  p = floor(p)
  d = floor(d)
  q = floor(q)
  
  #ic_list = c("aicc", "aic", "bic")
  
  model = Arima(data, order = c(p, d, q),
                method = "ML")
 
  plot.ts(data)
  lines(model$fitted, col = 2)
  plot(forecast(model, h = 5))
  return(getMSE(data, model$fitted))
}

getOptGPGA_Parameters = function(){
  
  popSize = 20

  # c() - p, d, q
  lower = c(01, 00, 01)
  upper = c(05, 02, 05)
  
  gaPar = ga(type = "real-valued", 
          fitness = function(x) -fitnessGA_ARIMA (x[1], x[2], x[3], dataTrain),
          lower = lower, upper = upper, 
          pcrossover = 0.85,
          pmutation = 0.15,
          popSize = popSize,
          elitism = base::max(1, round(popSize*0.5)),
          maxiter = 200,
          parallel = T,
          run = 20
          #seed = 1
          )
  
  gaPar@solution
  tamanho = length(summary(gaPar)$solution)
  ga_p = round(summary(gaPar)$solution[tamanho/3,1], 0)
  ga_d = round(summary(gaPar)$solution[tamanho/3,2], 0)
  ga_q = round(summary(gaPar)$solution[tamanho/3,3], 0)
  
  return(c(ga_p, ga_d, ga_q))
}

getOptILSGA_Parameters = function(){
  
  popSize = 20
  
  # c() - p, d, q
  lower = c(01, 00, 01)
  upper = c(05, 02, 05)
  
  gaPar = gaisl(type = "real-valued", 
             fitness = function(x) -fitnessGA_ARIMA (x[1], x[2], x[3]),
             numIslands = 5, 
             migrationInterval = 50, 
             lower = lower, upper = upper, 
             popSize = popSize,
             #elitism = base::max(1, round(popSize*0.5)),
             maxiter = 500
             #run = 20
             #seed = 1
  )
  
  
  GA <- gaisl(type = "real-valued", 
              fitness =  function(x) -Rastrigin(x[1], x[2]),
              lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
              popSize = 30, maxiter = 200, 
              numIslands = 4, migrationInterval = 50)
  
  
  gaPar@solution
  tamanho = length(summary(gaPar)$solution)
  ga_p = round(summary(gaPar)$solution[tamanho/3,1], 0)
  ga_d = round(summary(gaPar)$solution[tamanho/3,2], 0)
  ga_q = round(summary(gaPar)$solution[tamanho/3,3], 0)
  
  return(c(ga_p, ga_d, ga_q))
}

parGPGA = getOptGPGA_Parameters()
parISLGA = getOptILSGA_Parameters()

