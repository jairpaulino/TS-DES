#' getSlidingWindowMatrix
#' 
#' @description Creates a matrix (sliding window) from a time series
#' @param ar An intenger that specifies the autoregressive length
#' @param series A time series (array)
#'
#' @return A data.frame - Sliding window.  
#' @export
#'
#' @examples
#' ok
getSlidingWindowMatrix14SA = function(series, ar=5){
  
  #series = trainNorm_df; ar = 5
  matrix.sliding.window.ar = as.data.frame(matrix(nrow = length(series), ncol = (ar+1)))
  c = 0
  for(j in  1:(ar+1)){
    for(i in 1:length(series)){
      if(j == 1){
        matrix.sliding.window.ar[(i+c), j] = series[i+14]
      }else{
      matrix.sliding.window.ar[(i+c), j] = series[i]
      } 
    } 
    c = c + 1
  } 
  #View(matrix.sliding.window.ar)
  names_ar = NULL
  for(i in 1:(ar+1)){
    names_ar[i] = paste("t_", (i-1), sep = "")
  }
  names(matrix.sliding.window.ar) = names_ar
  
  #View(matriz.sliding.window.ar)
  return(na.omit(matrix.sliding.window.ar))
}


getSlidingWindowMatrix = function(series, ar=5){
  
  #series = trainSeries; ar = 3
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
  return(na.omit(matrix.sliding.window.ar))
}

#' getTfMLP
#' 
#' @description
#' @param
#' @return
#' @export
#'
#' @examples
getTfMLP = function(X_trainData, y_trainData, ar=5){
  #library(keras); library(tensorflow)
  #install_tensorflow(); install_keras()
  #trainSeries = trainNorm_df; allSeries = allData_df
  
  #use_session_with_seed(123)
  
  model = keras_model_sequential()
  model %>% 
    layer_dense(units = 20, activation = "relu", 
                input_shape = ar) %>% 
    #layer_dense(units = 20, activation = "relu") %>% 
    #layer_dense(units = 20, activation = "relu") %>% 
    #layer_dense(units = 20, activation = "relu") %>% 
    layer_dense(units = 1, activatio = "linear")
  
  model %>% compile(loss = "mean_squared_error",
                    optimizer  = "rmsprop",
                    metrics = 'mean_absolute_error')
  set.seed(123)
  history = model %>% 
    fit(X_trainData, 
        y_trainData,
        batch_size = 1, 
        verbose = T,
        epochs = 50,
        validation_split = 0.25,
    ) #plot(history)
  
  model %>% evaluate(X_trainData, y_trainData)
  #summary(model)
  
  return(model)
}

#' getPrediction
#'
#' @param model 
#' @param series 
#'
#' @return
#' @export
#'
#' @examples
getPrediction = function(model, series){
  #series = X_all_1
  pred = model %>%
    #predict_classes(X_all_1)
    #predict_on_batch(X_all_1)
    predict_on_batch(series)
    #predict(X_all_1)
    #prediction(X_all_1)
  return(pred)
}

createMatrixResults = function(n = y_trainData){
  resultsMatrix = as.data.frame(matrix(ncol = 31, nrow = length(n)))
  
  names = NULL
  for(i in 1:30){#i=1
     names = c(names, paste('model_', i, sep=''))
  }
  
  names(resultsMatrix) = c('target', names)
  resultsMatrix$target = n 
  #View(resultsMatrix)
  return(resultsMatrix)
}

generatePoolOfMLP = function(){
  m = 2; poolMLP = 1:(length(trainNorm_df)-5)
  #write.table(trainNorm_df, file = 'poolMLP.csv', sep = ';', col.names = 'target')
  
  resultsTrain = createMatrixResults(n = y_trainData)
  resultsAll = createMatrixResults(n = y_allData)
  
  epochs = 30
  
  y_allData
  acfFun = c('relu', 'tanh', 'sigmoid')
  nNeurons = c(10, 20, 30, 50, 100)
  for (i in 1:3){ #'relu', 'tanh', 'sigmoid'
    for (j in 1:2){ #1L, 2L
      for (k in 1:5){ #5, 10, 15, 20, 25
        if(j==1){ #i=1; k=1; j=1
          model = keras_model_sequential()
          model %>% 
            layer_dense(units = nNeurons[k], activation = acfFun[i], 
                        input_shape = 5) %>% 
            layer_dense(units = 1, activatio = "linear")
          
          model %>% compile(loss = "mean_squared_error",
                            optimizer  = "rmsprop",
                            metrics = 'mean_absolute_error')
          set.seed(123)
          history = model %>% 
            fit(X_trainData, 
                y_trainData,
                batch_size = 1, 
                verbose = T,
                epochs = epochs,
                validation_split = 0.25,
            ) #plot(history)
          
          model %>% evaluate(X_trainData, y_trainData)
          #summary(model)
        
          predict = getPrediction(model, X_allData)
        
          resultsAll[[m]] = predict
          
          print(paste('*********', m, '*********'))
          m = m + 1
        
          #return(model)
        }else{
          
          model = keras_model_sequential()
          model %>% 
            layer_dense(units = nNeurons[k], activation = acfFun[i], 
                        input_shape = 5) %>% 
            layer_dense(units = 1, activatio = "linear")
          
          model %>% compile(loss = "mean_squared_error",
                            optimizer  = "rmsprop",
                            metrics = 'mean_absolute_error')
          set.seed(123)
          history = model %>% 
            fit(X_trainData, 
                y_trainData,
                batch_size = 1, 
                verbose = T,
                epochs = epochs,
                validation_split = 0.25,
            ) #plot(history)
          
          model %>% evaluate(X_trainData, y_trainData)

          predict = getPrediction(model, X_allData)
          
          resultsAll[[m]] = predict
          
          print(paste('*********', m, '*********'))
          m = m + 1
          
        }        
      }
    }
  }
  
  View(resultsAll)
  write.table(resultsAll, file = paste('Results/test_210319_', countryNames, '.csv', sep = ''), sep = ";")
  plot.ts(resultsAll$target, ylim=c(0.2,0.9))
  for (i in 2:31){#i=6
    lines(resultsAll[[i]], col = 'gray')
  }
  lines(resultsAll$target, lwd=2)
  abline(v=242, col=2, lwd=2, lty=2)
  return(resultsAll)
}


