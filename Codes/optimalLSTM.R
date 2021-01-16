# 
library(keras)
#install_keras()

model = keras_model_sequential()
model %>%
  layer_lstm(units = 100,
             input_shape = c(datalags, 2),
             batch_size = 32,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)
model %>%
  compile(loss = 'mae', optimizer = 'adam')
model

model$inputs
model$outputs

for(i in 1:2000){
  model %>% fit(x = x.train,
                y = y.train,
                batch_size = batch.size,
                epochs = 1,
                verbose = 0,
                shuffle = FALSE)
  model %>% reset_states()
}

pred_out <- model %>% predict(x.test, batch_size = batch.size) %>% .[,1]
plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol) %>%
  add_trace(y = c(rep(NA, 2000), pred_out), x = myts$index, name = "LSTM prediction", mode = "lines")

plot(x = y.test, y = pred_out)
  

# model.add(Embedding(max_words, 64))
# model.add(LSTM(units=64))
# model.add(Dense(1, activation="sigmoid"))