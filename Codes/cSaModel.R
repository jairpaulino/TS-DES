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