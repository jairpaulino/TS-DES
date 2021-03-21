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

# plot.ts(resultsAll_w$target)
# lines(oracleModel, col = 2)
# getMAPE(resultsAll_w$target, oracleModel)
# getMSE(resultsAll_w$target, oracleModel)
# getTheil(resultsAll_w$target, oracleModel)
# getARV(resultsAll_w$target, oracleModel)
# 
# plot.ts(resultsAll_w$target)
# lines(oracleModel, col = 2)
# getMAPE(resultsAll_w$target, oracleModel)
# getMSE(resultsAll_w$target, oracleModel)
# getTheil(resultsAll_w$target, oracleModel)
# getARV(resultsAll_w$target, oracleModel)
# 
# plot.ts(resultsAll_w$target[238:297])#, ylim=c(0.23,0.4))
# for (i in 2:31){#i=6
#   lines(resultsAll_w[[i]][238:297], col = 'gray')
# }
# lines(resultsAll_w$target[238:297], lwd=3)
# lines(oracleModel[238:297], col=2, lwd=3)
# lines(saModel[238:297], col=3, lwd=3)
# lines(smModel[238:297], col=4, lwd=3)
# abline(v=238, col=2, lwd=2, lty=2)


