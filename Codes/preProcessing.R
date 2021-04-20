# Normalize functions
getNormalizedTS = function(array, x = 0, y = 1, max, min){
  #Normalize to [0, 1]
  range = max - min
  norm1 = (array - min) / range
  
  #Then scale to [x,y]
  range2 = y - x
  normalized = (norm1*range2) + x
  return(normalized)
}

# Normalize functions
getDenormalizedTS = function(array, max, min){
  #Normalize to [0, 1]
  range = max - min
  deNorm = array*range + min
  return(deNorm)
}

