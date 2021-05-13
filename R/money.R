
money <-  function(x) {
  z <- abs(x)
  
  if(is.na(z)) {return(0)}
  
  b.index <-  z >= 1e9
  m.index <-  z >= 1e5 & z < 1e9
  
  if(x > 0) {
    output <-  paste("£", formatC(z, format = "d", big.mark = ","), sep = "")
    output[b.index] <-  paste("£", formatC(z[b.index] / 1e9, digits = 1, format = "f"), "bn", sep = "")
    output[m.index] <-  paste("£", formatC(x[m.index] / 1e6, digits = 1, format = "f"), "m", sep = "")
    return(output)
  } else {
    output <-  paste("-£", formatC(z, format = "d", big.mark = ","), sep = "")
    output[b.index] <-  paste("-£", formatC(z[b.index] / 1e9, digits = 1, format = "f"), "bn", sep = "")
    output[m.index] <-  paste("-£", formatC(z[m.index] / 1e6, digits = 1, format = "f"), "m", sep = "")
    return(output)
  }
}