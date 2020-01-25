Euclidean_Distance <- function(x, y = rep(0, dim(x)[2])){
  if(length(y)==dim(x)[2]) {
    w = rep(0, dim(x)[1])
    g = matrix(c(y = rep(0, dim(x)[2]*dim(x)[1])), nrow = dim(x)[1])
    for (i in 1:dim(x)[1]) {
      for (j in 1:dim(x)[2]){
        g[i,j] = (x[i,j]-y[j])^2
        w[i] = w[i]+g[i,j]
      }
    }
  w = sqrt(w)
  return(w)
  }
  else print(paste0("x and y do not conform, numcol x = ", dim(x)[2], ", but numcol y = ", length(y)))
}