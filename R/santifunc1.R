santifunc1 <- function(x,n){
  stopifnot(length(x)>=3, n>0)
  vec1 <- vector(mode = "double", length = n)
  for (i in seq_along(vec1)) {
    if (i>3){
      vec1[i] <- vec1[i-1]+(vec1[i-3]-vec1[i-2])/i
    } else if(i==1){
      vec1[i] <- x[1]
    } else if (i==2){
      vec1[i] <- x[2]
    } else if (i==3){
      vec1[i] <- x[3]
    }
  }
  return(tail(vec1, n=1))
}

#Test

santifunc1(x = c(2, 4, 3), n = 4)

santifunc1(x = c(2, 4, 3), n = 6)
