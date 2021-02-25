#' Returns the nth term from the recursive sequence xn=xn−1+(xn−3−xn−2)/n.
#'
#' @param x a vector of length 3
#' @param n the number of terms for the recursive sequence
#'
#' @return returns the nth term from the recursive sequence
#' @export santifunc1
#'
#' @examples santifunc1(x = c(2, 4, 3), n = 2)
#' santifunc1(x = c(2, 4, 3), n = 6)
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
  return(utils::tail(vec1, n=1))
}


