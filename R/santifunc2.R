santifunc2 <- function(df){
  stopifnot(length(df)==4, is.double(c(df[[1]], df[[2]], df[[3]]))| is.numeric(c(df[[1]],df[[2]],df[[3]])), df[[4]]%%1==0)
  vect1 <- vector(mode = "double", length = length(df[[4]]))
  for (i in row_number(df[[4]])){
    x <- c(df[[1]][i], df[[2]][i], df[[3]][i])
    n <- df[[4]][i]
    vect1[i] <- santifunc1(x,n)
  }

  df[[5]] <- vect1
  names(df)[5] <- "output"
  df
  return(ggplot(df,(aes(n, y = output)))+
           geom_line())


}
