#' Returns a line graph for multiple values that go into santifunc1
#'
#' @param df A data frame that contains four columns. The first three columns are the values of the three numerics to be input to function 1 and the fourth column is the positive integer n for the sequence to be generated.
#'
#' @return Returns a line graph for multiple values that go into santifunc1
#' @export santifunc2
#'
#' @importFrom ggplot2 ggplot aes geom_line
#'
#' @examples
#'
santifunc2 <- function(df){
  stopifnot(length(df)==4, is.double(c(df[[1]], df[[2]], df[[3]]))| is.numeric(c(df[[1]],df[[2]],df[[3]])), df[[4]]%%1==0)
  vect1 <- vector(mode = "double", length = length(df[[4]]))
  for (i in dplyr::row_number(df[[4]])){
    x <- c(df[[1]][i], df[[2]][i], df[[3]][i])
    n <- df[[4]][i]
    vect1[i] <- santifunc1(x,n)
  }

  df[[5]] <- vect1
  names(df)[5] <- "output"
  df
  return(ggplot(df,(aes(x= n, y = output)))+
           geom_line())


}
