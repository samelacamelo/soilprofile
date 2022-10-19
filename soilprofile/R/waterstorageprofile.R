#' Water storage
#'
#' This function is used to calculate the water storage index of a given point in soil
#' Using three different mathematical methods.
#'
#' @export



waterstorageprofile <- function(){
  require(zoo)
  filePath <- file.choose()
  df <- read.csv(file = filePath,header = TRUE)

  df2 <- df[, colnames(df)[c(3:ncol(df))]]
  #edit(df2)
  plot(df2)
  #plot(df2$y, df2$x)
}
