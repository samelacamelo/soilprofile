#' Water storage
#'
#' This function is used to calculate the water storage index of a given point in soil
#' Using three different mathematical methods.
#'
#' @export



waterstorageprofile <- function(){
  library(ggplot2)
  #filePath <- file.choose()
  filePath <- "C:\\Users\\rroman12\\Downloads\\unicalinha.csv"
  df <- read.csv(file = filePath,header = TRUE)

  df2 <- df[, colnames(df)[c(3:ncol(df))]]

  df3 <- destroyX(df2)
  #edit(df3)
  ggplot(df3, aes(x=df3$x, y=df3$x))
  #plot(df2$y, df2$x)
}


destroyX = function(es) {
  f = es
  for (col in c(1:ncol(f))){ #for each column in dataframe
    if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}
