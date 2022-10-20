#' Water storage
#'
#' This function is used to calculate the water storage index of a given point in soil
#' Using three different mathematical methods.
#'
#' @export



waterstorageprofile <- function(){
  library(ggplot2)
  filePath <- file.choose()
  df <- read.csv(file = filePath,header = TRUE)

  df2 <- df[, colnames(df)[c(3:ncol(df))]]

  df3 <- destroyX(df2)

  eixo_y = as.numeric(colnames(df3))
  eixo_x <- (as.numeric(df3[1,]))
  result_trapezio <- fda.usc::int.simpson2(eixo_x, eixo_y, equi = TRUE, method = "TRAPZ")
  print("Resultado usando o método do trapézio: ")
  print(result_trapezio)
  result_simpson <- fda.usc::int.simpson2(eixo_x, eixo_y, equi = TRUE, method = "CSR")
  print("Resultado usando a regra de simpson: ")
  print(result_simpson)
  matplot(eixo_x,eixo_y,type="l", xlab="water amount", ylab="depth")
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
