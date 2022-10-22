#' Water storage
#'
#' This function is used to calculate the water storage index of a given point in soil
#' Using three different mathematical methods.
#'
#' @export



waterstorageprofile <- function(){
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  reports_dir <- paste("reports", timestamp, sep="/")
  dir.create(reports_dir,recursive=TRUE)
  library(ggplot2)
  filePath <- file.choose()


  df <- read.csv(file = filePath,header = TRUE)
  df2 <- df[, colnames(df)[c(3:ncol(df))]]
  df3 <- remove_x_label(df2)
  nrows <- nrow(df3)
  rows_qty <- 1:nrows
  par(mfrow=c(nrows,1))
  result_df <- data.frame(result_trapezio=NA, result_simpson=NA,result_spline=NA)[numeric(0), ]
  pdf(file= "sample.pdf" )

  for(i in rows_qty) {
    eixo_y <- as.numeric(colnames(df3))
    eixo_x <- (as.numeric(df3[i,]))
    result_trapezio <- fda.usc::int.simpson2(eixo_y, eixo_x, equi = TRUE, method = "TRAPZ")
    result_simpson <- fda.usc::int.simpson2(eixo_y, eixo_x, equi = TRUE, method = "CSR")
    library('splines')
    f <- splinefun(eixo_y,eixo_x)
    result_spline = integrate(f, lower = 0.1, upper = 1.1)
    result_spline <- result_spline$value
    #windows()
    plot(eixo_y,eixo_x, col="blue",sub=as.character(i))
    curve(f(x), 0.1, 1.1, col = "green", lwd = 1.5,add=TRUE)
    result_trapezio <- c(result_trapezio)
    result_simpson <- c(result_simpson)
    result_spline <- c(result_spline)
    partial_df <- data.frame(result_trapezio, result_simpson,result_spline )
    result_df<-rbind(partial_df,result_df )
  }
  edit(result_df)
}




remove_x_label = function(es) {
  f = es
  for (col in c(1:ncol(f))){ #for each column in dataframe
    if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}
