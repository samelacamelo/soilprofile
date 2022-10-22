#' Water storage
#'
#' This function is used to calculate the water storage index of a given point in soil
#' Using three different mathematical methods.
#'
#' @export



waterstorageprofile <- function(){
  library("stringr")
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  reports_dir <- paste("reports", timestamp, sep="/")
  dir.create(reports_dir,recursive=TRUE)
  library(ggplot2)
  filePath <- file.choose()
  library(readr)
  html_code <- read_file("assets\\report\\report_template.html")

  df <- read.csv(file = filePath,header = TRUE)
  df2 <- df[, colnames(df)[c(3:ncol(df))]]
  df3 <- remove_x_label(df2)
  nrows <- nrow(df3)
  rows_qty <- 1:nrows
  result_df <- data.frame(result_trapezio=NA, result_simpson=NA,result_spline=NA)[numeric(0), ]

  library("xtable")
  df_html = xtable(df)
  dataset_preview_html <-print(xtable(df_html), type="html",print.results = FALSE)
  html_code <- gsub("###DATASET PREVIEW###", dataset_preview_html, html_code)
  points_html <- ""
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
    img_file_name <- paste(as.character(i),".png",sep="")
    dev.copy(png,filename=paste(reports_dir,img_file_name,sep="/"));
    curve(f(x), 0.1, 1.1, col = "green", lwd = 1.5,add=TRUE)
    graphics.off();
    result_trapezio <- c(result_trapezio)
    result_simpson <- c(result_simpson)
    result_spline <- c(result_spline)
    partial_df <- data.frame(result_trapezio, result_simpson,result_spline )
    result_df<-rbind(partial_df,result_df )
    point_html = str_interp('<div id="pointContainer"><h3>${i}</h3><img src="${img_file_name}"></img></div> ')
    points_html <- paste(points_html,point_html,sep="\n")
  }
  #edit(result_df)
  html_code <- gsub("###POINTS###", points_html, html_code)
  result_df_html <-print(xtable(result_df), type="html",print.results = FALSE)
  html_code <- gsub("###RESULTS###", result_df_html, html_code)
  writeLines(text = html_code, con = paste(reports_dir,"index.html",sep="/"))
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
