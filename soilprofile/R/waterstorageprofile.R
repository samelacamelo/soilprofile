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
  filePath <- file.choose()
  library(readr)
  html_code <- read_file("assets\\report\\report_template.html")

  df <- read.csv(file = filePath,header = TRUE)
  df2 <- df[, colnames(df)[c(3:ncol(df))]]
  df3 <- remove_x_label(df2)
  nrows <- nrow(df3)
  rows_qty <- 1:nrows
  result_df <- data.frame(result_trapezio=NA, result_simpson=NA,result_spline=NA)[numeric(0), ]

  warning_sign <- ""
  datasetPreviewTableId = "datasetPreview"
  if(ncol(df3)%%2 == 0){
    df3 <- df3[1:(length(df3)-1)]
    warning_sign <- "<h2 class='warningSign'>⚠  The number of probe points must be odd and bigger than 3 in order to apply the Simpson Rule. The last point of the dataset was ignored  ⚠</h2>"
    datasetPreviewTableId = "datasetPreviewWithoutLastRow"
  }
  html_code <- gsub("###WARNING###", warning_sign, html_code)

  library("xtable")
  df_html = xtable(df)
  dataset_preview_html <-print(xtable(df_html), type="html",print.results = FALSE)
  dataset_preview_html <- gsub("<table", str_interp("<table id='${datasetPreviewTableId}'"), dataset_preview_html)
  html_code <- gsub("###DATASET PREVIEW###", dataset_preview_html, html_code)
  points_html <- ""
  for(i in rows_qty) {

    eixo_y <- as.numeric(colnames(df3))
    eixo_x <- (as.numeric(df3[i,]))

    #trapezy
    result_trapezio <- trapezoidal_rule(eixo_y,eixo_x)
    t_img_file_name <- paste(as.character(i),".png",sep="")
    t_img_file_name <- paste("t_",t_img_file_name)
    dev.copy(png,filename=paste(reports_dir,t_img_file_name,sep="/"));
    graphics.off();

    #simpson
    result_simpson <- simpson_rule(eixo_y,eixo_x)
    si_img_file_name <- paste(as.character(i),".png",sep="")
    si_img_file_name <- paste("si_",si_img_file_name)
    dev.copy(png,filename=paste(reports_dir,si_img_file_name,sep="/"));
    graphics.off();

    #spline
    result_spline <- splines_rule(eixo_x,eixo_y)
    sp_img_file_name <- paste(as.character(i),".png",sep="")
    sp_img_file_name <- paste("sp_",sp_img_file_name)
    dev.copy(png,filename=paste(reports_dir,sp_img_file_name,sep="/"));
    dev.off()
    graphics.off();

    #report
    result_trapezio <- format(round(result_trapezio, 6), nsmall = 2)
    result_trapezio <- c(result_trapezio)
    result_simpson <- format(round(result_simpson, 6), nsmall = 2)
    result_simpson <- c(result_simpson)
    result_spline <- format(round(result_spline, 6), nsmall = 2)
    result_spline <- c(result_spline)
    partial_df <- data.frame(result_trapezio, result_simpson,result_spline )
    result_df<-rbind(result_df,partial_df)
    point_html = str_interp('<h3>Probe point: ${i}</h3>
                            <div class="pointContainer">
                              <div class="imageContainer">
                                <h4>Trapezoid</h4>
                                  <img class="graph" src="${t_img_file_name}"</img>
                                  <h5>Water Profile: ${result_trapezio}</h5>
                              </div>
                              <div class="imageContainer">
                              <h4>Simpson</h4>
                                <img class="graph" src="${si_img_file_name}"></img>
                                <h5>Water Profile: ${result_simpson}</h5>
                              </div>
                              <div class="imageContainer">
                              <h4>Splines</h4>
                                <img class="graph" src="${sp_img_file_name}"></img>
                                <h5>Water Profile: ${result_spline}</h5>
                              </div>
                            </div> ')

    points_html <- paste(points_html,point_html,sep="\n")
  }
  html_code <- gsub("###POINTS###", points_html, html_code)
  result_df_html <-print(xtable(result_df), type="html",print.results = FALSE)
  html_code <- gsub("###RESULTS###", result_df_html, html_code)
  site_path <- paste(reports_dir,"index.html",sep="/")
  writeLines(text = html_code, con = site_path)
  browseURL(site_path)
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


