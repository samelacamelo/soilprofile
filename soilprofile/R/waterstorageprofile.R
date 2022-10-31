#' Water storage Profile
#'
#'     This R method reads a csv file, read the measurement points from the soil as a dataframe, plot the dots,
#' and calculates the area below the curve using three different mathematical methods:
#' * Trapezoidal rule
#' * Simpson method
#' * Splines method),
#'The area then corresponds to the water storage index of a given point in soil.
#'
#'Finally, a html file is generated and automatically opened. This is a complete report containing a
#' a preview from the dataset, a visual representation of each method, the calculated area, and a table containing all
#' the results.
#'
#' @export
# Initial setup
library("readr")
library("stringr")
library("xtable")
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
reports_dir <- paste("reports", timestamp, sep="/")
waterstorageprofile <- function(){

  dir.create(reports_dir,recursive=TRUE)
  filePath <- file.choose()

  #loads the html report template
  html_code <- read_file("assets\\report\\report_template.html")
  points_html <- ""

  #reads the csv, skips the first three columns
  df <- read.csv(file = filePath,header = TRUE)
  df2 <- df[, colnames(df)[c(3:ncol(df))]]
  df3 <- remove_x_label(df2)
  nrows <- nrow(df3)
  rows_qty <- 1:nrows

  #This dataset will be appended after each loop to each row/proble point
  result_df <- data.frame(result_trapezoidal=NA, result_simpson=NA,result_spline=NA)[numeric(0), ]

  #In case that the number of probe points is even, the second-last point is ignored (simpson rule demands a odd quantity).
  df3 <- df3[1:(length(df3)-1)]
  #Puts a warning in the final report
  warning_sign <- ""
  datasetPreviewTableId = "datasetPreview"
  if(ncol(df3)%%2 == 0){
    warning_sign <- "<h2 class='warningSign'>⚠  The number of probe points must be odd and bigger than 3 in order to apply the Simpson Rule. The second-last point of the dataset was ignored  ⚠</h2>"
    datasetPreviewTableId = "datasetPreviewWithoutLastRow"
  }
  html_code <- gsub("###WARNING###", warning_sign, html_code)

  #Creates the Dataset Preview section from the html report
  df_html = xtable(df)
  dataset_preview_html <-print(xtable(df_html), type="html",print.results = FALSE)
  dataset_preview_html <- gsub("<table", str_interp("<table id='${datasetPreviewTableId}'"), dataset_preview_html)
  html_code <- gsub("###DATASET PREVIEW###", dataset_preview_html, html_code)

  #Loops for each row in the dataset (each row corresponds to a probe point)
  for(i in rows_qty) {

    y_axis <- as.numeric(colnames(df3))
    x_axis <- (as.numeric(df3[i,]))

    #trapezoidal
    result_trapezoidal <- trapezoidal_rule(y_axis,x_axis)
    save_png("t_",i)

    #simpson
    result_simpson <- simpson_rule(y_axis,x_axis)
    save_png("si_",i)

    #spline
    result_spline <- splines_rule(x_axis,y_axis)
    save_png("sp_",i)


    #Converts to numeric and appends this single proble point result to the final dataframe
    result_trapezoidal <- format(round(result_trapezoidal, 5), nsmall = 5)
    result_trapezoidal <- c(result_trapezoidal)
    result_simpson <- format(round(result_simpson, 5), nsmall = 5)
    result_simpson <- c(result_simpson)
    result_spline <- format(round(result_spline, 5), nsmall = 5)
    result_spline <- c(result_spline)
    partial_df <- data.frame(result_trapezoidal, result_simpson,result_spline )
    result_df<-rbind(result_df,partial_df)

    result_trapezoidal_mm <- as.numeric(result_trapezoidal[1]) * 1000
    result_trapezoidal_mm <- format(round(result_trapezoidal_mm, 2), nsmall = 2)
    result_simpson_mm <- as.numeric(result_simpson[1]) * 1000
    result_simpson_mm <- format(round(result_simpson_mm, 2), nsmall = 2)
    result_spline_mm <- as.numeric(result_spline[1]) * 1000
    result_spline_mm <- format(round(result_spline_mm, 2), nsmall = 2)

    #Mounts the html report
    point_html = str_interp('<h3>Probe point: ${i}</h3>
                                <div class="pointContainer">
                                    <div class="imageContainer">
                                        <h4>Trapezoid</h4>
                                        <img class="graph" src="t_${i}.png"></img>
                                        <h5>Water Profile(m): ${result_trapezoidal}</h5>
                                        <h5>Water Profile(mm): ${result_trapezoidal_mm}</h5>
                                    </div>
                                    <div class="imageContainer">
                                        <h4>Simpson</h4>
                                        <img class="graph" src="si_${i}.png"></img>
                                        <h5>Water Profile(m): ${result_simpson}</h5>
                                        <h5>Water Profile(mm): ${result_simpson_mm}</h5>
                                    </div>
                                    <div class="imageContainer">
                                        <h4>Splines</h4>
                                        <img class="graph" src="sp_${i}.png"></img>
                                        <h5>Water Profile(m): ${result_spline}</h5>
                                        <h5>Water Profile(mm): ${result_spline_mm}</h5>
                                    </div>
                                </div> ')

    points_html <- paste(points_html,point_html,sep="\n")
  }

  #Finishes the html report
  html_code <- gsub("###POINTS###", points_html, html_code)
  result_df_html <-print(xtable(result_df), type="html",print.results = FALSE)
  html_code <- gsub("###RESULTS###", result_df_html, html_code)
  site_path <- paste(reports_dir,"index.html",sep="/")
  writeLines(text = html_code, con = site_path)
  #Force opening the report
  browseURL(site_path)
}

#Removes the X from the column name that R automatically inserts. This is important because the header is converted to numbers to create
#the X and Y axis values
remove_x_label = function(es) {
  f = es
  for (col in c(1:ncol(f))){
    if (startsWith(colnames(f)[col], "X") == TRUE)  {
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100)
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE)
}

#Saves the current plot as a png image. The file names are later on used on the html report
save_png = function(img_file_prefix,image_index) {
  img_file_name <- paste(as.character(image_index),".png",sep="")
  img_file_name <- paste(img_file_prefix,img_file_name,sep="")
  dev.copy(png,filename=paste(reports_dir,img_file_name,sep="/"));
  dev.off();
  graphics.off();
}
