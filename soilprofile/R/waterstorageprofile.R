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

timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
reports_dir <- paste("reports", timestamp, sep="/")
waterstorageprofile <- function(){
  library("readr")
  library("stringr")
  library("xtable")
  library("dplyr")
  options(digits=15)
  dir.create(reports_dir,recursive=TRUE)
  filePath <- file.choose()

  #loads the html report template
  html_code <- read_file("assets\\report\\report_template.html")
  points_html <- ""

  #reads the csv, skips the first five columns
  df <- read.csv(file = filePath,header = TRUE)
  df2 <- df[, colnames(df)[c(6:ncol(df))]]
  #transforms the name of the columns in real numbers
  df3 <- remove_x_label(df2)
  nrows <- nrow(df3)
  rows_qty <- 1:nrows

  #This dataset will be appended after each loop to each row/probe point
  result_df <- data.frame(
    index = NA,
    soil_description = NA,
    soil_moisture_condition = NA,
    result_simple_average=NA,
    result_trapezoidal=NA,
    result_simpson=NA,
    result_spline=NA
  )[numeric(0), ]

  datasetPreviewTableId = "datasetPreview"

  #Formats the measures with 5 decimal points and converts to string to help on the html conversion
  df[6:ncol(df)] <- lapply(df[6:ncol(df)], decimal_formatter)
  #Renames the index column to #
  df$row_number <- seq.int(nrow(df))
  df <- df %>% select(row_number, everything())
  colnames(df)[1] <- "#"
  #Creates the Dataset Preview section from the html report
  df_html <- xtable(df)
  dataset_preview_html <-print(xtable(df_html), include.rownames=FALSE,type="html",print.results = FALSE)
  dataset_preview_html <- gsub("<table", str_interp("<table id='${datasetPreviewTableId}'"), dataset_preview_html)
  html_code <- gsub("###DATASET PREVIEW###", dataset_preview_html, html_code)

  #Initialize the method winners count
  simple_average_winners_count <-0
  trapezoidal_winners_count <-0
  simpson_winners_count <-0
  spline_winners_count <-0

  #Loops for each row in the dataset (each row corresponds to a probe point)
  for(i in rows_qty) {

    #The information about the soil probe
    soil_type <- df$soil[i]
    soil_description <- df$description[i]
    equipment <- df$equipment[i]
    soil_moisture_condition <- df$soil_moisture_condition[i]
    textural_class <- df$textural_class[i]

    #Creates the two axis for the analytical analysis
    y_axis <- as.numeric(colnames(df3))
    x_axis <- (as.numeric(df3[i,]))

    #finds which elements in array are equal to "NA" and remove them
    elems_to_remove <- which(is.na(x_axis))
    if(length(elems_to_remove) > 0){
      x_axis = x_axis[-elems_to_remove]
      y_axis = y_axis[-elems_to_remove]
    }

    #simple_average
    result_simple_average <- simple_average(y_axis,x_axis)
    save_png("sa_",i)

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
    result_simple_average <- format(round(result_simple_average, 5), nsmall = 5)
    result_simple_average <- c(result_simple_average)
    result_trapezoidal <- format(round(result_trapezoidal, 5), nsmall = 5)
    result_trapezoidal <- c(result_trapezoidal)
    result_simpson <- format(round(result_simpson, 5), nsmall = 5)
    result_simpson <- c(result_simpson)
    result_spline <- format(round(result_spline, 5), nsmall = 5)
    result_spline <- c(result_spline)

    #Creates a single row of result and append it into the final dataframe
    partial_df <- data.frame(
      c(str_interp('#beginhref#${i}#middlehref#${i}#endhref#')), #To create the clickable links later on
      soil_description,
      soil_moisture_condition,
      result_simple_average,
      result_trapezoidal,
      result_simpson,
      result_spline
    )
    result_df<-rbind(result_df,partial_df)

    #Create the millimeter values for the report
    result_simple_average_mm <- as.numeric(result_simple_average[1]) * 1000
    result_simple_average_mm <- format(round(result_simple_average_mm, 2), nsmall = 2)
    result_trapezoidal_mm <- as.numeric(result_trapezoidal[1]) * 1000
    result_trapezoidal_mm <- format(round(result_trapezoidal_mm, 2), nsmall = 2)
    result_simpson_mm <- as.numeric(result_simpson[1]) * 1000
    result_simpson_mm <- format(round(result_simpson_mm, 2), nsmall = 2)
    result_spline_mm <- as.numeric(result_spline[1]) * 1000
    result_spline_mm <- format(round(result_spline_mm, 2), nsmall = 2)

    #Mounts the html report
    point_html <- str_interp('
                              <a id="${i}"><h3>Probe point: ${i}</h3></a>
                              <div class="pointHeader">
                                <p><b>Soil type:</b> ${soil_type}</p>
                                <p><b>Soil description:</b> ${soil_description}</p>
                                <p><b>Equipment used:</b> ${equipment}</p>
                                <p><b>Soil moisture condition:</b> ${soil_moisture_condition}</p>
                                <p><b>Textural class:</b> ${textural_class}</p>
                              </div>
                                <div class="pointContainer">
                                    <div class="imageContainer">
                                        <h4>Simple Average</h4>
                                        <img class="graph" src="sa_${i}.png"></img>
                                        <h5>Water Profile(m): ${result_simple_average}</h5>
                                        <h5>Water Profile(mm): ${result_simple_average_mm}</h5>
                                    </div>
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

  #Renames the columns of the final dataframe
  colnames(result_df) <- c(
    '#',
    'Soil description',
    'Soil Moisture condition',
    'Simple average',
    'Trapezoidal',
    'Simpson',
    'Splines'
    )

  #Finishes the html report
  html_code <- gsub("###POINTS###", points_html, html_code)
  result_df_html <-print(xtable(result_df), type="html",include.rownames=FALSE,print.results = FALSE)
  table_widths = '
    <table border=1>
    <colgroup>
      <col span="1" style="width: 1%;">
      <col span="1" style="width: 34%;">
      <col span="1" style="width: 25%;">
      <col span="1" style="width: 10%;">
      <col span="1" style="width: 10%;">
      <col span="1" style="width: 10%;">
      <col span="1" style="width: 10%;">
    </colgroup>
  '
  result_df_html <- gsub("<table border=1>", table_widths, result_df_html)
  html_code <- gsub("###RESULTS###", result_df_html, html_code)
  html_code <- gsub("#beginhref#", "<a href='#", html_code)
  html_code <- gsub("#middlehref#", "'>", html_code)
  html_code <- gsub("#endhref#", "</a>", html_code)
  site_path <- paste(reports_dir,"index.html",sep="/")
  writeLines(text = html_code, con = site_path)
  #Force opening the report
  browseURL(site_path)
}

#Removes the X from the column name that R automatically inserts. This is important because the header is converted to numbers to create
#the X and Y axis values
remove_x_label <- function(es) {
  f <- es
  for (col in c(1:ncol(f))){
    if (startsWith(colnames(f)[col], "X") == TRUE)  {
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100)
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE)
}

#Saves the current plot as a png image. The file names are later on used on the html report
save_png <- function(img_file_prefix,image_index) {
  img_file_name <- paste(as.character(image_index),".png",sep="")
  img_file_name <- paste(img_file_prefix,img_file_name,sep="")
  dev.copy(png,filename=paste(reports_dir,img_file_name,sep="/"));
  dev.off();
  graphics.off();
}

#Function to calculate how much a method is different from the 4 methods average
#Also formats with 3 decimal places
diff_calculator <- function(average,single_element){
  result <- (single_element*100)/average
  result <- 100-result
  result_formatted <- format(round(result, 3), nsmall = 3,scientific=F)
  output<-list(result,result_formatted)
  return(output)
}

# Function to format the numbers as strings with 5 decimal digits
decimal_formatter <- function(x){
  format(round(x, 5), nsmall = 5,scientific=F)
}
