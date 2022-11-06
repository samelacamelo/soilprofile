#' Math integrations
#'
#' Multiple functions for mathematical integrations/interpolations
#'
#' @export
#'
trapezoidal_rule <- function(original_x,original_y){
  library("stringr")

  areas = c()
  plot(original_x,original_y, col="blue", xlim = c(0, original_x[length(original_x)]),ylim=c((min(original_y)*0.99),(max(original_y)*1.01)),xlab="Soil Depth(m)",ylab="Soil Moisture (m3 m-3)",xaxs = "i",yaxs = "i")
  #add more ticks to the axis labels
  for(i in append(original_x, 0,0)) {
    l <- formatC(i, format="f", digits=1)
    axis(1,at=i,labels=l)
  }
  #For the first row, assumes that the initial point and zero are the same
  polygon(c(0,0,original_x[1],original_x[1]),c(0,original_y[1],original_y[1],0),col=("#FF00FF"),border="white")
  area <- original_x[1]*original_y[1]
  areas <- append(areas,area)
  for(i in c(2:length(original_x)-1)) {
    x <- c(original_x[i],original_x[i+1])
    y <- c(original_y[i],original_y[i+1])
    points_x <- append(x, x[1],1)
    points_x <- append(points_x, points_x[3])
    points_y <- append(y, 0,0)
    points_y <- append(points_y, 0)
    polygon(points_x,points_y,col=ifelse((i %% 2) == 0,"#00b43f","#009a36"),border="white")
    biggest_base <- max(y)
    smallest_base <- min(y)
    h = max(x)-min(x)
    area = (biggest_base+smallest_base)*h/2
    areas <- append(areas,area)
  }
  sum(areas)

}

simpson_rule <- function(original_x,original_y){

  #For the first row, assumes that the initial point and zero are the same
  original_x <- append(original_x, 0,0)
  original_y <- append(original_y, original_y[1],0)
  plot(original_x,original_y, col="blue", xlim = c(0, original_x[length(original_x)]),ylim=c((min(original_y)*0.99),(max(original_y)*1.01)),xlab="Soil Depth(m)",ylab="Soil Moisture (m3 m-3)",xaxs = "i",yaxs = "i")

  #add more ticks to the axis labels
  for(i in append(original_x, 0,0)) {
    l <- formatC(i, format="f", digits=1)
    axis(1,at=i,labels=l)
  }

  total_area = 0
  if(length(original_x)%%2 == 0){
    trapezoid_points_x <- c(
      original_x[length(original_x)-1],
      original_x[length(original_x)-1],
      original_x[length(original_x)],
      original_x[length(original_x)]
      )
    trapezoid_points_y <- c(
      0,
      original_y[length(original_y)-1],
      original_y[length(original_y)],
      0
    )
    polygon(trapezoid_points_x,trapezoid_points_y,col="#FF00FF",border="white")
    bases <- original_y[length(original_y)-1]+original_y[length(original_y)]
    h <- original_x[length(original_x)]-original_x[length(original_x)-1]
    trapezoid_area <- (bases)*h/2
    total_area <- sum(total_area,trapezoid_area)
  }

  loop_items <- c(3:length(original_x))
  loop_items <- loop_items[which(loop_items %% 2 == 1)]

  simpson_area <- 0
  for(i in loop_items) {
    x <- c(original_x[i-2],original_x[i-1],original_x[i])
    y <- c(original_y[i-2],original_y[i-1],original_y[i])
    spline_points <- spline(x, y, n=100)
    points_x <- append(spline_points$x, x[1],1)
    points_x <- append(points_x, points_x[length((points_x))])
    points_y <- append(spline_points$y, 0,0)
    points_y <- append(points_y, 0)
    polygon(points_x,points_y,col=str_interp("#0000${as.hexmode((i %% 10)+5)}${as.hexmode((i %% 10)+5)}"),border="white")
    area <- (((original_x[i]-original_x[i-2])/2)/3)*(original_y[i-2]+(4*original_y[i-1]+original_y[i]))
    simpson_area <- sum(simpson_area,area)
  }

  #simpson_area <- fda.usc::int.simpson2(original_x, original_y, equi = TRUE, method = "CSR")
  total_area <- sum(total_area,simpson_area)
  total_area
}

splines_rule <-function(original_x,original_y){
  library('splines')
  #add a extra element to represent x = 0
  original_y <- append(original_y, 0,0)
  original_x <- append(original_x, original_x[1],0)

  f <- splinefun(original_y,original_x)
  result_value = integrate(f, lower = original_y[1], upper = original_y[length(original_y)])
  plot(original_y,original_x, col="blue", ylim=c((min(original_x)*0.99),(max(original_x)*1.01)),xlab="Soil Depth(m)",ylab="Soil Moisture (m3 m-3)",xaxs = "i",yaxs = "i")
  #add more ticks to the axis labels
  for(i in append(original_y, 0,0)) {
    l <- formatC(i, format="f", digits=1)
    axis(1,at=i,labels=l)
  }
  curve_obj <- curve(f(x), original_y[1], original_y[length(original_y)], col = "green", lwd = 1.5,add=TRUE)
  points_x <- append(curve_obj$x, curve_obj$x[1],1)
  points_x <- append(points_x, points_x[length((points_x))])
  points_y <- append(curve_obj$y, 0,0)
  points_y <- append(points_y, 0)
  polygon(points_x,points_y, col = "red",border="white")
  result_value$value
}

