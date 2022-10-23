#' Math integrations
#'
#' Extension of the fda.usc R package for mathematical integrations/interpolations
#'
#' @export
#'
trapezoidal_rule <- function(original_x,original_y,i){
  areas = c()
  dev.off()
  plot(original_x,original_y)
  for(i in c(2:length(eixo_x)-1)) {
    x <- c(original_x[i],original_x[i+1])
    y <- c(original_y[i],original_y[i+1])
    points_x <- append(x, x[1],1)
    points_x <- append(points_x, points_x[3])
    points_y <- append(y, 0,0)
    points_y <- append(points_y, 0)
    polygon(points_x,points_y,col=ifelse((i %% 2) == 0,"#009a36","#1ef25a"),border="white")
    biggest_base <- max(y)
    smallest_base <- min(y)
    h = max(x)-min(x)
    area = (biggest_base+smallest_base)*h/2
    areas <- append(areas,area)
  }
  sum(areas)
}
