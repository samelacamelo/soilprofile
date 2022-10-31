#' SoilProfile main menu
#'
#' This function is meant to handle the initial menu
#' The "SoilProfile" module is a on-going project created by the students from the Instituto Federal Baiano-IF.
#'
#' @export
main <- function(){
  choice<-menu(gettext(c("Water storage profile", "Future project 1", "Future project 2")),
               graphics = TRUE,
               title = "Choose the feature")
  switch(
    choice,
    "1"= waterstorageprofile(),
    "2"= print("Not created yet. Closing."),
    "3"= print("Not created yet. Closing.")
  )
}
