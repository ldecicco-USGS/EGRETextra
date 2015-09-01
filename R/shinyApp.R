#' Explore EGRET
#' 
#' Open an interactive app
#' 
#' @param browse use browser for map rendering
#' @export
#' @importFrom shiny runApp
#' @import EGRET
explore_EGRET <- function(browse=TRUE){
  runApp(system.file('shiny', package='EGRETextra'), launch.browser = browse)
}