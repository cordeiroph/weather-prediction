lrUi <- function(id){
  ns <- NS(id)
  
  htmlTemplate("www/linearRegressionAnalyse.Rhtml", brush = ns("brush"))
  
}
