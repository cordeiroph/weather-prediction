lrUi <- function(id){
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      h3("Analyse"),
      h4("Variables analyse"),
      h5("The variables is, ir and iws are compose only by 0 with a few values over less than 105 of the dataset, and for that reason, they are not good to be add at this first model"),
      h5("PM2.5 is not homoscedastic, after removing the outliers the variable apresented a horizontal line, baically covering all the temperatures for each value and because and the correlation value proved this attribute doesn't have any significant correlation with temperature or others variables, and for that reason this variable wont be used in this first model"),
      h5("PRES and DEWP has a strong and linear correlation and for that reason those variable should be part of the model"), 
      h5("The categoriacal variable season affects the temp variable, as the day period and cbwd and for that reason those variable should be part of the model at otherside, the variable month period doesn't seems to be affecting the temp and for that reason it wont be included in the model "),
      h4("Model variables:"),
      h5("DWEP, PRES, Season, DayPeriod, cwbd")
      
    ),
    mainPanel(
      tabsetPanel( 
        tabPanel("Quantitative Variables Visualisation",
                 htmlTemplate("www/quantitativeVariablesAnalyse.html", brush = ns("brush1"))
        ),
        tabPanel("Quantitative Variables Visualisation without outliers",
                 htmlTemplate("www/quantitativeNoOutliersVariablesAnalyse.html", brush = ns("brush2"))
        ),
        tabPanel("Categorical Variables Visualisation", 
                 htmlTemplate("www/categoricalVariablesAnalyse.html", brush = ns("brush3"))
        ),
        tabPanel("Model Visualisation", 
                 htmlTemplate("www/linearRegressionModeling.html", brush = ns("brush3"))
        ),
        tabPanel("Prediction Visualisation", 
                 htmlTemplate("www/predictionView.html", brush = ns("brush3"))
        )
      )
    )
  )
}
