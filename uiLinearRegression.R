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
      h5("DWEP, PRES, Season, DayPeriod, cwbd"),
      h4("Data Separation: "),
      h5("30% of the data set was removed for validation purpose."),
      h5("Total data analysed: 43824"),
      h5("Total model data: 30,676"),
      h5("Total test data: 13,148"),
      h4("Application of the model:"),
      h5("Stepwise regression technique was used to find the best subset variables what has the best perfomace."),
      h5("3 models was created:"),
      h5("- The first contains DWEP, PRES, Season, DayPeriod, cwbd"),
      h5("- The second only contain DEWP, Season, DayPeriod, cwbd"),
      h5("- the third PRES, Season, DayPeriod, cwbd"),
      h5("Those models was created because there is a strong relation between PRES and DEWP and it could lead to a wrong prediction"),
      h5("From all the combinations the model with the best AIC was TEMP ~ season + PRES + dayPeriod + DEWP + cbwd with the AIC of 91988.35"),
      h5("The second best was TEMP ~ season + PRES + dayPeriod + DEWP with the AIC of 92378.41"),
      h5("All the others combinations had 5000 point or more from the second best value"),
      h5("Adding the variable cbwd gives a insignificant improvement in the model and because that the variable cbwd was removed from the model"),
      h4("P Value:"),
      h5("All the variables has the p-value smaller than 0.01 so all them are estimated being greather than 0, in other words, there is a relation between the independent variables and temperature "),
      h4("R Square"),
      h5("R-Squared is the statistics that measure the proportion of variation in the Temperature has been explained by this model."),
      h5("The R square value is around of 0.86 and values above 0.7 are considered good"),
      h4("Normality of the residuals:"),
      h5("The residual seems to be symetric around of 0 value, as possible to see in the histogram"),
      h4("Data set validation"),
      h5("The Correlation between the predict value and the real value is 0.929, what means there is a high linear correlation"),
      h4("Root Mean Square Error (RMSE)"),
      h5(""),
      h5(""),
      h5(""),
      h5(""),
      h5(""),
      h5("")
      
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
