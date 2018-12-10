lrUi <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Linear Regression"),
    mainPanel(
      textOutput("Check for linearity, Homoscedasticity for quantitative variables"),
      plotOutput(outputId = "plotLin"),
      textOutput("Result: \n Only Pres and Dewp has a linearity and homoscedasticity "),
      textOutput("Linear Regression for the variable Pres and Dewp "),
      textOutput(outputId = "sumLMPD"),
      plotOutput(outputId = "plotQQNormPD"),
      textOutput(outputId = "ksTestPD"),
      textOutput(outputId = "adTestPD"),
      textOutput(outputId = "resultLMPD"),
      
      textOutput("Linear Regression for the variable Pres"),
      textOutput(outputId = "sumLMP"),
      plotOutput(outputId = "plotQQNormPD"),
      textOutput(outputId = "ksTestP"),
      textOutput(outputId = "adTestP"),
      textOutput(outputId = "resultLMP"),
      
      textOutput("Linear Regression for the variable Dewp"),
      textOutput(outputId = "sumLMD"),
      plotOutput(outputId = "plotQQNormPD"),
      textOutput(outputId = "ksTestD"),
      textOutput(outputId = "adTestD"),
      textOutput(outputId = "resultLMD")
      
    )
  )
  
  
}