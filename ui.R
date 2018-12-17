source("datasetLoader.R", local = TRUE)
source("uiPlot.R", local = TRUE)
source("uiDataDescription.R", local = TRUE)
source("uiLinearRegression.R", local = TRUE)

shinyUI(
  navbarPage(
    "Weather Prediction!",
    id = "tabs",
    tabPanel(
      "Data Description",
      dataDescription("dataDescription"),
      actionButton('jumpToDtDesc', '')
    ),
    tabPanel(
      "Plots",
      plotUi("plotUi")
    ),
    tabPanel(
      "Linear Regression",
      lrUi("lrUi")
      
    )
  )
)
