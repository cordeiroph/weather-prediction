source("datasetLoader.R", local = TRUE)
source("uiPlot.R", local = TRUE)
source("uiDataDescription.R", local = TRUE)

shinyUI(
  navbarPage(
    "Weather Prediction!",
    tabPanel(
      "Data Description",
      dataDescription("dataDescription")
    ),
    tabPanel(
      "Plots",
      plotUi("plotUi")
    )
  )
)