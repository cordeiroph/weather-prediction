library(dygraphs)

plotUi <- function(id){
  ns <- NS(id)
  fluidPage(
    
    titlePanel("Create your graphic"),
    sidebarPanel(
      selectInput(inputId = "graphic",
                  label = "Choose a Graphic:",
                  choices = c("scatterplot","timeplot", "histogram", "boxplot", "correlation graph", "correlation matrix", "density")),
      # Scatter Panel ------------
      conditionalPanel(
        condition = "input.graphic == 'scatterplot'",
        selectInput(inputId = "xScatter",
                    label = "X Variable:",
                    choices = c("pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir"))      
      ),
      
      conditionalPanel(
        condition = "input.graphic == 'scatterplot'",
        selectInput(inputId = "yScatter",
                    label = "Y Variable:",
                    choices = c("pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir"))      
      ),
      
      conditionalPanel(
        condition = "input.graphic == 'scatterplot'",
        selectInput(inputId = "scatMethod",
                    label = "Linear method:",
                    choices = c("lm", "loess"))      
      ),
      
      # Histogram Panel ------------
      
      conditionalPanel(
        condition = "input.graphic == 'histogram'",
        selectInput(inputId = "xHist",
                    label = "X Variable:",
                    choices = c("year","month","day","hour","pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir"))      
      ),
      
      conditionalPanel(
        condition = "input.graphic == 'histogram'",
        sliderInput(inputId = "binHist",
                    label = "Bins:",
                    min = 0, max = 100,
                    value = 30)
      ),
      
      # Density Panel ------------
      conditionalPanel(
        condition = "input.graphic == 'density'",
        selectInput(inputId = "xDensity",
                    label = "X Variable:",
                    choices = colnames(getDataSet()))      
      ),
      
      # Correlation Panel ------------
      conditionalPanel(
        condition = "input.graphic == 'correlation graph' || input.graphic == 'correlation matrix'",
        selectInput(inputId = "corMethod",
                    label = "Correlation Method:",
                    choices = c("spearman", "pearson", "kendal"))      
      ),
      
      conditionalPanel(
        condition = "input.graphic == 'correlation graph'",
        numericInput(inputId = "minCor",
                     min = 0, max =1,
                     value = 0,
                     label = "Absolute value of Minimum Correlation:")      
      ),
      
      # BoxPlot Panel ------------
      conditionalPanel(
        condition = "input.graphic == 'boxplot'",
        selectInput(inputId = "xBoxPlot",
                    label = "X Variable:",
                    choices = c("cbwd", "year","month","day"))      
      ),
      
      conditionalPanel(
        condition = "input.graphic == 'boxplot'",
        selectInput(inputId = "yBoxPlot",
                    label = "Y Variable:",
                    choices = c("year","month","day","hour","pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir"))      
      ),
      # Time Plot Panel --------
      conditionalPanel(
        condition = "input.graphic == 'timeplot'",
        selectInput(inputId = "varTimePlot",
                    label = "Variable:",
                    choices = c("pm2.5","DEWP","TEMP","PRES","Iws","Is","Ir"))      
      ),
      conditionalPanel(
        condition = "input.graphic == 'timeplot'",
        selectInput(inputId = "pdTimePlot",
                    label = "Period Precission:",
                    choices = c("year","month","day","hour"), multiple = FALSE)      
      ),
      conditionalPanel(
        condition = "input.graphic == 'timeplot'",
        selectInput(inputId = "opTimePlot",
                    label = "Operation:",
                    choices = c("mean","median","sum", "max", "min"))      
      ),
      
      
      # Action Button ------------
      actionButton("graphiGenerator", "Create graphic")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.graphic != 'timeplot'",
        plotOutput("plot")
      ),
      conditionalPanel(
        condition = "input.graphic == 'scatterplot'",
        tableOutput("lrTable")
      ),
      conditionalPanel(
        condition = "input.graphic == 'timeplot'",
        dygraphOutput("dygraph")
      )
    )
  )  
}


