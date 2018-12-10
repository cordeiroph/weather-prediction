library(ggplot2)
library(xlsx)
library(plyr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(corrr)
library(gridExtra)
library(ggcorrplot)
library(GGally)
library(shiny)
library(dygraphs)
library(xts)
library(zoo)
library(lubridate)
source("datasetLoader.R", local = TRUE)
source("uiDataDescription.R", local = TRUE)
source("uiLinearRegression.R", local = TRUE)
source("ModelGraphicGenerator.R", local = TRUE)

shinyServer(function(input, output) {
  df <- getDataSet()
  df2 <- head(getNonNullDataSet(), n=100)
  df4 <- getDataSet2()
  df <- tidyr::unite_(df, "date", c('year', 'month', 'day', 'hour'), sep = "/", remove = FALSE)
  df$date <- as.POSIXct(df$date, format = "%Y/%m/%d/%H")
  
  # Description Tab ----------
  output$summary <- renderPrint({
    summary(df)
  })
  
  output$visualisation <- renderPrint({
    str(df)
  })
  
  # Plot Tab ----------  
  
  v <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$graphiGenerator, {
    v$doPlot <- input$graphiGenerator
    v$graphic <- input$graphic
    v$xScatter <- input$xScatter
    v$yScatter <- input$yScatter
    v$scatMethod <- input$scatMethod
    v$xHist <- input$xHist
    v$xDensity <- input$xDensity
    v$corMethod <- input$corMethod
    v$minCor <- input$minCor
    v$xBoxPlot <- input$xBoxPlot
    v$yBoxPlot <- input$yBoxPlot
    v$xBarPlot <- input$xBarPlot
    v$yBarPlot <- input$yBarPlot
    v$fillBarPlot <- input$fillBarPlot
    v$varTimePlot <- input$varTimePlot
    v$pdTimePlot <- input$pdTimePlot
    v$opTimePlot <- input$opTimePlot
    
  }) 
  
  output$dygraph <- renderDygraph({
    if(v$doPlot == FALSE) return()
    
    if(v$graphic == 'timeplot'){
      t <- df
      if(v$varTimePlot == "pm2.5"){
        t <- na.omit(df)
      }
      
      summ <- paste0(v$opTimePlot,'(', v$varTimePlot, ')')  
      
      
      dfGrouped <-  t %>%
        group_by(date = lubridate::floor_date(date, v$pdTimePlot)) %>%
        summarise_(.dots = setNames(summ, 'val'))
      
      g <- dygraph(xts(dfGrouped$val, dfGrouped$date)) %>% 
        dyAxis("y", valueRange = c(min(dfGrouped$val), max(dfGrouped$val))) %>% 
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
        dyRangeSelector()
      
      g
      
    } 
  })
  
  
  output$plot <- renderPlot({
    if(v$doPlot == FALSE) return()
    
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- GraphicGenerator$new(df)
                   g <- g$generateGraphic(
                     type = v$graphic,
                     xScatter = v$xScatter, 
                     yScatter = v$yScatter, 
                     xHist = v$xHist,
                     binHist = input$binHist,
                     xDensity = v$xDensity,
                     xBoxPlot = v$xBoxPlot,
                     yBoxPlot = v$yBoxPlot,
                     lmMethod = v$scatMethod, 
                     corMethod = v$corMethod, 
                     minCor = v$minCor)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })
  })
  
  output$lrTable <- renderTable(getLinearTable(df[[v$yScatter]], df[[v$xScatter]], df, v$doPlot))
  
  # Linear Regression Tab ----------  
  
  output$plotLin <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g<- grid.arrange( 
                     gPM <- ggplot(na.omit(df), aes(x=pm2.5, y=TEMP)) + 
                       ggplot2::stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gPRES <- ggplot(df, aes(x=PRES, y=TEMP)) + 
                       ggplot2::stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gDEWP <- ggplot(df, aes(x=DEWP, y=TEMP)) + 
                       ggplot2::stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gIws <- ggplot(df, aes(x=Iws, y=TEMP)) + 
                       ggplot2::stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gIs <- ggplot(df, aes(x=Is, y=TEMP)) + 
                       ggplot2::stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gIr <- ggplot(df, aes(x=Ir, y=TEMP)) + 
                       ggplot2::stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     nrow = 2
                   )
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  })
  
  
  
  
})

getLinearTable <- function(x, y, df, doPlot = FALSE){
  if(doPlot == FALSE) return()
  
  model <- lm(y~x, df)
  d <- data.frame(c(model$coefficients[1]), c(model$coefficients[2]))
  colnames(d) <- c('intercept', 'slope')
  
  return (d)
}