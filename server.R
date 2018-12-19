library(ggplot2)
library(xlsx)
library(plyr)
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
library(MASS)
library(R6)
library(nortest)
source("datasetLoader.R", local = TRUE)
source("uiDataDescription.R", local = TRUE)
source("uiLinearRegression.R", local = TRUE)
source("ModelGraphicGenerator.R", local = TRUE)
source("ModelLinearRegression.R", local = TRUE)

shinyServer(function(input, output, session) {
  
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
  
  linearModel <- ModelLinearRegression$new(df)
  
  output$plotOutLiers <- renderPlot({
    print("Enter here")
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   g <- linearModel$outlierPlot(linearModel$dfQnt)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     g
                   }
                   
                 })                   
  }, width = 600, height = 800)
  
  output$plotLinear <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$linearPlot(linearModel$dfQnt)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotCorrelation <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$correlationPlot(linearModel$dfQnt)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotDensity <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$densityPlot(linearModel$dfQnt)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotLinearNO <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$linearPlot(linearModel$dfQntNoOutLiers)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotCorrelationNO <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$correlationPlot(linearModel$dfQntNoOutLiers)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotDensityNO <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$densityPlot(linearModel$dfQntNoOutLiers)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)

  output$plotBoxPlot <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$catPlot(linearModel$dfCat)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotBoxPlotBySeason <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                  g <- linearModel$catPlotBySeason(linearModel$dfCat)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  output$plotDensityCat <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- linearModel$catDensityPlot(linearModel$dfCat)
                   
                   incProgress(1)
                   
                   if(!is.null(g)){
                     print(g)
                   }
                 })                   
  }, width = 600, height = 800)
  
  linearModel$setTrainAndTestDS(linearModel$dfModel)
  models <- linearModel$createModels(linearModel$dfModel)
  pred <- linearModel$predict(models$model)
  actuals_preds <- data.frame(cbind(actuals=linearModel$testDS$TEMP, predicteds=pred)) 
  
  output$mdCompPD <-  renderPrint({
    anova(models$pd)
  })
  
  output$mdCompD <-  renderPrint({
    anova(models$d)
  })
  
  output$mdCompP <-  renderPrint({
    anova(models$p)
  })
  
  output$mdCompModel <-  renderPrint({
    anova(models$model)
  })
  
  output$summaryPD <-  renderPrint({
    anova(models$model)
  })
  
  output$coefficientsPD <-  renderPrint({
    coefficients(models$model)
  })
  
  output$normalidadeResiduals <-  renderPrint({
    linearModel$normalidade(models$model$residuals)
  })
  
  output$normalidadeResidualsPlot <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- hist(models$model$residuals)

                   incProgress(1)
                   if(!is.null(g)){
                     print(g)
                   }
                   
                 })                   
  }, width = 600, height = 800)
  
  output$predictionPlot <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   g <- plot(linearModel$testDS$TEMP, (pred))
                   
                   incProgress(1)
                   if(!is.null(g)){
                     print(g)
                   }
                   
                 })                   
  }, width = 600, height = 800)
  
  output$correlationAccuracyPred <-  renderPrint({
    cor(actuals_preds)
  })
  
  output$rSquarePred <-  renderPrint({
    rmse <- sqrt(mean(models$model$residuals^2))
    c(RMSE = rmse, R2=summary(models$pd)$r.squared)
  })
  
  })

getLinearTable <- function(x, y, df, doPlot = FALSE){
  if(doPlot == FALSE) return()
  
  model <- lm(y~x, df)
  d <- data.frame(c(model$coefficients[1]), c(model$coefficients[2]))
  colnames(d) <- c('intercept', 'slope')
  
  return (d)
}