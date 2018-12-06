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
source("datasetLoader.R", local = TRUE)
source("uiDataDescription.R", local = TRUE)

shinyServer(function(input, output) {
  df <- getDataSet()
  df2 <- head(getNonNullDataSet(), n=100)
  df4 <- getDataSet2()

  output$summary <- renderPrint({
    summary(df)
  })
  
  output$visualisation <- renderPrint({
    str(df)
  })
  
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
  
  
  
  output$plot <- renderPlot({
    if(v$doPlot == FALSE) return()
    
    withProgress(message = 'Making plot', value = 0, 
                 {
                   incProgress(0.5)
                   
                   if(v$graphic == 'scatterplot'){
                     g <- ggplot(data = df, aes_string(v$xScatter, v$yScatter)) + 
                       stat_binhex() +
                       geom_smooth(method=v$scatMethod, fill="red", color="red") 
                   }
                   else if(v$graphic == 'histogram'){
                     g <- ggplot(df, aes_string(v$xHist))  + 
                       geom_histogram(bins = input$binHist)
                   } 
                   else if (v$graphic == 'density'){
                     g <- ggplot(df, aes_string(v$xDensity)) + 
                       geom_density()
                   }
                   else if (v$graphic == 'correlation graph'){
                     dfcor <- df4 %>% correlate(method=v$corMethod)
                     dfcor[is.na(dfcor)] <- 0
                     #                     dfcor <- rearrange(dfcor)
                     g <- dfcor %>% 
                       network_plot(colours = c("red","green", "blue"), min_cor = v$minCor)
                     
                   } 
                   else if (v$graphic == 'correlation matrix'){
                     g <- ggcorrplot(cor(df4, method = v$corMethod), hc.order = TRUE, 
                                     type = "lower", 
                                     lab = TRUE, 
                                     lab_size = 4, 
                                     method="square", 
                                     ggtheme=theme_bw,
                                     colors = c("red","white", "blue"))
                   }
                   else if (v$graphic == 'boxplot'){
                       g <- ggplot(df, aes_string(factor(df[[v$xBoxPlot]]), v$yBoxPlot)) + 
                         geom_boxplot()
                   } 
                   else if(v$graphic == 'timeplot'){
                     
                     summ <- paste0(v$opTimePlot,'(', v$varTimePlot, ')')  
                     summ_name <- v$varTimePlot  
                     
                     print(v$pdTimePlot)
                     dfGrouped <- df %>%
                       group_by_(.dots = v$pdTimePlot) %>%
                       summarise_(.dots = setNames(summ, summ_name))
                     
                     g <- ggplot(dfGrouped, aes_string(x=v$pdTimePlot)) + 
                       geom_line(aes_string(y=summ_name))
                   } 
                   
                   
                   
                   
                   incProgress(1)
                   
                   print(g)
                 })
  })
  
})