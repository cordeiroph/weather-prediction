library(R6)

GraphicGenerator <- R6Class(
  "GraphicGenerator",
  list(
  df = data.frame(),
  initialize = function(df){
    self$df <- df
  },
  generateGraphic = function(type,
                             xScatter, 
                             yScatter, 
                             xHist,
                             binHist,
                             xDensity,
                             xBoxPlot,
                             yBoxPlot, 
                             lmMethod, 
                             corMethod, 
                             minCor){
    if(type == 'scatterplot'){
      return(self$generateScatterPlot(xScatter, yScatter, lmMethod))
    }
    else if(type == 'histogram'){
      return(self$generateHistogram(xHist, binHist))
    } 
    else if (type == 'density'){
      return(self$generateDensity(xDensity))
    }
    else if (type == 'correlation graph'){
      return( self$generateCorrelationGraph(corMethod, minCor))
    } 
    else if (type == 'correlation matrix'){
      return(self$generateCorrelationMatrix(corMethod))
    }
    else if (type == 'boxplot'){
      return(self$generateBoxPlot(xBoxPlot, yBoxPlot))
    } 
  },
  generateScatterPlot = function(xVar, yVar, lmMethod){
    print(head(self$df))
    return (
      ggplot(data = self$df, aes_string(xVar, yVar)) + 
        stat_binhex() +
        geom_smooth(method=lmMethod, fill="red", color="red") 
    )
  },
  generateHistogram = function(xVar, binHist){
    return (
      ggplot(self$df, aes_string(xVar))  + 
      geom_histogram(bins = binHist)
      )
  },
  generateDensity = function(xVar){
    return(
      ggplot(self$df, aes_string(xVar)) + 
        geom_density()
    )
  },
  generateCorrelationGraph = function(corMethod, minCor){
    temp <- self$df
    temp$date <- NULL
    temp$cbwd <- NULL
    print(head(temp))
    dfcor <- temp %>% correlate(method=corMethod)
    dfcor[is.na(dfcor)] <- 0
    return ( dfcor %>% 
      network_plot(colours = c("red","green", "blue"), min_cor = minCor))
  },
  generateCorrelationMatrix = function(corMethod){
    temp <- self$df
    temp$date <- NULL
    temp$cbwd <- NULL
    print(head(temp))
    return(
      ggcorrplot(cor(na.omit(temp), method = corMethod), hc.order = TRUE, 
                 type = "lower", 
                 lab = TRUE, 
                 lab_size = 4, 
                 method="square", 
                 ggtheme=theme_bw,
                 colors = c("red","white", "blue"))
    )
  },
  generateBoxPlot = function(xVar, yVar){
    return(
      ggplot(self$df, aes_string(factor(self$df[[xVar]]), yVar)) + 
        geom_boxplot()
    )
  }
  )
)

