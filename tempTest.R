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
library(tidyr)


dfGrouped <- group_by(df, df[[x]]) + summarise(mean(TEMP) )

summ <- paste0('mean(', y, ')')  # construct summary method, e.g. mean(mpg)
summ_name <- paste0('mean_', y)  # construct summary variable name, e.g. mean_mpg

dfGrouped <- df %>% select_(x, y) %>% group_by_(x) %>% summarise_(.dots = setNames(sum, summ_name))

group_by(year) %>% summarise(TEMP = mean(TEMP))

data.frame()

eco

#bla
summarise_vars <- list(list(x, y))


  
  g <- ggplot(dfGrouped, aes_string(x=x)) + 
    geom_line(aes_string(y=y))
  
  print(df_summ)
  
  setwd("/Users/phrc/Documents/Projects/r projects/weather-prediction")
  df <- read.csv("Assignment 2.csv")
  x <- c('year', 'month', 'day', 'hour')
  df <- unite_(df, "date", x, sep = "/", remove = FALSE)
  df$date <- as.POSIXct(df$date, format = "%Y/%m/%d/%H")
  df$date2 <- as.POSIXct(df$date, format = "%Y/%m/%d/%H")
  
  x <- 'month'
  y <- 'TEMP'
  w <- 'min'
  
  
  summ <- paste0(w,'(', y, ')')  
  summ_name <- y  
  
  dfGrouped <-  df %>%
    group_by(date = floor_date(date, x)) %>%
    summarise_(.dots = setNames(summ, "val"))
  
  
  library(dygraphs)
  library(xts)
  library(zoo)
  
  
  xt <- xts(cbind(df$TEMP), df$date)
  g <- dygraph(xt) %>% 
    dyAxis("y", valueRange = c(min(df$TEMP), max(df$TEMP))) %>% 
    dyAxis("x", valueRange = c(min(df$date), max(df$date))) %>% 
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
    dyRangeSelector()
  
  g
  
  
  g <- ggplot(dfGrouped, aes(x=date)) + 
    geom_line(aes_string(y=summ_name))
  
  g
  
  
  
  
  summ <- paste0(w,'(', y, ')')  
  summ_name <- y  
  
  df_summ <- df %>%
    group_by_(.dots = x) %>%
    summarise_(.dots = setNames(summ, summ_name))
  
  
  
  g <- ggplot(dfGrouped, aes_string(x=x)) + 
    geom_line(aes_string(y=y))  
  
  setwd("/home/phrc/R Projects/TemperaturePrediction/")
  df <- read.csv("Assignment 2.csv")
  
  
  x <- c('year', 'month', 'day', 'hour')
  y <- 'TEMP'
  w <- 'mean'
  
  summ <- paste0(w,'(', y, ')')  
  summ_name <- y  
  
  summ2 <-paste0('paste0(',x,')')
  summ_name2 <- 'date'
  
  dfGrouped <- df %>%
    group_by_(.dots = x) %>%
    summarise_(.dots = setNames(summ, summ_name)) 
  
  
  
  df <- read.csv("Assignment 2.csv")
  df <- unite_(df, "date", x, sep = "/", remove = FALSE)
  df$date <- as.POSIXct(df$date, format = "%Y/%m/%d/%H")
  
  dfGrouped <-  df %>%
  group_by(month=floor_date(date, "month")) %>%
    summarize(summary_variable=sum(TEMP))
  
  g <- ggplot(dfGrouped, aes(x=date)) + 
    geom_line(aes_string(y))  
  print(g)  
  
    
    
    apply( df[ , x ] , 1 , paste , collapse = "-" )
  
  df$date <- as.Date(df$date)
  
  head(df)
  
  dfGrouped$year <- NULL
  dfGrouped$month <- NULL
  
  g <- autoplot(dfGrouped)
  g
  library(lubridate)
  library(tidyverse)
  library(nycflights13)

  
  
  print(v$pdTimePlot)
  dfGrouped <- df %>%
    group_by_(.dots = ) %>%
    summarise_(.dots = setNames(summ, summ_name))
  
  #                     dfGrouped$date <-
  #                       apply( dfGrouped[ , v$pdTimePlot ] , 1 , paste , collapse = "/" )
    
  df <- unite_(df, "date", x, sep = "/", remove = FALSE)
  df$date <- as.POSIXct(df$date, format = "%Y/%m/%d/%H")
  
  dfGrouped <-  df %>%
    group_by(year = floor_date(date, "month")) %>%
    summarize(mean=sum(TEMP))
  
  g <- ggplot(dfGrouped, aes(x=year)) + 
    geom_line(aes(y=mean))  
  print(g)  
  
  g <- ggplot(df, aes(x=date)) + 
    geom_line(aes(y=DEWP))  
  print(g)  
  
  e <- economics
  
  xt <- xts(cbind(e$pop, e$pce), e$date)
  g <- dygraph(xt) %>% 
#    dyAxis("y", valueRange = c(min(e$pop), max(df$TEMP))) %>% 
#    dyAxis("x", valueRange = c(min(df$date), max(df$date))) %>% 
#    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  #%>%
  #  dyRangeSelector()
  
  g
  
  
  library(WDI)
  library(tidyr)
  library(xts)
  library(dplyr)
  
  df <- WDI(country = c("CN", "US"), indicator = "TX.QTY.MRCH.XD.WD", start = 1980, end = 2013, extra = FALSE)
  
  df$exports <- df$TX.QTY.MRCH.XD.WD
  
  df1 <- df %>%
    select(country, year, exports) %>%
    mutate(country = gsub("United States", "USA", df$country)) %>%
    spread(key = country, value = exports) %>%
    mutate(date = as.Date(as.character(year), format = "%Y")) %>%
    select(-year) 
  
  xtdata <- xts(df1, order.by = df1$date) 
  
  xtdata$date <- NULL
  
  t <- dygraph(xtdata, main = "Export volume index, 1980-2013 (2000 = 100)") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyOptions(colors = c("red", "navy"))
  
  print(t)
  
  
  ggplot(economics, aes(x=date)) + 
    geom_line(aes(y=pce))
  

  
  head(dfGrouped)
  
    paste0(c(dfGrouped$year, dfGrouped$year))
  
  dfGrouped <- dfGrouped %>%
    group_by_(.dots = x) %>%
    mutate_(.dots = setNames(summ2, summ_name2))
  
  
  
  library(ggplot2)
  library(lubridate)
  
  e <- economics
  
  dt <- lubridate::as_date(e$date)
  
  
  
  xt <- xts(cbind(e$pce), lubridate::as_date(e$date))
  
  
  library(dygraphs)
  library(xts)
  library(zoo)
  data <- rnorm(5)
  
  dates <- seq(zoo::as.Date(2017), length=5, by="year")
  xts2 <- xts(x=data, order.by=dates)
  dygraph(xts2) %>% 
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
    dyRangeSelector()
  
  
  
  
setwd("/Users/phrc/Documents/Projects/r projects/weather-prediction")
df <- read.csv("Assignment 2.csv")
x <- 'DEWP'
y <- 'TEMP'

model <- lm(df[[y]]~df[[x]], df)
summary(model)

model$coefficients[1]
model$coefficients[2]

d <- data.frame(c(model$coefficients[1]), c(model$coefficients[2]))
colnames(d) <- c('intercept', 'slope')

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
grid.table(d, theme=tt)

typeof(model$terms)
#----------
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


observeEvent(input$jumpToLrUi,{
  
  print("Passed Here")
  
  output$plotLin <- renderPlot({
    withProgress(message = 'Making plot', value = 0, 
                 {
                   print("Passed Here 2")
                   incProgress(0.5)
                   grid.arrange( 
                     
                     gPM <- ggplot(na.omit(df), aes(x=pm2.5, y=TEMP)) + 
                       stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gPRES <- ggplot(df, aes(x=PRES, y=TEMP)) + 
                       stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gDEWP <- ggplot(df, aes(x=DEWP, y=TEMP)) + 
                       stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gIws <- ggplot(df, aes(x=Iws, y=TEMP)) + 
                       stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gIs <- ggplot(df, aes(x=Is, y=TEMP)) + 
                       stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     gIr <- ggplot(df, aes(x=Ir, y=TEMP)) + 
                       stat_binhex() +
                       geom_smooth(method="lm"),
                     
                     nrow = 3
                   )
                   
                   incProgress(1)
                   
                 })
  })  
})

,
tabPanel(
  "Linear Regresseion",
  lrUi("lrUi"),
  actionButton('jumpToLrUi', '')
)
#----------------
,
mainPanel(
  helpText("Check for linearity, Homoscedasticity for quantitative variables"),
  plotOutput(outputId = "plotLin"),
  helpText("Result: \n Only Pres and Dewp has a linearity and homoscedasticity "),
  helpText("Linear Regression for the variable Pres and Dewp "),
  textOutput(outputId = "sumLMPD"),
  plotOutput(outputId = "plotQQNormPD"),
  textOutput(outputId = "ksTestPD"),
  textOutput(outputId = "adTestPD"),
  textOutput(outputId = "resultLMPD"),
  
  helpText("Linear Regression for the variable Pres"),
  textOutput(outputId = "sumLMP"),
  plotOutput(outputId = "plotQQNormPD"),
  textOutput(outputId = "ksTestP"),
  textOutput(outputId = "adTestP"),
  textOutput(outputId = "resultLMP"),
  
  helpText("Linear Regression for the variable Dewp"),
  textOutput(outputId = "sumLMD"),
  plotOutput(outputId = "plotQQNormPD"),
  textOutput(outputId = "ksTestD"),
  textOutput(outputId = "adTestD"),
  textOutput(outputId = "resultLMD")
  
)
