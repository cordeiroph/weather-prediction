


dfGrouped <- group_by(df, df[[x]]) + summarise(mean(TEMP) )

summ <- paste0('mean(', y, ')')  # construct summary method, e.g. mean(mpg)
summ_name <- paste0('mean_', y)  # construct summary variable name, e.g. mean_mpg

dfGrouped <- df %>% select_(x, y) %>% group_by_(x) %>% summarise_(.dots = setNames(sum, summ_name))

group_by(year) %>% summarise(TEMP = mean(TEMP))





summarise_vars <- list(list(x, y))


  
  g <- ggplot(dfGrouped, aes_string(x=x)) + 
    geom_line(aes_string(y=y))
  
  print(df_summ)
  
  setwd("/Users/phrc/Documents/Projects/r projects")
  df <- read.csv("Assignment 2.csv")
  
  
  x <- 'year'
  y <- 'TEMP'
  w <- 'min'
  
  summ <- paste0(w,'(', y, ')')  
  summ_name <- y  
  
  df_summ <- df %>%
    group_by_(.dots = x) %>%
    summarise_(.dots = setNames(summ, summ_name))
  
  
  
  g <- ggplot(dfGrouped, aes_string(x=x)) + 
    geom_line(aes_string(y=y))  
  
  setwd("/Users/phrc/Documents/Projects/r projects")
  df <- read.csv("Assignment 2.csv")
  
  
  x <- c('year', 'month')
  y <- 'TEMP'
  w <- 'min'
  
  summ <- paste0(w,'(', y, ')')  
  summ_name <- y  
  
  dfGrouped <- df %>%
    group_by_(.dots = x) %>%
    summarise_(.dots = setNames(summ, summ_name))
  
  dfGrouped <- dfGrouped %>%
    group_by_(.dots = x) %>%
    mutate_(.dots = paste0('val = paste0(',x,')'))
  
  g <- ggplot(dfGrouped, aes_string(x=x[2])) + 
    geom_line(aes_string(y=y))  
  
  print(g)
  
