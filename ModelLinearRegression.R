library(R6)

ModelLinearRegression <- R6Class(
  "ModelLinearRegression",
  public = list(
    df = data.frame(),
    dfQnt = data.frame(),
    dfQntNoOutLiers = data.frame(),
    dfCat = data.frame(),
    dfModel = data.frame(),
    trainDS = data.frame(),
    testDS = data.frame(),
    
    initialize = function(df){
      self$df <- df
      self$dfQnt <- self$df
      self$dfQnt$No <- NULL
      self$dfQnt$cbwd <- NULL
      self$dfQnt$year <- NULL
      self$dfQnt$month <- NULL
      self$dfQnt$day <- NULL
      self$dfQnt$hour <- NULL
      self$dfQnt$date <- NULL
      
      self$dfCat <- df
      
      self$dfCat$season <- lapply(self$dfCat$month, self$toSeason)
      self$dfCat$season <- factor(self$dfCat$season, levels = unique(self$dfCat$season))
      self$dfCat$dayPeriod <- lapply(self$dfCat$hour, self$toDayPeriod)
      self$dfCat$dayPeriod <- factor(self$dfCat$dayPeriod, levels = unique(self$dfCat$dayPeriod))
      self$dfCat$monthPeriod <- lapply(self$dfCat$day, self$toMonthPeriod)
      self$dfCat$monthPeriod <- factor(self$dfCat$monthPeriod, levels = unique(self$dfCat$monthPeriod))
      
      self$dfQntNoOutLiers <- self$dfQnt
      
      
      boxStats <- boxplot.stats(self$dfQntNoOutLiers$pm2.5)$stats
      self$dfQntNoOutLiers[(!is.na(self$dfQntNoOutLiers$pm2.5) & (self$dfQntNoOutLiers$pm2.5 < boxStats[1] | self$dfQntNoOutLiers$pm2.5 > boxStats[5]) ),]$pm2.5 <- NA
      
      boxStats <- boxplot.stats(self$dfQntNoOutLiers$Iws)$stats
      self$dfQntNoOutLiers[(self$dfQntNoOutLiers$Iws < boxStats[1] |self$dfQntNoOutLiers$Iws > boxStats[5]),]$Iws <- NA 
      
      
      boxStats <- boxplot.stats(self$dfQntNoOutLiers$Is)$stats
      self$dfQntNoOutLiers[(self$dfQntNoOutLiers$Is < boxStats[1] | self$dfQntNoOutLiers$Is > boxStats[5]),]$Is <- NA 
      
      
      boxStats <- boxplot.stats(self$dfQntNoOutLiers$Ir)$stats
      self$dfQntNoOutLiers[(self$dfQntNoOutLiers$Ir < boxStats[1] | self$dfQntNoOutLiers$Ir > boxStats[5]),]$Ir <- NA 
      
      self$dfModel <- df
      self$dfModel$season <- lapply(self$dfModel$month, self$toSeason)
      self$dfModel$season <- factor(self$dfModel$season, levels = unique(self$dfModel$season))
      self$dfModel$dayPeriod <- lapply(self$dfModel$hour, self$toDayPeriod)
      self$dfModel$dayPeriod <- factor(self$dfModel$dayPeriod, levels = unique(self$dfModel$dayPeriod))
      self$dfModel$cbwd <- factor(self$dfModel$cbwd, levels = unique(self$dfModel$cbwd))
      self$dfModel$No <- NULL
      self$dfModel$pm2.5 <- NULL
      self$dfModel$year <- NULL
      self$dfModel$month <- NULL
      self$dfModel$day <- NULL
      self$dfModel$hour <- NULL
      self$dfModel$Iws <- NULL
      self$dfModel$Ir <- NULL
      self$dfModel$Is <- NULL
      
    },
    
    outlierPlot = function(df){
      grid.arrange( 
        ggplot(df, aes(x="", y=pm2.5)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) +
          labs(subtitle=paste("Outlier rows: ", length(boxplot.stats(df$pm2.5)$out))),
        ggplot(df, aes(x="", y=PRES)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) +
          labs(subtitle=paste("Outlier rows: ", length(boxplot.stats(df$PRES)$out))),
        ggplot(df, aes(x="", y=DEWP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) +
          labs(subtitle=paste("Outlier rows: ", length(boxplot.stats(df$DEWP)$out))),
        ggplot(df, aes(x="", y=Iws)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) +
          labs(subtitle=paste("Outlier rows: ", length(boxplot.stats(df$Iws)$out))),
        ggplot(df, aes(x="", y=Is)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) +
          labs(subtitle=paste("Outlier rows: ", length(boxplot.stats(df$Is)$out))),
        ggplot(df, aes(x="", y=Ir)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) +
          labs(subtitle=paste("Outlier rows: ", length(boxplot.stats(df$Ir)$out))),
        nrow = 3
      )
    },
    
    densityPlot = function(df){
      grid.arrange( 
        ggplot(na.omit(df), aes(x=pm2.5)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=PRES)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=DEWP)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=Iws)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=Is)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=Ir)) + 
          geom_density(na.rm = TRUE),
        nrow = 3
      )
    },
    
    linearPlot = function(df){
      grid.arrange(
        ggplot(df, aes(x=pm2.5, y=TEMP)) + 
          stat_binhex(na.rm = TRUE) +
          geom_smooth(method="lm", na.rm = TRUE),
        ggplot(df, aes(x=PRES, y=TEMP)) + 
          stat_binhex(na.rm = TRUE) +
          geom_smooth(method="lm", na.rm = TRUE),
        ggplot(df, aes(x=DEWP, y=TEMP)) + 
          stat_binhex(na.rm = TRUE) +
          geom_smooth(method="lm", na.rm = TRUE),
        ggplot(df, aes(x=Iws, y=TEMP)) + 
          stat_binhex(na.rm = TRUE) +
          geom_smooth(method="lm", na.rm = TRUE),
        ggplot(df, aes(x=Is, y=TEMP)) + 
          stat_binhex(na.rm = TRUE) +
          geom_smooth(method="lm", na.rm = TRUE),
        ggplot(df, aes(x=Ir, y=TEMP)) + 
          stat_binhex(na.rm = TRUE) +
          geom_smooth(method="lm", na.rm = TRUE),
        nrow = 3
      )
    },
    
    correlationPlot = function(df){
      corTab <- cor(df, use = "pairwise.complete.obs")
      corTab[is.na(corTab)] <- 0
      ggcorrplot(corTab, hc.order = TRUE, 
                 type = "lower", 
                 lab = TRUE, 
                 lab_size = 4, 
                 method="square", 
                 ggtheme=theme_bw,
                 colors = c("red","white", "blue"))
    },
    
    toDayPeriod = function(hour){
      if (hour < 6) {
        return ("night")
      } else if(hour < 12){
        return ("morning")
      } else if (hour < 18){
        return ("afternoon")
      } else if (hour < 24){
        return ("evening")
      } else{
        return(NULL)
      }
      
    },
    
    toMonthPeriod = function(day){
      if(day < 11){
        return("begin")
      }else if (day < 21){
        return ("middle")
      }else if (day < 32){
        return("end")
      }else {
        return(NULL)
      }
      
    },
    
    toSeason = function(month){
      if(month > 12 || month < 1){
        return(NULL)
      }else if(month > 2 & month < 6){
        return ("spring")
      }else if (month > 5 & month < 9){
        return ("summer")
      }else if (month > 8 & month < 12){
        return ("fall")
      }else{
        return ("winter")
      }
    },

    catPlot = function(df){
      return (grid.arrange( 
        ggplot(df, aes(x=dayPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) ,
        ggplot(df, aes(x=monthPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        ggplot(df, aes(x=season, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) ,
        ggplot(df, aes(x=cbwd, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) ,
        nrow = 2
      ))
    },
    
    catPlotBySeason = function(df){
      return (grid.arrange( 
        ggplot(df[df$season == 'winter',], aes(x=dayPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) ,
        ggplot(df[df$season == 'winter',], aes(x=monthPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        ggplot(df[df$season == "spring",], aes(x=dayPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        ggplot(df[df$season == 'spring',], aes(x=monthPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        ggplot(df[df$season == 'summer',], aes(x=dayPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE) ,
        ggplot(df[df$season == 'summer',], aes(x=monthPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        ggplot(df[df$season == "fall",], aes(x=dayPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        ggplot(df[df$season == 'fall',], aes(x=monthPeriod, y=TEMP)) +  
          geom_boxplot(outlier.colour="black", outlier.shape=16,
                       outlier.size=2, notch=FALSE, na.rm = TRUE),
        nrow = 4
      ))
    },
    
    catDensityPlot = function(df){
      grid.arrange( 
        ggplot(na.omit(df), aes(x=dayPeriod)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=monthPeriod)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=season)) + 
          geom_density(na.rm = TRUE),
        ggplot(na.omit(df), aes(x=cbwd)) + 
          geom_density(na.rm = TRUE),
        nrow = 2
      )
    },
    
    setTrainAndTestDS = function(dfModel){
      row.number <- sample(1:nrow(dfModel), 0.7*nrow(dfModel))
      self$trainDS <- dfModel[row.number,]
      self$testDS <- dfModel[-row.number,]
    },
      
    createModels = function(dfModel){

      fit1 <- lm(TEMP ~ season+dayPeriod+cbwd+DEWP+PRES,data=self$trainDS)
      
      fit1a <- lm(TEMP ~ season+dayPeriod+cbwd+DEWP,data=self$trainDS)
      
      fit1b <- lm(TEMP ~ season+dayPeriod+cbwd+PRES,data=self$trainDS)
      
      fit2 <- lm(TEMP ~ 1, self$trainDS)
      
      both1 <- stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
      
      both1a <- stepAIC(fit2,direction="both",scope=list(upper=fit1a,lower=fit2))
      
      both1b <- stepAIC(fit2,direction="both",scope=list(upper=fit1b,lower=fit2))

      return(list("pd" = both1, "d" = both1a, "p" = both1b))
      
    },
    
    predict = function(both) {
      pred1 <- predict(both, newdata = self$testDS)
      return(pred1)
    },
    
    normalidade = function(x){
      t1 <- ks.test(x, "pnorm",mean(x), sd(x)) # KS  
      t2 <- lillie.test(x) # Lilliefors
      t3 <- cvm.test(x) # Cram?r-von Mises
      t6 <- ad.test(x) # Anderson-Darling
      t7<-pearson.test(x) # Pearson Test of Normality
      
      testes <- c(t1$method, t2$method, t3$method, t6$method,t7$method)
      valorp <- c(t1$p.value, t2$p.value, t3$p.value,t6$p.value,t7$p.value)
      
      result <- cbind(valorp)
      rownames(result) <- testes
      print(result, digits = 4)
      
    }

  )
)