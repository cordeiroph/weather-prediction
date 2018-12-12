library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(MASS)
library(nortest)

#setwd("/Users/phrc/Documents/Projects/R projects/TemperaturePrediction")
dfMaster <- read.csv("Assignment 2.csv")

df <-dfMaster

df$No <- NULL
df$cbwd <- NULL
df$year <- NULL
df$month <- NULL
df$day <- NULL
df$hour <- NULL

print(head(df))
# Select only quantitative data
print(summary(df))
# pm2.5 DEWP TEMP PRESS IWS IS IR

# Quantitative Functions ------
outlierPlot <- function(df){
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
}

densityPlot <- function(df){
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
}

linearPlot <- function(df){
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
}

correlationPlot <- function(df, corMethod){
  corTab <- cor(df, use = "pairwise.complete.obs")
  corTab[is.na(corTab)] = 0
  ggcorrplot(corTab, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 4, 
             method="square", 
             ggtheme=theme_bw,
             colors = c("red","white", "blue"))
}


# Quantitative Data Analyse ----
outlierPlot(df)
linearPlot(df)
densityPlot(df)
correlationPlot(df, "pearson")

# Quantitative Data Analyse without outliers -------

dfNoOutLiers <- df

boxStats <- boxplot.stats(df$pm2.5)$stats
dfNoOutLiers[(!is.na(dfNoOutLiers$pm2.5) & (dfNoOutLiers$pm2.5 < boxStats[1] | dfNoOutLiers$pm2.5 > boxStats[5]) ),]$pm2.5 <- NA

boxStats <- boxplot.stats(df$Iws)$stats
dfNoOutLiers[(dfNoOutLiers$Iws < boxStats[1] |dfNoOutLiers$Iws > boxStats[5]),]$Iws <- NA 

boxStats <- boxplot.stats(df$Is)$stats
dfNoOutLiers[(dfNoOutLiers$Is < boxStats[1] | dfNoOutLiers$Is > boxStats[5]),]$Is <- NA 

boxStats <- boxplot.stats(df$Ir)$stats
dfNoOutLiers[(dfNoOutLiers$Ir < boxStats[1] | dfNoOutLiers$Ir > boxStats[5]),]$Ir <- NA 

print(summary(dfNoOutLiers))

outlierPlot(dfNoOutLiers)
linearPlot(dfNoOutLiers)
densityPlot(dfNoOutLiers)
correlationPlot(dfNoOutLiers, "")

# Categorical data -----

toDayPeriod <- function(hour){
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
  
}

toMonthPeriod <- function(day){
  if(day < 11){
    return("begin")
  }else if (day < 21){
    return ("middle")
  }else if (day < 32){
    return("end")
  }else {
    return(NULL)
  }
  
}

toSeason <- function(month){
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
}

catPlot <- function(df){
  grid.arrange( 
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
  )
}

catPlotBySeason <- function(df){
  grid.arrange( 
    ggplot(df[df$season == 'winter',], aes(x=dayPeriod, y=TEMP)) +  
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE, na.rm = TRUE) ,
    ggplot(df[df$season == 'winter',], aes(x=monthPeriod, y=TEMP)) +  
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE, na.rm = TRUE),
    ggplot(dfCat[dfCat$season == "spring",], aes(x=dayPeriod, y=TEMP)) +  
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
    ggplot(dfCat[dfCat$season == "fall",], aes(x=dayPeriod, y=TEMP)) +  
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE, na.rm = TRUE),
    ggplot(df[df$season == 'fall',], aes(x=monthPeriod, y=TEMP)) +  
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE, na.rm = TRUE),
    nrow = 4
  )
}

catDensityPlot <- function(df){
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
}

dfCat <- dfMaster

dfCat$season <- lapply(dfCat$month, toSeason)
dfCat$season <- factor(dfCat$season, levels = unique(dfCat$season))
dfCat$dayPeriod <- lapply(dfCat$hour, toDayPeriod)
dfCat$dayPeriod <- factor(dfCat$dayPeriod, levels = unique(dfCat$dayPeriod))
dfCat$monthPeriod <- lapply(dfCat$day, toMonthPeriod)
dfCat$monthPeriod <- factor(dfCat$monthPeriod, levels = unique(dfCat$monthPeriod))

catPlot(dfCat)
catPlotBySeason(dfCat)
catDensityPlot(dfCat)


# variables analyse --------
# The variables is, ir and iws are compose only by 0 with a few values over less than 105 of the dataset, 
# and for that reason, they are not good to be add at this first model
# PM2.5 is not homoscedastic, after removing the outliers the variable apresented a horizontal line, baically covering all 
# the temperatures for each value and because and the correlation value proved this attribute doesn't have any significant correlation
# with temperature or others variables, and for that reason this variable wont be used in this first model
# PRES and DEWP has a strong and linear correlation and for that reason those variable should be part of the model 
# The categoriacal variable season affects the temp variable, as the day period and cbwd 
# and for that reason those variable should be part of the model 
# at otherside, the variable month period doesn't seems to be affecting the temp and for that reason it wont be included in the model 
# Model variables:
# DWEP, PRES, Season, DayPeriod, cwbd



# MODELING ------

normalidade<-function(x){
  t1 <- ks.test(x, "pnorm",mean(x), sd(x)) # KS  
  t2 <- lillie.test(x) # Lilliefors
  t3 <- cvm.test(x) # Cram?r-von Mises
  # t4 <- shapiro.test(x) # Shapiro-Wilk 
  #  t5 <- sf.test(x) # Shapiro-Francia
  t6 <- ad.test(x) # Anderson-Darling
  t7<-pearson.test(x) # Pearson Test of Normality
  
  testes <- c(t1$method, t2$method, t3$method, t6$method,t7$method)
  valorp <- c(t1$p.value, t2$p.value, t3$p.value,t6$p.value,t7$p.value)
  
  resultados <- cbind(valorp)
  rownames(resultados) <- testes
  print(resultados, digits = 4)
  
}

dfModel <- dfMaster
dfModel$season <- lapply(dfModel$month, toSeason)
dfModel$season <- factor(dfModel$season, levels = unique(dfModel$season))
dfModel$dayPeriod <- lapply(dfModel$hour, toDayPeriod)
dfModel$dayPeriod <- factor(dfModel$dayPeriod, levels = unique(dfModel$dayPeriod))
dfModel$cbwd <- factor(dfModel$cbwd, levels = unique(dfModel$cbwd))
dfModel$No <- NULL
dfModel$pm2.5 <- NULL
dfModel$year <- NULL
dfModel$month <- NULL
dfModel$day <- NULL
dfModel$hour <- NULL
dfModel$Iws <- NULL
dfModel$Ir <- NULL
dfModel$Is <- NULL

head(dfModel, 5)



# Split dataset
row.number <- sample(1:nrow(dfModel), 0.7*nrow(dfModel))
train = dfModel[row.number,]
test = dfModel[-row.number,]
dim(train)
dim(test)

# Modeling 

# Stepwise Regression
fit1 <- lm(TEMP ~ season+dayPeriod+cbwd+DEWP+PRES,data=train)

fit1a <- lm(TEMP ~ season+dayPeriod+cbwd+DEWP,data=train)

fit1b <- lm(TEMP ~ season+dayPeriod+cbwd+PRES,data=train)

fit2 <- lm(TEMP ~ 1, train)

both1 <- stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))

both1a <- stepAIC(fit2,direction="both",scope=list(upper=fit1a,lower=fit2))

both1b <- stepAIC(fit2,direction="both",scope=list(upper=fit1b,lower=fit2))

both1$anova
both1a$anova
both1b$anova

both <-both1
summary(both)
coefficients(both) # model coefficients
confint(both, level=0.95) # CIs for model parameters 
anova(both) # anova table 
normalidade(both$residuals)
hist(both$residuals)




actuals_preds <- data.frame(cbind(actuals=test$TEMP, predicteds=pred1)) 
correlation_accuracy <- cor(actuals_preds)

correlation_accuracy

pred1 <- predict(both, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$TEMP)^2)/length(test$TEMP))
c(RMSE = rmse, R2=summary(both)$r.squared)

par(mfrow=c(1,1))
plot(test$TEMP, (pred1))

#---------------

# Stepwise Regression
fit1 <- lm(TEMP ~ season+dayPeriod+DEWP+PRES,data=train)

fit1a <- lm(TEMP ~ season+dayPeriod+DEWP,data=train)

fit1b <- lm(TEMP ~ season+dayPeriod+PRES,data=train)

fit2 <- lm(TEMP ~ 1, train)

both1 <- stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))

both1a <- stepAIC(fit2,direction="both",scope=list(upper=fit1a,lower=fit2))

both1b <- stepAIC(fit2,direction="both",scope=list(upper=fit1b,lower=fit2))

both1$anova
both1a$anova
both1b$anova

both <-both1a
summary(both)
coefficients(both) # model coefficients
confint(both, level=0.95) # CIs for model parameters 
anova(both) # anova table 
normalidade(both$residuals)
hist(both$residuals)


pred1 <- predict(both, newdata = test)

actuals_preds <- data.frame(cbind(actuals=test$TEMP, predicteds=pred1)) 
correlation_accuracy <- cor(actuals_preds)

correlation_accuracy

rmse <- sqrt(sum((exp(pred1) - test$TEMP)^2)/length(test$TEMP))
c(RMSE = rmse, R2=summary(both)$r.squared)

par(mfrow=c(1,1))
plot(test$TEMP, (pred1))

#-----
# AIC / T value / P value / Rsquare R adjust Square / normality / Confidence Interval 

# Check for statisc significance -> (T|P value)
# Check for normality  -> (RMSE R2 R adjust Square)
# Check fitting  -> AIC (compare models) | RMSE
# Check the sample its true repesentation of the population -> CI


