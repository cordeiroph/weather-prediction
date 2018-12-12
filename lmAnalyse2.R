library(ggplot2)
library(gridExtra)
library(ggcorrplot)

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
  corTab <- cor(dfNoOutLiers, use = "pairwise.complete.obs")
  corTab[is.na(corTab)] = 0
  ggcorrplot(corTab, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 4, 
             method="square", 
             ggtheme=theme_bw,
             colors = c("red","white", "blue"))
}

outlierPlot(df)
linearPlot(df)
densityPlot(df)
correlationPlot(df, "pearson")

# Analyse without outliers -------

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




