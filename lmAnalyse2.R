library(ggplot2)
library(gridExtra)

setwd("/Users/phrc/Documents/Projects/r projects/weather-prediction")
df <- read.csv("Assignment 2.csv")

print(head(df))
# Select only quantitative data
print(summary(df))
# pm2.5 DEWP TEMP PRESS IWS IS IR

gPM <-  ggplot(na.omit(df), aes(x=pm2.5)) + 
  boxplot(df$pm2.5, sub=paste("Outlier rows: ", boxplot.stats(df$pm2.5)$out)) +
  labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$pm2.5)$out))

print(gPM)

gPM <- ggplot(na.omit(df), aes(x="", y=pm2.5)) +  
  geom_boxplot(outlier.colour="black", outlier.shape=16,
                     outlier.size=2, notch=FALSE) +
  labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$pm2.5)$out))
print(gPM)

d <- boxplot.stats(df$pm2.5)
d$stats
d$n
d$conf
d$out

grid.arrange( 
  
  gPM <- ggplot(na.omit(df), aes(x="", y=pm2.5)) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$pm2.5)$out)),
  
  gPM <- ggplot(df, aes(x="", y=PRES)) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$PRES)$out)),
  
  gPM <- ggplot(df, aes(x="", y=DEWP)) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$DEWP)$out)),
  
  gPM <- ggplot(df, aes(x="", y=Iws)) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$Iws)$out)),
  
  gPM <- ggplot(df, aes(x="", y=Is)) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$Is)$out)),
  
  gPM <- ggplot(df, aes(x="", y=Ir)) +  
    geom_boxplot(outlier.colour="black", outlier.shape=16,
                 outlier.size=2, notch=FALSE) +
    labs(subtitle=paste("Outlier rows: ", boxplot.stats(df$Ir)$out)),
  
  nrow = 3
)

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

  grid.arrange( 
    
    gPM <- ggplot(na.omit(df), aes(x=pm2.5)) + 
      geom_density() ,
    
    gPRES <- ggplot(df, aes(x=PRES)) + 
      geom_density(),
    
    gDEWP <- ggplot(df, aes(x=DEWP)) + 
      geom_density(),
    
    gIws <- ggplot(df, aes(x=Iws)) + 
      geom_density(),
    
    gIs <- ggplot(df, aes(x=Is)) + 
      geom_density(),
    
    gIr <- ggplot(df, aes(x=Ir)) + 
      geom_density(),
    
    nrow = 3
  )



dfN <- na.omit(df)
boxplot.stats(dfN$DEWP)$stats
dfOutlier <- dfN[(dfN$DEWP < -40 | dfN$DEWP > 28),]

dfOutlier <- na.omit(dfOutlier)

grid.arrange( 
  
  gPM <- ggplot(na.omit(dfOutlier), aes(x=pm2.5, y=TEMP)) + 
    stat_binhex() +
    geom_smooth(method="lm"),
  
  gPRES <- ggplot(dfOutlier, aes(x=PRES, y=TEMP)) + 
    stat_binhex() +
    geom_smooth(method="lm"),
  
  gDEWP <- ggplot(dfOutlier, aes(x=DEWP, y=TEMP)) + 
    stat_binhex() +
    geom_smooth(method="lm"),
  
  gIws <- ggplot(dfOutlier, aes(x=Iws, y=TEMP)) + 
    stat_binhex() +
    geom_smooth(method="lm"),
  
  gIs <- ggplot(dfOutlier, aes(x=Is, y=TEMP)) + 
    stat_binhex() +
    geom_smooth(method="lm"),
  
  gIr <- ggplot(dfOutlier, aes(x=Ir, y=TEMP)) + 
    stat_binhex() +
    geom_smooth(method="lm"),
  
  nrow = 3
)

grid.arrange( 
  
  gPM <- ggplot(na.omit(dfOutlier), aes(x=pm2.5)) + 
    geom_density() ,
  
  gPRES <- ggplot(dfOutlier, aes(x=PRES)) + 
    geom_density(),
  
  gDEWP <- ggplot(dfOutlier, aes(x=DEWP)) + 
    geom_density(),
  
  gIws <- ggplot(dfOutlier, aes(x=Iws)) + 
    geom_density(),
  
  gIs <- ggplot(dfOutlier, aes(x=Is)) + 
    geom_density(),
  
  gIr <- ggplot(dfOutlier, aes(x=Ir)) + 
    geom_density(),
  
  nrow = 3
)

ggplot(dfOutlier, aes(x=DEWP)) + 
  geom_density()

ggplot(dfOutlier, aes(x=Iws)) + 
  geom_density()

ggplot(dfOutlier, aes(x=Is)) + 
  geom_density()

ggplot(dfOutlier, aes(x=Ir)) + 
  geom_density()