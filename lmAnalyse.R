library(ggplot2)
library(gridExtra)

setwd("/Users/phrc/Documents/Projects/r projects/weather-prediction")
df <- read.csv("Assignment 2.csv")

print(head(df))
# Select only quantitative data
print(summary(df))
# pm2.5 DEWP TEMP PRESS IWS IS IR

# check for linearity, Homoscedasticity and Independence of Error
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

#Result: DEWp and PRES

modelDEWP <- lm(TEMP~DEWP, df)


summary(modelDEWP)
qqnorm(modelDEWP$residuals)
ad.test(modelDEWP$residuals)
ks <- ks.test(modelDEWP$residuals,y='pnorm',alternative='two.sided')
ks 

modelPRES <- lm(TEMP~PRES, df)

summary(modelPRES)
qqnorm(modelPRES$residuals)
ad.test(modelPRES$residuals)
ks <- ks.test(modelPRES$residuals,y='pnorm',alternative='two.sided')
ks

modelPD <- lm(TEMP~PRES + DEWP, df)

summary(modelPD)
qqnorm(modelPD$residuals)
ad.test(modelPD$residuals)
ks <- ks.test(modelPD$residuals,y='pnorm',alternative='two.sided')
ks



library(ggpubr)
library(nortest)
grid.arrange(
  ggdensity(df$DEWP),
ggqqplot(df$DEWP),
ggdensity(df$PRES),
ggqqplot(df$PRES),
nrow = 2)


# test if the variable are normal distribiute
ks <- ks.test(df$DEWP,y='pnorm',alternative='two.sided')

ad.test(df$DEWP)
ad.test(df$PRES)
ad.test(df$pm2.5)
ad.test(df$Iws)
ad.test(df$Is)
ad.test(df$Ir)

print(ks)
qqnorm(df$DEWP)
shapiro.test(df$DEWP)
qqnorm(df$PRES)
shapiro.test(df$PRES)

