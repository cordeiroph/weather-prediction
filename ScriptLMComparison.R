df <- read.csv("Assignment 2.csv")

row.number <- sample(1:nrow(df), 0.7*nrow(df))
train1 = df[row.number,]
train2 = df[row.number,]
test1 = df[-row.number,]
test2 = df[-row.number,]

fit1 <- lm(TEMP ~ PRES,data=train1)

fit2 <- lm(TEMP ~ PRES+Iws,data=train2)

anova(fit1) # anova table 
anova(fit2) # anova table 


pred1 <- predict(fit1, newdata = test1)
pred2 <- predict(fit2, newdata = test2)

grid.arrange(
  ggplot(test1, aes(TEMP, pred1)) + stat_binhex(),
  ggplot(test2, aes(TEMP, pred2)) + stat_binhex(),
    nrow = 2)

