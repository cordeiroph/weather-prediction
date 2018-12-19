library(R6)

ModelLMComparison <- R6Class(
  "ModelLMComparison",
  public = list(
    df = data.frame(),
    train1 =  data.frame(),
    train2 =  data.frame(),
    test1 =  data.frame(),
    test2 =  data.frame(),
    fit1 =  data.frame(),
    fit2 =  data.frame(),
    pred1 =  data.frame(),
    pred2 =  data.frame(),
    
    initialize = function(df){
      self$df <- df
      row.number <- sample(1:nrow(self$df), 0.7*nrow(self$df))
      self$train1 = self$df[row.number,]
      self$train2 = self$df[row.number,]
      self$test1 = self$df[-row.number,]
      self$test2 = self$df[-row.number,]
      
    },
    
    createModels = function(){
      self$fit1 = lm(TEMP ~ PRES,data=self$train1)
      
      self$fit2 = lm(TEMP ~ PRES+Iws,data=self$train2)
      
      self$pred1 = predict(self$fit1, newdata = self$test1)
      
      self$pred2 = predict(self$fit2, newdata = self$test2)
    },
    
    generateScatterPlot = function(){
      grid.arrange(
        ggplot(self$test1, aes(TEMP, self$pred1)) + ggtitle("TEMP ~ PRES") + stat_binhex(),
        ggplot(self$test2, aes(TEMP, self$pred2)) + ggtitle("TEMP ~ PRES + Iws") + stat_binhex(),
        nrow = 2)
    }
  )
  
  
  
  
)
