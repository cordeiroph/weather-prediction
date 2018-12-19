lmComparison <- function(id){
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      h2("Analyse"),
      h3("Analyse of Variance Table"),
      h5("P-value Interpretation: "),
      h5("Null: removed factors are not different from 0, full model is not better"),
      h5("Alt: removed factors are different from 0, full model is better"),
      h5("p < 0.05 => choose full model"),
      h5("p > 0.05 => choose reduced model"),
      h5("The p-value is 0.47 then the variable should be removed, because doesn't have any relation with the temperature"),
      h3("Analyse of Simulation Plot"),
      h5("The both model are similar what show the variable Iws doesn't cause any effect in the prediction")
    ),
    mainPanel(
      htmlTemplate("www/lmComparison.html", brush = ns("brush21"))
    )
    )
}