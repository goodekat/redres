
#' Shiny App for 'redressing' linear mixed models
#'
#' @description
#' This is a shiny app for model diagnoistic of mixed model using \code{lmer} function from lme4 package.
#' This app includes residual plot computed from six types of residuals (conditional raw, Pearson, and
#' studentized, marginal raw, Pearson, and studentized, and generalized residuals) and QQ plot using
#' generalized residual. This app can also be used to do model selection through pairwise comparison
#' two models from different linear mixed models using lmer function.
#'
#' @param model Model fit using \code{lmer} function from lme4 package.
#'
#' @usage redres_app(model)
#'
#' @importFrom cowplot plot_grid
#' @import shiny
#' @export redres_app
#'
#'
#' @examples
#' \dontrun{
#' # fits a linear mixed effects model
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' redres_app(model = fm1)

#' # comparing two different linear mixed effects models
#' fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy)
#' cmbd <- c(fm1,fm2)
#' redres_app(model = cmbd)
#' }


# Function for running shiny app
redres_app <- function(model) {

  # # make sure model is a list
  # model = list(model)

  if (length(model) == 1){
    checkmate::expect_class(model, "lmerMod",
                            info = "The input model is not accepted by redres. Model must be fit using 'lmer'.")
  } else if (length(model) == 2){
    checkmate::expect_class(model[[1]], "lmerMod",
                            info = "The first input model is not accepted by redres. Model must be fit using 'lmer'.")
    checkmate::expect_class(model[[2]], "lmerMod",
                            info = "The second input model is not accepted by redres. Model must be fit using 'lmer'.")
  } else {
    stop("redres_app currently only accepts 1 or 2 models.")
  }


  ui <- ui_fun()
  server <- server_fun(model)

  redresApp <- shinyApp(ui, server)
  shiny::runApp(redresApp, display.mode = "normal")

}

# Shiny app
ui_fun <- function(){

  navbarPage(
    theme = "yeti",
    tags$title(""),
    div(tags$header(p("Diagnostic Plots under Linear Mixed-effects Model",
                      style="font-size:40px"),
                    p("group 6", style="font-size:30px")),
        align = "center", style="color:#ffffff; background-color: #4d728d"),

    tabPanel("Residual Plot",
             sidebarLayout(sidebarPanel(
               selectInput("residual_type", "Residual type",
                           choices = c("raw_cond", "raw_mar",
                                       "pearson_cond", "pearson_mar",
                                       "std_cond", "std_mar"), selected = "raw_cond")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Residual Plot", plotOutput("resid"))
                 ))
             )),

    tabPanel("Quantile Plot",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Random Effects Quantile Plot",
                            plotOutput("rand_eff_quantile")),
                   tabPanel("Residual Quantile Plot",
                            plotOutput("quantile"))
                 ))
             )
  )

}


server_fun <- function (model) {

  shiny::shinyServer( function(input, output) {

    output$resid <- renderPlot({
      if (length(model) == 1){
        plot_redres(model, input$residual_type)
      } else {
        m1_resid <- plot_redres(model[[1]], input$residual_type) + xlab("model_1")
        m2_resid <- plot_redres(model[[2]], input$residual_type) + xlab("model_2")
        plot_grid(m1_resid, m2_resid)
      }
    })

    output$rand_eff_quantile <- renderPlot({
      if (length(model) == 1){
        plot_ranef(model)
      } else {
        m1_rand_quant <- plot_ranef(model[[1]]) + labs(tag = "model_1")
        m2_rand_quant <- plot_ranef(model[[2]]) + labs(tag = "model_2")
        plot_grid(m1_rand_quant, m2_rand_quant, nrow = 2)
      }
    })

    output$quantile <- renderPlot({
      if (length(model) == 1){
        plot_resqq(model)
      } else {
        m1_qq <- plot_resqq(model[[1]]) + xlab("model_1")
        m2_qq <- plot_resqq(model[[2]]) + xlab("model_2")
        plot_grid(m1_qq, m2_qq)
      }
    })
  })
}


