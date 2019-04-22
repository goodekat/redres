#' App for 'redressing' linear mixed models
#'
#' @description
#' app app
#'
#' @importFrom shiny checkboxInput div fileInput h5 helpText mainPanel navbarPage plotOutput radioButtons selectInput shinyApp sidebarLayout sidebarPanel tabPanel tabsetPanel tags p uiOutput verbatimTextOutput
#' @export redres_app


# Function for running shiny app
redres_app <- function(model) {

  ui <- ui_fun()
  server <- server_fun(model)

  redresApp <- shinyApp(ui, server)
  shiny::runApp(redresApp, display.mode = "normal")

}

# Shiny app
ui_fun <- function(){

  navbarPage(
    theme = "yeti",
    tags$title(" "),
    div(tags$header(p("Diagnostic Plots under Linear Mixed-effects Model",
                      style="font-size:40px"),
                    p("group 6", style="font-size:30px")),
        align = "center", style="color:#ffffff; background-color: #4d728d"),

    tabPanel("Data Import",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file","Upload your CSV File", multiple = FALSE),
                 tags$hr(),
                 h5(helpText("Select the read.table parameters below")),
                 checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                 radioButtons(inputId = 'sep', label = 'Separator',
                              choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')),
               mainPanel(uiOutput("tb1")))),

    tabPanel("Model",
             sidebarLayout(sidebarPanel(
               uiOutput("res_select"),
               uiOutput("fixed_select"),
               uiOutput("random_select")),
               mainPanel(helpText("Your selected variables"),
                         verbatimTextOutput("other_val_show")))),

    tabPanel("Plots",
             sidebarLayout(sidebarPanel(
               selectInput("residual_type", "Residual Type",
                           choices = c("raw_cond", "raw_mar",
                                       "pearson_cond", "pearson_mar",
                                       "std_cond", "std_mar"), selected = "raw_cond")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Residual Plot", plotOutput("resid")),
                   tabPanel("Generalized Residual Quantile Plot",
                            plotOutput("quantile"))))
             ))
  )

}


server_fun <- function(model){

  shiny::shinyServer(function(input, output) {
    data <- reactive({
      if (is.null(input$file)) {
        return(NULL)
      }
      read.table(file = input$file$datapath, sep = input$sep,
                 header = input$header)
    })
    output$table <- renderTable({
      if(is.null(data())){return ()}
      data()
    })
    output$tb1 <- renderUI({
      tableOutput("table")
    })
    output$res_select <- renderUI({
      selectInput("res_select","Select Response",
                  choices = as.list(names(data())), multiple = FALSE)
    })
    output$fixed_select <- renderUI({
      selectInput("fixed_select","Select Fixed Effect",
                  choices = as.list(names(data())), multiple = FALSE)
    })
    output$random_select <- renderUI({
      selectInput("random_select","Select Random Effect",
                  choices = as.list(names(data())), multiple = FALSE)
    })

    output$other_val_show <- renderPrint({
      input$res_select
      input$fixed_select
      input$random_select
      f <- data()

      form <- sprintf("%s~%s", input$res_select,
                      paste0(
                        input$fixed_select, "+", "(",
                        input$fixed_select, "|",
                        input$random_select, ")"))
      print(form)

      model <- lmer(as.formula(form), data = f)
      print(summary(model))
    })

    model <- reactive({
      f <- data()
      form <- sprintf("%s~%s", input$res_select,
                      paste0(
                        input$fixed_select, "+", "(",
                        input$fixed_select, "|",
                        input$random_select, ")"))
      lmer(as.formula(form), data = f)
    })

    output$resid <- renderPlot({
      model <- model()
      plot_redres(model, input$residual_type)
    })

    output$quantile <- renderPlot({
      model <- model()
      plot_genres(model)
    })
  })

}

