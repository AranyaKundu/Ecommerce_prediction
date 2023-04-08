source("visuals.R", local = T)
source("ml_models.R", local = T)

library(shiny)
library(shinycssloaders)
library(bslib)
library(DT)
library(thematic)
library(ragg) # In order for auto/custom fonts to work properly

options(shiny.useragg = TRUE) # to handle the font rendering in a Shiny app

# Call thematic_shiny() prior to launching the app, to change R plot themes 
# defaults for all plots
thematic_shiny(font = "auto")

page_theme = bslib::bs_theme(
  bg = "aliceblue", fg = "#2F1C6A", primary = "#00ABE4",
  base_font = bslib::font_google("Roboto")) # import CSS fonts from bslib
ui <- shinyUI(
  navbarPage(
    theme = page_theme,
    title = "Sales Data Prediction",
    # Home tab: Only has a hero image
    tabPanel(title = "Home", value = "home", icon = icon("fas fa-house", verify_fa = F),
      div(style = "background-size: cover; height: 89vh; display: flex; 
        align-items: center; justify-content: center;",
        tags$img(src = "superstore.jpg", 
           style = "height: 100%; width: 100%; max-width: 100%")
      )
    ),
    tabPanel("Data Distribution", icon = icon("fa-solid fa-table", verify_fa = F),
       fluidRow(
         column(3, selectInput("dataCol", label = "Select Column", choices = x_axes_options, multiple = F)),
         column(2, HTML("<br>"), actionButton("showdist", label = "Display"))
       ), 
       fluidRow(column(3),
                column(6, DTOutput("showdata") %>% withSpinner(type = 1)),
                column(3)
       )
    ),
    # Data Visualization Tab
    nav_menu("Data Visualization", value = "dataviz", icon = icon("database", verify_fa = F),
      tabPanel("Basic", 
        fluidRow(
          column(3, selectInput("x_axis", label = "Choose X-axis", 
                    choices = x_axes_options, multiple = F)),
          column(3, selectInput("y_axis", label = "Choose Y-axis", 
                    choices = y_axes_options, multiple = F)),
          column(2, HTML("<br>"), actionButton("display", label = "Display"))
          ),
        fluidRow(column(12, plotOutput("showplot") %>% withSpinner(type = 4))
        )
      ),
      tabPanel("Advanced", 
        fluidRow(
          column(3, selectInput("axis_x", label = "Choose X-axis", choices = x_axes_options, multiple = F)),
          column(3, selectInput("axis_y", label = "Choose Y-axis", choices = y_axes_options, multiple = F)),
          column(3, selectInput("facet_wrap", label = "Choose Facet Wrap", choices = x_axes_options, 
                                multiple = F)),
          column(2, HTML("<br>"), actionButton("displot", label = "Display"))
          ),
        fluidRow(column(12, plotOutput("showViz") %>% withSpinner(type = 5))
        )
      )
    ),
    nav_menu("Time Series Analysis", icon = icon("fa-solid fa-chart-line", verify_fa = F),
      tabPanel("Time Series Visuals", 
        fluidRow(
          column(3, selectInput("selectseries", label = "Select Total Series", 
                        choices = c("Total Sales", "Total Profit"), multiple = F)),
          column(3, selectInput("mavg", label = "Select Moving Average Flag", 
                        choices = c("Yes", "No"), multiple = F)),
          column(2, HTML("<br>"), actionButton("showtime", label = "Display"))
          ),
        fluidRow(column(12, plotOutput("showtimeplot") %>% withSpinner(type = 1))
        )
      ),
      tabPanel("Time Series Models",
        fluidRow(
          column(3, selectInput("selecttimeseries", label = "Select Total Series", 
                           choices = c("Total Sales", "Total Profit"), multiple = F)),
          column(3, selectInput("valid_duration", label = "Select Validation Period", 
                          choices = c(4:24), multiple = F)),
          column(2, HTML("<br>"), actionButton("compute", label = "Compute"))
        ),
        fluidRow(column(12, DTOutput("showresults") %>% withSpinner(type = 5))
        )
      )
    ),
    tabPanel("Machine Learning Models", icon = icon("fa-solid fa-gears", verify_fa = F),
      fluidRow(
        column(3, selectInput("selectmodel", label = "Select ML Model", 
                 choices = c("Linear Regression", "General Additive Model"))),
        column(2, HTML("<br>"), actionButton("computeresult", label = "Compute"))
      ),
      fluidRow(column(2),
        column(8, withSpinner(DTOutput("showtable"))),
        column(2)
      )
    ), 
    tabPanel("Predict Product", icon = icon("fa-solid fa-gear", verify_fa = F),
      fluidRow(
        column(3, selectInput("choosemodel", label = "Select Classification Model", 
          choices = c("Decision Tree Classifier", "Bootstrap Aggregation", "Xtreme Gradient Boost"), 
          multiple = F)),
        column(3, selectInput("choosesubcat", label = "Select Product Sub Category", 
                              choices = subcat, multiple = F)),
        column(2, HTML("<br>"), actionButton("fitmodel", label = "Predict"))
      ),
      fluidRow(column(2), 
        column(8, withSpinner(DTOutput("dispTable"))), 
        column(2)
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  
  # For the Data Visualization tab: Basic
  viz_plot <- eventReactive(input$display, {
    plot_fn(x_axis = input$x_axis, y_axis = input$y_axis)
  })
  output$showplot <- renderPlot(viz_plot(), width = "auto", height = 500)
  
  # For the Data Visualization Tab: Advanced
  advanced_viz <- eventReactive(input$displot, {
    plot_facet_fn(x_axis = input$axis_x, y_axis = input$axis_y, 
                  facet_wrap = input$facet_wrap)
  })
  output$showViz <- renderPlot(advanced_viz(), width = "auto", height = 500)
  
  # For the Data Visualization Tab:Distribution
  data_Dist <- eventReactive(input$showdist, {
    data_dist_table(select_data = input$dataCol)
  })
  output$showdata <- renderDT(data_Dist())
  
  # For the Time Series Analysis Tab: Time Series Visuals
  time_plot <- eventReactive(input$showtime, {
    time_series_viz(selectx = input$selectseries, mov_avg = input$mavg)
  })
  
  output$showtimeplot <- renderPlot(time_plot(), width = "auto", height = 500)
  
  # Code for the Time Series Tab
  time_pred <- eventReactive(input$compute, {
    ts_predict(selectcol = input$selecttimeseries, validation = input$valid_duration)
  })
  
  output$showresults <- renderDT(time_pred())
  
  # For the ML models Tab
  model_output <- eventReactive(input$computeresult, {
    fit_ml_models(model = input$selectmodel)
  })
  
  output$showtable <- renderDT(model_output())
  
  # For the Classification Models: Predict Tab
  predict_output <- eventReactive(input$fitmodel, {
    ml_models_classification(model = input$choosemodel, prd_subcat = input$choosesubcat)
  })
  
  output$dispTable <- renderDT(predict_output())
})


shinyApp(ui, server)