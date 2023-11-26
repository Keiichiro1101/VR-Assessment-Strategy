# Load required libraries
library(shinyjs)
library(shiny)
library(ggplot2)
library(corrplot)

# Set Shiny option for maximum request size
options(shiny.maxRequestSize = 100 * 1024^2)

# Define the user interface (UI)
ui <- fluidPage(
  tags$head(
    tags$style(HTML('
            body {
        background-color: #f0f0f0; /* Light gray background */
        color: purple;
      }
      .navbar {
        background-color: purple;
      }
      .navbar-inverse .navbar-nav > li > a {
        color: #ffffff; /* White text color for navbar links */
      }
      .navbar-inverse .navbar-brand {
        color: purple;
      }
      .navbar-inverse .navbar-toggle {
        border-color: purple;
      }
      .navbar-inverse .navbar-toggle .icon-bar {
        background-color: #ffffff; /* White color for the toggle bars */
      }
      .navbar-inverse .navbar-text {
        color: #ffffff; /* White text color for navbar text */
      }
      .navbar-inverse .navbar-form .form-group {
        color: purple;
      }
      .nav-tabs > li > a {
        background-color: purple;
        color: #ffffff; /* White text color for tabs */
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        background-color: #ffffff; /* White background for active tab */
        color: purple;
      }
    '))
  ),
  titlePanel(
    h1("GCIEL Assessment Strategy", style = "color: purple;")
  ),

  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ffffff; color: purple;",
      fileInput("file",
                label = "Upload your dataset (CSV)",
                accept = c("text/csv"),
                multiple = FALSE,
                width = "80%"),
      # Add a link to download the PDF file
      downloadLink("dataDescriptionLink", "Download Data Description"),
      br(),
      downloadLink("mockDataDownloadLink", "Download Data (CSV)")
    ),

    mainPanel(
      style = "background-color: #ffffff; color: purple;",
      tabsetPanel(type = "tabs",
                  tabPanel("Data", dataTableOutput("outFile")),
                  tabPanel("Time vs. Distance Correlation for Each Piece", h1("Time vs. Distance Correlation for Each Piece", style = "color: purple;"), verbatimTextOutput("statistics")),
                  tabPanel("Average Time and Distance by Piece", h1("Average Time and Distance by Piece", style = "color: purple;"), plotOutput("histograms")),
                  tabPanel("Player Movement Heatmap", h1("Player Movement Heatmap", style = "color: purple;"), plotOutput("interestingPlot")),
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {

  # Reactive expression for uploaded file
  inFile <- reactive({
    tmp <- input$file
    if (is.null(tmp)) {
      return(NULL)
    } else {
      df <- read.csv(tmp$datapath, header = TRUE)
      return(df)
    }
  })

    # Render the uploaded file in a data table
    output$outFile <- renderDataTable({
        data.frame(inFile())
    })

    # Animate data table when all rows are selected
    observeEvent(input$outFile_rows_all, {
        shinyjs::animate("outFile", animation = "fadeIn")
    })

    # Time vs. Distance Correlation for Each Piece
    output$statistics <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
    }
    })


    # Average Time and Distance by Piece
    output$statistics <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
    }
    })

    # Player Movement Heatmap
    output$statistics <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
    }
    })


  # Define download link for data description PDF
  output$dataDescriptionLink <- downloadHandler(
    filename = function() {
      "dataDescription.pdf"
    },
    content = function(file) {
      file.copy("dataDescription.pdf", file)
    }
  )

  # Define download link for the dataset in CSV format
  output$dataDownloadLink <- downloadHandler(
    filename = function() {
    "data.csv"
    },
    content = function(file) {
      file.copy("mockData.csv", file)
    }
  )
}

# Launch the Shiny app
shinyApp(ui, server)
