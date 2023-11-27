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
          background-color: #000000; /* Black background */
          color: #aa66cc;
        }
        .navbar {
          background-color: #aa66cc;
        }
        .nav-tabs > li > a {
          background-color: #aa66cc;
          color: #ffffff; /* White text color for tabs */
        }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
          background-color: #ffffff; /* White background for active tab */
          color: #aa66cc;
        }
        .animated-title {
          animation: fadeIn 1s; /* Use fadeIn animation for 1 second */
        }
        @keyframes fadeIn {
          from {
            opacity: 0;
          }
          to {
            opacity: 1;
          }
        }
        .download-link {
          color: #aa66cc !important; /* Make the download links purple */
          text-decoration: underline; /* Add underline to the download links */
        }
      '))
  ),
  titlePanel(
    h1("GCIEL Assessment Strategy", style = "color: #aa66cc; font-size: 60px;", class = "animated-title")
  ),

  sidebarLayout(
    sidebarPanel(
      style = "background-color: #000000; color: #aa66cc;",  # Set background color to black and text color to purple
      fileInput("file",
                label = "Upload your dataset (CSV)",
                accept = c("text/csv"),
                multiple = FALSE,
                width = "80%"),
      # Add a link to download the PDF file
      downloadLink("dataDescriptionLink", "Download Data Description", class = "download-link"),
      br(),
      downloadLink("vikingshipDataLink", "Download Data (CSV)", class = "download-link"),
    ),

    mainPanel(
      style = "background-color: #000000; color: #aa66cc;",
      tabsetPanel(type = "tabs",
                  tabPanel("Data", dataTableOutput("outFile")),
                  tabPanel("Time vs. Distance Correlation for Each Piece", h1("Time vs. Distance Correlation for Each Piece", style = "color: #aa66cc;"), verbatimTextOutput("statistics")),
                  tabPanel("Average Time and Distance by Piece", h1("Average Time and Distance by Piece", style = "color: #aa66cc;"), plotOutput("histograms")),
                  tabPanel("Player Movement Heatmap", h1("Player Movement Heatmap", style = "color: #aa66cc;"), plotOutput("interestingPlot")),
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {

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

  # Time vs. Distance Correlation for Each Piece
  output$statistics <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      # Add your plot logic here
    }
  })

  # Average Time and Distance by Piece
  output$statistics <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      # Add your plot logic here
    }
  })

  # Player Movement Heatmap
  output$statistics <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      # Add your plot logic here
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
  output$vikingshipDataLink <- downloadHandler(
    filename = function() {
      "vikingshipData.csv"
    },
    content = function(file) {
      file.copy("vikingshipData.csv", file)
    }
  )
}

# Launch the Shiny app
shinyApp(ui, server)
