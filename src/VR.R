# Load required libraries
library(shinyjs)
library(shiny)
library(ggplot2)
library(corrplot)
library(reshape2)
library(dplyr)

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
      # Instructional text for users
      HTML('<p style="color: #aa66cc;">Please ensure to review the data description before uploading a CSV file. Make sure that the dataset you upload matches the data description in order for the app to work correctly.</p>'),
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
                  tabPanel("Completion Time Analysis", plotOutput("completionTimePlot")),
                  tabPanel("Distance Analysis", plotOutput("distancePlot")),
                  tabPanel("Video Engagement Analysis", plotOutput("videoEngagementPlot"))
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

  # Plot for Completion Time Analysis
  output$completionTimePlot <- renderPlot({
    req(inFile())
    ggplot(inFile(), aes(x = piece, y = completion_time)) +
      geom_bar(stat = "summary", fun = "mean", fill = "#aa66cc") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Average Completion Time per Piece", x = "Piece", y = "Average Completion Time")
  })
  
  # Plot for Distance Analysis
  output$distancePlot <- renderPlot({
    req(inFile())
    ggplot(inFile(), aes(x = distance, y = completion_time, color = piece)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Distance vs Completion Time", x = "Distance", y = "Completion Time")
  })
  
  # Plot for Video Engagement Analysis
  output$videoEngagementPlot <- renderPlot({
    req(inFile())
    ggplot(inFile(), aes(x = piece, y = percentage_video)) +
      geom_bar(stat = "summary", fun = "mean", fill = "#aa66cc") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Average Percentage of Video Watched per Piece", x = "Piece", y = "Average Percentage Watched")
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
