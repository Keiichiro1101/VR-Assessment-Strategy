# Load required libraries
library(shinyjs)
library(shiny)
library(ggplot2)
library(corrplot)
library(reshape2)
library(dplyr)
library(plotly)
library(stringr)

# Set Shiny option for maximum request size
options(shiny.maxRequestSize = 100 * 1024^2)

# Define the user interface (UI)
ui <- fluidPage(
  tags$head(
    tags$style(HTML('
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
      '))
  ),
  titlePanel(
    h1("GCIEL Assessment Strategy", style = "color: #aa66cc; font-size: 60px;", class = "animated-title")
  ),
  
  sidebarLayout(
    sidebarPanel(
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
      tabsetPanel(type = "tabs",
                  tabPanel("Data", 
                           h1("Data", style = "color: aa66cc;"),
                           dataTableOutput("outFile")),
                  tabPanel("Average Completion Time per Piece", 
                           h1("Completion Time Analysis", style = "color: aa66cc;"), 
                           plotlyOutput("completionTimePlot")),
                  tabPanel("Distance Analysis by Player", 
                           h1("Distance vs Completion Time", style = "color: aa66cc;"), 
                           plotlyOutput("distancePlot1")),
                  tabPanel("Distance Analysis by Piece", 
                           h1("Distance vs Completion Time", style = "color: aa66cc;"), 
                           plotlyOutput("distancePlot2")),
                  tabPanel("Video Engagement Analysis", 
                           h1("Average Percentage of Video Watched per Piece", style = "color: aa66cc;"), 
                           plotlyOutput("videoEngagementPlot"))
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
  output$completionTimePlot <- renderPlotly({
    req(inFile())
    p <- ggplot(inFile(), aes(x = piece, y = completion_time)) +
      geom_bar(stat = "summary", fun = "mean", fill = "#aa66cc") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Piece", y = "Average Completion Time")
    
    ggplotly(p)
  })
  
  # Plot for Distance Analysis by Player
  output$distancePlot1 <- renderPlotly({
    req(inFile())
    # p <- ggplot(inFile(), aes(x = distance, y = completion_time, color = piece)) +
    #   geom_point() +
    #   theme_minimal() +
    #   labs(x = "Distance", y = "Completion Time")
    # 
    # ggplotly(p)
    
    plot_ly(data = inFile(), type = "scatter3d", mode = "markers", 
           x = ~distance, y = ~completion_time, z = ~playerID, color = ~playerID,
           marker = list(size = 3), text = ~piece)
      
    
  })
  
  # Plot for Distance Analysis by Piece
  output$distancePlot2 <- renderPlotly({
    req(inFile())
    
    plot_ly(data = inFile(), type = "scatter3d", mode = "markers", 
            x = ~distance, y = ~completion_time, z = ~piece, color = ~piece,
            marker = list(size = 3), text = ~playerID)
    
    
  })
  
  # Plot for Video Engagement Analysis
  output$videoEngagementPlot <- renderPlotly({
    req(inFile())
    p <- ggplot(inFile(), aes(x = piece, y = percentage_video)) +
      geom_bar(stat = "summary", fun = "mean", fill = "#aa66cc") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Piece", y = "Average Percentage Watched")
    
    ggplotly(p)
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

