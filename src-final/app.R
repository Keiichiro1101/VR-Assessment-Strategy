# Load required libraries
library(shinyjs)
library(shiny)
library(ggplot2)
library(corrplot)
library(reshape2)
library(dplyr)
library(plotly)
library(stringr)
library(readxl)

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
                  tabPanel("Total Completion Time per Piece", 
                           h1("Completion Time Analysis", style = "color: aa66cc;"), 
                           plotlyOutput("completionTimePlot")),
                  tabPanel("2D Distance Analysis by Piece", 
                           h1("Distance vs Completion Time", style = "color: aa66cc;"), 
                           plotlyOutput("distancePlot1")),
                  tabPanel("3D Distance Analysis by Piece", 
                           h1("Distance vs Completion Time", style = "color: aa66cc;"), 
                           plotlyOutput("distancePlot2")),
                  tabPanel("Video Engagement Analysis", 
                           h1("Average Percentage of Video Watched per Piece", style = "color: aa66cc;"), 
                           plotlyOutput("videoEngagementPlot")),
                  tabPanel("Player Positions Heatmap",
                           h1("Player Positions Heatmap and 3D Scatter Plot", style = "color: #aa66cc;"),
                           plotlyOutput("heatmapPlot")
                  ),
                  tabPanel("ARCS Model Based Evaluation",
                           htmlOutput("googleFormTab"))
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

  # Create a new output for the heatmap and scatter plot
  output$heatmapPlot <- renderPlotly({
    # 2D Heatmap using ggplot2
    heatmap <- ggplot(inFile(), aes(x = X, y = Y, fill = ..density..)) +
      geom_bin2d(bins = 30) +
      scale_fill_viridis_c() +
      labs(title = "2D Heatmap of Player Positions", x = "X Coordinate", y = "Y Coordinate")
    
    # # 3D Scatter Plot using plotly
    # scatter_plot <- plot_ly(inFile(), x = ~X, y = ~Y, z = ~Z, color = ~Timestamp, type = "scatter3d", mode = "markers") %>%
    #   layout(scene = list(aspectmode = "cube"))
    # 
    # # Return both plots in a list
    # list(heatmap, scatter_plot)
  })
  
  # Plot for Completion Time Analysis
  output$completionTimePlot <- renderPlotly({
    req(inFile())
    
    p <- ggplot(inFile(), aes(x = piece, y = completion_time)) +
      geom_bar(stat = "summary", fun = "sum", fill = "#aa66cc") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Piece", y = "Total Completion Time")
    
    ggplotly(p)
    
    
    
  })
  
  # Plot for Distance Analysis by Player
  output$distancePlot1 <- renderPlotly({
    req(inFile())
    p <- ggplot(inFile(), aes(x = distance, y = completion_time, color = piece)) +
      geom_point() +
      theme_minimal() +
      labs(x = "Distance", y = "Completion Time")
    
    ggplotly(p)
    
    # plot_ly(data = inFile(), type = "scatter3d", mode = "markers", 
    #        x = ~distance, y = ~completion_time, z = ~playerID, color = ~playerID,
    #        marker = list(size = 3), text = ~piece)
    
    
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
  
  
  # Render the Google Form in the "Google Form" tab
  output$googleFormTab <- renderUI({
    HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSeUbp57sYcssoH45croShmzwlVomcMjLJ-xJHMAlTpB4cBC5Q/viewform?embedded=true" width="100%" height="800" frameborder="0" marginheight="0" marginwidth="0">読み込んでいます…</iframe>')
  })
}

# Launch the Shiny app
shinyApp(ui, server)