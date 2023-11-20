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
      downloadLink("dataDownloadLink", "Download Data (CSV)")
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

  # Render summary statistics
  output$statistics <- renderPrint({
    data <- inFile()
    if (!is.null(data)) {
      summary(data)
    }
  })

  # Render the histogram of shares vs. day published
  output$histograms <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      day_columns <- c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
                       "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday")

      day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      shares_by_day <- sapply(data[, day_columns], function(col) {
        sum(data$shares * col)
      })
      mean_shares_by_day <- sapply(data[, day_columns], function(col) {
        mean(data$shares * col)
      })

      day_shares <- data.frame(Day = day_names, Mean_Shares = mean_shares_by_day)

      ggplot(day_shares, aes(x = Day, y = Mean_Shares, fill = Day)) +
        geom_bar(stat = "identity") +
        labs(title = "Histogram of Mean Shares per Day of the Week", x = "Day of the Week", y = "Mean Shares") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

  # Render the categorical heatmap of shares vs. topic
  output$categoricalHeatmap <- renderPlot({
    data <- inFile()
    if (!is.null(data)) {
      subset_data <- data[, c(
        "data_channel_is_lifestyle",
        "data_channel_is_entertainment",
        "data_channel_is_bus",
        "data_channel_is_socmed",
        "data_channel_is_tech",
        "data_channel_is_world",
        "shares" # Target variable
      )]

      correlation_matrix <- cor(subset_data, method = "pearson")

      colnames(correlation_matrix) <- c(
        "Lifestyle",
        "Entertainment",
        "Business",
        "Social Media",
        "Tech",
        "World",
        "Shares" # Target variable
      )

      rownames(correlation_matrix) <- colnames(correlation_matrix)

      par(mar = c(1, 1, 1, 1))
      corrplot(
        correlation_matrix,
        method = "color",
        type = "upper",
        tl.col = "black",
        tl.cex = 0.7,
        tl.srt = 45,
        addrect = 6,
        is.corr = FALSE # Show legend
      )
    }
  })


  # Define download link for data description PDF
  output$dataDescriptionLink <- downloadHandler(
    filename = function() {
      "dataDescription.pdf"
    },
    content = function(file) {
      file.copy("../finalSubmission/dataDescription.pdf", file)
    }
  )

  # Define download link for the dataset in CSV format
  output$dataDownloadLink <- downloadHandler(
    filename = function() {
    "data.csv"
    },
    content = function(file) {
      file.copy("../finalSubmission/OnlineNewsPopularity.csv", file)
    }
  )
}

# Launch the Shiny app
shinyApp(ui, server)
