library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate) # New: To extract the hour from starttime
library(plotly)    # New: To make the plot interactive

# 1. Load Data and Extract start_hour
# Note: Ensure the path matches your actual file structure
df <- read_csv("data/raw/201306-citibike-tripdata.csv") %>%
  # Extract the hour from the starttime column.
  # (If your column is named 'start time' with a space, use `start time` instead)
  mutate(start_hour = hour(ymd_hms(starttime)))

# 2. Define the UI
ui <- page_sidebar(
  title = "Citi Bike Insights",

  sidebar = sidebar(
    selectInput(
      inputId = "user_type",
      label = "Select User Type:",
      choices = c("All", unique(df$usertype)),
      selected = "All"
    )
  ),

  layout_columns(
    value_box(
      title = "Total Trips",
      value = textOutput("trip_count"),
      theme = "primary"
    ),
    card(
      card_header("Trips by Start Hour"),
      # Changed from plotOutput to plotlyOutput
      plotlyOutput("hour_histogram")
    )
  )
)

# 3. Define the Server logic
server <- function(input, output, session) {

  filtered_df <- reactive({
    req(input$user_type)

    if (input$user_type == "All") {
      return(df)
    } else {
      return(df %>% filter(usertype == input$user_type))
    }
  })

  output$trip_count <- renderText({
    format(nrow(filtered_df()), big.mark = ",") # Adds a nice comma to large numbers
  })

  # Upgraded visually pleasing plot
  output$hour_histogram <- renderPlotly({
    # Create a beautiful base ggplot
    p <- ggplot(filtered_df(), aes(x = start_hour)) +
      geom_histogram(
        binwidth = 1,
        fill = "#023047",  # A sleek dark blue matching your previous Python aesthetic
        color = "white",
        alpha = 0.9
      ) +
      scale_x_continuous(breaks = seq(0, 23, by = 2)) + # Show every 2 hours
      theme_minimal() +
      labs(
        x = "Hour of the Day (0-23)",
        y = "Number of Trips"
      ) +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      )

    # Wrap it in ggplotly to make it interactive!
    ggplotly(p, tooltip = c("x", "count")) %>%
      layout(hovermode = "x unified")
  })
}

shinyApp(ui = ui, server = server)
