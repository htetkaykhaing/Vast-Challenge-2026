pacman::p_load(shiny, tidyverse, lubridate, plotly, DT)

rounds_tbl <- readr::read_rds("data/rds/rounds.rds")
comms_tbl  <- readr::read_rds("data/rds/communications.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Embargo Breach Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
              "channel",
              "Select channel type:",
              choices = c("All", unique(comms_tbl$channel_type))
            )
        ),
            
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("volume_plot"),
           DTOutput("evidence_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    filtered_comms <- reactive({
      if (input$channel == "All") {
        comms_tbl
      } else {
        comms_tbl |> filter(channel_type == input$channel)
      }
    })
    
    output$volume_plot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        p <- filtered_comms() |>
          mutate(day = as.Date(round_hour)) |>
          count(day, channel_type) |>
          ggplot(aes(day, n, fill = channel_type)) +
          geom_col() +
          labs(
            title = "Message Volume Over Time",
            x = NULL,
            y = "Messages"
          ) +
          theme_minimal()
        
        ggplotly(p)
    })
    
    output$evidence_table <- renderDT({
      filtered_comms() |>
        select(timestamp, agent_label, channel, content) |>
        arrange(timestamp) |>
        datatable(options = list(pageLength = 8))
    })
}

# Run the application 
shinyApp(ui, server)
