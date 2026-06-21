pacman::p_load(shiny, tidyverse, lubridate, plotly, DT, scales, bslib)

# Data files must be inside shiny_app/data/
rounds_tbl <- readr::read_rds("data/rds/rounds.rds")
comms_tbl  <- readr::read_rds("data/rds/communications.rds")

# Simple safety cleaning
comms_tbl <- comms_tbl %>%
  mutate(
    timestamp = as_datetime(timestamp),
    round_hour = as_datetime(round_hour),
    day = as_date(round_hour),
    content = coalesce(content, ""),
    flag_any = coalesce(flag_any, FALSE),
    is_public = coalesce(is_public, FALSE),
    is_side_channel = coalesce(is_side_channel, FALSE),
    is_anonymous = coalesce(is_anonymous, FALSE)
  )

judge_warning_time <- ymd_hms("2046-06-05 15:08:00")
legal_go_time <- ymd_hms("2046-06-05 17:19:00")
embargo_time <- ymd_hms("2046-06-05 18:00:00")

ui <- page_navbar(
  title = "BreachLens",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#1f5f6b",
    secondary = "#1d3440",
    base_font = font_google("Asap")
  ),
  
  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        dateRangeInput(
          "date_range",
          "Select date range",
          start = min(comms_tbl$day, na.rm = TRUE),
          end = max(comms_tbl$day, na.rm = TRUE)
        )
      ),
      
      h3("VAST Challenge MC1: Embargo Breach Explorer"),
      p("This app explores how TenantThread's embargo breach developed through communication volume, channel shifts, public posts and weak control by Judge-Agent."),
      
      layout_columns(
        value_box(
          "Total messages",
          textOutput("total_msg"),
          showcase = bsicons::bs_icon("chat-dots")
        ),
        value_box(
          "Public posts",
          textOutput("public_msg"),
          showcase = bsicons::bs_icon("broadcast")
        ),
        value_box(
          "Sensitive messages",
          textOutput("sensitive_msg"),
          showcase = bsicons::bs_icon("exclamation-triangle")
        )
      ),
      
      plotlyOutput("overview_volume", height = "420px")
    )
  ),
  
  nav_panel(
    "Timeline",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "event_lines",
          "Show event markers",
          choices = c("Judge warning", "Legal GO", "6 PM clearance"),
          selected = c("Judge warning", "Legal GO", "6 PM clearance")
        )
      ),
      
      h3("Crisis Timeline"),
      p("This view shows communication volume over time and marks the key control and release moments."),
      
      plotlyOutput("timeline_plot", height = "450px")
    )
  ),
  
  nav_panel(
    "Channels",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "channel_group_select",
          "Select channel groups",
          choices = c("Main huddle", "Side / 1-to-1", "Public posts"),
          selected = c("Main huddle", "Side / 1-to-1", "Public posts")
        )
      ),
      
      h3("Channel Shift"),
      p("This view compares main communication, side-channel coordination and public posts."),
      
      plotlyOutput("channel_plot", height = "450px")
    )
  ),
  
  nav_panel(
    "Judge Warning",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "signal_filter",
          "Filter signal type",
          choices = c("All", "Deal-related", "Embargo-related", "Crisis-related", "No flagged signal")
        )
      ),
      
      h3("Public Posts After Judge's Warning"),
      p("This tab checks whether public communication continued after Judge-Agent's 3:08 PM warning and before the 6:00 PM clearance."),
      
      plotlyOutput("post_warning_plot", height = "380px"),
      DTOutput("post_warning_table")
    )
  ),
  
  nav_panel(
    "Leading Indicators",
    h3("Leading Indicators Before the Early Release"),
    p("This view summarises warning signs before Legal's 5:19 PM GO decision."),
    
    plotlyOutput("indicator_plot", height = "430px")
  ),
  
  nav_panel(
    "Evidence",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "agent_select",
          "Select agent",
          choices = c("All", sort(unique(comms_tbl$agent_label)))
        ),
        selectInput(
          "channel_select",
          "Select channel",
          choices = c("All", sort(unique(comms_tbl$channel)))
        ),
        checkboxInput("sensitive_only", "Sensitive messages only", FALSE)
      ),
      
      h3("Evidence Table"),
      p("Use this table to inspect the messages behind the visuals."),
      
      DTOutput("evidence_table")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_comms <- reactive({
    req(input$date_range)
    
    comms_tbl %>%
      filter(day >= input$date_range[1], day <= input$date_range[2])
  })
  
  output$total_msg <- renderText({
    comma(nrow(filtered_comms()))
  })
  
  output$public_msg <- renderText({
    comma(sum(filtered_comms()$is_public, na.rm = TRUE))
  })
  
  output$sensitive_msg <- renderText({
    comma(sum(filtered_comms()$flag_any, na.rm = TRUE))
  })
  
  output$overview_volume <- renderPlotly({
    p <- filtered_comms() %>%
      count(day, channel_type) %>%
      ggplot(aes(day, n, fill = channel_type)) +
      geom_col() +
      scale_fill_manual(values = c("Internal" = "#1d3440", "Public" = "#1f5f6b")) +
      labs(
        title = "Daily message volume by channel type",
        x = NULL,
        y = "Messages",
        fill = "Channel type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$timeline_plot <- renderPlotly({
    event_tbl <- tibble(
      time = c(judge_warning_time, legal_go_time, embargo_time),
      label = c("Judge warning", "Legal GO", "6 PM clearance")
    ) %>%
      filter(label %in% input$event_lines)
    
    p <- filtered_comms() %>%
      count(round_hour, channel_type) %>%
      ggplot(aes(round_hour, n, colour = channel_type)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      geom_vline(
        data = event_tbl,
        aes(xintercept = time),
        linetype = "dashed",
        colour = "#a7661c"
      ) +
      scale_colour_manual(values = c("Internal" = "#1d3440", "Public" = "#1f5f6b")) +
      labs(
        title = "Communication volume around the breach",
        x = NULL,
        y = "Messages per round",
        colour = "Channel type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$channel_plot <- renderPlotly({
    p <- filtered_comms() %>%
      mutate(
        channel_group = case_when(
          channel == "comms_huddle" ~ "Main huddle",
          channel %in% c("side_huddle", "one_on_one_chat") ~ "Side / 1-to-1",
          is_public ~ "Public posts",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(channel_group %in% input$channel_group_select) %>%
      count(day, channel_group) %>%
      ggplot(aes(day, n, colour = channel_group)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.5) +
      geom_vline(
        xintercept = as.numeric(ymd("2046-06-05")),
        linetype = "dashed",
        colour = "#a7661c"
      ) +
      scale_colour_manual(values = c(
        "Main huddle" = "#1d3440",
        "Side / 1-to-1" = "#1f5f6b",
        "Public posts" = "#a7661c"
      )) +
      labs(
        title = "Side-channel use before the breach",
        x = NULL,
        y = "Messages per day",
        colour = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  post_warning_data <- reactive({
    out <- comms_tbl %>%
      filter(
        timestamp >= judge_warning_time,
        timestamp < embargo_time,
        is_public
      ) %>%
      mutate(
        signal_type = case_when(
          flag_deal_terms ~ "Deal-related",
          flag_embargo_terms ~ "Embargo-related",
          flag_crisis_terms ~ "Crisis-related",
          TRUE ~ "No flagged signal"
        )
      )
    
    if (input$signal_filter != "All") {
      out <- out %>% filter(signal_type == input$signal_filter)
    }
    
    out
  })
  
  output$post_warning_plot <- renderPlotly({
    p <- post_warning_data() %>%
      count(timestamp, signal_type) %>%
      ggplot(aes(timestamp, n, colour = signal_type)) +
      geom_point(size = 3) +
      geom_vline(
        xintercept = as.numeric(c(judge_warning_time, embargo_time)),
        linetype = "dashed",
        colour = "#a7661c"
      ) +
      labs(
        title = "Public posts after Judge warning",
        x = NULL,
        y = "Number of posts",
        colour = "Signal type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$post_warning_table <- renderDT({
    post_warning_data() %>%
      select(timestamp, agent_label, channel, signal_type, content) %>%
      arrange(timestamp) %>%
      datatable(options = list(pageLength = 8), rownames = FALSE)
  })
  
  output$indicator_plot <- renderPlotly({
    p <- comms_tbl %>%
      filter(timestamp < legal_go_time) %>%
      mutate(
        `Sensitive language` = flag_any,
        `Side-channel use` = is_side_channel,
        `Public posts` = is_public,
        `Anonymous posts` = is_anonymous
      ) %>%
      summarise(
        `Sensitive language` = sum(`Sensitive language`, na.rm = TRUE),
        `Side-channel use` = sum(`Side-channel use`, na.rm = TRUE),
        `Public posts` = sum(`Public posts`, na.rm = TRUE),
        `Anonymous posts` = sum(`Anonymous posts`, na.rm = TRUE),
        .by = day
      ) %>%
      pivot_longer(-day, names_to = "indicator", values_to = "count") %>%
      ggplot(aes(day, count, fill = indicator)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Sensitive language" = "#1d3440",
        "Side-channel use" = "#1f5f6b",
        "Public posts" = "#a7661c",
        "Anonymous posts" = "#7c6a4b"
      )) +
      labs(
        title = "Warning signs before Legal's 5:19 PM GO",
        x = NULL,
        y = "Number of signals",
        fill = "Indicator"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  evidence_data <- reactive({
    out <- comms_tbl
    
    if (input$agent_select != "All") {
      out <- out %>% filter(agent_label == input$agent_select)
    }
    
    if (input$channel_select != "All") {
      out <- out %>% filter(channel == input$channel_select)
    }
    
    if (isTRUE(input$sensitive_only)) {
      out <- out %>% filter(flag_any)
    }
    
    out
  })
  
  output$evidence_table <- renderDT({
    evidence_data() %>%
      select(timestamp, agent_label, channel, channel_type, content) %>%
      arrange(timestamp) %>%
      datatable(
        options = list(pageLength = 12, scrollX = TRUE),
        rownames = FALSE
      )
  })
}

shinyApp(ui, server)