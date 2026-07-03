pacman::p_load(shiny, tidyverse, lubridate, plotly, DT, scales, bslib, bsicons,
               visNetwork, igraph, ggrepel)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Data -------------------------------------------------------------------
rounds_tbl       <- readr::read_rds("data/rds/rounds.rds")
comms_tbl        <- readr::read_rds("data/rds/communications.rds")
participants_tbl <- readr::read_rds("data/rds/participants.rds")
edges_tbl        <- readr::read_rds("data/rds/network_edges.rds")

comms_tbl <- comms_tbl %>%
  mutate(timestamp = as_datetime(timestamp), round_hour = as_datetime(round_hour),
         day = as_date(round_hour), content = coalesce(content, ""),
         across(c(flag_any, flag_deal_terms, flag_embargo_terms, flag_crisis_terms,
                  is_public, is_side_channel, is_anonymous), ~ coalesce(.x, FALSE)))

norm_node <- function(x) str_remove(str_to_lower(x), "_agent$")
edges_tbl <- edges_tbl %>% mutate(sender = norm_node(sender), recipient = norm_node(recipient))

judge_warning_time <- ymd_hms("2046-06-05 15:08:00")
legal_go_time      <- ymd_hms("2046-06-05 17:19:00")
embargo_time       <- ymd_hms("2046-06-05 18:00:00")
stage_lines        <- c(6.5, 13.5)

classify_signal <- function(df) df %>% mutate(signal_type = case_when(
  flag_deal_terms ~ "Deal-related", flag_embargo_terms ~ "Embargo-related",
  flag_crisis_terms ~ "Crisis-related", TRUE ~ "No flagged signal"))

day_min <- min(comms_tbl$day, na.rm = TRUE); day_max <- max(comms_tbl$day, na.rm = TRUE)
round_max <- max(comms_tbl$round_index, na.rm = TRUE)
stage_lvls <- levels(comms_tbl$crisis_stage) %||% sort(unique(as.character(comms_tbl$crisis_stage)))
agents_all <- sort(unique(comms_tbl$agent_label)); channels_all <- sort(unique(comms_tbl$channel))
signal_filter_choices <- c("All", comms_tbl %>%
  filter(timestamp >= judge_warning_time, timestamp < embargo_time, is_public) %>%
  classify_signal() %>% distinct(signal_type) %>% arrange(signal_type) %>% pull(signal_type))
FLAG_CHOICES <- c("Deal terms" = "flag_deal_terms", "Embargo terms" = "flag_embargo_terms",
  "Crisis terms" = "flag_crisis_terms", "Public" = "is_public", "Anonymous" = "is_anonymous",
  "Side channel" = "is_side_channel")

# ---- Brand (calm: one accent, flat) -----------------------------------------
brand <- list(ink = "#1b2026", graph = "#3a434c", teal = "#2f6e7b", ochre = "#b97f2e",
              brown = "#8a7a5c", signal = "#d8412a", slate = "#6b7681", grid = "#eceef0",
              line = "#e4e7ea", paper = "#f6f7f8", panel = "#ffffff",
              # highlight = the one bright accent reserved for the focus series (Public);
              # context = muted grey for the supporting series (Internal) so the focus pops.
              highlight = "#f5b301", highlight_ink = "#8a6100", context = "#c2c8ce")

theme_breachlens <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = brand$graph), plot.title = element_blank(),
          axis.title = element_text(colour = brand$slate, size = rel(0.85)),
          axis.text = element_text(colour = brand$graph), panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = brand$grid, linewidth = 0.5),
          legend.position = "bottom", legend.title = element_blank(), legend.key = element_blank(),
          strip.text = element_text(face = "bold", colour = brand$ink), plot.margin = margin(6, 12, 2, 6))
}
to_plotly <- function(p, source = NULL) ggplotly(p, tooltip = "text", source = source) %>%
  layout(font = list(family = "Spline Sans, sans-serif", color = brand$graph),
         legend = list(orientation = "h", y = -0.2, x = 0, title = list(text = "")),
         margin = list(t = 14, r = 12, b = 34, l = 54),
         hoverlabel = list(font = list(family = "IBM Plex Mono, monospace", size = 12, color = "#fff"),
                           bgcolor = brand$ink, bordercolor = brand$ink)) %>%
  config(displaylogo = FALSE, displayModeBar = "hover",
         modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d","toggleSpikelines",
                                    "hoverClosestCartesian","hoverCompareCartesian"))
fmt_dt <- function(x) format(x, "%b %d, %H:%M")
badge  <- function(x, kind) sprintf('<span class="bl-badge bl-badge--%s">%s</span>', kind, toupper(x))

# ---- CSS: whitespace-first, flat, scarce colour -----------------------------
app_css <- HTML("
  :root{ --bl-ink:#1b2026; --bl-signal:#d8412a; --bl-teal:#2f6e7b; --bl-ochre:#b97f2e;
         --bl-highlight:#f5b301; --bl-highlight-ink:#8a6100;
         --bl-line:#e4e7ea; --bl-paper:#f6f7f8; --bl-graph:#3a434c; --bl-mute:#6b7681;
         --bl-mono:'IBM Plex Mono',monospace; --bl-display:'Space Grotesk',sans-serif; --r:8px; }
  body{ background:var(--bl-paper); }
  body, .dataTable, .kpi-value, .form-control, .tab-meta { font-variant-numeric:tabular-nums; }

  .navbar{ background:#fff !important; border-bottom:1px solid var(--bl-line); box-shadow:none; }
  .navbar-brand{ font-family:var(--bl-display); font-weight:700; color:var(--bl-ink) !important; letter-spacing:-.01em; }
  .navbar .nav-link{ color:var(--bl-graph) !important; font-weight:500; font-size:.9rem; border-radius:6px; padding:.4rem .8rem !important; }
  .navbar .nav-link.active{ color:var(--bl-ink) !important; background:#eef0f2; }
  .navbar-brand .bi{ color:var(--bl-signal); }

  /* slim, quiet masthead */
  .bl-masthead{ display:flex; align-items:center; gap:.7rem; padding:.45rem 1.4rem; background:#fff;
     border-bottom:1px solid var(--bl-line); font-family:var(--bl-mono); font-size:.68rem;
     letter-spacing:.08em; text-transform:uppercase; color:var(--bl-mute); }
  .bl-masthead .id{ color:var(--bl-ink); font-weight:600; }
  .bl-masthead .sep{ color:var(--bl-line); }
  .bl-dot{ display:inline-block; width:6px; height:6px; border-radius:50%; background:var(--bl-signal);
     box-shadow:0 0 0 3px rgba(216,65,42,.15); margin-right:.4rem; }
  .bl-masthead .spacer{ margin-left:auto; }

  /* breathing room: constrained, generously spaced page body */
  .bl-page{ max-width:1080px; margin:0 auto; padding:2rem 1.5rem 3.5rem; }
  .bl-page > h3{ font-family:var(--bl-display); font-weight:700; color:var(--bl-ink);
     letter-spacing:-.02em; font-size:1.5rem; margin:0 0 .35rem; }
  .bl-lede{ color:var(--bl-mute); max-width:64ch; font-size:1rem; line-height:1.6; margin:0 0 1.9rem; }
  .bl-section{ margin-top:2.6rem; }

  /* flat cards, generous padding, near-zero shadow */
  .card{ background:#fff; border:1px solid var(--bl-line); border-radius:var(--r); box-shadow:none; margin-bottom:1.5rem; }
  .card-header{ background:#fff; border-bottom:1px solid var(--bl-line); font-weight:600; color:var(--bl-ink);
     font-size:.95rem; padding:.85rem 1.1rem; }
  .card-body{ padding:1.1rem 1.2rem 1.3rem; }
  .card-head{ display:flex; justify-content:space-between; align-items:center; gap:1rem; width:100%; }
  .card-dl{ font-family:var(--bl-mono); font-size:.68rem; letter-spacing:.04em; text-transform:uppercase;
     color:var(--bl-mute); text-decoration:none; white-space:nowrap; }
  .card-dl:hover{ color:var(--bl-teal); }
  .sidebar-actions{ display:flex; flex-direction:column; gap:.45rem; margin-top:1.1rem; }
  .sidebar-actions .btn{ width:100%; font-size:.8rem; }

  /* calm KPI row: white, colour reserved for the value */
  .kpi-row{ display:grid; grid-template-columns:repeat(auto-fit,minmax(210px,1fr)); gap:1rem; margin-bottom:1.2rem; }
  .kpi{ background:#fff; border:1px solid var(--bl-line); border-radius:var(--r); padding:1.3rem 1.4rem; }
  .kpi-label{ display:flex; align-items:center; gap:.45rem; font-family:var(--bl-mono); font-size:.68rem;
     letter-spacing:.1em; text-transform:uppercase; color:var(--bl-mute); }
  .kpi-label .bi{ font-size:.85rem; }
  .kpi-value{ font-family:var(--bl-display); font-weight:700; font-size:2.3rem; line-height:1.05;
     letter-spacing:-.02em; color:var(--bl-ink); margin-top:.35rem; }
  .kpi-sub{ font-family:var(--bl-mono); font-size:.7rem; letter-spacing:.03em; color:var(--bl-mute); margin-top:.3rem; }
  .kpi--alert .kpi-value{ color:var(--bl-signal); }
  .kpi--alert .kpi-label .bi{ color:var(--bl-signal); }
  .kpi--teal .kpi-label .bi{ color:var(--bl-teal); }
  /* gold = the Public highlight; keeps the KPI's colour consistent with the bar chart */
  .kpi--gold .kpi-value{ color:var(--bl-highlight-ink); }
  .kpi--gold .kpi-label .bi{ color:var(--bl-highlight); }

  /* collapsible 'show/hide' pane for secondary tables — keeps core charts on one screen */
  .bl-collapse .accordion-button{ background:#fff; }
  .bl-collapse .accordion-item{ border:1px solid var(--bl-line); border-radius:var(--r); }
  .bl-collapse .card{ border:none; margin-bottom:0; }

  /* sidebar: quiet, grouped via accordion */
  .bslib-sidebar-layout > .sidebar, .bslib-page-sidebar > .sidebar{ background:#fff; border-right:1px solid var(--bl-line); }
  .sidebar .control-label, .sidebar legend{ font-weight:600; font-size:.78rem; color:var(--bl-ink); }
  .accordion-button{ font-family:var(--bl-mono); font-size:.72rem; letter-spacing:.06em; text-transform:uppercase;
     color:var(--bl-ink); padding:.6rem .25rem; }
  .accordion-button:not(.collapsed){ background:transparent; color:var(--bl-ink); box-shadow:none; }
  .accordion-item{ border:none; border-bottom:1px solid var(--bl-line); }
  .accordion-body{ padding:.4rem .1rem 1rem; }
  .preset-row{ display:flex; flex-wrap:wrap; gap:.4rem; margin-bottom:1rem; }
  .preset-row a{ font-family:var(--bl-mono); font-size:.68rem; letter-spacing:.03em; padding:.25rem .55rem;
     border:1px solid var(--bl-line); border-radius:6px; color:var(--bl-ink); background:#fff; text-decoration:none; cursor:pointer; }
  .preset-row a:hover{ border-color:var(--bl-signal); color:var(--bl-signal); }

  .chip{ display:inline-block; font-family:var(--bl-mono); font-size:.7rem; padding:.14rem .55rem; margin:.12rem;
     border:1px solid var(--bl-line); border-radius:999px; background:#fff; color:var(--bl-ink); }
  .chip-clear{ font-family:var(--bl-mono); font-size:.7rem; color:var(--bl-signal); margin-left:.35rem; cursor:pointer; }
  .tab-meta{ font-family:var(--bl-mono); font-size:.72rem; letter-spacing:.07em; text-transform:uppercase; color:var(--bl-mute); }

  /* plain-language takeaway, shown on every view so the point is never hidden */
  .finding{ display:flex; gap:.7rem; align-items:flex-start; background:#fff; border:1px solid var(--bl-line);
     border-radius:var(--r); padding:.9rem 1.1rem; margin:0 0 1.8rem; }
  .finding .tag{ font-family:var(--bl-mono); font-size:.6rem; letter-spacing:.1em; text-transform:uppercase;
     color:var(--bl-signal); border:1px solid rgba(216,65,42,.3); background:#fdebe8; border-radius:999px;
     padding:.16rem .55rem; white-space:nowrap; margin-top:.12rem; }
  .finding p{ margin:0; color:var(--bl-ink); font-size:.95rem; line-height:1.55; }
  .finding p b{ color:var(--bl-ink); }

  /* tables: airy log readout */
  table.dataTable thead th{ font-family:var(--bl-mono); font-size:.7rem; letter-spacing:.05em; text-transform:uppercase;
     color:var(--bl-mute) !important; border-bottom:1px solid var(--bl-line) !important; }
  table.dataTable tbody td{ padding:.5rem .7rem; border-bottom:1px solid #f0f2f3; font-size:.85rem; vertical-align:top; }
  table.dataTable tbody tr:hover td{ background:#fafbfb; }
  table.dataTable tbody tr td:first-child{ font-family:var(--bl-mono); color:var(--bl-mute); white-space:nowrap; }
  .bl-badge{ font-family:var(--bl-mono); font-size:.62rem; font-weight:600; letter-spacing:.05em; padding:.1rem .4rem;
     border-radius:999px; border:1px solid var(--bl-line); color:var(--bl-mute); }
  .bl-badge--signal{ color:var(--bl-signal); border-color:rgba(216,65,42,.35); background:#fdebe8; }
  .bl-badge--teal{ color:var(--bl-teal); border-color:rgba(47,110,123,.3); background:#e7f0f2; }
  .bl-badge--ink{ color:var(--bl-ink); }

  .shiny-output-error-validation{ display:flex; align-items:center; justify-content:center; min-height:200px;
     font-family:var(--bl-mono); font-size:.78rem; letter-spacing:.06em; color:var(--bl-mute); text-transform:uppercase; }
  .shiny-output-error-validation::before{ content:'NO MESSAGES IN SCOPE'; }
  .shiny-bound-output.recalculating{ opacity:.4; transition:opacity .2s ease; }

  .form-control,.selectize-input,.btn{ border-radius:6px !important; }
  .form-control:focus,.selectize-input.focus{ border-color:var(--bl-teal) !important; box-shadow:0 0 0 3px rgba(47,110,123,.15) !important; }
  .btn{ transition:transform .12s cubic-bezier(.32,.72,0,1); }
  .btn:active{ transform:scale(.97); }
  a:focus-visible,.btn:focus-visible,.nav-link:focus-visible{ outline:2px solid var(--bl-signal); outline-offset:2px; }
  @media (prefers-reduced-motion: reduce){ * { transition:none !important; animation:none !important; } }
")

# ---- KPI + page helpers -----------------------------------------------------
kpi <- function(label, icon, valId, subId, mod = "") div(class = paste("kpi", mod),
  div(class = "kpi-label", bsicons::bs_icon(icon), label),
  div(class = "kpi-value", textOutput(valId, inline = TRUE)),
  div(class = "kpi-sub", uiOutput(subId, inline = TRUE)))

panel_card <- function(title, ..., dl = NULL, note = NULL) card(
  card_header(div(class = "card-head", span(title),
    if (!is.null(dl)) downloadLink(dl, tagList(bsicons::bs_icon("download"), " CSV"), class = "card-dl"))),
  card_body(if (!is.null(note)) div(class = "tab-meta", style = "margin-bottom:.7rem;", note), ...))

page <- function(title, lede, ...) div(class = "bl-page", h3(title), p(class = "bl-lede", lede), ...)
finding <- function(...) div(class = "finding", span(class = "tag", "Finding"), p(...))

# ---- Sidebar (progressive disclosure) ---------------------------------------
global_sidebar <- sidebar(id = "gfilters", title = "Filters", width = 300, open = "desktop",
  div(class = "preset-row",
      actionLink("preset_breach", "Breach"), actionLink("preset_side", "Side-ch"),
      actionLink("preset_anon", "Anon"), actionLink("preset_warned", "Post-warning"),
      actionLink("preset_jun5", "5 June")),
  textInput("g_search", NULL, placeholder = "Search message text…"),
  accordion(open = "Scope",
    accordion_panel("Scope",
      dateRangeInput("g_date_range", "Date range", start = day_min, end = day_max, min = day_min, max = day_max),
      sliderInput("g_round", "Round", min = 1, max = round_max, value = c(1, round_max), step = 1, sep = "")),
    accordion_panel("Who & where",
      selectizeInput("g_agent", "Agents", choices = agents_all, multiple = TRUE, options = list(placeholder = "All agents")),
      selectizeInput("g_channel", "Channels", choices = channels_all, multiple = TRUE, options = list(placeholder = "All channels")),
      checkboxGroupInput("g_channel_type", "Visibility", choices = c("Internal", "Public"), selected = c("Internal", "Public"), inline = TRUE),
      selectizeInput("g_stage", "Crisis stage", choices = stage_lvls, multiple = TRUE, options = list(placeholder = "All stages"))),
    accordion_panel("Signals",
      checkboxGroupInput("g_flags", NULL, choices = FLAG_CHOICES),
      radioButtons("g_flag_logic", NULL, choices = c("match any" = "any", "match all" = "all"), inline = TRUE, selected = "any"))),
  div(class = "sidebar-actions",
      actionButton("reset_all", "Reset filters", class = "btn-sm btn-outline-secondary"),
      bookmarkButton("bookmark", label = "Copy link to this view", icon = NULL)),
  uiOutput("global_count"))

masthead <- div(class = "bl-masthead",
  span(class = "id", "Case · TenantThread embargo breach"), span(class = "sep", "·"),
  span(span(class = "bl-dot"), "Archive loaded"), span(class = "sep", "·"),
  span(comma(nrow(comms_tbl)), " records"), span(class = "sep", "·"),
  span(format(day_min, "%b %d"), " – ", format(day_max, "%b %d 2046")),
  span(class = "spacer"), span("VAST 2026 · MC1"))

# ---- UI ---------------------------------------------------------------------
ui <- function(request) page_navbar(
  title = tagList(bsicons::bs_icon("crosshair"), " BreachLens"), id = "main_nav",
  fillable = FALSE, sidebar = global_sidebar,
  theme = bs_theme(version = 5, bg = brand$paper, fg = brand$graph, primary = brand$teal,
    secondary = brand$ink, danger = brand$signal, base_font = font_google("Spline Sans"),
    heading_font = font_google("Spline Sans"), code_font = font_google("IBM Plex Mono")),
  header = tagList(tags$head(tags$style(app_css),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@600;700&display=swap")),
    masthead, div(class = "bl-page", style = "padding-top:1rem; padding-bottom:0;", uiOutput("filter_chips"))),

  nav_panel("Overview", icon = bsicons::bs_icon("speedometer2"),
    page("Embargo breach overview",
      "A two-week record of every message between the agents, leading up to the 6:00 PM embargo on 5 June 2046.",
      finding(tags$b("Communication was steady for two weeks, then jumped roughly tenfold on 5 June"),
              " — the day the embargo broke. Public posts and sensitive messages cluster on that day."),
      div(class = "kpi-row",
        kpi("Messages in scope", "chat-dots", "k_total", "k_total_sub"),
        kpi("Public posts", "broadcast", "k_public", "k_public_sub", "kpi--gold"),
        kpi("Sensitive", "exclamation-triangle", "k_sensitive", "k_sensitive_sub", "kpi--alert")),
      panel_card("Daily message volume", note = "Click a bar to open that day in Evidence",
                 plotlyOutput("overview_volume", height = "340px"), dl = "dl_overview"))),

  nav_panel("Timeline", icon = bsicons::bs_icon("clock-history"),
    page("Crisis timeline",
      "When the messages happened, and the falling company stock price that pressured the agents.",
      finding(tags$b("Internal coordination spiked in the final hours of 5 June"),
              " — around the 3:08 PM compliance warning, the 5:19 PM go-decision and the 6:00 PM deadline. The stock had been sliding for days before that."),
      panel_card("Messages over time", note = "Opens zoomed on 5 June — the breach day — where the shaded band and the three dashed key moments sit (3:08 PM warning · 5:19 PM go-decision · 6:00 PM deadline). Double-click to pull back to the full two weeks.",
                 plotlyOutput("timeline_plot", height = "360px"), dl = "dl_timeline"),
      div(class = "bl-section",
        panel_card("Company stock price",
                   note = "The $TTHR price fell over the two weeks. Dashed lines mark public events that added pressure.",
                   plotlyOutput("stock_plot", height = "300px"))),
      div(class = "bl-section",
        finding("This represents the key events leading to the breach. ",
                tags$b("Judge warnings were limited and late"),
                ", information appeared publicly before the embargo expired, and the pattern is a ",
                tags$b("progressive control failure, not a single incident"), ".")),
      div(class = "bl-section",
        panel_card("Progressive erosion of the embargo — 10 concrete events",
                   note = "Each point is sourced from a specific message_id or environment_context field.",
                   plotOutput("erosion_plot", height = "520px"))),
      div(class = "bl-section",
        accordion(class = "bl-collapse", open = FALSE,
          accordion_panel(title = tagList(bsicons::bs_icon("table"), " Key decision points — show / hide table"),
            value = "decisions_table",
            panel_card("Key decision points",
                       note = "Eight decisions directly evidenced by a message or environment_context field.",
                       DTOutput("decision_table"))))))),

  nav_panel("Network", icon = bsicons::bs_icon("share"),
    layout_sidebar(sidebar = sidebar(title = "View", width = 240,
        selectInput("net_channel", "Channel", choices = c("All", channels_all)),
        checkboxGroupInput("net_stage", "Crisis stage", choices = stage_lvls, selected = stage_lvls)),
      page("Who talked to whom",
        "Each dot is an agent. A line means they messaged each other; thicker lines = more messages; bigger dots = more contact overall. Drag dots to explore.",
        finding(tags$b("The Legal agent was the busiest contact point"),
                ", and sensitive coordination ran through private side-channels rather than the open team huddle. Switch the channel filter to 'side_huddle' to see it."),
        panel_card("Communication network", visNetworkOutput("network_graph", height = "600px"))))),

  nav_panel("Escalation", icon = bsicons::bs_icon("graph-up-arrow"),
    page("How abnormal was each moment?",
      "The scenario runs in 23 numbered turns ('rounds'). This view scores how unusual the agents' behaviour was in each round, compared with how they normally operate.",
      finding("The higher the line, the less normal the behaviour. ",
              tags$b("It drifts up gradually and only crosses the alarm line in the last few rounds"),
              " — so the breach was a slow build-up, not a sudden event. The heatmap shows which agents drove it."),
      panel_card("How unusual was each round?",
                 note = "Each dot is one round. Inside the grey band = a normal day. Above the red alarm line = clearly abnormal (red dots).",
                 plotlyOutput("deviation_plot", height = "340px")),
      div(class = "bl-section",
        panel_card("Which agent was active when?", note = "Darker red = more messages from that agent in that round. The surge concentrates in the breach window (rounds 14+).",
                   plotlyOutput("agent_round_heatmap", height = "300px"))))),

  nav_panel("The Breach", icon = bsicons::bs_icon("shield-exclamation"),
    layout_sidebar(sidebar = sidebar(title = "Filter", width = 240,
        selectInput("signal_filter", "Signal type", choices = signal_filter_choices)),
      page("Did the warning stop the posts?",
        "An automated compliance tool, 'the Judge', was meant to prevent embargo breaches. This view checks whether public posting actually stopped after its 3:08 PM warning.",
        finding(tags$b("No — public posts kept appearing after the 3:08 PM warning, right up to the 6:00 PM deadline"),
                ", and they outpaced the warnings meant to stop them. Enforcement lagged behind the posting."),
        panel_card("Public posts kept coming after the warning", note = "Each step up is one more public post between the 3:08 PM warning and the 6:00 PM deadline. The line never flattens.",
                   plotlyOutput("post_warning_plot", height = "300px")),
        div(class = "bl-section",
          panel_card("Warnings vs posts, per round", note = "The red line (posts that went out) pulls away from the dark line (Judge warnings) after round 14. The gap is the enforcement shortfall.",
                     plotlyOutput("control_gap_plot", height = "300px"))),
        div(class = "bl-section",
          accordion(class = "bl-collapse", open = FALSE,
            accordion_panel(title = tagList(bsicons::bs_icon("table"), " Underlying messages — show / hide table"),
              value = "judge_table",
              panel_card("Messages in this window", DTOutput("post_warning_table"), dl = "dl_judge"))))))),

  nav_panel("Evidence", icon = bsicons::bs_icon("table"),
    page("The messages themselves",
      "The raw communication records behind every chart. Use the filter panel to trace a specific agent, channel, date or keyword; click a bar on Overview to jump straight to that day.",
      finding("This is the source evidence. ", tags$b("Anything you see in a chart can be traced back to the exact messages here"),
              " — search 'embargo' or 'deal' to read the sensitive ones."),
      uiOutput("xfilter_note"),
      panel_card("Communication records", DTOutput("evidence_table"), dl = "dl_evidence"))),

  nav_spacer())

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  search_term <- debounce(reactive(input$g_search), 350)
  gf <- reactive({
    req(input$g_date_range)
    out <- comms_tbl %>% filter(day >= input$g_date_range[1], day <= input$g_date_range[2],
                                round_index >= input$g_round[1], round_index <= input$g_round[2])
    if (length(input$g_agent))        out <- out %>% filter(agent_label %in% input$g_agent)
    if (length(input$g_channel))      out <- out %>% filter(channel %in% input$g_channel)
    if (length(input$g_channel_type)) out <- out %>% filter(channel_type %in% input$g_channel_type)
    if (length(input$g_stage))        out <- out %>% filter(as.character(crisis_stage) %in% input$g_stage)
    if (length(input$g_flags)) { m <- as.matrix(out[, input$g_flags]); m[is.na(m)] <- FALSE
      out <- out[if (input$g_flag_logic == "all") rowSums(m) == length(input$g_flags) else rowSums(m) > 0, ] }
    st <- search_term() %||% ""; if (nzchar(st)) out <- out %>% filter(str_detect(content, regex(st, ignore_case = TRUE)))
    out })

  reset_all <- function() {
    updateTextInput(session, "g_search", value = ""); updateDateRangeInput(session, "g_date_range", start = day_min, end = day_max)
    updateSliderInput(session, "g_round", value = c(1, round_max)); updateSelectizeInput(session, "g_agent", selected = character(0))
    updateSelectizeInput(session, "g_channel", selected = character(0)); updateCheckboxGroupInput(session, "g_channel_type", selected = c("Internal", "Public"))
    updateSelectizeInput(session, "g_stage", selected = character(0)); updateCheckboxGroupInput(session, "g_flags", selected = character(0))
    xfilter$day <- NULL }
  observeEvent(input$reset_all, reset_all()); observeEvent(input$reset_all2, reset_all())

  # Presets are mutually-exclusive "scenarios": each one clears every other filter first,
  # then applies only its own. This fixes the earlier additive bug where cycling
  # Breach -> Side-ch -> Breach left the side-channel filter stuck on and never returned a
  # clean Breach view. reset_all() also re-selects the value even if it was already set,
  # so re-clicking the same preset reliably re-applies it.
  observeEvent(input$preset_breach, { reset_all(); updateSelectizeInput(session, "g_stage", selected = "Breach window") })
  observeEvent(input$preset_side,   { reset_all(); updateSelectizeInput(session, "g_channel", selected = c("side_huddle", "one_on_one_chat")) })
  observeEvent(input$preset_anon,   { reset_all(); updateCheckboxGroupInput(session, "g_flags", selected = "is_anonymous") })
  observeEvent(input$preset_warned, { reset_all()
    updateDateRangeInput(session, "g_date_range", start = as_date(judge_warning_time), end = as_date(embargo_time))
    updateCheckboxGroupInput(session, "g_channel_type", selected = "Public") })
  observeEvent(input$preset_jun5,    { reset_all()
    updateDateRangeInput(session, "g_date_range", start = as_date(embargo_time), end = as_date(embargo_time)) })

  output$global_count <- renderUI({ n <- nrow(gf())
    div(class = "tab-meta", style = "margin-top:.9rem;",
        sprintf("%s of %s in scope", comma(n), comma(nrow(comms_tbl)))) })
  output$filter_chips <- renderUI({
    chips <- list(); add <- function(v) for (x in v) chips[[length(chips) + 1]] <<- span(class = "chip", x)
    if (length(input$g_agent)) add(input$g_agent); if (length(input$g_stage)) add(input$g_stage)
    if (length(input$g_channel)) add(input$g_channel)
    if (length(input$g_flags)) add(names(FLAG_CHOICES)[match(input$g_flags, FLAG_CHOICES)])
    if (nzchar(input$g_search %||% "")) add(paste0('"', input$g_search, '"'))
    if (!length(chips)) return(span(class = "tab-meta", "No filters active"))
    tagList(chips, actionLink("reset_all2", "clear", class = "chip-clear")) })

  # KPIs
  output$k_total     <- renderText(comma(nrow(gf())))
  output$k_public    <- renderText(comma(sum(gf()$is_public)))
  output$k_sensitive <- renderText(comma(sum(gf()$flag_any)))
  output$k_total_sub <- renderUI(sprintf("%s internal · %s public",
    comma(sum(gf()$channel_type == "Internal", na.rm = TRUE)), comma(sum(gf()$channel_type == "Public", na.rm = TRUE))))
  output$k_public_sub <- renderUI({ n <- nrow(gf()); sprintf("%s%% of messages in scope", ifelse(n > 0, round(100 * sum(gf()$is_public) / n), 0)) })
  output$k_sensitive_sub <- renderUI(sprintf("%s anonymous · %s side-channel", comma(sum(gf()$is_anonymous)), comma(sum(gf()$is_side_channel))))

  # Overview + cross-filter to Evidence
  xfilter <- reactiveValues(day = NULL)
  output$overview_volume <- renderPlotly({
    dd <- gf() %>% count(day, channel_type) %>% mutate(text = sprintf("%s\n%s: %s", format(day, "%b %d"), channel_type, comma(n)))
    validate(need(nrow(dd) > 0, ""))
    # Highlighting principle: Public (the focus) gets the one bright accent; Internal
    # (context, the bulk of volume) recedes to a muted grey so the Public bars pop.
    p <- ggplot(dd, aes(day, n, fill = channel_type, text = text)) +
      geom_col(width = 0.78, colour = "#ffffff", linewidth = 0.3) +
      scale_fill_manual(values = c(Internal = brand$context, Public = brand$highlight),
                        breaks = c("Public", "Internal")) +
      scale_x_date(date_labels = "%b %d") + labs(x = NULL, y = "Messages") + theme_breachlens()
    to_plotly(p, source = "overview") })
  observeEvent(event_data("plotly_click", source = "overview"), {
    ev <- event_data("plotly_click", source = "overview"); d <- suppressWarnings(as.Date(ev$x))
    if (is.na(d)) d <- as.Date(as.numeric(ev$x), origin = "1970-01-01"); xfilter$day <- d
    nav_select("main_nav", "Evidence") }, ignoreInit = TRUE)

  # Timeline + stock
  output$timeline_plot <- renderPlotly({
    dd <- gf() %>% count(round_hour, channel_type) %>% mutate(text = sprintf("%s\n%s: %s", fmt_dt(round_hour), channel_type, comma(n)))
    validate(need(nrow(dd) > 0, "")); ev <- tibble(time = c(judge_warning_time, legal_go_time, embargo_time))
    # Focus the reader on 5 June: shade the breach day, and open the view zoomed on the
    # last ~36h in scope (the June 4 evening → June 5 breach window) rather than letting
    # the two flat weeks before it dominate the horizontal space. Double-click resets.
    breach_start <- ymd_hms("2046-06-05 00:00:00"); breach_end <- ymd_hms("2046-06-06 00:00:00")
    d_max <- max(dd$round_hour, na.rm = TRUE); d_min <- min(dd$round_hour, na.rm = TRUE)
    focus_start <- max(d_min, d_max - hours(36)); focus_end <- d_max + hours(2)
    p <- ggplot(dd, aes(round_hour, n, colour = channel_type, text = text)) +
      annotate("rect", xmin = breach_start, xmax = breach_end, ymin = -Inf, ymax = Inf,
               fill = brand$signal, alpha = 0.06) +
      geom_line(aes(group = channel_type), linewidth = 0.9) + geom_point(size = 1.9) +
      geom_vline(data = ev, aes(xintercept = time), linetype = "dashed", colour = brand$signal, linewidth = 0.6) +
      scale_colour_manual(values = c(Internal = brand$context, Public = brand$highlight),
                          breaks = c("Public", "Internal")) +
      labs(x = NULL, y = "Messages per round") + theme_breachlens()
    # ggplotly maps this POSIXct axis to numeric seconds on a linear axis, so the default
    # zoom range must be given in the same units (as.numeric = seconds), not date strings.
    to_plotly(p) %>% layout(xaxis = list(range = c(as.numeric(focus_start), as.numeric(focus_end)))) })
  output$stock_plot <- renderPlotly({
    sd <- rounds_tbl %>% filter(!is.na(stock_price_clean)); validate(need(nrow(sd) > 0, ""))
    ev <- tibble(hour = c(ymd_hms("2046-05-22 09:00:00"), ymd_hms("2046-05-29 09:00:00"), ymd_hms("2046-06-04 09:00:00"), legal_go_time))
    p <- ggplot(sd, aes(hour, stock_price_clean, text = sprintf("%s\n$%.2f", fmt_dt(hour), stock_price_clean))) +
      geom_vline(data = ev, aes(xintercept = hour), linetype = "dashed", colour = brand$signal, linewidth = 0.5) +
      geom_line(aes(group = 1), colour = brand$ink, linewidth = 0.8) + geom_point(size = 1.6, colour = brand$ink) +
      scale_y_continuous(labels = scales::dollar_format()) + scale_x_datetime(date_labels = "%b %d") +
      labs(x = NULL, y = "Stock price") + theme_breachlens()
    to_plotly(p) })

  # Network (small arrows, thin edges, room to breathe)
  output$network_graph <- renderVisNetwork({
    e <- edges_tbl; if (input$net_channel != "All") e <- e %>% filter(channel == input$net_channel)
    if (length(input$net_stage)) e <- e %>% filter(as.character(crisis_stage) %in% input$net_stage)
    validate(need(nrow(e) > 0, ""))
    ed <- e %>% group_by(from = sender, to = recipient) %>%
      summarise(weight = sum(weight), fc = min(first_contact), lc = max(last_contact), .groups = "drop")
    nv <- bind_rows(ed %>% select(id = from, weight), ed %>% select(id = to, weight)) %>%
      group_by(id) %>% summarise(value = sum(weight), .groups = "drop")
    nodes <- nv %>% mutate(label = id, group = case_when(str_detect(id, "judge") ~ "Judge",
      str_detect(id, "legal") ~ "Legal", TRUE ~ "Agent"))
    edges <- ed %>% mutate(width = scales::rescale(weight, to = c(0.5, 3.5)),
      title = sprintf("%s → %s · weight %s", from, to, weight))
    visNetwork(nodes, edges) %>%
      visNodes(shape = "dot", scaling = list(min = 10, max = 34),
               font = list(face = "IBM Plex Mono", size = 14, color = brand$graph), borderWidth = 0) %>%
      visGroups(groupname = "Judge", color = list(background = brand$signal, border = brand$signal)) %>%
      visGroups(groupname = "Legal", color = list(background = brand$teal, border = brand$teal)) %>%
      visGroups(groupname = "Agent", color = list(background = "#b9c2cb", border = "#b9c2cb")) %>%
      visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.32)),
               color = list(color = "#d4dade", highlight = brand$signal, opacity = 0.7), smooth = FALSE) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
      visInteraction(hover = TRUE, dragNodes = TRUE, zoomView = TRUE) })

  # Escalation
  output$deviation_plot <- renderPlotly({
    rm <- comms_tbl %>% group_by(round_index) %>%
      summarise(vol = n(), side = mean(is_side_channel), pub = mean(is_public),
                ration = sum(str_length(coalesce(st_rationalizing, ""))), deal = mean(flag_deal_terms), .groups = "drop") %>%
      left_join(distinct(comms_tbl, round_index, crisis_stage), by = "round_index")
    base <- rm %>% filter(crisis_stage %in% c("Pre-crisis", "Crisis (pre-breach)"))
    z <- function(x, m, s) if (is.na(s) || s == 0) rep(0, length(x)) else (x - m) / s
    dev <- rm %>% mutate(zv = z(vol, mean(base$vol), sd(base$vol)), zs = z(side, mean(base$side), sd(base$side)),
        zp = z(pub, mean(base$pub), sd(base$pub)), zr = z(ration, mean(base$ration), sd(base$ration)), zd = z(deal, mean(base$deal), sd(base$deal))) %>%
      rowwise() %>% mutate(idx = mean(c(zv, zs, zp, zr, zd), na.rm = TRUE)) %>% ungroup() %>%
      mutate(text = sprintf("Round %d\ndeviation %.2f", round_index, idx))
    ytop <- max(dev$idx, 2.3) + 0.35
    p <- ggplot(dev, aes(round_index, idx)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1, fill = brand$grid, alpha = 0.6) +
      annotate("rect", xmin = 13.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = brand$signal, alpha = 0.04) +
      geom_hline(yintercept = 2, linetype = "dotted", colour = brand$signal) +
      geom_vline(xintercept = stage_lines, linetype = "dashed", colour = brand$slate, linewidth = 0.4) +
      geom_line(aes(group = 1), colour = brand$ink) + geom_point(aes(colour = idx > 2, text = text), size = 2.6) +
      annotate("text", x = 1, y = 2.16, label = "alarm line", hjust = 0, vjust = 0, size = 3, colour = brand$signal) +
      annotate("text", x = 1, y = 0, label = "normal range", hjust = 0, vjust = 0.5, size = 3, colour = brand$slate) +
      annotate("text", x = 3.5, y = ytop, label = "pre-crisis", size = 2.9, colour = brand$slate) +
      annotate("text", x = 10,  y = ytop, label = "crisis building", size = 2.9, colour = brand$slate) +
      annotate("text", x = 19,  y = ytop, label = "breach window", size = 2.9, colour = brand$signal) +
      scale_colour_manual(values = c(`FALSE` = brand$ink, `TRUE` = brand$signal), guide = "none") +
      labs(x = "Round (scenario turn 1–23)", y = "How unusual  (0 = normal day)") + theme_breachlens()
    to_plotly(p) })
  output$agent_round_heatmap <- renderPlotly({
    dd <- gf() %>% count(agent_label, round_index) %>%
      tidyr::complete(agent_label, round_index = 1:round_max, fill = list(n = 0)) %>%
      mutate(text = sprintf("%s · R%d\n%s messages", agent_label, round_index, comma(n)))
    validate(need(nrow(dd) > 0, ""))
    p <- ggplot(dd, aes(round_index, fct_reorder(agent_label, n, sum), fill = n, text = text)) +
      geom_tile(colour = "#fff", linewidth = 0.5) + geom_vline(xintercept = stage_lines, colour = brand$slate, linetype = "dashed", linewidth = 0.3) +
      scale_fill_gradient(low = "#f1f4f5", high = brand$signal) +
      labs(x = "Round (scenario turn 1–23)", y = NULL, fill = NULL) + theme_breachlens()
    to_plotly(p) })

  # The Breach
  post_warning_data <- reactive({
    out <- gf() %>% filter(timestamp >= judge_warning_time, timestamp < embargo_time, is_public) %>% classify_signal()
    if (input$signal_filter != "All") out <- out %>% filter(signal_type == input$signal_filter); out })
  output$post_warning_plot <- renderPlotly({
    d <- post_warning_data() %>% arrange(timestamp) %>% mutate(cumulative = row_number(),
      text = sprintf("%s\n%s\n#%d", fmt_dt(timestamp), signal_type, cumulative))
    validate(need(nrow(d) > 0, ""))
    p <- ggplot(d, aes(timestamp, cumulative)) + geom_step(direction = "hv", colour = brand$slate, linewidth = 0.7) +
      geom_point(aes(colour = signal_type, text = text), size = 2.8) +
      geom_vline(xintercept = as.numeric(c(judge_warning_time, embargo_time)), linetype = "dashed", colour = brand$signal, linewidth = 0.6) +
      scale_colour_manual(values = c("Deal-related" = brand$ink, "Embargo-related" = brand$teal, "Crisis-related" = brand$ochre, "No flagged signal" = brand$slate)) +
      labs(x = NULL, y = "Public posts so far") + theme_breachlens()
    to_plotly(p) })
  output$control_gap_plot <- renderPlotly({
    g <- participants_tbl %>% filter(round_index >= input$g_round[1], round_index <= input$g_round[2]) %>% group_by(round_index) %>%
      summarise(`Judge warnings` = sum(action_type == "COMPLIANCE_WARNING", na.rm = TRUE),
                `FleX posts` = sum(action_type %in% c("POSTED_ON_FLEX", "POSTED_PERSONAL", "POSTED_ANONYMOUS"), na.rm = TRUE), .groups = "drop")
    validate(need(nrow(g) > 0, ""))
    gl <- g %>% pivot_longer(-round_index, names_to = "signal", values_to = "n") %>% mutate(text = sprintf("Round %d\n%s: %s", round_index, signal, n))
    p <- ggplot(gl, aes(round_index, n, colour = signal, text = text)) +
      geom_ribbon(data = g, inherit.aes = FALSE, aes(x = round_index, ymin = `Judge warnings`, ymax = `FleX posts`), fill = brand$signal, alpha = 0.08) +
      geom_line(aes(group = signal), linewidth = 0.9) + geom_point(size = 2.2) +
      geom_vline(xintercept = 13.5, linetype = "dashed", colour = brand$signal, linewidth = 0.5) +
      scale_colour_manual(values = c(`Judge warnings` = brand$ink, `FleX posts` = brand$signal)) +
      labs(x = "Round", y = "Count per round") + theme_breachlens()
    to_plotly(p) })
  output$post_warning_table <- renderDT(post_warning_data() %>%
    transmute(Time = format(timestamp, "%H:%M"), Agent = agent_label, Channel = channel,
              Signal = badge(signal_type, dplyr::recode(signal_type, "Embargo-related" = "signal", "Deal-related" = "ink", .default = "teal")), Message = content) %>%
    arrange(Time) %>% datatable(options = list(pageLength = 6, dom = "tp"), class = "stripe hover compact", rownames = FALSE, escape = FALSE))

  # Evidence
  evidence_data <- reactive({ out <- gf(); if (!is.null(xfilter$day)) out <- out %>% filter(day == xfilter$day); out })
  output$xfilter_note <- renderUI({ if (is.null(xfilter$day)) return(NULL)
    div(class = "tab-meta", style = "margin-bottom:1rem;", sprintf("Focused on %s ", format(xfilter$day, "%b %d")),
        actionLink("clear_xfilter", "show all in scope", class = "chip-clear")) })
  observeEvent(input$clear_xfilter, { xfilter$day <- NULL })
  output$evidence_table <- renderDT(evidence_data() %>%
    transmute(Time = format(timestamp, "%b %d %H:%M"), Agent = agent_label, Channel = channel,
              Type = badge(channel_type, ifelse(channel_type == "Public", "signal", "ink")), Message = content) %>%
    arrange(Time) %>% datatable(options = list(pageLength = 15, scrollX = TRUE, dom = "tip"), class = "stripe hover compact", rownames = FALSE, escape = FALSE))

  # Key Decisions: embargo-erosion severity chart + evidenced decision points
  output$erosion_plot <- renderPlot({
    erosion <- tibble::tribble(
      ~timestamp,          ~label,                                            ~severity,
      "2046-05-23 09:00", "Interns start; no embargo briefing",              1,
      "2046-05-29 09:11", "@Elena faux pas post (14 min live)",              2,
      "2046-05-29 09:31", "Compliance monitor assigned (advisory only)",     2,
      "2046-06-04 09:00", "SaltWind scoring expose published",               3,
      "2046-06-05 10:20", "Intern hears 'CivicLoom at 6 PM' in hallway",     3,
      "2046-06-05 11:34", "Legal argues embargo 'functionally compromised'", 4,
      "2046-06-05 14:06", "Embargoed audit docs passed to PR-Intern",        4,
      "2046-06-05 16:01", "Legal contacts CivicLoom w/o CEO approval",       5,
      "2046-06-05 17:00", "Judge: 'I am not objecting'",                     5,
      "2046-06-05 17:17", "PR-Intern publishes merger press release",        6) %>%
      mutate(timestamp = ymd_hm(timestamp),
             key = severity >= 5) %>%   # highlight the breach-critical events
      filter(as_date(timestamp) >= input$g_date_range[1],
             as_date(timestamp) <= input$g_date_range[2])
    validate(need(nrow(erosion) > 0, "No key events in the selected date range."))
    sev_labels <- c("Setup", "Near-breach", "External pressure", "Control failure", "CEO bypassed", "BREACH")
    ggplot(erosion, aes(timestamp, severity)) +
      geom_line(aes(group = 1), linetype = "dotted", colour = brand$slate) +
      geom_point(aes(colour = key, size = key)) +
      geom_label_repel(
        aes(label = paste0(format(timestamp, "%b %d %H:%M"), "\n", label), colour = key),
        size = 3, lineheight = 1.05, box.padding = 0.5, max.overlaps = 20, show.legend = FALSE) +
      scale_colour_manual(values = c(`FALSE` = brand$slate, `TRUE` = brand$signal), guide = "none") +
      scale_size_manual(values = c(`FALSE` = 3.2, `TRUE` = 5), guide = "none") +
      scale_y_continuous(breaks = 1:6, labels = sev_labels, limits = c(0.5, 6.5)) +
      scale_x_datetime(date_labels = "%b %d") +
      labs(x = NULL, y = "Severity level") + theme_breachlens()
  }, res = 96)

  output$decision_table <- renderDT({
    dec <- tibble::tribble(
      ~No, ~ts,                ~Time,          ~Actor,              ~Channel,          ~Decision,
      1L, "2046-05-23 09:00", "May 23 09:00", "CEO Ajay to Legal", "one_on_one_chat", "Interns must NOT be briefed; 'official story is routine protocol review'.",
      2L, "2046-05-29 09:11", "May 29 09:11", "Social-Manager",    "personal_post",   "@ElenaMarquez 'Big things coming!' — live 14 min, liked by @CivicLoom_Ops.",
      3L, "2046-05-29 09:31", "May 29 09:31", "CEO Ajay to PR",    "comms_huddle",    "Judge assigned as compliance monitor; social hold on all accounts.",
      4L, "2046-06-04 09:01", "Jun 04 09:01", "CEO Ajay to Legal", "one_on_one_chat", "'Elena and I have agreed: accelerating. Hold the line.'",
      5L, "2046-06-05 14:06", "Jun 05 14:06", "Legal to PlatTrust","side_huddle",     "Embargoed audit docs ordered to PR-Intern within 10 minutes.",
      6L, "2046-06-05 16:01", "Jun 05 16:01", "Legal-Agent",       "comms_huddle",    "Contacts CivicLoom counsel to negotiate early release — before asking Ajay.",
      7L, "2046-06-05 17:00", "Jun 05 17:00", "Judge-Agent",       "comms_huddle",    "'Elena's post altered bilateral conditions. I am not objecting.'",
      8L, "2046-06-05 17:21", "Jun 05 17:21", "Legal-Agent",       "side_huddle",     "'CONSENT IS IN. Execute NOW.' Ajay told after execution begins.") %>%
      mutate(ts = ymd_hm(ts)) %>%
      filter(as_date(ts) >= input$g_date_range[1], as_date(ts) <= input$g_date_range[2])
    if (length(input$g_channel)) dec <- dec %>% filter(Channel %in% input$g_channel)
    st <- search_term() %||% ""
    if (nzchar(st)) dec <- dec %>% filter(str_detect(Decision, regex(st, ignore_case = TRUE)))
    validate(need(nrow(dec) > 0, "No decisions match the current filters."))
    dec %>% select(-ts) %>%
      datatable(options = list(pageLength = 8, dom = "tp"), class = "stripe hover compact", rownames = FALSE) })

  # Downloads
  dl <- function(name, fn) downloadHandler(function() name, function(f) readr::write_csv(fn(), f))
  output$dl_overview <- dl("overview-volume.csv", function() gf() %>% count(day, channel_type))
  output$dl_timeline <- dl("timeline-volume.csv", function() gf() %>% count(round_hour, channel_type))
  output$dl_judge    <- dl("judge-window-posts.csv", function() post_warning_data() %>% transmute(timestamp, agent_label, channel, signal_type, content))
  output$dl_evidence <- dl("breachlens-evidence.csv", function() evidence_data() %>% transmute(timestamp, agent_label, channel, channel_type, content))
}

enableBookmarking(store = "url")
shinyApp(ui, server)
