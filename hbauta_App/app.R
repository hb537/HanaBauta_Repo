# ============================================================
# Tech Engagement & Mental Health — Interactive Shiny App
# Author: Hana Bauta
# Professional Academic Design
# ============================================================
suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(DT)
  library(scales)
  library(plotly)
})

# ---- Professional color palette ----------------------------
color_primary <- "#1a3a52"      # Dark navy
color_secondary <- "#2d5a7b"    # Medium navy
color_accent <- "#0a7ea4"       # Teal accent
color_border <- "#d8dce0"       # Light border
color_bg <- "#f5f6f7"           # Off-white background
color_text <- "#2c3e50"         # Dark text
color_text_light <- "#7a8491"   # Light text

# Age group colors - professional palette
age_colours <- c(
  "<25"   = "#0a7ea4",
  "25-34" = "#1a8b7d",
  "35-44" = "#2d5a7b",
  "45-54" = "#5d3a6b",
  "55+"   = "#6b4c5c"
)

# Refined academic theme
academic_theme <- theme_minimal(base_size = 10, base_family = "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif") +
  theme(
    # Typography
    axis.title = element_text(size = 10, face = "bold", color = color_primary, margin = margin(t=6, b=6)),
    axis.text = element_text(size = 9, color = color_text_light),
    plot.title = element_text(size = 13, face = "bold", color = color_primary, margin = margin(b=8)),
    plot.subtitle = element_text(size = 9, color = color_text_light, margin = margin(b=6)),
    
    # Grid & background
    panel.grid.major = element_line(color = "#e8eaec", size = 0.25),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    
    # Facets
    strip.background = element_rect(fill = color_bg, color = color_border, size = 0.4),
    strip.text = element_text(size = 9, face = "bold", color = color_primary, margin = margin(t=4, b=4, l=6, r=6)),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9, color = color_primary),
    legend.text = element_text(size = 8, color = color_text_light),
    
    # Margins
    plot.margin = margin(10, 10, 10, 10)
  )

usage_labels <- c("None", "<1hr", "1-2hr", "2-3hr", "3-4hr", "4-6hr", "6-9hr", "10+hr")
ai_freq_labels <- c("0" = "Never", "1" = "Rarely", "2" = "Monthly", "3" = "Weekly",
                    "4" = "Few times/week", "5" = "Daily", "6" = "Many times/day")

# ---- Data loaders ----------------------------
resolve_path <- function(...) {
  for (p in c(...)) {
    if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
  }
  NULL
}

safe_read_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(suppressWarnings(readr::read_csv(path, show_col_types = FALSE)), error = function(e) NULL)
}

.home <- Sys.getenv("HOME")
apex_path <- resolve_path(
  "data/mWEL_Data.csv", "data/mWel_Data.csv", "data/APEX_Data.csv",
  "../data/raw/mWEL_Data.csv", "../data/raw/mWel_Data.csv", "../data/raw/APEX_Data.csv",
  file.path(.home, "Desktop/HanaBauta_Repo/data/raw/mWEL_Data.csv"),
  file.path(.home, "Desktop/HanaBauta_Repo/data/raw/mWel_Data.csv"),
  file.path(.home, "Desktop/HanaBauta_Repo/data/raw/APEX_Data.csv")
)

apex_raw <- safe_read_csv(apex_path)
apex_df <- if (!is.null(apex_raw)) {
  apex_raw %>%
    mutate(
      age_numeric = suppressWarnings(as.numeric(age)),
      tech_usage_avg = rowMeans(select(., tech_1, tech_2, tech_3, tech_4, tech_5, tech_6, tech_7), na.rm = TRUE),
      age_group = cut(age_numeric, breaks = c(-Inf, 24, 34, 44, 54, Inf), labels = c("<25", "25-34", "35-44", "45-54", "55+"))
    ) %>%
    filter(!is.na(age_group), !is.na(tech_usage_avg))
} else {
  NULL
}

sim_defaults <- list(b0 = 12, b1 = -0.8, sigma = 4.5, mean_x = 3.5, sd_x = 1.2, n = 150)
if (!is.null(apex_df) && "GAD7_Sum1" %in% names(apex_raw)) {
  dat <- apex_raw %>%
    mutate(tech_score = rowMeans(select(., tech_1, tech_2, tech_3, tech_4, tech_5, tech_6, tech_7), na.rm = TRUE)) %>%
    filter(!is.na(tech_score), !is.na(GAD7_Sum1))
  if (nrow(dat) > 10) {
    fit <- lm(GAD7_Sum1 ~ tech_score, data = dat)
    sim_defaults <- list(
      b0 = unname(coef(fit)[1]), b1 = unname(coef(fit)[2]), sigma = sigma(fit),
      mean_x = mean(dat$tech_score), sd_x = sd(dat$tech_score), n = nrow(dat)
    )
  }
}

hms_path <- resolve_path(
  "data/hms_processed_subset.csv", "../data/processed/hms_processed_subset.csv",
  file.path(.home, "Desktop/HanaBauta_Repo/data/processed/hms_processed_subset.csv")
)

hms_raw <- safe_read_csv(hms_path)
hms_df <- if (!is.null(hms_raw)) {
  hms_raw %>%
    filter(!is.na(internet_1)) %>%
    mutate(
      tech_engagement_level = as.numeric(internet_1),
      usage_cat = factor(internet_1, levels = 1:8, labels = usage_labels),
      gender = case_when(
        !is.na(gender_female) & gender_female == 1 ~ "Female",
        !is.na(gender_male) & gender_male == 1 ~ "Male",
        TRUE ~ "Other / NA"
      ),
      used_digital_mh_bin = ifelse(!is.na(used_digital_mh) & used_digital_mh == "Yes", 1L, 0L),
      ai_use_freq_label = dplyr::recode(as.character(ai_use_freq), !!!ai_freq_labels, .missing = "Unknown")
    )
} else {
  NULL
}

# ---- Helpers ------------------------------------------------
run_simulation <- function(effect_size, noise, sample_size, b0, mean_x, sd_x) {
  tech <- rnorm(sample_size, mean = mean_x, sd = sd_x)
  tech <- pmin(pmax(tech, 1), 6)
  gad7 <- b0 + effect_size * tech + rnorm(sample_size, mean = 0, sd = noise)
  gad7 <- pmin(pmax(round(gad7), 0), 21)
  tibble::tibble(tech_score = tech, GAD7_Sum1 = gad7)
}

metric_box <- function(label, value, sub = NULL) {
  div(
    style = "
      padding: 16px;
      border-top: 3px solid #0a7ea4;
      background: white;
      border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      flex: 1 1 200px;
      margin: 6px;
    ",
    div(style = "font-size: 11px; color: #7a8491; text-transform: uppercase; letter-spacing: 0.3px; font-weight: 600; margin-bottom: 8px;", label),
    div(style = "font-size: 26px; font-weight: 700; color: #1a3a52; margin-bottom: 4px;", value),
    if (!is.null(sub)) div(style = "font-size: 11px; color: #7a8491;", sub)
  )
}

hms_outcomes <- list(
  anx_score = list(label = "Anxiety (GAD-7)", var = "anx_score", kind = "continuous", range = c(0, 21), color = "#d32f2f",
                   help = "GAD-7 total score (0-21). Higher = more anxiety symptoms."),
  deprawsc = list(label = "Depression (raw score)", var = "deprawsc", kind = "continuous", range = c(0, 27), color = "#f57c00",
                  help = "Depression raw score. Higher = more depressive symptoms."),
  lone_lackcompanion = list(label = "Loneliness (1-3)", var = "lone_lackcompanion", kind = "ordinal", range = c(1, 3), color = "#5d3a6b",
                            help = "Lack-of-companionship item: 1 = hardly ever, 2 = some of the time, 3 = often."),
  used_digital_mh_bin = list(label = "Used digital MH tool (0/1)", var = "used_digital_mh_bin", kind = "binary", range = c(0, 1), color = "#1a8b7d",
                             help = "Binary indicator: used a digital mental health tool (1 = Yes).")
)

# ==================================================================
# UI
# ==================================================================
ui <- navbarPage(
  title = "Tech Engagement & Mental Health",
  header = tags$head(tags$style(HTML("
    * { box-sizing: border-box; }

    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif;
      background-color: #f5f6f7;
      color: #2c3e50;
      line-height: 1.5;
    }

    .navbar {
      background-color: #1a3a52;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      border: none;
    }

    .navbar-brand {
      font-weight: 700;
      font-size: 16px;
      color: white !important;
      letter-spacing: 0.3px;
    }

    .nav-tabs {
      border-bottom: 1px solid #d8dce0;
      background-color: white;
    }

    .nav-tabs .nav-link {
      color: #7a8491;
      border: none;
      padding: 12px 18px;
      font-weight: 600;
      font-size: 12px;
      border-radius: 0;
      transition: all 0.2s ease;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }

    .nav-tabs .nav-link:hover {
      color: #0a7ea4;
      background-color: transparent;
      border-bottom: 2px solid #0a7ea4;
    }

    .nav-tabs .nav-link.active {
      background-color: transparent;
      color: #1a3a52;
      border-bottom: 3px solid #0a7ea4;
    }

    .well {
      background: white;
      border: 1px solid #d8dce0;
      border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      padding: 18px;
    }

    .form-group { margin-bottom: 14px; }

    label {
      font-weight: 700;
      color: #1a3a52;
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.3px;
      margin-bottom: 6px;
      display: block;
    }

    input[type='text'], input[type='number'], select {
      border: 1px solid #d8dce0;
      border-radius: 4px;
      padding: 8px 10px;
      font-size: 13px;
      transition: all 0.2s ease;
      background: white;
      color: #2c3e50;
    }

    input[type='text']:focus, input[type='number']:focus, select:focus {
      border-color: #0a7ea4;
      box-shadow: 0 0 0 2px rgba(10, 126, 164, 0.1);
      outline: none;
    }

    input[type='checkbox'], input[type='radio'] {
      margin-right: 6px;
      cursor: pointer;
      accent-color: #0a7ea4;
    }

    .btn {
      border: none;
      border-radius: 4px;
      font-weight: 600;
      font-size: 11px;
      padding: 8px 16px;
      transition: all 0.2s ease;
      text-transform: uppercase;
      letter-spacing: 0.3px;
      cursor: pointer;
    }

    .btn-primary {
      background-color: #0a7ea4;
      color: white;
    }

    .btn-primary:hover {
      background-color: #085a82;
      box-shadow: 0 2px 6px rgba(10, 126, 164, 0.2);
    }

    .btn-primary:active {
      background-color: #064656;
    }

    h1, h2 {
      color: #1a3a52;
      font-weight: 700;
      letter-spacing: 0.2px;
    }

    h3 {
      color: #1a3a52;
      font-weight: 700;
      font-size: 14px;
      margin-top: 0;
      margin-bottom: 12px;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }

    h4 {
      color: #1a3a52;
      font-weight: 700;
      font-size: 12px;
      margin-bottom: 10px;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }

    .intro-section {
      background: white;
      padding: 28px;
      border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      margin-bottom: 20px;
    }

    .intro-section h2 {
      margin-top: 0;
      font-size: 22px;
      margin-bottom: 8px;
    }

    .intro-section .subtitle {
      color: #7a8491;
      font-size: 14px;
      margin-bottom: 16px;
    }

    .objective-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 14px;
      margin: 16px 0;
    }

    .objective-card {
      background: white;
      padding: 16px;
      border-left: 3px solid #0a7ea4;
      border-radius: 4px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }

    .objective-card h4 {
      color: #0a7ea4;
      margin-top: 0;
    }

    .objective-card p {
      font-size: 13px;
      color: #7a8491;
      margin: 0;
      line-height: 1.5;
    }

    .metric-row {
      display: flex;
      flex-wrap: wrap;
      margin: -6px;
      margin-bottom: 12px;
    }

    .dataTables_wrapper {
      border-radius: 4px;
      overflow: hidden;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
    }

    .dataTables_wrapper table {
      background: white;
      border: 1px solid #d8dce0;
      margin-top: 0;
    }

    .dataTables_wrapper thead th {
      background-color: #f5f6f7;
      color: #1a3a52;
      font-weight: 700;
      padding: 10px !important;
      border-color: #d8dce0;
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }

    .dataTables_wrapper td {
      padding: 10px !important;
      border-color: #e8eaec !important;
      color: #2c3e50;
      font-size: 13px;
    }

    .dataTables_wrapper tr:hover {
      background: #fafbfc !important;
    }

    .help-block {
      font-size: 11px;
      color: #7a8491;
      margin-top: 4px;
      font-style: italic;
    }

    ul, ol {
      line-height: 1.8;
      margin-bottom: 14px;
      color: #2c3e50;
    }

    li {
      margin-bottom: 6px;
      font-size: 13px;
    }

    hr {
      border: none;
      border-top: 1px solid #d8dce0;
      margin: 14px 0;
    }

    @media (max-width: 992px) {
      .objective-grid { grid-template-columns: 1fr; }
    }
  "))),
  
  tabPanel(
    "Overview",
    div(class = "intro-section",
        h2("Tech Engagement & Mental Health"),
        p(class = "subtitle", "Interactive analysis of technology use and mental health outcomes across multiple populations."),
        
        h3("Research Question"),
        p("How does daily technology engagement relate to mental health outcomes—and how confident can we be in these associations given realistic effect sizes and sample sizes?"),
        
        h3("Data Sources"),
        tags$ul(
          tags$li(tags$strong("mWEL"), "—Educator sample (n=; composite tech engagement score by age)"),
          tags$li(tags$strong("HMS 2024–25"), "—College student survey (daily internet use, multiple MH outcomes)")
        ),
        
        h3("Objectives"),
        div(class = "objective-grid",
            div(class = "objective-card",
                h4("1. Describe"),
                p("Examine tech engagement variation across age groups with multiple visualization options.")),
            div(class = "objective-card",
                h4("2. Simulate"),
                p("Explore how effect size, noise, and sample size affect statistical power using calibrated parameters.")),
            div(class = "objective-card",
                h4("3. Model"),
                p("Analyze tech use associations with four mental health outcomes with demographic adjustments."))
        ),
        
        h3("Navigation"),
        tags$ol(
          tags$li(tags$strong("Age & Tech"), "— Explore engagement across age groups with boxplot, violin, or confidence interval views."),
          tags$li(tags$strong("Power Simulation"), "— Adjust effect size, noise, and sample size to see impact on statistical power."),
          tags$li(tags$strong("HMS Analysis"), "— Select mental health outcome and apply demographic filters to explore associations.")
        ),
        
        h3("Data Ethics"),
        p("All data are de-identified and used under original IRB approvals. No individual is identifiable in this analysis.")
    )
  ),
  
  tabPanel(
    "Age & Tech",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("Controls"),
        checkboxGroupInput("apex_groups", "Age groups:", choices = names(age_colours), selected = names(age_colours)),
        br(),
        radioButtons("apex_plot_type", "Visualization:", choices = c("Boxplot + jitter" = "box", "Violin + mean" = "violin", "Mean with 95% CI" = "ci")),
        hr(),
        helpText("Data source: mWEL_Data.csv. Interactive plot — hover for values, zoom, pan, or click legend items.")
      ),
      mainPanel(
        width = 9,
        h3("Technology Engagement by Age Group"),
        plotlyOutput("apex_plot", height = "480px"),
        br(),
        h4("Summary Statistics"),
        DTOutput("apex_summary")
      )
    )
  ),
  
  tabPanel(
    "Power Simulation",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("Parameters"),
        sliderInput("sim_b1", "Effect size (b₁):", min = -2.0, max = 0.0, value = round(sim_defaults$b1, 2), step = 0.1),
        sliderInput("sim_sigma", "Residual noise (σ):", min = 1, max = 8, value = round(sim_defaults$sigma, 1), step = 0.5),
        sliderInput("sim_n", "Sample size (n):", min = 30, max = 500, value = min(sim_defaults$n, 200), step = 10),
        numericInput("sim_reps", "Replicates:", value = 200, min = 50, max = 2000, step = 50),
        br(),
        actionButton("sim_go", "Run Simulation", class = "btn-primary", width = "100%"),
        hr(),
        helpText("Parameters calibrated to mWEL data when available.")
      ),
      mainPanel(
        width = 9,
        h3("Simulated Dataset"),
        plotlyOutput("sim_scatter", height = "400px"),
        br(),
        fluidRow(
          column(6, h4("Model Results"), verbatimTextOutput("sim_fit_text")),
          column(6, h4("Statistical Power"), verbatimTextOutput("sim_power_text"))
        )
      )
    )
  ),
  
  tabPanel(
    "HMS Analysis",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("Outcome"),
        radioButtons("hms_outcome", NULL, choices = setNames(names(hms_outcomes), vapply(hms_outcomes, `[[`, character(1), "label")), selected = "anx_score"),
        uiOutput("hms_outcome_help"),
        hr(),
        h3("Filters"),
        radioButtons("hms_gender", "Gender:", choices = c("All" = "All", "Female" = "Female", "Male" = "Male", "Other / NA" = "Other / NA"), selected = "All", inline = FALSE),
        sliderInput("hms_age", "Age range:", min = 18, max = 80, value = c(18, 35), step = 1),
        checkboxGroupInput("hms_ai", "AI use frequency:", choices = c("All" = "__all__", setNames(unname(ai_freq_labels), unname(ai_freq_labels))), selected = "__all__"),
        hr(),
        h3("Model"),
        checkboxInput("hms_adjust_age", "Adjust for age", value = FALSE),
        checkboxInput("hms_adjust_gender", "Adjust for gender", value = FALSE),
        radioButtons("hms_plot_type", "Visualization:", choices = c("Boxplot" = "box", "Scatter + fit" = "scatter", "Bar chart" = "bar"))
      ),
      mainPanel(
        width = 9,
        h3(textOutput("hms_title", inline = TRUE)),
        div(class = "metric-row", uiOutput("hms_kpis")),
        br(),
        plotlyOutput("hms_plot", height = "440px"),
        br(),
        fluidRow(
          column(7, h4("Model Results"), verbatimTextOutput("hms_model_text")),
          column(5, h4("Summary by Usage"), DTOutput("hms_desc_tbl"))
        )
      )
    )
  )
)

# ==================================================================
# Server
# ==================================================================
server <- function(input, output, session) {
  
  apex_subset <- reactive({
    req(apex_df)
    validate(need(length(input$apex_groups) > 0, "Select at least one age group."))
    apex_df %>% filter(age_group %in% input$apex_groups)
  })
  
  output$apex_plot <- renderPlotly({
    if (is.null(apex_df)) {
      return(plotly_empty() %>% layout(title = "Data file not found. Check data/ directory."))
    }
    df <- apex_subset()
    p <- switch(
      input$apex_plot_type,
      box = ggplot(df, aes(x = age_group, y = tech_usage_avg, fill = age_group)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.5, colour = "#d8dce0", linewidth = 0.4) +
        geom_jitter(aes(colour = age_group), width = 0.15, size = 1.2, alpha = 0.5) +
        scale_fill_manual(values = age_colours) +
        scale_colour_manual(values = age_colours) +
        coord_cartesian(ylim = c(1, 7)) +
        scale_y_continuous(breaks = 1:7) +
        labs(x = "Age Group", y = "Tech engagement (1-7)"),
      violin = {
        summary_df <- df %>% group_by(age_group) %>%
          summarise(mean = mean(tech_usage_avg, na.rm = TRUE), se = sd(tech_usage_avg, na.rm = TRUE) / sqrt(n()), .groups = "drop")
        ggplot(df, aes(x = age_group, y = tech_usage_avg, fill = age_group)) +
          geom_violin(trim = TRUE, alpha = 0.6, colour = NA) +
          geom_pointrange(data = summary_df, aes(y = mean, ymin = mean - se, ymax = mean + se),
                          inherit.aes = FALSE, mapping = aes(x = age_group, y = mean, ymin = mean - se, ymax = mean + se),
                          colour = "#1a3a52", size = 0.4, linewidth = 0.6) +
          scale_fill_manual(values = age_colours) +
          scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
          labs(x = "Age Group", y = "Tech engagement (1-7)")
      },
      ci = {
        ci_df <- df %>% group_by(age_group) %>%
          summarise(n = n(), mean = mean(tech_usage_avg, na.rm = TRUE), sd = sd(tech_usage_avg, na.rm = TRUE), .groups = "drop") %>%
          mutate(se = sd / sqrt(n), ci = qt(0.975, df = pmax(n - 1, 1)) * se, ylo = mean - ci, yhi = mean + ci)
        ggplot(ci_df, aes(x = age_group, y = mean, fill = age_group)) +
          geom_col(alpha = 0.7, width = 0.6, colour = "#d8dce0", linewidth = 0.4) +
          geom_errorbar(aes(ymin = ylo, ymax = yhi), width = 0.2, linewidth = 0.5) +
          scale_fill_manual(values = age_colours) +
          scale_y_continuous(limits = c(0, 7), breaks = 0:7, expand = expansion(mult = c(0, 0.05))) +
          labs(x = "Age Group", y = "Mean tech engagement")
      }
    )
    p <- p + academic_theme + theme(legend.position = "none")
    ggplotly(p) %>% config(displayModeBar = "hover")
  })
  
  output$apex_summary <- renderDT({
    req(apex_df)
    df <- apex_subset()
    tbl <- df %>% group_by(age_group) %>%
      summarise(n = n(), mean = round(mean(tech_usage_avg, na.rm = TRUE), 2), sd = round(sd(tech_usage_avg, na.rm = TRUE), 2),
                median = round(median(tech_usage_avg, na.rm = TRUE), 2), IQR = round(IQR(tech_usage_avg, na.rm = TRUE), 2),
                min = round(min(tech_usage_avg, na.rm = TRUE), 2), max = round(max(tech_usage_avg, na.rm = TRUE), 2), .groups = "drop")
    datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  
  sim_once <- eventReactive(input$sim_go, {
    set.seed(NULL)
    run_simulation(effect_size = input$sim_b1, noise = input$sim_sigma, sample_size = input$sim_n,
                   b0 = sim_defaults$b0, mean_x = sim_defaults$mean_x, sd_x = sim_defaults$sd_x)
  }, ignoreNULL = FALSE)
  
  sim_power <- eventReactive(input$sim_go, {
    reps <- max(1, as.integer(input$sim_reps))
    sig_flags <- logical(reps); est_b1 <- numeric(reps)
    withProgress(message = "Running replicates...", value = 0, {
      step <- max(1, reps %/% 20)
      for (i in seq_len(reps)) {
        d <- run_simulation(effect_size = input$sim_b1, noise = input$sim_sigma, sample_size = input$sim_n,
                            b0 = sim_defaults$b0, mean_x = sim_defaults$mean_x, sd_x = sim_defaults$sd_x)
        fit <- tryCatch(lm(GAD7_Sum1 ~ tech_score, data = d), error = function(e) NULL)
        if (!is.null(fit)) {
          s <- summary(fit)$coefficients
          est_b1[i] <- s["tech_score", "Estimate"]
          sig_flags[i] <- s["tech_score", "Pr(>|t|)"] < 0.05
        }
        if (i %% step == 0) incProgress(1/20)
      }
    })
    list(power = mean(sig_flags), mean_b1 = mean(est_b1))
  }, ignoreNULL = FALSE)
  
  output$sim_scatter <- renderPlotly({
    d <- sim_once()
    p <- ggplot(d, aes(x = tech_score, y = GAD7_Sum1)) +
      geom_jitter(alpha = 0.4, width = 0.05, color = color_accent, size = 1.5) +
      geom_smooth(method = "lm", se = TRUE, color = "#d32f2f", linewidth = 0.8, fill = "#d32f2f", alpha = 0.15) +
      scale_x_continuous(limits = c(1, 6)) +
      scale_y_continuous(limits = c(0, 21)) +
      labs(title = sprintf("Simulated data (n=%d, β₁=%.2f, σ=%.2f)", input$sim_n, input$sim_b1, input$sim_sigma),
           x = "Tech score (1-6)", y = "GAD-7 (0-21)") +
      academic_theme
    ggplotly(p) %>% config(displayModeBar = "hover")
  })
  
  output$sim_fit_text <- renderPrint({
    d <- sim_once()
    fit <- lm(GAD7_Sum1 ~ tech_score, data = d)
    print(summary(fit)$coefficients, digits = 3)
  })
  
  output$sim_power_text <- renderPrint({
    res <- sim_power()
    cat(sprintf("Estimated power: %.3f (α = .05)\n", res$power))
    cat(sprintf("Mean β₁ estimate: %.3f\n", res$mean_b1))
    cat(sprintf("\nTrue parameters: β₁ = %.2f, σ = %.2f, n = %d, replicates = %d\n",
                input$sim_b1, input$sim_sigma, input$sim_n, as.integer(input$sim_reps)))
  })
  
  hms_filtered <- reactive({
    req(hms_df)
    d <- hms_df
    if (!identical(input$hms_gender, "All")) d <- d %>% filter(gender == input$hms_gender)
    if ("age" %in% names(d)) d <- d %>% filter(!is.na(age), age >= input$hms_age[1], age <= input$hms_age[2])
    if (!("__all__" %in% input$hms_ai) && length(input$hms_ai) > 0) d <- d %>% filter(ai_use_freq_label %in% input$hms_ai)
    d
  })
  
  hms_data <- reactive({
    d <- hms_filtered()
    oc <- hms_outcomes[[input$hms_outcome]]
    d <- d %>% filter(!is.na(.data[[oc$var]]))
    d
  })
  
  output$hms_outcome_help <- renderUI({
    oc <- hms_outcomes[[input$hms_outcome]]
    helpText(oc$help)
  })
  
  output$hms_title <- renderText({
    oc <- hms_outcomes[[input$hms_outcome]]
    paste0(oc$label, " vs. Daily Tech Use")
  })
  
  output$hms_kpis <- renderUI({
    req(hms_df)
    d <- hms_data()
    oc <- hms_outcomes[[input$hms_outcome]]
    if (nrow(d) == 0) return(metric_box("Sample Size", "0", "Adjust filters"))
    mean_val <- mean(d[[oc$var]], na.rm = TRUE)
    mean_str <- if (oc$kind == "binary") paste0(round(100 * mean_val, 1), "%") else format(round(mean_val, 2), nsmall = 2)
    r_val <- suppressWarnings(tryCatch(cor(d$tech_engagement_level, d[[oc$var]], use = "complete.obs"), error = function(e) NA_real_))
    r_str <- if (is.na(r_val)) "—" else sprintf("%.3f", r_val)
    tagList(
      metric_box("Sample Size", format(nrow(d), big.mark = ","), sub = sprintf("of %s total", format(nrow(hms_df), big.mark = ","))),
      metric_box("Mean Outcome", mean_str, sub = oc$label),
      metric_box("Pearson r", r_str, sub = "Tech vs. outcome (raw)"),
      metric_box("Gender Distribution", {g <- table(d$gender); paste0(names(g), ": ", format(unname(g), big.mark = ","), collapse = " | ")}, sub = "Filtered data")
    )
  })
  
  output$hms_plot <- renderPlotly({
    if (is.null(hms_df)) return(plotly_empty() %>% layout(title = "Data file not found."))
    d <- hms_data()
    oc <- hms_outcomes[[input$hms_outcome]]
    validate(need(nrow(d) > 0, "No data match current filters."))
    
    if (input$hms_plot_type == "box") {
      p <- ggplot(d, aes(x = usage_cat, y = .data[[oc$var]], fill = usage_cat)) +
        geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.5) +
        scale_fill_brewer(palette = "Greys", guide = "none") +
        labs(x = "Daily usage (hours)", y = oc$label, title = sprintf("n = %s", format(nrow(d), big.mark = ","))) +
        academic_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$hms_plot_type == "scatter") {
      p <- ggplot(d, aes(x = tech_engagement_level, y = .data[[oc$var]])) +
        geom_jitter(alpha = 0.15, width = 0.25, height = 0.15, size = 0.8, color = oc$color) +
        geom_smooth(method = if (oc$kind == "binary") "glm" else "lm",
                    method.args = if (oc$kind == "binary") list(family = binomial) else list(),
                    se = TRUE, color = "#1a3a52", linewidth = 0.9, fill = oc$color, alpha = 0.1) +
        scale_x_continuous(breaks = 1:8, labels = usage_labels) +
        labs(x = "Daily internet/social media use", y = oc$label,
             title = sprintf("n = %s  |  %s", format(nrow(d), big.mark = ","), if (oc$kind == "binary") "logistic" else "linear")) +
        academic_theme +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))
    } else {
      agg <- d %>% group_by(usage_cat) %>%
        summarise(n = n(), mean = mean(.data[[oc$var]], na.rm = TRUE), sd = sd(.data[[oc$var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(se = sd / sqrt(pmax(n, 1)), ci = qt(0.975, df = pmax(n - 1, 1)) * se, ylo = mean - ci, yhi = mean + ci)
      p <- ggplot(agg, aes(x = usage_cat, y = mean, fill = usage_cat)) +
        geom_col(alpha = 0.7, width = 0.6, colour = "#d8dce0", linewidth = 0.4) +
        geom_errorbar(aes(ymin = ylo, ymax = yhi), width = 0.2, linewidth = 0.5, color = "#1a3a52") +
        scale_fill_brewer(palette = "Greys", guide = "none") +
        labs(x = "Daily usage (hours)", y = paste0("Mean ", oc$label), title = sprintf("n = %s", format(nrow(d), big.mark = ","))) +
        academic_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    gg <- suppressWarnings(if (input$hms_plot_type == "bar") ggplotly(p) else ggplotly(p))
    gg %>% config(displayModeBar = "hover")
  })
  
  output$hms_model_text <- renderPrint({
    req(hms_df)
    d <- hms_data()
    oc <- hms_outcomes[[input$hms_outcome]]
    if (nrow(d) < 10) { cat("Insufficient observations for model.\n"); return(invisible()) }
    covars <- c()
    if (isTRUE(input$hms_adjust_age) && "age" %in% names(d)) covars <- c(covars, "age")
    if (isTRUE(input$hms_adjust_gender) && "gender" %in% names(d)) covars <- c(covars, "gender")
    rhs <- paste(c("tech_engagement_level", covars), collapse = " + ")
    frm <- as.formula(paste0(oc$var, " ~ ", rhs))
    if (oc$kind == "binary") {
      fit <- glm(frm, family = binomial(), data = d)
      s <- summary(fit)
      cat(sprintf("Logistic Regression\n%s\n\nN = %s | AIC = %.1f\n\n", deparse(formula(fit)), format(nrow(d), big.mark = ","), s$aic))
    } else {
      fit <- lm(frm, data = d)
      s <- summary(fit)
      cat(sprintf("Linear Regression\n%s\n\nN = %s | Adj. R² = %.4f\n\n", deparse(formula(fit)), format(nrow(d), big.mark = ","), s$adj.r.squared))
    }
    cat("Coefficients:\n")
    print(round(s$coefficients, 4))
    ci <- tryCatch(round(suppressMessages(confint(fit)), 4), error = function(e) NULL)
    if (!is.null(ci)) { cat("\n95% CI:\n"); print(ci) }
  })
  
  output$hms_desc_tbl <- renderDT({
    req(hms_df)
    d <- hms_data()
    oc <- hms_outcomes[[input$hms_outcome]]
    tbl <- d %>% group_by(usage_cat) %>%
      summarise(n = n(), Mean = round(mean(.data[[oc$var]], na.rm = TRUE), 2), SD = round(sd(.data[[oc$var]], na.rm = TRUE), 2),
                Median = round(median(.data[[oc$var]], na.rm = TRUE), 1), .groups = "drop")
    datatable(tbl, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
}

shinyApp(ui, server)
