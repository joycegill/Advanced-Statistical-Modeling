library(shiny)
library(plotly)
library(dplyr)
library(readr)

# Load cleaned data from GitHub
url <- "https://raw.githubusercontent.com/joycegill/Advanced-Statistical-Modeling/main/data/cleaned/FINAL_DATA.csv"
raw_df <- read_csv(url, show_col_types = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(x))

app_df <- raw_df %>%
  transmute(
    INSTNM = as.character(INSTNM),
    CONTROL = as.character(CONTROL),
    TUITION2 = to_num(TUITION2),
    TUITION3 = to_num(TUITION3),
    ACTCM50 = to_num(ACTCM50),
    SATVR50 = to_num(SATVR50),
    SATMT50 = to_num(SATMT50),
    DVADM01 = to_num(DVADM01),
    STUFACR = to_num(STUFACR),
    RMINSTTP = to_num(RMINSTTP),
    RMOUSTTP = to_num(RMOUSTTP),
    ENRTOT = to_num(ENRTOT),
    AGRNT_T = to_num(AGRNT_T),
    SLO6 = ifelse(SLO6 == "Yes", 1, 0),
    APPLFEEU = to_num(APPLFEEU)
  )

pretty_names <- c(
  TUITION2 = "In-State Tuition",
  TUITION3 = "Out-of-State Tuition",
  ACTCM50 = "ACT 50th Percentile",
  SATVR50 = "SAT Verbal 50th Percentile",
  SATMT50 = "SAT Math 50th Percentile",
  DVADM01 = "Admission Rate",
  STUFACR = "Student-to-Faculty Ratio",
  RMINSTTP = "In-State Room/Board",
  RMOUSTTP = "Out-of-State Room/Board",
  ENRTOT = "Total Enrollment",
  AGRNT_T = "Grant Aid (Total)",
  SLO6 = "Academic Advising",
  APPLFEEU = "Application Fee"
)

label_var <- function(v) {
  if (!length(v)) return(character(0))
  ifelse(v %in% names(pretty_names), pretty_names[v], v)
}

sig_stars <- function(p) {
  vapply(p, function(pv) {
    if (length(pv) != 1L || is.na(pv)) return("")
    if (pv < 0.001) return("***")
    if (pv < 0.01) return("**")
    if (pv < 0.05) return("*")
    if (pv < 0.1) return(".")
    ""
  }, character(1))
}

# Map CONTROL values to two display groups
sector_from_control <- function(ctrl) ifelse(ctrl == "Public", "Public", "Private")

student_choices <- c(
  "ACT 50th Percentile" = "ACTCM50",
  "SAT Verbal 50th Percentile" = "SATVR50",
  "SAT Math 50th Percentile" = "SATMT50",
  "Admission Rate (%)" = "DVADM01"
)

faculty_choices <- c(
  "Student-to-Faculty Ratio" = "STUFACR",
  "Application Fee (proxy)" = "APPLFEEU"
)

resource_choices <- c(
  "Academic Advising (Yes=1)" = "SLO6",
  "Grant Aid (Total)" = "AGRNT_T",
  "Total Enrollment" = "ENRTOT",
  "In-State Room/Board (%)" = "RMINSTTP",
  "Out-of-State Room/Board (%)" = "RMOUSTTP"
)

MIN_N_MODEL <- 25L
MIN_N_MODEL_SECTOR <- 15L

ui <- fluidPage(
  titlePanel(
    title = div(style = "text-align:center;", "Interactive Visualizations (Tuition)"),
    windowTitle = "Interactive Visualizations (Tuition)"
  ),
  fluidRow(
    column(
      width = 12,
      div(
        style = "max-width:420px; margin: 0 auto 12px auto;",
        textInput("school_search", "Search school name", placeholder = "Type part of a school name...")
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        tags$p(
          style = "font-weight: bold; margin-bottom: 8px;",
          "Select predictors (X). You may select more than one option in each category."
        ),
        tags$div(
          style = "color: #737373;",
          selectInput("x1_vars", "Student abilities (X1)", choices = student_choices, multiple = TRUE),
          selectInput("x2_vars", "Faculty qualifications (X2)", choices = faculty_choices, multiple = TRUE),
          selectInput("x3_vars", "College resources (X3)", choices = resource_choices, multiple = TRUE)
        ),
        selectInput("y_var", "Select tuition type (Y)", choices = c("In-State Tuition" = "TUITION2", "Out-of-State Tuition" = "TUITION3")),
        checkboxGroupInput(
          "sector_inc",
          "Select sector",
          choices = c("Public" = "Public", "Private" = "Private"),
          selected = c("Public", "Private"),
          inline = TRUE
        )
      )
    ),
    column(
      width = 6,
      plotlyOutput("tuition_plot", height = "600px"),
      uiOutput("selected_x_note")
    ),
    column(
      width = 3,
      wellPanel(h4("Model Statistics"), uiOutput("model_stats"))
    )
  )
)

# Add marker traces split by sector (Public / Private)
add_sector_markers <- function(p, df, colors, size, show_legend, outline = NULL) {
  df <- df[!is.na(df$x_plot) & !is.na(df$y_plot), , drop = FALSE]
  for (sect in c("Public", "Private")) {
    d <- df[df$sector == sect, , drop = FALSE]
    if (nrow(d) == 0) next
    mk <- list(color = colors[[sect]], size = size, opacity = if (size >= 12) 1 else 0.8)
    if (!is.null(outline)) mk$line <- outline
    p <- plotly::add_markers(
      p, data = d, x = ~x_plot, y = ~y_plot, text = ~ht, hoverinfo = "text",
      name = sect, legendgroup = sect, marker = mk, showlegend = show_legend
    )
  }
  p
}

# Shared chart layout + plotly controls
tuition_plot_layout <- function(p, lims, y_title, x_title) {
  p %>% layout(
    xaxis = list(
      title = x_title,
      range = lims$x_range,
      fixedrange = FALSE,
      autorange = FALSE,
      rangeslider = list(visible = TRUE, thickness = 0.08)
    ),
    yaxis = list(
      title = y_title,
      range = c(0, 100000),
      dtick = 20000,
      fixedrange = FALSE
    ),
    margin = list(l = 70, r = 30, t = 20, b = 100),
    dragmode = "zoom",
    legend = list(orientation = "h", y = -0.30, x = 0.5, xanchor = "center"),
    showlegend = TRUE
  ) %>%
    plotly::config(scrollZoom = TRUE, displayModeBar = TRUE)
}

# Safely fit lm only when there is enough data
fit_lm_safe <- function(y_var, x_vars, dat, min_n) {
  if (length(x_vars) == 0 || nrow(dat) < min_n) return(NULL)
  stats::lm(as.formula(paste(y_var, "~", paste(x_vars, collapse = " + "))), data = dat)
}

# Multi-select inputs can be NULL; treat as no selection
null_chr <- function(x) if (is.null(x)) character(0) else x

server <- function(input, output, session) {
  # Combined predictor list from X1 / X2 / X3
  selected_predictors <- reactive({
    unique(c(null_chr(input$x1_vars), null_chr(input$x2_vars), null_chr(input$x3_vars)))
  })

  # Public/private check box
  sectors_included <- reactive({
    si <- input$sector_inc
    if (is.null(si) || !length(si)) character(0) else si
  })

  # Keep complete cases for Y and selected X columns
  model_data_base <- reactive({
    y_var <- input$y_var
    x_vars <- selected_predictors()
    cols <- unique(c("INSTNM", "CONTROL", y_var, x_vars))
    dat <- app_df %>% select(all_of(cols)) %>% filter(!is.na(.data[[y_var]]), .data[[y_var]] > 0)
    if (length(x_vars) > 0) dat <- dat %>% filter(if_all(all_of(x_vars), ~ !is.na(.)))
    dat
  })

  # Restrict rows to selected sector(s)
  model_data <- reactive({
    dat <- model_data_base()
    sect <- sectors_included()
    if (!length(sect)) return(dat[0, , drop = FALSE])
    dat %>%
      mutate(sector = sector_from_control(CONTROL)) %>%
      filter(sector %in% sect)
  })

  # Fit one model (single sector) or two models (Public + Private)
  model_fits <- reactive({
    dat <- model_data()
    y_var <- input$y_var
    x_vars <- selected_predictors()
    sect <- sectors_included()
    if (!length(x_vars) || !length(sect)) return(NULL)

    if (length(sect) == 1) {
      return(list(mode = "single", fit = fit_lm_safe(y_var, x_vars, dat, MIN_N_MODEL)))
    }
    d_pub <- dat[dat$sector == "Public", , drop = FALSE]
    d_priv <- dat[dat$sector == "Private", , drop = FALSE]
    list(
      mode = "dual",
      public = fit_lm_safe(y_var, x_vars, d_pub, MIN_N_MODEL_SECTOR),
      private = fit_lm_safe(y_var, x_vars, d_priv, MIN_N_MODEL_SECTOR)
    )
  })

  # Current x-axis variable (first selected predictor)
  first_predictor <- reactive({
    xs <- selected_predictors()
    if (length(xs) == 0) NULL else xs[[1]]
  })

  # Check whether at least one model was fit successfully
  has_valid_plot_model <- function(mf) {
    !is.null(mf) && (
      (identical(mf$mode, "single") && !is.null(mf$fit)) ||
      (identical(mf$mode, "dual") && (!is.null(mf$public) || !is.null(mf$private)))
    )
  }

  # Build plotting data (x, y, sector, search highlight)
  plot_data <- reactive({
    dat <- model_data()
    x_vars <- selected_predictors()
    y_var <- input$y_var
    x1 <- first_predictor()
    q <- trimws(tolower(input$school_search))
    mf <- model_fits()

    if (!length(x_vars) || !has_valid_plot_model(mf) || is.null(x1)) {
      return(dat %>% mutate(
        x_plot = NA_real_,
        y_plot = NA_real_,
        sector = sector_from_control(CONTROL),
        is_match = FALSE
      ))
    }

    dat$x_plot <- dat[[x1]]
    dat$y_plot <- dat[[y_var]]
    dat$is_match <- nchar(q) > 0 & grepl(q, tolower(dat$INSTNM), fixed = TRUE)
    dat
  })

  # Compute x-axis bounds with small padding
  axis_limits <- reactive({
    x1 <- first_predictor()
    dat <- model_data()
    default <- list(x_range = c(0, 1))
    if (is.null(x1) || !(x1 %in% names(dat)) || !nrow(dat)) return(default)
    x_vals <- dat[[x1]]
    x_vals <- x_vals[is.finite(x_vals)]
    if (!length(x_vals)) return(default)
    span <- diff(range(x_vals))
    pad <- if (is.finite(span) && span > 0) 0.03 * span else 1
    list(x_range = c(min(x_vals) - pad, max(x_vals) + pad))
  })

  # Small note under chart showing active predictors
  output$selected_x_note <- renderUI({
    x_vars <- selected_predictors()
    if (!length(x_vars)) return(NULL)
    div(
      style = "margin-top:8px; color:#555;",
      HTML(paste0(
        "<em>Selected predictors: ", paste(label_var(x_vars), collapse = ", "),
        ". Use the range slider and zoom tools to explore.</em>"
      ))
    )
  })

  # Main scatter plot + fitted line(s)
  output$tuition_plot <- renderPlotly({
    dat <- plot_data()
    if (!nrow(dat)) {
      return(plot_ly() %>% layout(title = "No data available for selected options"))
    }

    lims <- axis_limits()
    y_var <- input$y_var
    y_title <- pretty_names[[y_var]]
    hover <- paste0(
      "<b>", dat$INSTNM, "</b><br>", y_title, ": ",
      format(dat[[y_var]], big.mark = ",", trim = TRUE, scientific = FALSE)
    )
    colors <- c(Public = "#1f77b4", Private = "#ff7f0e")

    br <- which(!dat$is_match)
    base_dat <- dat[br, , drop = FALSE]
    if (nrow(base_dat) > 0) base_dat$ht <- hover[br]
    mr <- which(dat$is_match)
    match_dat <- dat[mr, , drop = FALSE]
    if (nrow(match_dat) > 0) match_dat$ht <- hover[mr]

    p <- plot_ly()
    if (nrow(base_dat) > 0) p <- add_sector_markers(p, base_dat, colors, 7, TRUE, NULL)
    if (nrow(match_dat) > 0) {
      p <- add_sector_markers(p, match_dat, colors, 12, FALSE, list(color = "black", width = 2))
    }

    mf <- model_fits()
    x_vars <- selected_predictors()
    x1 <- first_predictor()
    md <- model_data()

    if (length(x_vars) < 1 || is.null(x1) || !nrow(md) || is.null(mf)) {
      x_title <- if (!is.null(x1)) label_var(x1) else ""
      return(tuition_plot_layout(p, lims, y_title, x_title))
    }

    x_seq <- seq(lims$x_range[1], lims$x_range[2], length.out = 100)

    # Draw fitted line while holding non-x predictors at medians
    add_line_for_fit <- function(p, fit, med_df, line_color) {
      if (is.null(fit)) return(p)
      meds <- vapply(med_df[x_vars], stats::median, na.rm = TRUE, FUN.VALUE = numeric(1))
      newdata <- as.data.frame(lapply(meds, rep, length(x_seq)))
      names(newdata) <- x_vars
      newdata[[x1]] <- x_seq
      plotly::add_lines(
        p,
        data = data.frame(x_plot = x_seq, y_hat = as.numeric(predict(fit, newdata = newdata))),
        x = ~x_plot, y = ~y_hat,
        line = list(color = line_color, width = 2),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    }

    if (identical(mf$mode, "single") && !is.null(mf$fit)) {
      p <- add_line_for_fit(p, mf$fit, md, "#1f77b4")
    } else if (identical(mf$mode, "dual")) {
      md_pub <- md[md$sector == "Public", , drop = FALSE]
      md_priv <- md[md$sector == "Private", , drop = FALSE]
      if (!is.null(mf$public)) p <- add_line_for_fit(p, mf$public, md_pub, colors["Public"])
      if (!is.null(mf$private)) p <- add_line_for_fit(p, mf$private, md_priv, colors["Private"])
    }

    tuition_plot_layout(p, lims, y_title, label_var(x1))
  })

  # Build model summary text for the right panel
  format_model_html <- function(fit, y_name) {
    if (is.null(fit)) {
      return("<p><em>Not enough rows in this sector to fit the model.</em></p>")
    }
    s <- summary(fit)
    coefs <- s$coefficients
    pred <- setdiff(rownames(coefs), "(Intercept)")
    lbl <- label_var(pred)

    eq <- paste0(
      y_name, " = ", round(coefs["(Intercept)", "Estimate"], 3),
      paste0(" + (", round(coefs[pred, "Estimate"], 3), " × ", lbl, ")", collapse = "")
    )

    p_lines <- vapply(seq_along(pred), function(i) {
      pv <- coefs[pred[i], "Pr(>|t|)"]
      st <- sig_stars(pv)
      paste0(lbl[i], ": p = ", formatC(pv, format = "e", digits = 2), if (nzchar(st)) paste0(" ", st) else "")
    }, character(1))

    fs <- s$fstatistic
    n_obs <- stats::nobs(fit)
    if (!is.null(fs) && all(c("value", "numdf", "dendf") %in% names(fs))) {
      f_p <- stats::pf(as.numeric(fs["value"]), as.numeric(fs["numdf"]), as.numeric(fs["dendf"]), lower.tail = FALSE)
      f_st <- sig_stars(f_p)
      f_line <- sprintf(
        "F = %.2f on %.0f and %.0f df (n = %s)",
        as.numeric(fs["value"]), as.numeric(fs["numdf"]), as.numeric(fs["dendf"]),
        format(n_obs, big.mark = ",")
      )
      f_p_str <- paste0(
        if (is.na(f_p)) "NA" else formatC(f_p, format = "e", digits = 2),
        if (!is.na(f_p) && nzchar(f_st)) paste0(" ", f_st) else ""
      )
    } else {
      f_p_str <- "NA"
      f_line <- "F-statistic not available"
    }

    paste0(
      "<b>Equation</b><br>", eq,
      "<br><br><b>Predictor p-values</b><br>", paste(p_lines, collapse = "<br>"),
      "<br><br><b>Overall model</b><br>", f_line,
      "<br>p-value: ", f_p_str,
      "<br><br><b>R-squared</b><br>", round(s$r.squared, 4),
      "<br><b>Adjusted R-squared</b><br>", round(s$adj.r.squared, 4)
    )
  }

  # Significance code legend
  stats_note <- function() {
    tags$p(
      style = "font-size:0.9em;color:#555;margin-bottom:0;",
      "Signif. codes: 0 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019 0.1 \u2018 \u2019 1"
    )
  }

  # Render one summary or separate Public / Private tabs
  output$model_stats <- renderUI({
    x_vars <- selected_predictors()
    y_name <- pretty_names[[input$y_var]]
    mf <- model_fits()
    sect <- sectors_included()

    if (!length(sect)) return(HTML("Select at least one sector (Public and/or Private)."))
    if (!length(x_vars)) return(HTML("Select at least one predictor in X1, X2, or X3."))
    if (is.null(mf)) return(HTML("Not enough complete rows to fit a model."))

    if (identical(mf$mode, "single")) {
      return(tagList(HTML(format_model_html(mf$fit, y_name)), tags$br(), stats_note()))
    }

    tab_body <- function(html) tags$div(style = "margin-top: 12px;", HTML(html))
    tagList(
      tabsetPanel(
        id = "model_stats_tabs",
        type = "tabs",
        tabPanel("Public", tab_body(format_model_html(mf$public, y_name))),
        tabPanel("Private", tab_body(format_model_html(mf$private, y_name)))
      ),
      tags$br(),
      stats_note()
    )
  })
}

shinyApp(ui = ui, server = server)
