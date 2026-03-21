library(shiny)
library(plotly)
library(dplyr)
library(readr)

# Load data (temporarily from github's final_data.csv)
url <- "https://raw.githubusercontent.com/joycegill/Advanced-Statistical-Modeling/main/data/cleaned/FINAL_DATA.csv"
raw_df <- read_csv(url, show_col_types = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(x))

# Temp small number of variables
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
  DVADM01 = "Admission Rate (%)",
  STUFACR = "Student-to-Faculty Ratio",
  RMINSTTP = "In-State Room/Board (%)",
  RMOUSTTP = "Out-of-State Room/Board (%)",
  ENRTOT = "Total Enrollment",
  AGRNT_T = "Grant Aid (Total)",
  SLO6 = "Academic Advising (Yes=1)",
  APPLFEEU = "Application Fee"
)

# Collapse controls into two plot groups for coloring/legend.
sector_from_control <- function(ctrl) ifelse(ctrl == "Public", "Public", "Private")

student_choices <- c(
  "None" = "None",
  "ACT 50th Percentile" = "ACTCM50",
  "SAT Verbal 50th Percentile" = "SATVR50",
  "SAT Math 50th Percentile" = "SATMT50",
  "Admission Rate (%)" = "DVADM01"
)

faculty_choices <- c(
  "None" = "None",
  "Student-to-Faculty Ratio" = "STUFACR",
  "Application Fee (proxy)" = "APPLFEEU"
)

resource_choices <- c(
  "None" = "None",
  "Academic Advising (Yes=1)" = "SLO6",
  "Grant Aid (Total)" = "AGRNT_T",
  "Total Enrollment" = "ENRTOT",
  "In-State Room/Board (%)" = "RMINSTTP",
  "Out-of-State Room/Board (%)" = "RMOUSTTP"
)

# UI
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
        selectInput("y_var", "Tuition type (Y)", choices = c("In-State Tuition" = "TUITION2", "Out-of-State Tuition" = "TUITION3")),
        selectInput("x1_var", "Student abilities (X1)", choices = student_choices, selected = "None"),
        selectInput("x2_var", "Faculty qualifications (X2)", choices = faculty_choices, selected = "None"),
        selectInput("x3_var", "College resources (X3)", choices = resource_choices, selected = "None"),
        numericInput("alpha", "Significance level (alpha)", value = 0.05, min = 0.001, max = 0.2, step = 0.001)
      )
    ),
    column(
      width = 6,
      plotlyOutput("tuition_plot", height = "600px"),
      uiOutput("selected_x_note")
    ),
    column(
      width = 3,
      wellPanel(h4("Model Statistics"), htmlOutput("model_stats"))
    )
  )
)

# Draw Public/Private markers with consistent styling.
add_sector_markers <- function(p, df, colors, size, show_legend, outline = NULL) {
  ok <- !is.na(df$x_plot) & !is.na(df$y_plot)
  df <- df[ok, , drop = FALSE]
  for (sect in c("Public", "Private")) {
    d <- df[df$sector == sect, , drop = FALSE]
    if (nrow(d) == 0) next
    mk <- list(color = colors[[sect]], size = size, opacity = if (size >= 12) 1 else 0.8)
    if (!is.null(outline)) mk$line <- outline
    p <- plotly::add_markers(
      p,
      data = d,
      x = ~x_plot,
      y = ~y_plot,
      text = ~ht,
      hoverinfo = "text",
      name = sect,
      legendgroup = sect,
      marker = mk,
      showlegend = show_legend
    )
  }
  p
}

# Server
server <- function(input, output, session) {
  # All selected predictors excluding none
  selected_predictors <- reactive({
    unique(Filter(function(x) x != "None", c(input$x1_var, input$x2_var, input$x3_var)))
  })

  # First selected predictor is used for x axis
  first_predictor <- reactive({
    xs <- selected_predictors()
    if (length(xs) == 0) NULL else xs[[1]]
  })

  # Selected Y and selected predictors.
  model_data <- reactive({
    y_var <- input$y_var
    x_vars <- selected_predictors()
    cols <- unique(c("INSTNM", "CONTROL", y_var, x_vars))
    dat <- app_df %>% select(all_of(cols)) %>% filter(!is.na(.data[[y_var]]), .data[[y_var]] > 0)
    if (length(x_vars) > 0) dat <- dat %>% filter(if_all(all_of(x_vars), ~ !is.na(.)))
    dat
  })

  # Multivariate model: Y ~ selected predictors.
  model_fit <- reactive({
    dat <- model_data()
    y_var <- input$y_var
    x_vars <- selected_predictors()
    if (length(x_vars) == 0 || nrow(dat) < 25) return(NULL)
    stats::lm(as.formula(paste(y_var, "~", paste(x_vars, collapse = " + "))), data = dat)
  })

  # School-level x/y values + sector + search highlighting.
  plot_data <- reactive({
    dat <- model_data()
    fit <- model_fit()
    x_vars <- selected_predictors()
    y_var <- input$y_var
    x1 <- first_predictor()
    q <- trimws(tolower(input$school_search))

    if (length(x_vars) == 0 || is.null(fit) || is.null(x1)) {
      return(dat %>% mutate(
        x_plot = NA_real_,
        y_plot = NA_real_,
        sector = sector_from_control(CONTROL),
        is_match = FALSE
      ))
    }

    dat$x_plot <- dat[[x1]]
    dat$y_plot <- dat[[y_var]]
    dat$sector <- sector_from_control(dat$CONTROL)
    dat$is_match <- nchar(q) > 0 & grepl(q, tolower(dat$INSTNM), fixed = TRUE)
    dat
  })

  # Keep y-axis stable at 0-100k.
  # Recompute x-axis from whichever predictor is currently on the x-axis.
  axis_limits <- reactive({
    # first_predictor() is the first non-None choice among X1/X2/X3.
    x1 <- first_predictor()
    # model_data() is the complete-case data used for the model.
    dat <- model_data()

    # Fallback range when there is no usable x variable yet.
    if (is.null(x1) || !(x1 %in% names(dat)) || nrow(dat) == 0) {
      x_rng <- c(0, 1)
    } else {
      # Use finite values only so NA does not break axis calculations.
      x_vals <- dat[[x1]]
      x_vals <- x_vals[is.finite(x_vals)]
      if (length(x_vals) == 0) {
        x_rng <- c(0, 1)
      } else {
        # Data-driven x range for best display.
        x_min <- min(x_vals)
        x_max <- max(x_vals)
        span <- x_max - x_min
        # Add a small pad so points/line at edges are not clipped.
        pad <- if (is.finite(span) && span > 0) 0.03 * span else 1
        x_rng <- c(x_min - pad, x_max + pad)
      }
    }

    list(x_range = x_rng)
  })

  # X axis label listing currently selected predictors.
  output$selected_x_note <- renderUI({
    x_vars <- selected_predictors()
    if (length(x_vars) == 0) return(NULL)
    labs <- vapply(x_vars, function(v) pretty_names[[v]], character(1))
    div(
      style = "margin-top:8px; color:#555;",
      HTML(paste0("<em>Selected predictors: ", paste(labs, collapse = ", "), "</em>"))
    )
  })

  # Main multivariate plot
  output$tuition_plot <- renderPlotly({
    dat <- plot_data()
    if (nrow(dat) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected options"))
    }

    # Axis limits come from the selected x and fixed y 
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
    # Base points and search-highlight points share sector colors.
    if (nrow(base_dat) > 0) p <- add_sector_markers(p, base_dat, colors, 7, TRUE, NULL)
    if (nrow(match_dat) > 0) {
      p <- add_sector_markers(p, match_dat, colors, 12, FALSE, list(color = "black", width = 2))
    }

    fit <- model_fit()
    x_vars <- selected_predictors()
    x1 <- first_predictor()
    if (!is.null(fit) && length(x_vars) >= 1 && !is.null(x1)) {
      # For a 2D line from a multivariate model, vary x1 and hold others at medians.
      md <- model_data()
      meds <- vapply(md[x_vars], stats::median, na.rm = TRUE, FUN.VALUE = numeric(1))
      # Build x grid across the current x-axis display range.
      x_seq <- seq(lims$x_range[1], lims$x_range[2], length.out = 100)
      newdata <- as.data.frame(lapply(meds, rep, length(x_seq)))
      names(newdata) <- x_vars
      newdata[[x1]] <- x_seq
      line_df <- data.frame(x_plot = x_seq, y_hat = as.numeric(predict(fit, newdata = newdata)))
      p <- p %>% add_lines(
        data = line_df,
        x = ~x_plot,
        y = ~y_hat,
        line = list(color = "#1f77b4", width = 2),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    }

    p %>% layout(
      xaxis = list(title = "", range = lims$x_range, fixedrange = FALSE, autorange = FALSE),
      yaxis = list(title = y_title, range = c(0, 100000), dtick = 20000, fixedrange = TRUE),
      margin = list(l = 70, r = 30, t = 20, b = 60),
      legend = list(orientation = "h", y = -0.12, x = 0.5, xanchor = "center"),
      showlegend = TRUE
    )
  })

  # Right panel: summary table of model statistics
  output$model_stats <- renderUI({
    fit <- model_fit()
    x_vars <- selected_predictors()
    a <- input$alpha

    if (length(x_vars) == 0) return(HTML("Select at least one X predictor to fit a model."))
    if (is.null(fit)) return(HTML("Not enough complete rows to fit a model."))

    s <- summary(fit)
    coefs <- s$coefficients
    pred <- setdiff(rownames(coefs), "(Intercept)")

    eq <- paste0(
      pretty_names[[input$y_var]], " = ", round(coefs["(Intercept)", "Estimate"], 3),
      paste0(" + (", round(coefs[pred, "Estimate"], 3), " * ", pred, ")", collapse = "")
    )

    p_lines <- vapply(pred, function(v) {
      pv <- coefs[v, "Pr(>|t|)"]
      paste0(v, ": p = ", formatC(pv, format = "e", digits = 2),
             " (", if (pv < a) "Significant" else "Not significant", ")")
    }, character(1))

    fs <- s$fstatistic
    n_obs <- stats::nobs(fit)
    if (!is.null(fs) && all(c("value", "numdf", "dendf") %in% names(fs))) {
      # Overall model significance test (H0: all slopes are 0).
      f_p <- stats::pf(as.numeric(fs["value"]), as.numeric(fs["numdf"]), as.numeric(fs["dendf"]), lower.tail = FALSE)
      f_line <- sprintf(
        "F = %.2f on %.0f and %.0f df (n = %s)",
        as.numeric(fs["value"]), as.numeric(fs["numdf"]), as.numeric(fs["dendf"]),
        format(n_obs, big.mark = ",")
      )
    } else {
      f_p <- NA_real_
      f_line <- "F-statistic not available"
    }

    HTML(paste(
      paste0("<b>Equation</b><br>", eq),
      paste0("<br><br><b>Predictor p-values</b><br>", paste(p_lines, collapse = "<br>")),
      paste0("<br><br><b>Overall model (omnibus F-test)</b><br>", f_line),
      paste0("<br>p-value: ", if (is.na(f_p)) "NA" else formatC(f_p, format = "e", digits = 2)),
      paste0("<br><br><b>R-squared</b><br>", round(s$r.squared, 4)),
      paste0("<br><b>Adjusted R-squared</b><br>", round(s$adj.r.squared, 4)),
      sep = ""
    ))
  })
}

shinyApp(ui = ui, server = server)
