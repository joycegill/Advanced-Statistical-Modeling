library(shiny)
library(plotly)
library(dplyr)
library(readr)

# Load cleaned data from GitHub
url <- "https://raw.githubusercontent.com/joycegill/Advanced-Statistical-Modeling/main/data/cleaned/FINAL_DATA.csv"
raw_df <- read_csv(url, show_col_types = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(x))

# log(x) for x > 0 only (else NA)
safe_log_pos <- function(x) {
  xn <- to_num(x)
  out <- rep(NA_real_, length(xn))
  ok <- !is.na(xn) & xn > 0
  out[ok] <- log(xn[ok])
  out
}

# Percent admitted (DVADM01): IPEDS often stores 0 when there is no first-time admit count
dvadm01_clean <- function(dvadm, admssn) {
  dvp <- to_num(dvadm)
  adm <- to_num(admssn)
  no_pool <- is.na(adm) | adm <= 0
  dvp[no_pool] <- NA_real_
  dvp
}

# 6-year bachelor's grad rate (GBA6RTT): IPEDS sometimes stores 0 when no rate is applicable
# or not reported (not a literal 0% completion). Also not defined without a first-time UG cohort.
gba6rtt_clean <- function(gba6, efug1st) {
  g <- to_num(gba6)
  ft <- to_num(efug1st)
  bad <- is.na(ft) | ft <= 0 | (!is.na(g) & g <= 0)
  g[bad] <- NA_real_
  g
}

app_df <- raw_df %>%
  transmute(
    INSTNM = as.character(INSTNM),
    CONTROL = as.character(CONTROL),
    TUITION2 = to_num(TUITION2),
    TUITION3 = to_num(TUITION3),
    # Enrollment & composition (log transforms)
    log_ENRTOT = safe_log_pos(ENRTOT),
    log_EFUG1ST = safe_log_pos(EFUG1ST),
    log_EFUGCNT = safe_log_pos(EFUGCNT),
    log_EFASIAT = safe_log_pos(EFASIAT),
    log_EFHISPT = safe_log_pos(EFHISPT),
    log_EFWHITT = safe_log_pos(EFWHITT),
    log_EFNHPIT = safe_log_pos(EFNHPIT),
    # Selectivity / outcomes
    # Graduation/admission/room-board percents: IPEDS stores as 0-100 (not 0-1).
    GBA6RTT = if ("EFUG1ST" %in% names(raw_df)) gba6rtt_clean(GBA6RTT, EFUG1ST) else to_num(GBA6RTT),
    GRRTM = to_num(GRRTM),
    ACTCM50 = to_num(ACTCM50),
    SATVR50 = to_num(SATVR50),
    SATMT50 = to_num(SATMT50),
    DVADM01 = if ("ADMSSN" %in% names(raw_df)) dvadm01_clean(DVADM01, ADMSSN) else to_num(DVADM01),
    # Campus / resources
    sqrt_STUFACR = {
      s <- to_num(STUFACR)
      out <- rep(NA_real_, length(s))
      ok <- !is.na(s) & s >= 0
      out[ok] <- sqrt(s[ok])
      out
    },
    RMINSTTP = to_num(RMINSTTP),
    RMOUSTTP = to_num(RMOUSTTP),
    SLO6_YES = as.integer(!is.na(SLO6) & as.character(SLO6) == "Yes"),
    # Costs / aid (log transforms)
    log_AGRNT_N = safe_log_pos(AGRNT_N),
    log_AGRNT_T = safe_log_pos(AGRNT_T),
    log_UDGPGRNTN = safe_log_pos(UDGPGRNTN),
    log_UFLOANN = safe_log_pos(UFLOANN),
    APPLFEEU = to_num(APPLFEEU),
    # Institution profile (dummies)
    HBCU_YES = as.integer(!is.na(HBCU) & as.character(HBCU) == "Yes"),
    RELAFFIL_NO = as.integer(!is.na(RELAFFIL) & as.character(RELAFFIL) == "Not applicable"),
    ASSOC1_YES = as.integer(!is.na(ASSOC1) & as.character(ASSOC1) == "Yes"),
    RSCH_HIGH = as.integer(!is.na(CARNEGIERSCH) & as.character(CARNEGIERSCH) %in% c(
      "Research 1: Very High Spending and Doctorate Production",
      "Research 2: High Spending and Doctorate Production"
    )),
    RSCH_INSTS = as.integer(!is.na(CARNEGIERSCH) & as.character(CARNEGIERSCH) == "Research Colleges and Universities"),
    MIXED = as.integer(!is.na(CARNEGIEAPM) & as.character(CARNEGIEAPM) == "Mixed"),
    HEALTH = as.integer(!is.na(CARNEGIEAPM) & as.character(CARNEGIEAPM) %in% c(
      "Special Focus: Nursing",
      "Special Focus: Other Health Professions",
      "Special Focus: Medical Schools and Centers"
    )),
    PROFESSIONS = as.integer(!is.na(CARNEGIEAPM) & as.character(CARNEGIEAPM) == "Professions-focused"),
    ARTS = as.integer(!is.na(CARNEGIEAPM) & as.character(CARNEGIEAPM) %in% c(
      "Special Focus: Arts and Sciences",
      "Special Focus: Arts, Music, and Design"
    )),
    STEM = as.integer(!is.na(CARNEGIEAPM) & as.character(CARNEGIEAPM) == "Special Focus: Technology, Engineering, and Sciences"),
    FORPROFIT = as.integer(CONTROL == "Private for-profit")
  )

pretty_names <- c(
  TUITION2 = "In-State Tuition",
  TUITION3 = "Out-of-State Tuition",
  log_ENRTOT = "log(Total enrollment)",
  log_EFUG1ST = "log(First-time UG enrollment)",
  log_EFUGCNT = "log(UG degree-seeking count)",
  log_EFASIAT = "log(Asian enrollment)",
  log_EFHISPT = "log(Hispanic enrollment)",
  log_EFWHITT = "log(White enrollment)",
  log_EFNHPIT = "log(Native Hawaiian / Pacific Islander enrollment)",
  GBA6RTT = "6-year grad rate",
  GRRTM = "Men's grad rate (IPEDS %, 0-100)",
  ACTCM50 = "ACT 50th percentile",
  SATVR50 = "SAT Verbal 50th percentile",
  SATMT50 = "SAT Math 50th percentile",
  DVADM01 = "Admission rate",
  sqrt_STUFACR = "sqrt(Student-faculty ratio)",
  RMINSTTP = "In-state room/board (%)",
  RMOUSTTP = "Out-of-state room/board (%)",
  SLO6_YES = "Academic advising (SLO6, 1 = Yes)",
  log_AGRNT_N = "log(Grant recipients count)",
  log_AGRNT_T = "log(Grant aid total)",
  log_UDGPGRNTN = "log(UG Pell recipients)",
  log_UFLOANN = "log(Federal loan recipients)",
  APPLFEEU = "Application fee",
  HBCU_YES = "HBCU (1 = Yes)",
  RELAFFIL_NO = "No religious affiliation (1 = N/A)",
  ASSOC1_YES = "NCAA member (1 = Yes)",
  RSCH_HIGH = "Carnegie R1/R2 research (1 = Yes)",
  RSCH_INSTS = "Carnegie research institutions (1 = Yes)",
  MIXED = "Carnegie program mix: Mixed (1 = Yes)",
  HEALTH = "Carnegie health focus (1 = Yes)",
  PROFESSIONS = "Carnegie professions-focused (1 = Yes)",
  ARTS = "Carnegie arts focus (1 = Yes)",
  STEM = "Carnegie STEM focus (1 = Yes)",
  FORPROFIT = "For-profit institution (1 = Yes)"
)

enrollment_choices <- c(
  "log(Total Enrollment)" = "log_ENRTOT",
  "log(First-time UG Enrollment)" = "log_EFUG1ST",
  "log(UG Degree-Seeking Count)" = "log_EFUGCNT",
  "log(Asian Enrollment)" = "log_EFASIAT",
  "log(Hispanic Enrollment)" = "log_EFHISPT",
  "log(White Enrollment)" = "log_EFWHITT",
  "log(NHPI Enrollment)" = "log_EFNHPIT"
)

selectivity_choices <- c(
  "6 Year Graduation Rate" = "GBA6RTT",
  "Graduation Rate (Men) (GRRTM)" = "GRRTM",
  "ACT 50th Percentile" = "ACTCM50",
  "SAT Verbal 50th" = "SATVR50",
  "SAT Math 50th" = "SATMT50",
  "Admission Rate" = "DVADM01"
)

campus_choices <- c(
  "sqrt(Student-Faculty Ratio)" = "sqrt_STUFACR",
  "In-State Room/Board (%)" = "RMINSTTP",
  "Out-of-State Room/Board (%)" = "RMOUSTTP",
  "Academic Advising (Yes=1)" = "SLO6_YES"
)

costs_choices <- c(
  "log(Grant Recipients)" = "log_AGRNT_N",
  "log(Grant Aid Total)" = "log_AGRNT_T",
  "log(UG Pell Recipients)" = "log_UDGPGRNTN",
  "log(Federal Loan Recipients)" = "log_UFLOANN",
  "Application Fee" = "APPLFEEU"
)

profile_choices <- c(
  "HBCU (1=Yes)" = "HBCU_YES",
  "No Religious Affiliation (1=N/A)" = "RELAFFIL_NO",
  "NCAA Member (1=Yes)" = "ASSOC1_YES",
  "Carnegie R1/R2" = "RSCH_HIGH",
  "Carnegie Research Colleges" = "RSCH_INSTS",
  "Mixed Programs" = "MIXED",
  "Health Programs" = "HEALTH",
  "Professional Programs" = "PROFESSIONS",
  "Arts Programs" = "ARTS",
  "STEM Programs" = "STEM",
  "For-Profit" = "FORPROFIT"
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

# Predictors whose values are already on a 0-100 percentage scale (IPEDS)
vars_ipeds_percent_scale <- c("GBA6RTT", "GRRTM", "DVADM01", "RMINSTTP", "RMOUSTTP")

hover_x_lines <- function(x1, xv) {
  if (is.null(x1) || !length(xv)) return(character(0))
  lab <- label_var(x1)
  if (x1 %in% vars_ipeds_percent_scale) {
    paste0(
      lab, ": ",
      ifelse(is.na(xv), "\u2014", paste0(format(xv, trim = TRUE, scientific = FALSE), "%"))
    )
  } else {
    paste0(
      lab, ": ",
      ifelse(is.na(xv), "\u2014", format(xv, big.mark = ",", trim = TRUE, scientific = FALSE))
    )
  }
}

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
          "Select predictors (X). You may choose more than one per category; log/sqrt transforms are applied in the data where indicated."
        ),
        tags$div(
          style = "color: #737373;",
          selectInput("enrollment_vars", "Enrollment & student composition", choices = enrollment_choices, multiple = TRUE),
          selectInput("selectivity_vars", "Selectivity & student outcomes", choices = selectivity_choices, multiple = TRUE),
          selectInput("campus_vars", "Campus life & resources", choices = campus_choices, multiple = TRUE),
          selectInput("costs_vars", "Costs, aid & debt", choices = costs_choices, multiple = TRUE),
          selectInput("profile_vars", "Institution profile", choices = profile_choices, multiple = TRUE)
        ),
        selectInput("x_axis_var", "Select predictor for X axis", choices = character(0)),
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
      tabsetPanel(
        id = "main_plot_tabs",
        type = "tabs",
        tabPanel(
          "General",
          plotlyOutput("tuition_plot", height = "600px")
        ),
        tabPanel(
          "Residuals",
          plotOutput("residual_plots", height = "520px")
        )
      ),
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
      autorange = FALSE
    ),
    yaxis = list(
      title = y_title,
      range = lims$y_range,
      fixedrange = FALSE,
      autorange = FALSE
    ),
    margin = list(l = 70, r = 30, t = 20, b = 60),
    dragmode = "zoom",
    legend = list(orientation = "h", y = -0.12, x = 0.5, xanchor = "center"),
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

# Keep x-axis dropdown selection when choices update; else fall back to b
`%||%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(a[1])) a else b

server <- function(input, output, session) {
  # Combined predictor list from all category multi-selects
  selected_predictors <- reactive({
    unique(c(
      null_chr(input$enrollment_vars),
      null_chr(input$selectivity_vars),
      null_chr(input$campus_vars),
      null_chr(input$costs_vars),
      null_chr(input$profile_vars)
    ))
  })

  # X-axis dropdown: only current predictors
  observe({
    xs <- selected_predictors()
    if (length(xs) == 0) {
      updateSelectInput(session, "x_axis_var", choices = character(0))
    } else {
      named <- setNames(xs, label_var(xs))
      sel <- input$x_axis_var %||% xs[[1]]
      if (!sel %in% xs) sel <- xs[[1]]
      updateSelectInput(session, "x_axis_var", choices = named, selected = sel)
    }
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

  # X-axis variable from dropdown (must be one of selected_predictors)
  first_predictor <- reactive({
    xs <- selected_predictors()
    if (length(xs) == 0) return(NULL)
    xa <- input$x_axis_var
    if (!is.null(xa) && length(xa) && nzchar(xa[1]) && xa[1] %in% xs) xa[1] else xs[[1]]
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

    if (!length(x_vars) || is.null(x1)) {
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
    y_var <- input$y_var
    default <- list(x_range = c(0, 1), y_range = c(0, 1))
    if (is.null(x1) || !(x1 %in% names(dat)) || !nrow(dat) || !(y_var %in% names(dat))) return(default)
    x_vals <- dat[[x1]]
    x_vals <- x_vals[is.finite(x_vals)]
    y_vals <- dat[[y_var]]
    y_vals <- y_vals[is.finite(y_vals)]
    if (!length(x_vals) || !length(y_vals)) return(default)
    x_span <- diff(range(x_vals))
    x_pad <- if (is.finite(x_span) && x_span > 0) 0.03 * x_span else 1
    y_span <- diff(range(y_vals))
    y_pad <- if (is.finite(y_span) && y_span > 0) 0.05 * y_span else 1000
    list(x_range = c(min(x_vals) - x_pad, max(x_vals) + x_pad), y_range = c(max(0, min(y_vals) - y_pad), max(y_vals) + y_pad))
  })

  # Note under chart: model predictors and x-axis choice
  output$selected_x_note <- renderUI({
    x_vars <- selected_predictors()
    if (!length(x_vars)) return(NULL)
    x1 <- first_predictor()
    x_lab <- if (!is.null(x1)) label_var(x1) else ""
    div(
      style = "margin-top:8px; color:#555;",
      HTML(paste0(
        "<em>Model predictors: ", paste(label_var(x_vars), collapse = ", "), "</em><br>",
        "<em>X-axis: ", x_lab, " (Predictor for X axis).</em><br>",
        "<em>Open the Residuals tab for Q-Q and histogram.</em>"
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
    x1 <- first_predictor()
    xv <- if (!is.null(x1) && x1 %in% names(dat)) dat[[x1]] else rep(NA_real_, nrow(dat))
    hx <- hover_x_lines(x1, xv)
    hover <- paste0(
      "<b>", dat$INSTNM, "</b><br>",
      if (length(hx)) paste0(hx, "<br>") else "",
      y_title, ": ",
      format(dat[[y_var]], big.mark = ",", trim = TRUE, scientific = FALSE)
    )
    colors <- c(Public = "#1f77b4", Private = "#ff7f0e")

    br <- which(!dat$is_match)
    base_dat <- dat[br, , drop = FALSE]
    if (nrow(base_dat) > 0) base_dat$ht <- hover[br]
    mr <- which(dat$is_match)
    match_dat <- dat[mr, , drop = FALSE]
    if (nrow(match_dat) > 0) match_dat$ht <- hover[mr]

    p <- plot_ly(type = "scatter", mode = "markers")
    if (nrow(base_dat) > 0) p <- add_sector_markers(p, base_dat, colors, 7, TRUE, NULL)
    if (nrow(match_dat) > 0) {
      p <- add_sector_markers(p, match_dat, colors, 12, FALSE, list(color = "black", width = 2))
    }

    mf <- model_fits()
    x_vars <- selected_predictors()
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
      sect_one <- sectors_included()
      line_col <- if (length(sect_one) == 1L && sect_one[1] %in% names(colors)) {
        colors[[sect_one[1]]]
      } else {
        "#1f77b4"
      }
      p <- add_line_for_fit(p, mf$fit, md, line_col)
    } else if (identical(mf$mode, "dual")) {
      md_pub <- md[md$sector == "Public", , drop = FALSE]
      md_priv <- md[md$sector == "Private", , drop = FALSE]
      if (!is.null(mf$public)) p <- add_line_for_fit(p, mf$public, md_pub, colors["Public"])
      if (!is.null(mf$private)) p <- add_line_for_fit(p, mf$private, md_priv, colors["Private"])
    }

    tuition_plot_layout(p, lims, y_title, label_var(x1))
  })

  # Q-Q + histogram of residuals (single model or Public / Private rows)
  output$residual_plots <- renderPlot({
    mf <- model_fits()
    x_vars <- selected_predictors()
    sect <- sectors_included()

    empty_msg <- function(msg) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, msg)
    }

    if (!length(sect) || !length(x_vars)) {
      empty_msg("Select at least one sector and predictor.")
      return(invisible(NULL))
    }
    if (is.null(mf) || !has_valid_plot_model(mf)) {
      empty_msg("Residuals unavailable (model did not fit).")
      return(invisible(NULL))
    }

    draw_qq_hist <- function(res, qq_main, hist_main, fill_col) {
      stats::qqnorm(res, main = qq_main, ylab = "Sample quantiles")
      stats::qqline(res)
      graphics::hist(
        res,
        main = hist_main,
        xlab = "Residuals",
        col = grDevices::adjustcolor(fill_col, alpha.f = 0.45),
        border = "white",
        breaks = "Sturges"
      )
    }

    if (identical(mf$mode, "single") && !is.null(mf$fit)) {
      graphics::par(mfrow = c(1, 2), mgp = c(2, 0.7, 0), mar = c(4, 4, 3, 1))
      res <- stats::residuals(mf$fit)
      ynm <- pretty_names[[input$y_var]]
      draw_qq_hist(res, paste0("Normal Q-Q (", ynm, ")"), "Histogram of residuals", "#666666")
    } else if (identical(mf$mode, "dual")) {
      graphics::par(mfrow = c(2, 2), mgp = c(2, 0.7, 0), mar = c(3.5, 3.5, 2.5, 1))
      if (!is.null(mf$public)) {
        rp <- stats::residuals(mf$public)
        draw_qq_hist(rp, "Public: Normal Q-Q", "Public: histogram", "#1f77b4")
      } else {
        empty_msg("Public: no fit")
        empty_msg("")
      }
      if (!is.null(mf$private)) {
        rv <- stats::residuals(mf$private)
        draw_qq_hist(rv, "Private: Normal Q-Q", "Private: histogram", "#ff7f0e")
      } else {
        empty_msg("Private: no fit")
        empty_msg("")
      }
    }
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
    y_var <- input$y_var
    x_vars <- selected_predictors()
    y_name <- pretty_names[[y_var]]
    mf <- model_fits()
    sect <- sectors_included()
    dat <- model_data()

    if (!length(sect)) return(HTML("Select at least one sector (Public and/or Private)."))
    if (!length(x_vars)) return(HTML("Select at least one predictor from the category lists."))
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
