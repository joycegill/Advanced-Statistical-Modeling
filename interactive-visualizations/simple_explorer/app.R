# Simplified scatter explorer: one X, one Y (same variable pool as main app), linear fit
# + 95% CI band, and model statistics matching the main app.
library(shiny)
library(plotly)
library(dplyr)
library(readr)

url <- "https://raw.githubusercontent.com/joycegill/Advanced-Statistical-Modeling/main/data/cleaned/FINAL_DATA.csv"
raw_df <- read_csv(url, show_col_types = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(x))

safe_log_pos <- function(x) {
  xn <- to_num(x)
  out <- rep(NA_real_, length(xn))
  ok <- !is.na(xn) & xn > 0
  out[ok] <- log(xn[ok])
  out
}

dvadm01_clean <- function(dvadm, admssn) {
  dvp <- to_num(dvadm)
  adm <- to_num(admssn)
  no_pool <- is.na(adm) | adm <= 0
  dvp[no_pool] <- NA_real_
  dvp
}

gba6rtt_clean <- function(gba6, efug1st) {
  g <- to_num(gba6)
  ft <- to_num(efug1st)
  bad <- is.na(ft) | ft <= 0 | (!is.na(g) & g <= 0)
  g[bad] <- NA_real_
  g
}

# Same `transmute` as interactive-visualizations/app.R
app_df <- raw_df %>%
  transmute(
    INSTNM = as.character(INSTNM),
    CONTROL = as.character(CONTROL),
    TUITION2 = to_num(TUITION2),
    TUITION3 = to_num(TUITION3),

    # Enrollment & composition (log transforms)
    ENRTOT = to_num(ENRTOT),
    EFUG1ST = to_num(EFUG1ST),
    EFUGCNT = to_num(EFUGCNT),
    EFASIAT = to_num(EFASIAT),
    EFBKAAT = to_num(EFBKAAT),
    EFHISPT = to_num(EFHISPT),
    EFNRALT = to_num(EFNRALT),
    EFWHITT = to_num(EFWHITT),
    EFNHPIT = to_num(EFNHPIT),
    log_ENRTOT = safe_log_pos(ENRTOT),
    log_EFUG1ST = safe_log_pos(EFUG1ST),
    log_EFUGCNT = safe_log_pos(EFUGCNT),
    log_EFASIAT = safe_log_pos(EFASIAT),
    log_EFBKAAT = safe_log_pos(EFBKAAT),
    log_EFHISPT = safe_log_pos(EFHISPT),
    log_EFNRALT = safe_log_pos(EFNRALT),
    log_EFWHITT = safe_log_pos(EFWHITT),
    log_EFNHPIT = safe_log_pos(EFNHPIT),

    # Selectivity / outcomes
    GBA6RTT = if ("EFUG1ST" %in% names(raw_df)) gba6rtt_clean(GBA6RTT, EFUG1ST) else to_num(GBA6RTT),
    GBA4RTT = to_num(GBA4RTT),
    GRRTM = to_num(GRRTM),
    GRRTW = to_num(GRRTW),
    ACTCM50 = to_num(ACTCM50),
    SATVR50 = to_num(SATVR50),
    SATMT50 = to_num(SATMT50),
    DVADM01 = if ("ADMSSN" %in% names(raw_df)) dvadm01_clean(DVADM01, ADMSSN) else to_num(DVADM01),

    # Campus / resources
    STUFACR = to_num(STUFACR),
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
    AGRNT_N = to_num(AGRNT_N),
    AGRNT_T = to_num(AGRNT_T),
    UDGPGRNTN = to_num(UDGPGRNTN),
    UFLOANN = to_num(UFLOANN),
    log_AGRNT_N = safe_log_pos(AGRNT_N),
    log_AGRNT_T = safe_log_pos(AGRNT_T),
    log_UDGPGRNTN = safe_log_pos(UDGPGRNTN),
    log_UFLOANN = safe_log_pos(UFLOANN),
    APPLFEEU = to_num(APPLFEEU),

    # Institution profile (dummies)
    HBCU_YES = as.integer(!is.na(HBCU) & as.character(HBCU) == "Yes"),
    RELAFFIL_NO = as.integer(!is.na(RELAFFIL) & as.character(RELAFFIL)%in% c(
      "Not applicable",
      "Non-Denominational"
    )),
    RELAFFIL_YES = as.integer(
      !is.na(RELAFFIL) &
        !(as.character(RELAFFIL) %in% c(
          "Not applicable",
          "Non-Denominational"
        ))
    ),

    ATHASSOC_YES = as.integer(!is.na(ATHASSOC) & as.character(ATHASSOC) == "Yes"),
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
    FORPROFIT = as.integer(CONTROL == "Private for-profit"),

    # Locale
    LOCALE_SUBURB = as.integer(LOCALE %in% c("Suburb: Small", "Suburb: Large", "Suburb: Midsize")),
    LOCALE_TOWN = as.integer(LOCALE %in% c("Town: Distant", "Town: Remote", "Town: Fringe")),
    LOCALE_RURAL = as.integer(LOCALE %in% c("Rural: Remote", "Rural: Fringe", "Rural: Distant")),
    LOCALE_CITY = as.integer(LOCALE %in% c("City: Midsize", "City: Small", "City: Large")),

    # Region
    NORTHEAST = as.integer(OBEREG %in% c(
      "New England (CT, ME, MA, NH, RI, VT)",
      "Mid East (DE, DC, MD, NJ, NY, PA)"
    )),

    MIDWEST = as.integer(OBEREG %in% c(
      "Great Lakes (IL, IN, MI, OH, WI)",
      "Plains (IA, KS, MN, MO, NE, ND, SD)"
    )),

    SOUTH = as.integer(OBEREG %in% c(
      "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",
      "Southwest (AZ, NM, OK, TX)"
    )),

    WEST = as.integer(OBEREG %in% c(
      "Far West (AK, CA, HI, NV, OR, WA)",
      "Rocky Mountains (CO, ID, MT, UT, WY)"
    )),

    # Highest degree offered
    HDEOFR_DOC = ifelse(HDEGOFR1 %in% c("Doctor's degree - research/scholarship", "Doctor's degree - professional practice", "Doctor's degree - research/scholarship and professional practice", "Doctor's degree - other"), 1, 0),
    HDEOFR_MAS = ifelse(HDEGOFR1 %in% c("Master's degree"), 1, 0),
    HDEOFR_BAC = ifelse(HDEGOFR1 %in% c("Bachelor's degree"), 1, 0)
  )

# Same `var_meta` as interactive-visualizations/app.R (plus tuition: in app_df, not listed in main var_meta)
var_meta <- tibble::tribble(
  ~var, ~label, ~category,

  # Enrollment
  "ENRTOT", "Total Enrollment", "Enrollment & student composition",
  "EFUG1ST", "First-time UG Enrollment", "Enrollment & student composition",
  "EFUGCNT", "UG Degree-Seeking Count", "Enrollment & student composition",

  "EFASIAT", "Asian Enrollment", "Enrollment & student composition",
  "EFBKAAT", "Black/African American Enrollment", "Enrollment & student composition",
  "EFHISPT", "Hispanic Enrollment", "Enrollment & student composition",
  "EFNRALT", "International Enrollment", "Enrollment & student composition",
  "EFWHITT", "White Enrollment", "Enrollment & student composition",
  "EFNHPIT", "NHPI Enrollment", "Enrollment & student composition",

  # log transforms (still same var, just labeled)
  "log_ENRTOT", "log(Total Enrollment)", "Enrollment & student composition",
  "log_EFUG1ST", "log(First-time UG Enrollment)", "Enrollment & student composition",
  "log_EFUGCNT", "log(UG Degree-Seeking Count)", "Enrollment & student composition",
  "log_EFASIAT", "log(Asian Enrollment)", "Enrollment & student composition",
  "log_EFBKAAT", "log(Black Enrollment)", "Enrollment & student composition",
  "log_EFHISPT", "log(Hispanic Enrollment)", "Enrollment & student composition",
  "log_EFNRALT", "log(International Enrollment)", "Enrollment & student composition",
  "log_EFWHITT", "log(White Enrollment)", "Enrollment & student composition",
  "log_EFNHPIT", "log(NHPI Enrollment)", "Enrollment & student composition",

  # Selectivity
  "GBA6RTT", "6-Year Graduation Rate", "Selectivity & outcomes",
  "GBA4RTT", "4-Year Graduation Rate", "Selectivity & outcomes",
  "GRRTM", "Graduation Rate (Men)", "Selectivity & outcomes",
  "GRRTW", "Graduation Rate (Women)", "Selectivity & outcomes",
  "ACTCM50", "ACT 50th Percentile", "Selectivity & outcomes",
  "SATVR50", "SAT Verbal 50th", "Selectivity & outcomes",
  "SATMT50", "SAT Math 50th", "Selectivity & outcomes",
  "DVADM01", "Admission Rate", "Selectivity & outcomes",

  # Campus
  "STUFACR", "Student-Faculty Ratio", "Campus life & resources",
  "sqrt_STUFACR", "sqrt(Student-Faculty Ratio)", "Campus life & resources",
  "SLO6_YES", "Study Abroad (Yes=1)", "Campus life & resources",

  # Costs
  "AGRNT_N", "Grant Recipients", "Costs & aid",
  "log_AGRNT_N", "log(Grant Recipients)", "Costs & aid",
  "AGRNT_T", "Grant Aid Total", "Costs & aid",
  "log_AGRNT_T", "log(Grant Aid Total)", "Costs & aid",
  "UDGPGRNTN", "Pell Recipients", "Costs & aid",
  "log_UDGPGRNTN", "log(Pell Recipients)", "Costs & aid",
  "UFLOANN", "Federal Loan Recipients", "Costs & aid",
  "log_UFLOANN", "log(Loan Recipients)", "Costs & aid",
  "APPLFEEU", "Application Fee", "Costs & aid",

  # Profile
  "HDEOFR_DOC", "Doctor - Highest Degree (Yes=1)", "Institution profile",
  "HDEOFR_MAS", "Master - Highest Degree (Yes=1)", "Institution profile",
  "HDEOFR_BAC", "Bachelor - Highest Degree (Yes=1)", "Institution profile",
  "HBCU_YES", "HBCU (Yes=1)", "Institution profile",
  "RELAFFIL_NO", "No Religious Affiliation", "Institution profile",
  "ASSOC1_YES", "NCAA Member", "Institution profile",
  "RSCH_HIGH", "High Research (R1/R2)", "Institution profile",
  "RSCH_INSTS", "Research Institutions", "Institution profile",
  "MIXED", "Mixed Carnegie Type", "Institution profile",
  "HEALTH", "Health Focus", "Institution profile",
  "PROFESSIONS", "Professional Focus", "Institution profile",
  "ARTS", "Arts Focus", "Institution profile",
  "STEM", "STEM Focus", "Institution profile",
  "FORPROFIT", "For-Profit", "Institution profile",

  # Locale
  "LOCALE_SUBURB", "Suburban Campus", "School Location",
  "LOCALE_TOWN", "Small Town / College Town", "School Location",
  "LOCALE_RURAL", "Rural / Remote Campus", "School Location",
  "LOCALE_CITY", "Urban / City Campus", "School Location",

  # Region
  "NORTHEAST", "Northeast Region", "School Region",
  "MIDWEST", "Midwest Region", "School Region",
  "SOUTH", "Southern Region", "School Region",
  "WEST", "Western Region", "School Region",

  "RELAFFIL_YES", "Religious Affliated", "Institution profile",
  "ATHASSOC_YES", "Member of National Athletic Association", "Institution profile"
)

model_var_tbl <- var_meta %>%
  distinct(var, .keep_all = TRUE) %>%
  filter(var %in% names(app_df), !var %in% c("INSTNM", "CONTROL"))

xy_choices <- setNames(model_var_tbl$var, model_var_tbl$label)

label_var <- function(v) {
  out <- var_meta$label[match(v, var_meta$var)]
  ifelse(is.na(out), v, out)
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

MIN_N_MODEL <- 25L

ui <- fluidPage(
  titlePanel(div(style = "text-align:center;", "Simple College Explorer")),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        selectInput("x_var", "Select predictors (X)", choices = xy_choices, selected = "GBA6RTT"),
        selectInput("y_var", "Select response variables (Y)", choices = xy_choices, selected = "ENRTOT")
      )
    ),
    column(
      width = 6,
      plotlyOutput("scatter", height = "620px")
    ),
    column(
      width = 3,
      wellPanel(h4("Model Statistics"), uiOutput("model_stats"))
    )
  )
)

plot_df <- function(df, xv, yv) {
  df %>%
    mutate(
      x = .data[[xv]],
      y = .data[[yv]]
    ) %>%
    filter(is.finite(x), is.finite(y))
}

fit_lm_xy <- function(dat, xv, yv, min_n) {
  if (is.null(dat) || nrow(dat) < min_n) return(NULL)
  if (!xv %in% names(dat) || !yv %in% names(dat)) return(NULL)
  tryCatch(
    stats::lm(stats::reformulate(xv, response = yv), data = dat),
    error = function(e) NULL,
    warning = function(w) NULL
  )
}

add_lm_band <- function(p, d, xv, yv, fit = NULL, line_color = "rgba(0,0,0,0.85)", fill_color = "rgba(31,119,180,0.18)", name_prefix = "") {
  if (is.null(fit)) fit <- fit_lm_xy(d, xv, yv, MIN_N_MODEL)
  if (is.null(fit)) return(p)
  xr <- range(d[[xv]], na.rm = TRUE)
  if (!all(is.finite(xr)) || diff(xr) == 0) return(p)
  xg <- seq(xr[1], xr[2], length.out = 120)
  nd <- d[1, , drop = FALSE]
  nd <- nd[rep(1L, length(xg)), , drop = FALSE]
  nd[[xv]] <- xg
  pr <- tryCatch(
    as.data.frame(stats::predict(fit, newdata = nd, interval = "confidence", level = 0.95)),
    error = function(e) NULL
  )
  if (is.null(pr) || !all(c("lwr", "upr", "fit") %in% names(pr))) return(p)
  band <- data.frame(x = xg, fit = pr$fit, lwr = pr$lwr, upr = pr$upr)
  nm <- if (nzchar(name_prefix)) paste0(name_prefix, " 95% CI") else "Linear fit 95% CI"
  p <- plotly::add_ribbons(
    p,
    data = band,
    x = ~x, ymin = ~lwr, ymax = ~upr,
    name = nm,
    legendgroup = "lm",
    fillcolor = fill_color,
    line = list(color = "transparent"),
    hoverinfo = "skip",
    showlegend = TRUE,
    inherit = FALSE
  )
  plotly::add_lines(
    p,
    data = band,
    x = ~x, y = ~fit,
    name = if (nzchar(name_prefix)) paste0(name_prefix, " linear") else "Linear fit",
    legendgroup = "lm",
    line = list(color = line_color, width = 2.5),
    hoverinfo = "skip",
    showlegend = TRUE,
    inherit = FALSE
  )
}

format_model_html <- function(fit, y_name) {
  if (is.null(fit)) {
    return("<p><em>Not enough rows in this group to fit the model.</em></p>")
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

stats_note <- function() {
  tags$p(
    style = "font-size:0.9em;color:#555;margin-bottom:0;",
    "Signif. codes: 0 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019 0.1 \u2018 \u2019 1"
  )
}

server <- function(input, output, session) {
  plot_data <- reactive({
    xv <- input$x_var
    yv <- input$y_var
    plot_df(app_df, xv, yv)
  })

  model_fit <- reactive({
    d <- plot_data()
    xv <- input$x_var
    yv <- input$y_var
    ynm <- label_var(yv)

    if (identical(xv, yv)) return(NULL)
    if (!nrow(d)) return(NULL)

    list(fit = fit_lm_xy(d, xv, yv, MIN_N_MODEL), y_name = ynm)
  })

  output$scatter <- renderPlotly({
    xv <- input$x_var
    yv <- input$y_var

    if (is.null(xv) || is.null(yv) || !nzchar(xv) || !nzchar(yv)) {
      return(plot_ly() %>% layout(title = "Choose X and Y"))
    }
    if (identical(xv, yv)) {
      return(plot_ly() %>% layout(title = "X and Y must be different variables"))
    }

    d <- plot_data()
    if (nrow(d) < 3) {
      return(plot_ly() %>% layout(title = "Not enough complete data for this combination"))
    }

    ttl <- paste0(label_var(yv), " vs ", label_var(xv))
    p <- plot_ly(type = "scatter", mode = "markers")
    p <- plotly::add_markers(
      p,
      data = d,
      x = d[[xv]],
      y = d[[yv]],
      text = d$INSTNM,
      name = "Schools",
      marker = list(size = 8, opacity = 0.75, color = "#1f77b4"),
      hoverinfo = "text+x+y",
      inherit = FALSE
    )
    mf <- model_fit()
    p <- add_lm_band(p, d, xv, yv, fit = if (!is.null(mf)) mf$fit else NULL)

    p %>% plotly::layout(
      title = list(text = ttl, font = list(size = 15)),
      xaxis = list(title = label_var(xv)),
      yaxis = list(title = label_var(yv)),
      legend = list(orientation = "h", y = -0.14, x = 0.5, xanchor = "center"),
      margin = list(b = 80, t = 50),
      hovermode = "closest",
      dragmode = "zoom"
    ) %>%
      plotly::config(scrollZoom = TRUE, displayModeBar = TRUE)
  })

  output$model_stats <- renderUI({
    xv <- input$x_var
    yv <- input$y_var
    if (is.null(xv) || is.null(yv) || !nzchar(xv) || !nzchar(yv)) {
      return(HTML("Select X and Y."))
    }
    if (identical(xv, yv)) {
      return(HTML("Choose different variables for X and Y."))
    }

    mf <- model_fit()
    if (is.null(mf) || is.null(mf$fit)) {
      return(HTML("Not enough complete rows to fit a model."))
    }

    tagList(HTML(format_model_html(mf$fit, mf$y_name)), tags$br(), stats_note())
  })
}

shinyApp(ui, server)
