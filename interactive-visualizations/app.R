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
    # Graduation/admission/room-board percents: IPEDS stores as 0-100 (not 0-1).
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

# ----------- PRETTY NAMES SOURCE ----------- 
# Format: "VARIABLE_NAME", "Pretty Name", "Category Name"
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
  
  # Highest degree offered
  "HDEOFR_DOC", "Doctor's Degree", "Highest Degree Offered",
  "HDEOFR_MAS", "Master's Degree", "Highest Degree Offered",
  "HDEOFR_BAC", "Bachelor's Degree", "Highest Degree Offered",
  
  # Institution special type
  "HBCU_YES", "Historically Black College/University (HBCU)", "Institution Special Type",
  "RELAFFIL_YES", "Religious Affliated", "Institution Special Type",
  "ASSOC1_YES", "Member of National Collegiate Athletic Association (NCAA)", "Institution Special Type",
  "ATHASSOC_YES", "Member of National Athletic Association", "Institution Special Type",
)

label_var <- function(v) {
  out <- var_meta$label[match(v, var_meta$var)]
  ifelse(is.na(out), v, out)
}

choices_by_category <- function(cat) {
  subset <- var_meta[var_meta$category == cat, ]
  setNames(subset$var, subset$label)
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

# Combined Y-group control options
y_group_choices <- c(
  "Public in state" = "public_in_state",
  "Public out of state" = "public_out_of_state",
  "Private" = "private"
)

y_group_labels <- c(
  public_in_state = "Public in-state tuition",
  public_out_of_state = "Public out-of-state tuition",
  private = "Private tuition"
)

y_group_meta <- list(
  public_in_state = list(y_var = "TUITION2", sector = "Public"),
  public_out_of_state = list(y_var = "TUITION3", sector = "Public"),
  private = list(y_var = "TUITION3", sector = "Private")
)

# Predictors whose values are already on a 0-100 percentage scale (IPEDS)
vars_ipeds_percent_scale <- c("GBA6RTT", "GBA4RTT", "GRRTM", "GRRTW", "DVADM01", "RMINSTTP", "RMOUSTTP")

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
        h4("Model Builder"),
        tabsetPanel(
          id = "left_panel_tabs",
          type = "tabs",
          
          # -------------------------
          # TAB 1: MODEL INPUTS
          # -------------------------
          tabPanel(
            "Model Inputs",
            
            checkboxGroupInput(
              "y_groups",
              "Select tuition groups (Y)",
              choices = y_group_choices,
              selected = unname(y_group_choices)
            ),
            
            tags$p(
              style = "font-weight: bold; margin-bottom: 8px;",
              "Select predictors (X). You may choose more than one per category; log/sqrt transforms are applied in the data where indicated."
            ),
            
            tags$div(
              style = "color: #737373;",
              selectInput("enrollment_vars", "Enrollment & student composition", choices = choices_by_category("Enrollment & student composition"), multiple = TRUE),
              selectInput(
                "selectivity_vars",
                "Selectivity & student outcomes",
                choices = choices_by_category("Selectivity & outcomes"),
                multiple = TRUE,
                # pre-select
                selected = "GBA6RTT"
                ),
              selectInput("campus_vars", "Campus life & resources", choices = choices_by_category("Campus life & resources"), multiple = TRUE),
              selectInput("costs_vars", "Costs, aid & debt", choices = choices_by_category("Costs & aid"), multiple = TRUE),
              selectInput("profile_vars", "Institution profile", choices = choices_by_category("Institution profile"), multiple = TRUE)
            ),
            
            selectInput("x_axis_var", "Select predictor for X axis", choices = character(0))
          ),
          
          # -------------------------
          # TAB 2: SCHOOL FILTER
          # -------------------------
          tabPanel(
            "School Filter",
            tags$div(
              style = "padding:10px; color:#666;",
              
              checkboxGroupInput(
                "locale_filter",
                label = "Degree of urbanization (Locale)",
                choices = choices_by_category("School Location"),
                selected = choices_by_category("School Location") %>% names()
              ),
              
              tags$hr(),
              
              checkboxGroupInput(
                "region_filter",
                label = "US Region",
                choices = choices_by_category("School Region"),
                selected = NULL
              ),
              
              tags$hr(),
              
              checkboxGroupInput(
                "degree_filter",
                "Highest Degree Offered",
                choices = choices_by_category("Highest Degree Offered"),
                selected = NULL
              ),
              
              tags$hr(),
              
              checkboxGroupInput(
                "special_filter",
                "Institution Special Type",
                choices = choices_by_category("Institution Special Type"),
                selected = NULL
              )
            )
          )
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

# Fit lm only when there is enough data
fit_lm_safe <- function(y_var, x_vars, dat, min_n) {
  if (length(x_vars) == 0 || nrow(dat) < min_n) return(NULL)
  stats::lm(as.formula(paste(y_var, "~", paste(x_vars, collapse = " + "))), data = dat)
}

# Multi-select inputs can be NULL; treat as no selection
null_chr <- function(x) if (is.null(x)) character(0) else x

# Keep x-axis dropdown selection when choices update; else fall back to b
`%||%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(a[1])) a else b

server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- app_df
    
    loc <- input$locale_filter
    reg <- input$region_filter
    deg <- input$degree_filter
    spe <- input$special_filter

    # -----------------------
    # LOCALE FILTER
    # -----------------------
    if (!is.null(loc) && length(loc)) {
      locale_map <- c(
        LOCALE_CITY   = "LOCALE_CITY",
        LOCALE_SUBURB = "LOCALE_SUBURB",
        LOCALE_TOWN   = "LOCALE_TOWN",
        LOCALE_RURAL  = "LOCALE_RURAL"
      )
      
      keep <- rep(FALSE, nrow(df))
      
      for (v in loc) {
        keep <- keep | df[[locale_map[[v]]]] == 1
      }
      
      df <- df[keep, ]
    }
    
    # -----------------------
    # REGION FILTER
    # -----------------------
    if (!is.null(reg) && length(reg)) {
      region_map <- c(
        NORTHEAST = "NORTHEAST",
        MIDWEST   = "MIDWEST",
        SOUTH     = "SOUTH",
        WEST      = "WEST"
      )
      
      keep <- rep(FALSE, nrow(df))
      
      for (v in reg) {
        keep <- keep | df[[region_map[[v]]]] == 1
      }
      
      df <- df[keep, ]
    }
    
    # -----------------------
    # DEGREE FILTER
    # -----------------------
    if (!is.null(deg) && length(deg)) {
      degree_map <- c(
        HDEOFR_DOC = "HDEOFR_DOC",
        HDEOFR_MAS = "HDEOFR_MAS",
        HDEOFR_BAC = "HDEOFR_BAC"
      )
      
      keep <- rep(FALSE, nrow(df))
      
      for (v in deg) {
        keep <- keep | df[[degree_map[[v]]]] == 1
      }
      
      df <- df[keep, ]
    }
    
    # -----------------------
    # SPECIAL TYPE FILTER
    # -----------------------
    if (!is.null(spe) && length(spe)) {
      
      if ("HBCU_YES" %in% spe) {
        df <- df[df$HBCU_YES == 1, ]
      }
      
      if ("RELAFFIL_YES" %in% spe) {
        df <- df[df$RELAFFIL_YES == 1, ]
      }
      
      if ("ASSOC1_YES" %in% spe) {
        df <- df[df$ASSOC1_YES == 1, ]
      }
      
      if ("ATHASSOC_YES" %in% spe) {
        df <- df[df$ATHASSOC_YES == 1, ]
      }
    }
    
    df 
  })
  
  selected_y_groups <- reactive({
    gy <- input$y_groups
    gy <- gy[gy %in% names(y_group_meta)]
    if (is.null(gy) || !length(gy)) character(0) else gy
  })

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
      sel <- input$x_axis_var %||%
        if ("GBA6RTT" %in% xs) "GBA6RTT" else xs[[1]]
      if (!sel %in% xs) sel <- xs[[1]]
      updateSelectInput(session, "x_axis_var", choices = named, selected = sel)
    }
  })

  # Build selected Y groups as long data
  model_data <- reactive({
    x_vars <- selected_predictors()
    gy <- selected_y_groups()
    if (!length(gy)) return(app_df[0, , drop = FALSE])
    cols <- unique(c("INSTNM", "CONTROL", "TUITION2", "TUITION3", x_vars))
    base_dat <- filtered_data() %>% select(all_of(cols))
    if (length(x_vars) > 0) base_dat <- base_dat %>% filter(if_all(all_of(x_vars), ~ !is.na(.)))

    out_list <- lapply(gy, function(g) {
      meta <- y_group_meta[[g]]
      y_col <- meta$y_var
      sect <- meta$sector
      d <- base_dat %>%
        mutate(sector = sector_from_control(CONTROL)) %>%
        filter(sector == sect) %>%
        mutate(y_group = g, y_plot = .data[[y_col]]) %>%
        filter(!is.na(y_plot), y_plot > 0)
      d
    })
    bind_rows(out_list)
  })

  # Fit one model per selected Y group
  model_fits <- reactive({
    dat <- model_data()
    x_vars <- selected_predictors()
    gy <- selected_y_groups()
    if (!length(x_vars) || !length(gy)) return(list())

    setNames(lapply(gy, function(g) {
      d <- dat[dat$y_group == g, , drop = FALSE]
      fit_lm_safe("y_plot", x_vars, d, if (length(gy) == 1) MIN_N_MODEL else MIN_N_MODEL_SECTOR)
    }), gy)
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
    length(mf) > 0 && any(vapply(mf, function(x) !is.null(x), logical(1)))
  }

  # Build plotting data (x, y, sector, search highlight)
  plot_data <- reactive({
    dat <- model_data()
    x_vars <- selected_predictors()
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
    dat$is_match <- nchar(q) > 0 & grepl(q, tolower(dat$INSTNM), fixed = TRUE)
    dat
  })

  # Compute x-axis bounds with small padding
  axis_limits <- reactive({
    x1 <- first_predictor()
    dat <- model_data()
    default <- list(x_range = c(0, 1), y_range = c(0, 1))
    if (is.null(x1) || !(x1 %in% names(dat)) || !nrow(dat) || !("y_plot" %in% names(dat))) return(default)
    x_vals <- dat[[x1]]
    x_vals <- x_vals[is.finite(x_vals)]
    y_vals <- dat[["y_plot"]]
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
        "<em>X-axis: ", x_lab, " (Predictor for X axis).</em><br>"
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
    y_title <- "Tuition"
    x1 <- first_predictor()
    xv <- if (!is.null(x1) && x1 %in% names(dat)) dat[[x1]] else rep(NA_real_, nrow(dat))
    hx <- hover_x_lines(x1, xv)
    y_group_lab <- y_group_labels[dat$y_group]
    hover <- paste0(
      "<b>", dat$INSTNM, "</b><br>",
      if (length(hx)) paste0(hx, "<br>") else "",
      y_group_lab, ": ",
      format(dat[["y_plot"]], big.mark = ",", trim = TRUE, scientific = FALSE)
    )
    colors <- c(public_in_state = "#1f77b4", public_out_of_state = "#17becf", private = "#ff7f0e")

    br <- which(!dat$is_match)
    base_dat <- dat[br, , drop = FALSE]
    if (nrow(base_dat) > 0) base_dat$ht <- hover[br]
    mr <- which(dat$is_match)
    match_dat <- dat[mr, , drop = FALSE]
    if (nrow(match_dat) > 0) match_dat$ht <- hover[mr]

    p <- plot_ly(type = "scatter", mode = "markers")
    if (nrow(base_dat) > 0) {
      for (grp in names(colors)) {
        dg <- base_dat[base_dat$y_group == grp, , drop = FALSE]
        if (!nrow(dg)) next
        p <- plotly::add_markers(
          p, data = dg, x = ~x_plot, y = ~y_plot, text = ~ht, hoverinfo = "text",
          name = y_group_labels[[grp]], legendgroup = grp,
          marker = list(color = colors[[grp]], size = 7, opacity = 0.8),
          showlegend = TRUE
        )
      }
    }
    if (nrow(match_dat) > 0) {
      for (grp in names(colors)) {
        dg <- match_dat[match_dat$y_group == grp, , drop = FALSE]
        if (!nrow(dg)) next
        p <- plotly::add_markers(
          p, data = dg, x = ~x_plot, y = ~y_plot, text = ~ht, hoverinfo = "text",
          name = y_group_labels[[grp]], legendgroup = grp,
          marker = list(color = colors[[grp]], size = 12, opacity = 1, line = list(color = "black", width = 2)),
          showlegend = FALSE
        )
      }
    }

    mf <- model_fits()
    x_vars <- selected_predictors()
    md <- model_data()

    if (length(x_vars) < 1 || is.null(x1) || !nrow(md) || !length(mf)) {
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

    for (grp in names(mf)) {
      fit <- mf[[grp]]
      if (is.null(fit)) next
      md_grp <- md[md$y_group == grp, , drop = FALSE]
      p <- add_line_for_fit(p, fit, md_grp, colors[[grp]])
    }

    tuition_plot_layout(p, lims, y_title, label_var(x1))
  })

  # Q-Q + histogram of residuals (single model or Public / Private rows)
  output$residual_plots <- renderPlot({
    mf <- model_fits()
    x_vars <- selected_predictors()
    gy <- selected_y_groups()

    empty_msg <- function(msg) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, msg)
    }

    if (!length(gy) || !length(x_vars)) {
      empty_msg("Select at least one tuition group and predictor.")
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

    valid_groups <- names(mf)[vapply(mf, function(x) !is.null(x), logical(1))]
    if (!length(valid_groups)) {
      empty_msg("Residuals unavailable (model did not fit).")
      return(invisible(NULL))
    }

    n_valid <- length(valid_groups)
    graphics::par(
      mfrow = c(n_valid, 2),
      mgp = c(2, 0.7, 0),
      mar = if (n_valid > 1) c(3.5, 3.5, 2.5, 1) else c(4, 4, 3, 1)
    )
    fill_cols <- c(public_in_state = "#1f77b4", public_out_of_state = "#17becf", private = "#ff7f0e")
    for (grp in valid_groups) {
      fit <- mf[[grp]]
      res <- stats::residuals(fit)
      lab <- y_group_labels[[grp]]
      draw_qq_hist(res, paste0(lab, ": Normal Q-Q"), paste0(lab, ": histogram"), fill_cols[[grp]])
    }
  })

  # Build model summary text for the right panel
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

  # Significance code legend
  stats_note <- function() {
    tags$p(
      style = "font-size:0.9em;color:#555;margin-bottom:0;",
      "Signif. codes: 0 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019 0.1 \u2018 \u2019 1"
    )
  }

  # Render one summary or grouped tabs
  output$model_stats <- renderUI({
    x_vars <- selected_predictors()
    gy <- selected_y_groups()
    mf <- model_fits()

    if (!length(gy)) return(HTML("Select at least one tuition group."))
    if (!length(x_vars)) return(HTML("Select at least one predictor from the category lists."))
    if (!length(mf)) return(HTML("Not enough complete rows to fit a model."))

    if (length(gy) == 1) {
      grp <- gy[[1]]
      return(tagList(HTML(format_model_html(mf[[grp]], y_group_labels[[grp]])), tags$br(), stats_note()))
    }

    tab_body <- function(html) tags$div(style = "margin-top: 12px;", HTML(html))
    tabs <- lapply(gy, function(grp) tabPanel(y_group_labels[[grp]], tab_body(format_model_html(mf[[grp]], y_group_labels[[grp]]))))
    tagList(
      do.call(tabsetPanel, c(list(id = "model_stats_tabs", type = "tabs"), tabs)),
      tags$br(),
      stats_note()
    )
  })
}

shinyApp(ui = ui, server = server)
