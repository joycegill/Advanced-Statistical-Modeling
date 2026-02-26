# Shiny App for Interactive College Ranking Visualizations

library(shiny)
library(plotly)
library(tidyverse)
library(effectsize)
library(readr)

# On plot error: log real message to console
safe_error_text <- function(e) {
  real_msg <- tryCatch({
    m <- conditionMessage(e)
    if (is.character(m)) paste(m, collapse = " ") else toString(m)
  }, error = function(x) "Unknown error")
  message("Plot error: ", real_msg)
  "An error occurred. Check the R console for details."
}

# ---- Data ----
url <- "https://raw.githubusercontent.com/joycegill/Advanced-Statistical-Modeling/main/data/cleaned/FINAL_DATA.csv"
df <- read_csv(url)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Interactive Data Visualizations"),
  mainPanel(
    width = 12,
    tabsetPanel(
        # Tab 1: SAT Scores by Admission Requirement
        tabPanel("SAT Scores by Requirement",
          h3("SAT Scores by Admission Requirement"),
          p("Research Question: Do schools that require SAT scores have higher average SAT scores than schools that do not require them?"),
          plotlyOutput("sat_scores_plot"),
          verbatimTextOutput("sat_scores_test")
        ),
        
        # Tab 2: SAT Requirement vs Admission Rate
        tabPanel("SAT Requirement vs Admission Rate",
          h3("SAT Requirement vs Admission Rate"),
          p("Research Question: Do schools that require SAT scores have different admission rates compared to schools that do not require them?"),
          plotlyOutput("sat_admission_plot"),
          verbatimTextOutput("sat_admission_test")
        ),
        
        # Tab 3: SAT Requirements by State
        tabPanel("SAT Requirements by State",
          h3("SAT Requirements by State"),
          p("Research Question: Are SAT requirements geographically clustered across states?"),
          plotlyOutput("sat_state_plot", height = "900px")
        ),
        
        # Tab 4: SAT Requirements by State and Sector
        tabPanel("SAT Requirements by State & Sector",
          h3("SAT Requirements by State and Sector"),
          p("Research Question: Are SAT requirements geographically clustered even after accounting for school type (public vs. private)?"),
          plotlyOutput("sat_state_sector_private_plot"),
          plotlyOutput("sat_state_sector_public_plot")
        ),
        
        # Tab 5: SAT Requirements by Region
        tabPanel("SAT Requirements by Region",
          h3("SAT Requirements by Region"),
          p("Research Question: Are SAT requirements geographically clustered by region?"),
          plotlyOutput("sat_region_counts_plot")
        ),
        
        # Tab 6: Enrollment Growth vs Graduation Rates
        tabPanel("Enrollment Growth vs Graduation",
          h3("Enrollment Growth vs Graduation Rates"),
          p("Research Question: Do schools that grow enrollment faster sacrifice graduation rates, or do some manage both?"),
          plotlyOutput("enrollment_growth_plot"),
          verbatimTextOutput("enrollment_growth_corr")
        ),
        
        # Tab 7: Enrollment Growth vs Graduation by Sector
        tabPanel("Enrollment Growth by Sector",
          h3("Enrollment Growth vs Graduation Rate by Sector"),
          p("Research Question: Is the relationship between enrollment growth and graduation rates consistent across public and private institutions?"),
          plotlyOutput("enrollment_growth_sector_private_plot"),
          plotlyOutput("enrollment_growth_sector_public_plot")
        ),
        
        # Tab 8: Nonlinear Enrollment Growth
        tabPanel("Nonlinear Enrollment Growth",
          h3("Nonlinear Relationship: Enrollment Growth vs Graduation Rates"),
          p("Research Question: Is there a nonlinear relationship between enrollment growth and graduation rates, suggesting an optimal growth rate?"),
          plotlyOutput("enrollment_nonlinear_plot"),
          verbatimTextOutput("enrollment_quad_model")
        ),
        
        # Tab 9: Faculty-to-Student Ratio
        tabPanel("Faculty-to-Student Ratio",
          h3("Faculty-to-Student Ratio vs Graduation Rates"),
          p("Research Question: Does having more faculty per student always translate to better graduation outcomes?"),
          plotlyOutput("faculty_ratio_plot"),
          verbatimTextOutput("faculty_ratio_corr")
        ),
        
        # Tab 10: Class Size vs Tuition
        tabPanel("Class Size vs Tuition",
          h3("Class Size vs Tuition Cost"),
          plotlyOutput("class_size_small_plot"),
          plotlyOutput("class_size_large_plot")
        ),
        
        # Tab 11: Tuition vs Room & Board
        tabPanel("Tuition vs Room & Board",
          h3("Average Tuition vs Combined Food and Housing Charge"),
          plotlyOutput("tuition_roomboard_instate_plot"),
          plotlyOutput("tuition_roomboard_outstate_plot"),
          verbatimTextOutput("tuition_roomboard_model")
        )
      )
    )
)

# ---- Server ----
server <- function(input, output, session) {

  # Graph 1: Boxplot of SAT scores by requirement (required vs not)
  output$sat_scores_plot <- renderPlotly({
    adm_test_f2024_clean <- df %>%
      mutate(
        # not just sat but sat/act
        sat_required = ifelse(
          ADMCON7 == "Required to be considered for admission",
          "Required", "Not Required"
        ),
        sat_total = SATVR50 + SATMT50
      ) %>%
      filter(!is.na(sat_total)) %>%
      mutate(
        hover_text = paste("School:", INSTNM, "<br>SAT Score:", sat_total)
      )
    
    p <- plot_ly(adm_test_f2024_clean, x = ~sat_required, y = ~sat_total, 
                 type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = 0, text = ~hover_text, hoverinfo = "text",
                 color = ~sat_required, colors = c("Required" = "#F8766D", "Not Required" = "#00BFC4"),
                 showlegend = FALSE) %>%
      layout(title = list(text = "SAT Scores by Admission Requirement"),
             xaxis = list(title = "SAT Requirement"),
             yaxis = list(title = "Total SAT Score"))
    
    p
  })
  
  # T-test and Cohen's d for Graph 1
  output$sat_scores_test <- renderPrint({
    adm_test_f2024_prep <- df %>%
      mutate(
        UNITID = as.character(UNITID),
        sat_required = ifelse(
          ADMCON7 == "Required to be considered for admission",
          "Required", "Not Required"
        ),
        sat_total = SATVR50 + SATMT50
      )
    
    adm_test_f2024_clean <- adm_test_f2024_prep %>%
      filter(!is.na(sat_total))
    
    cat("T-test:\n")
    print(t.test(sat_total ~ sat_required, data = adm_test_f2024_clean))
    cat("\n\nCohen's d:\n")
    print(cohens_d(sat_total ~ sat_required, data = adm_test_f2024_clean))
  })
  
  # Graph 2: Boxplot of admission rate by SAT requirement
  output$sat_admission_plot <- renderPlotly({
    sat_admission_rate <- df %>%
      mutate(
        sat_required = ifelse(
          ADMCON7 == "Required to be considered for admission",
          "Required", "Not Required"
        ),
        admit_rate = as.numeric(DVADM01)
      ) %>%
      filter(between(admit_rate, 0, 100)) %>%
      mutate(
        hover_text = paste("School:", INSTNM, "<br>Admission Rate:", round(admit_rate, 1), "%")
      )
    p <- plot_ly(sat_admission_rate, x = ~sat_required, y = ~admit_rate,
                 type = "box", boxpoints = "all", jitter = 0.3,
                 pointpos = 0, text = ~hover_text, hoverinfo = "text",
                 color = ~sat_required, colors = c("Required" = "#F8766D", "Not Required" = "#00BFC4"),
                 showlegend = FALSE) %>%
      layout(title = list(text = "Admission Rate by SAT Requirement"),
             xaxis = list(title = "SAT Requirement"),
             yaxis = list(title = "Admission Rate (%)"))
    
    p
  })
  
  # T-test and Cohen's d for Graph 2
  output$sat_admission_test <- renderPrint({
    sat_admission_rate <- df %>%
      mutate(
        sat_required = ifelse(
          ADMCON7 == "Required to be considered for admission",
          "Required", "Not Required"
        ),
        admit_rate = as.numeric(DVADM01)
      ) %>%
      filter(between(admit_rate, 0, 100))
    
    cat("T-test:\n")
    print(t.test(admit_rate ~ sat_required, data = sat_admission_rate))
    cat("\n\nCohen's d:\n")
    print(cohens_d(admit_rate ~ sat_required, data = sat_admission_rate))
  })
  
  # Graph 3: Horizontal bar chart 
  output$sat_state_plot <- renderPlotly({
    sat_state_requirements <- df %>%
      mutate(
        sat_required = ADMCON7 == "Required to be considered for admission"
      ) %>%
      select(UNITID, INSTNM, sat_required, STATE) %>%
      filter(!is.na(STATE), !is.na(sat_required))
    
    state_rates <- sat_state_requirements %>%
      group_by(STATE) %>%
      summarise(rate = mean(sat_required), n = n(), .groups = "drop") %>%
      slice_max(rate, n = 10, with_ties = FALSE) %>%
      mutate(hover_text = paste("State:", STATE, "<br>Rate:", round(rate * 100, 1), "%<br>Schools:", n))
    if (nrow(state_rates) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper",
                                           x = 0.5, y = 0.5, showarrow = FALSE))
    }
    n_states <- nrow(state_rates)
    plot_height <- max(500, n_states * 22)
    
    p <- plot_ly(state_rates, x = ~rate, y = ~reorder(STATE, rate), type = "bar",
                 orientation = "h", text = ~hover_text, hoverinfo = "text",
                 marker = list(color = "steelblue"),
                 textposition = "none") %>%
      layout(
        title = list(text = "SAT Requirement Rate by State"),
        height = plot_height,
        margin = list(l = 80),
        xaxis = list(title = "Percent Requiring SAT", tickformat = ".0%"),
        yaxis = list(
          title = "State",
          tickfont = list(size = 12),
          automargin = TRUE
        )
      )
    p
  })
  
  # Helper: SAT state-by-sector rates for one control; returns df ready to plot (top 5 states).
  sat_state_sector_data <- function(control_name) {
    df %>%
      mutate(sat_required = ADMCON7 == "Required to be considered for admission") %>%
      select(UNITID, sat_required, STATE, CONTROL, INSTNM) %>%
      filter(!is.na(STATE), !is.na(sat_required), CONTROL == control_name) %>%
      group_by(STATE) %>%
      summarise(rate = mean(sat_required), n = n(), .groups = "drop") %>%
      mutate(hover_text = paste("State:", STATE, "<br>Rate:", round(rate * 100, 1), "%<br>Schools:", n)) %>%
      slice_max(rate, n = 5, with_ties = FALSE)
  }

  # Graph 4a: Private 4-year — top 5 states by % requiring SAT
  output$sat_state_sector_private_plot <- renderPlotly({
    df_plot <- sat_state_sector_data("Private not-for-profit")
    plot_ly(df_plot, x = ~rate, y = ~reorder(STATE, rate), type = "bar",
            orientation = "h", text = ~hover_text, hoverinfo = "text",
            marker = list(color = "steelblue"), textposition = "none") %>%
      layout(title = list(text = "Private 4-year: SAT Requirement Rate by State"),
             xaxis = list(title = "Percent Requiring SAT", tickformat = ".0%"),
             yaxis = list(title = "State"))
  })

  # Graph 4b: Public 4-year — top 5 states by % requiring SAT
  output$sat_state_sector_public_plot <- renderPlotly({
    df_plot <- sat_state_sector_data("Public")
    plot_ly(df_plot, x = ~rate, y = ~reorder(STATE, rate), type = "bar",
            orientation = "h", text = ~hover_text, hoverinfo = "text",
            marker = list(color = "steelblue"), textposition = "none") %>%
      layout(title = list(text = "Public 4-year: SAT Requirement Rate by State"),
             xaxis = list(title = "Percent Requiring SAT", tickformat = ".0%"),
             yaxis = list(title = "State"))
  })
  
  # Graph 5: Count of schools requiring vs not requiring SAT by region (top 5 regions by total schools)
  output$sat_region_counts_plot <- renderPlotly({
    sat_region <- df %>%
      mutate(sat_required = ADMCON7 == "Required to be considered for admission") %>%
      filter(!is.na(OBEREG), OBEREG != "U.S. Service schools", !is.na(sat_required))
    region_counts <- sat_region %>%
      mutate(policy = if_else(sat_required, "Required", "Not required")) %>%
      count(OBEREG, policy, name = "n")
    if (nrow(region_counts) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
    }
    region_totals <- region_counts %>% group_by(OBEREG) %>% summarise(total = sum(n), .groups = "drop")
    top5_order <- region_totals %>% slice_max(total, n = 5, with_ties = FALSE) %>% arrange(total) %>% pull(OBEREG)
    plot_df <- region_counts %>% filter(OBEREG %in% top5_order) %>% left_join(region_totals, by = "OBEREG")
    plot_df <- plot_df %>% mutate(region_label = factor(OBEREG, levels = top5_order))
    plot_ly(as.data.frame(plot_df), x = ~n, y = ~region_label, color = ~policy, type = "bar", orientation = "h",
            text = ~paste0("Region: ", OBEREG, "<br>Policy: ", policy, "<br>Count: ", n), hoverinfo = "text",
            colors = c("Required" = "#F8766D", "Not required" = "#00BFC4"), textposition = "none") %>%
      layout(title = list(text = "Number of Schools Requiring vs Not Requiring SAT by Region"),
             xaxis = list(title = "Number of Schools"), yaxis = list(title = "Region"), barmode = "group")
  })

  # Graph 6: Scatter of first-time enrollment % vs 6-year graduation rate
  output$enrollment_growth_plot <- renderPlotly({
    tryCatch({
      enrollment_growth_graduation <- df %>%
        mutate(
          total_enroll = as.numeric(ENRTOT),
          first_time = as.numeric(EFUG1ST),
          grad_rate = as.numeric(GBA6RTT)
        ) %>%
        filter(!is.na(total_enroll), !is.na(first_time), total_enroll > 0, first_time > 0, !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
        mutate(growth_pct = 100 * first_time / total_enroll) %>%
        select(UNITID, growth_pct, grad_rate, INSTNM) %>%
        mutate(
          hover_text = paste("School:", INSTNM, "<br>Growth:", round(growth_pct, 1), "%<br>Grad Rate:", round(grad_rate, 1), "%")
        ) %>%
        filter(!is.na(growth_pct), !is.na(grad_rate))
      
      if (nrow(enrollment_growth_graduation) == 0) {
        return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", 
                                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      lm_fit <- lm(grad_rate ~ growth_pct, data = enrollment_growth_graduation)
      x_range <- seq(min(enrollment_growth_graduation$growth_pct, na.rm = TRUE),
                     max(enrollment_growth_graduation$growth_pct, na.rm = TRUE), length.out = 100)
      y_pred <- predict(lm_fit, newdata = data.frame(growth_pct = x_range))
      valid_idx <- !is.na(y_pred)
      x_range <- x_range[valid_idx]
      y_pred <- y_pred[valid_idx]
      df_plot <- as.data.frame(enrollment_growth_graduation)
      x_vec <- as.numeric(df_plot$growth_pct)
      y_vec <- as.numeric(df_plot$grad_rate)
      text_vec <- as.character(df_plot$hover_text)
      p <- plot_ly(x = x_vec, y = y_vec, text = text_vec,
                   type = "scatter", mode = "markers", hoverinfo = "text",
                   marker = list(size = 5, opacity = 0.4, color = "steelblue"),
                   name = "Schools")
      if (length(x_range) > 0 && length(y_pred) > 0) {
        n_line <- length(x_range)
        p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", n_line), type = "scatter", mode = "lines",
                             line = list(color = "red", width = 2), name = "Trend",
                             hoverinfo = "skip")
      }
      p <- p %>% layout(title = list(text = "Enrollment Growth vs Graduation Rates"),
                       xaxis = list(title = "First-Time Enrollment (%)"),
                       yaxis = list(title = "6-Year Graduation Rate (%)"))
      
      p
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", 
                                   x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Correlation test for Graph 6.
  output$enrollment_growth_corr <- renderPrint({
    enrollment_growth_graduation <- df %>%
      mutate(
        total_enroll = as.numeric(ENRTOT),
        first_time = as.numeric(EFUG1ST),
        grad_rate = as.numeric(GBA6RTT)
      ) %>%
      filter(!is.na(total_enroll), !is.na(first_time), total_enroll > 0, first_time > 0, !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
      mutate(growth_pct = 100 * first_time / total_enroll) %>%
      select(UNITID, growth_pct, grad_rate, INSTNM)
    
    cat("Correlation test:\n")
    print(cor.test(enrollment_growth_graduation$growth_pct, enrollment_growth_graduation$grad_rate))
  })
  
  # Helper: build growth vs grad data and filter to one sector; return data frame for that sector
  growth_grad_sector_data <- function(sector_filter) {
    enrollment_growth_graduation <- df %>%
      mutate(
        total_enroll = as.numeric(ENRTOT),
        first_time = as.numeric(EFUG1ST),
        grad_rate = as.numeric(GBA6RTT)
      ) %>%
      filter(!is.na(total_enroll), !is.na(first_time), total_enroll > 0, first_time > 0, !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
      mutate(growth_pct = 100 * first_time / total_enroll) %>%
      filter(!is.na(growth_pct), !is.na(grad_rate)) %>%
      select(UNITID, growth_pct, grad_rate, CONTROL, INSTNM) %>%
      filter(CONTROL == sector_filter) %>%
      mutate(hover_text = paste("School:", INSTNM, "<br>Growth:", round(growth_pct, 1), "%<br>Grad Rate:", round(grad_rate, 1), "%")) %>%
      as.data.frame()
    enrollment_growth_graduation
  }

  # Build enrollment growth vs graduation scatter + linear trend for one sector.
  make_growth_sector_plot <- function(sector_filter, title_text) {
    df <- growth_grad_sector_data(sector_filter)
    if (nrow(df) == 0) return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
    lm_fit <- lm(grad_rate ~ growth_pct, data = df)
    x_range <- seq(min(df$growth_pct, na.rm = TRUE), max(df$growth_pct, na.rm = TRUE), length.out = 100)
    y_pred <- predict(lm_fit, newdata = data.frame(growth_pct = x_range))
    valid_idx <- !is.na(y_pred)
    x_range <- as.numeric(x_range[valid_idx])
    y_pred <- as.numeric(y_pred[valid_idx])
    p <- plot_ly(x = as.numeric(df$growth_pct), y = as.numeric(df$grad_rate), text = as.character(df$hover_text),
                 type = "scatter", mode = "markers", hoverinfo = "text",
                 marker = list(size = 5, opacity = 0.4, color = "steelblue"), showlegend = FALSE)
    if (length(x_range) > 0 && length(y_pred) > 0) {
      p <- p %>% add_trace(x = x_range, y = y_pred, text = rep("", length(x_range)), type = "scatter", mode = "lines",
                           line = list(color = "red", width = 2), hoverinfo = "skip", showlegend = FALSE)
    }
    p %>% layout(title = list(text = title_text),
                 xaxis = list(title = "First-Time Enrollment (%)"),
                 yaxis = list(title = "6-Year Graduation Rate (%)"))
  }

  # Graph 7a: Enrollment growth vs graduation — Private 4-year
  output$enrollment_growth_sector_private_plot <- renderPlotly({
    tryCatch(make_growth_sector_plot("Private not-for-profit", "Private 4-year: Enrollment Growth vs Graduation Rate"),
             error = function(e) plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
  })

  # Graph 7b: Enrollment growth vs graduation — Public 4-year
  output$enrollment_growth_sector_public_plot <- renderPlotly({
    tryCatch(make_growth_sector_plot("Public", "Public 4-year: Enrollment Growth vs Graduation Rate"),
             error = function(e) plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
  })
  
  # Graph 8: Same as Graph 6 but with loess trend (nonlinear) instead of linear
  output$enrollment_nonlinear_plot <- renderPlotly({
    tryCatch({
      enrollment_growth_graduation <- df %>%
        mutate(
          total_enroll = as.numeric(ENRTOT),
          first_time = as.numeric(EFUG1ST),
          grad_rate = as.numeric(GBA6RTT)
        ) %>%
        filter(!is.na(total_enroll), !is.na(first_time), total_enroll > 0, first_time > 0, !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
        mutate(growth_pct = 100 * first_time / total_enroll) %>%
        select(UNITID, growth_pct, grad_rate, INSTNM) %>%
      mutate(
        hover_text = paste("School:", INSTNM, "<br>Growth:", round(growth_pct, 1), "%<br>Grad Rate:", round(grad_rate, 1), "%")
      ) %>%
      filter(!is.na(growth_pct), !is.na(grad_rate))
    if (nrow(enrollment_growth_graduation) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", 
                                           x = 0.5, y = 0.5, showarrow = FALSE))
    }
    loess_fit <- tryCatch(loess(grad_rate ~ growth_pct, data = enrollment_growth_graduation, span = 0.5),
                          error = function(e) lm(grad_rate ~ growth_pct, data = enrollment_growth_graduation))
      x_range <- seq(min(enrollment_growth_graduation$growth_pct, na.rm = TRUE),
                     max(enrollment_growth_graduation$growth_pct, na.rm = TRUE), length.out = 100)
      y_pred <- tryCatch(predict(loess_fit, newdata = data.frame(growth_pct = x_range)),
                         error = function(e) predict(lm(grad_rate ~ growth_pct, data = enrollment_growth_graduation), newdata = data.frame(growth_pct = x_range)))
      valid_idx <- !is.na(y_pred)
      x_range <- as.numeric(x_range[valid_idx])
      y_pred <- as.numeric(y_pred[valid_idx])
      df_plot <- as.data.frame(enrollment_growth_graduation)
      x_vec <- as.numeric(df_plot$growth_pct)
      y_vec <- as.numeric(df_plot$grad_rate)
      text_vec <- as.character(df_plot$hover_text)
      p <- plot_ly(x = x_vec, y = y_vec, text = text_vec,
                   type = "scatter", mode = "markers", hoverinfo = "text",
                   marker = list(size = 5, opacity = 0.3, color = "steelblue"),
                   name = "Schools")
      if (length(x_range) > 0 && length(y_pred) > 0) {
        n_line <- length(x_range)
        p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", n_line), type = "scatter", mode = "lines",
                             line = list(color = "red", width = 2), name = "Trend",
                             hoverinfo = "skip")
      }
      p <- p %>% layout(title = list(text = "Nonlinear Relationship Between Growth and Graduation Rates"),
                       xaxis = list(title = "First-Time Enrollment (%)"),
                       yaxis = list(title = "6-Year Graduation Rate (%)"))
      
      p
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", 
                                   x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Quadratic model summary for Graph 8
  output$enrollment_quad_model <- renderPrint({
    enrollment_growth_graduation <- df %>%
      mutate(
        total_enroll = as.numeric(ENRTOT),
        first_time = as.numeric(EFUG1ST),
        grad_rate = as.numeric(GBA6RTT)
      ) %>%
      filter(!is.na(total_enroll), !is.na(first_time), total_enroll > 0, first_time > 0, !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
      mutate(growth_pct = 100 * first_time / total_enroll) %>%
      select(UNITID, INSTNM, growth_pct, grad_rate)
    
    quad_model <- lm(grad_rate ~ growth_pct + I(growth_pct^2), data = enrollment_growth_graduation)
    cat("Quadratic Model:\n")
    print(summary(quad_model))
  })
  
  # Graph 9: Scatter of student–faculty ratio vs graduation rate
  output$faculty_ratio_plot <- renderPlotly({
    tryCatch({
    faculty_ratio_graduation <- df %>%
        mutate(
          student_faculty_ratio = as.numeric(STUFACR),
          grad_rate = as.numeric(GBA6RTT)
        ) %>%
        filter(!is.na(student_faculty_ratio), between(student_faculty_ratio, 5, 50), !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
        select(UNITID, INSTNM, student_faculty_ratio, grad_rate) %>%
      mutate(
        hover_text = paste("School:", INSTNM, "<br>Ratio:", round(student_faculty_ratio, 1), "<br>Grad Rate:", round(grad_rate, 1), "%")
      ) %>%
      filter(!is.na(student_faculty_ratio), !is.na(grad_rate))
    if (nrow(faculty_ratio_graduation) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", 
                                           x = 0.5, y = 0.5, showarrow = FALSE))
    }
    loess_fit <- tryCatch(loess(grad_rate ~ student_faculty_ratio, data = faculty_ratio_graduation, span = 0.75),
                          error = function(e) lm(grad_rate ~ student_faculty_ratio, data = faculty_ratio_graduation))
      x_range <- seq(min(faculty_ratio_graduation$student_faculty_ratio, na.rm = TRUE),
                     max(faculty_ratio_graduation$student_faculty_ratio, na.rm = TRUE), length.out = 100)
      y_pred <- tryCatch(predict(loess_fit, newdata = data.frame(student_faculty_ratio = x_range)),
                         error = function(e) predict(lm(grad_rate ~ student_faculty_ratio, data = faculty_ratio_graduation), newdata = data.frame(student_faculty_ratio = x_range)))
      valid_idx <- !is.na(y_pred)
      x_range <- as.numeric(x_range[valid_idx])
      y_pred <- as.numeric(y_pred[valid_idx])
      df_plot <- as.data.frame(faculty_ratio_graduation)
      x_vec <- as.numeric(df_plot$student_faculty_ratio)
      y_vec <- as.numeric(df_plot$grad_rate)
      text_vec <- as.character(df_plot$hover_text)
      p <- plot_ly(x = x_vec, y = y_vec, text = text_vec,
                   type = "scatter", mode = "markers", hoverinfo = "text",
                   marker = list(size = 5, opacity = 0.4, color = "steelblue"),
                   name = "Schools")
      
      if (length(x_range) > 0 && length(y_pred) > 0) {
        n_line <- length(x_range)
        p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", n_line), type = "scatter", mode = "lines",
                             line = list(color = "red", width = 2), name = "Trend",
                             hoverinfo = "skip")
      }
      
      p <- p %>% layout(title = list(text = "Student–Faculty Ratio vs Graduation Rate"),
                       xaxis = list(title = "Student–Faculty Ratio"),
                       yaxis = list(title = "6-Year Graduation Rate (%)"))
      
      p
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", 
                                   x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Correlation test for Graph 9.
  output$faculty_ratio_corr <- renderPrint({
    faculty_ratio_graduation <- df %>%
      mutate(
        student_faculty_ratio = as.numeric(STUFACR),
        grad_rate = as.numeric(GBA6RTT)
      ) %>%
      filter(!is.na(student_faculty_ratio), between(student_faculty_ratio, 5, 50), !is.na(grad_rate), between(grad_rate, 0, 100)) %>%
      select(UNITID, INSTNM, grad_rate, student_faculty_ratio)
    
    cat("Correlation test:\n")
    print(cor.test(faculty_ratio_graduation$student_faculty_ratio, faculty_ratio_graduation$grad_rate))
  })
  
  class_size_costs_data <- function() {
    df %>%
      select(UNITID, INSTNM, STUFACR, CLASIZUND20, CLASIZOVE50, TUITION3) %>%
      filter(
        !is.na(TUITION3),
        !is.na(STUFACR),
        dplyr::between(STUFACR, 5, 40)
      )
  }

  # Graph 10a: Scatter of student–faculty ratio vs out-of-state tuition; bubble size = prob classes <20.
  output$class_size_small_plot <- renderPlotly({
    tryCatch({
      analysis_df <- class_size_costs_data()
      if (nrow(analysis_df) == 0) {
        return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
      }
      tuition_col_name <- "TUITION3"
      tuition_vec <- analysis_df[[tuition_col_name]]
      analysis_df$hover_text <- paste("School:", analysis_df$INSTNM, "<br>Ratio:", round(analysis_df$STUFACR, 1), "<br>Tuition: $", round(tuition_vec, 0), "<br>Small Class Prob:", round(analysis_df$CLASIZUND20, 2))
      marker_sizes <- analysis_df$CLASIZUND20 * 20
      marker_sizes[is.na(marker_sizes)] <- 5
      marker_sizes[marker_sizes < 1] <- 1
      marker_sizes[marker_sizes > 100] <- 100
      lm_fit <- lm(tuition_vec ~ STUFACR, data = analysis_df)
      x_range <- seq(min(analysis_df$STUFACR, na.rm = TRUE), max(analysis_df$STUFACR, na.rm = TRUE), length.out = 100)
      y_pred <- predict(lm_fit, newdata = data.frame(STUFACR = x_range))
      valid_idx <- !is.na(y_pred)
      x_range <- x_range[valid_idx]
      y_pred <- y_pred[valid_idx]
      p <- plot_ly(x = as.numeric(analysis_df$STUFACR), y = as.numeric(analysis_df[[tuition_col_name]]),
                   type = "scatter", mode = "markers", text = as.character(analysis_df$hover_text), hoverinfo = "text",
                   marker = list(size = as.numeric(marker_sizes), opacity = 0.3, color = "darkblue", sizemode = "diameter"),
                   name = "Schools")
      if (length(x_range) > 0 && length(y_pred) > 0) {
        p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", length(x_range)), type = "scatter", mode = "lines",
                             line = list(color = "red", width = 2), name = "Trend", hoverinfo = "skip")
      }
      p %>% layout(title = list(text = "Predicting 2024-25 Tuition by Student-Faculty Ratio"),
                   xaxis = list(title = "Student-to-Faculty Ratio"),
                   yaxis = list(title = "Out of state average tuition"))
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })

  # Graph 10b: Same as 10a but bubble size = prob classes >50
  output$class_size_large_plot <- renderPlotly({
    tryCatch({
      analysis_df <- class_size_costs_data()
      if (nrow(analysis_df) == 0) {
        return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE))
      }
      tuition_col_name <- "TUITION3"
      tuition_vec <- analysis_df[[tuition_col_name]]
      analysis_df$hover_text <- paste("School:", analysis_df$INSTNM, "<br>Ratio:", round(analysis_df$STUFACR, 1), "<br>Tuition: $", round(tuition_vec, 0), "<br>Large Class Prob:", round(analysis_df$CLASIZOVE50, 2))
      marker_sizes <- analysis_df$CLASIZOVE50 * 20
      marker_sizes[is.na(marker_sizes)] <- 5
      marker_sizes[marker_sizes < 1] <- 1
      marker_sizes[marker_sizes > 100] <- 100
      lm_fit <- lm(tuition_vec ~ STUFACR, data = analysis_df)
      x_range <- seq(min(analysis_df$STUFACR, na.rm = TRUE), max(analysis_df$STUFACR, na.rm = TRUE), length.out = 100)
      y_pred <- predict(lm_fit, newdata = data.frame(STUFACR = x_range))
      valid_idx <- !is.na(y_pred)
      x_range <- x_range[valid_idx]
      y_pred <- y_pred[valid_idx]
      p <- plot_ly(x = as.numeric(analysis_df$STUFACR), y = as.numeric(analysis_df[[tuition_col_name]]),
                   type = "scatter", mode = "markers", text = as.character(analysis_df$hover_text), hoverinfo = "text",
                   marker = list(size = as.numeric(marker_sizes), opacity = 0.3, color = "darkblue", sizemode = "diameter"),
                   name = "Schools")
      if (length(x_range) > 0 && length(y_pred) > 0) {
        p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", length(x_range)), type = "scatter", mode = "lines",
                             line = list(color = "red", width = 2), name = "Trend", hoverinfo = "skip")
      }
      p %>% layout(title = list(text = "Predicting 2024-25 Tuition by Student-Faculty Ratio"),
                   xaxis = list(title = "Student-to-Faculty Ratio"),
                   yaxis = list(title = "Out of state average tuition"))
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Graph 11a: Scatter of in-state tuition vs room & board; trend line
  output$tuition_roomboard_instate_plot <- renderPlotly({
    tryCatch({
      df_costs <- df %>%
      select(
        UNITID,
        INSTNM,
        InState = TUITION2,
        OutState = TUITION3,
        RoomBoard = RMBRDAMT
      ) %>%
      filter(!is.na(InState) & !is.na(OutState) & !is.na(RoomBoard)) %>%
      mutate(
        hover_text_inst = paste("School:", INSTNM, "<br>In-State Tuition: $", round(InState, 0), "<br>Room & Board: $", round(RoomBoard, 0)),
        hover_text_out = paste("School:", INSTNM, "<br>Out-of-State Tuition: $", round(OutState, 0), "<br>Room & Board: $", round(RoomBoard, 0))
      )
    
    if (nrow(df_costs) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper",
                                           x = 0.5, y = 0.5, showarrow = FALSE))
    }
    lm_fit <- lm(RoomBoard ~ InState, data = df_costs)
    x_range <- seq(min(df_costs$InState, na.rm = TRUE),
                   max(df_costs$InState, na.rm = TRUE), length.out = 100)
    y_pred <- predict(lm_fit, newdata = data.frame(InState = x_range))
    
    df_plot <- as.data.frame(df_costs)
    df_plot$hover_text_inst <- as.character(df_plot$hover_text_inst)
    x_vec <- as.numeric(df_plot$InState)
    y_vec <- as.numeric(df_plot$RoomBoard)
    text_inst <- as.character(df_plot$hover_text_inst)
    p <- plot_ly(x = x_vec, y = y_vec, text = text_inst,
                 type = "scatter", mode = "markers", hoverinfo = "text",
                 marker = list(size = 5, opacity = 0.3, color = "darkblue"),
                 name = "Schools")
    if (length(x_range) > 0 && length(y_pred) > 0) {
      n_line <- length(x_range)
      p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", n_line), type = "scatter", mode = "lines",
                           line = list(color = "blue", width = 2), name = "Trend",
                           hoverinfo = "skip")
    }
    p <- p %>% layout(title = list(text = "In-State Tuition vs. Living Costs"),
                     xaxis = list(title = "Avg In-State Tuition"),
                     yaxis = list(title = "Combined Food/Housing"))
    p
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", 
                                   x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Graph 11b: Scatter of out-of-state tuition vs room & board; trend line
  output$tuition_roomboard_outstate_plot <- renderPlotly({
    tryCatch({
      df_costs <- df %>%
        select(
        UNITID,
        INSTNM,
        InState = TUITION2,
        OutState = TUITION3,
        RoomBoard = RMBRDAMT
      ) %>%
      filter(!is.na(InState) & !is.na(OutState) & !is.na(RoomBoard)) %>%
      mutate(
        hover_text_inst = paste("School:", INSTNM, "<br>In-State Tuition: $", round(InState, 0), "<br>Room & Board: $", round(RoomBoard, 0)),
        hover_text_out = paste("School:", INSTNM, "<br>Out-of-State Tuition: $", round(OutState, 0), "<br>Room & Board: $", round(RoomBoard, 0))
      )
    
    if (nrow(df_costs) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", xref = "paper", yref = "paper",
                                           x = 0.5, y = 0.5, showarrow = FALSE))
    }
    lm_fit <- lm(RoomBoard ~ OutState, data = df_costs)
    x_range <- seq(min(df_costs$OutState, na.rm = TRUE),
                   max(df_costs$OutState, na.rm = TRUE), length.out = 100)
    y_pred <- predict(lm_fit, newdata = data.frame(OutState = x_range))
    
    df_plot <- as.data.frame(df_costs)
    df_plot$hover_text_out <- as.character(df_plot$hover_text_out)
    x_vec <- as.numeric(df_plot$OutState)
    y_vec <- as.numeric(df_plot$RoomBoard)
    text_out <- as.character(df_plot$hover_text_out)
    p <- plot_ly(x = x_vec, y = y_vec, text = text_out,
                 type = "scatter", mode = "markers", hoverinfo = "text",
                 marker = list(size = 5, opacity = 0.3, color = "darkred"),
                 name = "Schools")
    if (length(x_range) > 0 && length(y_pred) > 0) {
      n_line <- length(x_range)
      p <- p %>% add_trace(x = as.numeric(x_range), y = as.numeric(y_pred), text = rep("", n_line), type = "scatter", mode = "lines",
                           line = list(color = "red", width = 2), name = "Trend",
                           hoverinfo = "skip")
    }
    p <- p %>% layout(title = list(text = "Out-of-State Tuition vs. Living Costs"),
                     xaxis = list(title = "Avg Out-of-State Tuition"),
                     yaxis = list(title = "Combined Food/Housing"))
    p
    }, error = function(e) {
      plot_ly() %>% add_annotations(text = safe_error_text(e), xref = "paper", yref = "paper", 
                                   x = 0.5, y = 0.5, showarrow = FALSE)
    })
  })
  
  # Regression summary: room & board ~ in-state + out-of-state tuition.
  output$tuition_roomboard_model <- renderPrint({
    df_costs <- df %>%
      select(
        UNITID,
        INSTNM,
        InState = TUITION2,
        OutState = TUITION3,
        RoomBoard = RMBRDAMT
      ) %>%
      filter(!is.na(InState) & !is.na(OutState) & !is.na(RoomBoard))
    
    cost_model <- lm(RoomBoard ~ InState + OutState, data = df_costs)
    cat("Multiple Linear Regression Model:\n")
    print(summary(cost_model))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
