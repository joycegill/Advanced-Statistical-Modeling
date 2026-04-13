deploy_app_with_retry <- function(
  app_dir = ".",
  app_name = NULL,
  account = NULL,
  server = "shinyapps.io",
  max_attempts = 6L,
  initial_wait = 10
) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Package 'rsconnect' is required. Install it with install.packages('rsconnect').")
  }

  is_task_conflict <- function(e) {
    msg <- conditionMessage(e)
    grepl("HTTP status 409", msg, fixed = TRUE) &&
      grepl("tasks? already in progress|task.*in progress", msg, ignore.case = TRUE)
  }

  attempt <- 1L
  wait_sec <- as.numeric(initial_wait)

  repeat {
    message(sprintf("[deploy] Attempt %d of %d ...", attempt, max_attempts))

    ok <- tryCatch({
      rsconnect::deployApp(
        appDir = app_dir,
        appName = app_name,
        account = account,
        server = server
      )
      TRUE
    }, error = function(e) {
      if (is_task_conflict(e) && attempt < max_attempts) {
        message(sprintf(
          "[deploy] Server busy (409: task in progress). Retrying in %s seconds ...",
          wait_sec
        ))
        Sys.sleep(wait_sec)
        FALSE
      } else {
        stop(e)
      }
    })

    if (ok) {
      message("[deploy] Success.")
      return(invisible(TRUE))
    }

    attempt <- attempt + 1L
    wait_sec <- min(wait_sec * 2, 300)
  }
}

# Example usage:
# source("deploy_helper.R")
# deploy_app_with_retry(
#   app_dir = ".",
#   app_name = "interactive-visualizations"
# )
