# app.R -------------------------------------------------------------
# Shiny dashboard for ER–opioid spatial models (sdmTMB)

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(broom)     # for tidy()
library(sdmTMB)
library(DHARMa)

# ------------------------------------------------------------------
# Load pre-fitted objects:
#   models.RData:   df, fits, aic_table_coef       (Deaths models)
#   models_ER.RData: df_ER, fits_ER               (ER models)
#   aic_table_coef_ER.rds: aic_table_coef_ER
# ------------------------------------------------------------------
load("models.RData")          # df, fits, aic_table_coef
load("models_ER.RData")       # df_ER, fits_ER  (adjust if different)
aic_table_coef_ER <- readRDS("aic_table_coef_ER.rds")

# sanity check (optional)
stopifnot(
  exists("df"), exists("fits"), exists("aic_table_coef"),
  exists("df"), exists("fits_ER"), exists("aic_table_coef_ER")
)

# ============================== UI =================================

ui <- fluidPage(
  titlePanel("Opioid ER–Death & ER Spatial Models (sdmTMB)"),
  
  sidebarLayout(
    sidebarPanel(
      # NEW: choose which model set to view -------------------------
      selectInput(
        inputId = "model_set",
        label   = "Model set:",
        choices = c("Deaths models" = "deaths",
                    "ER models"     = "er"),
        selected = "deaths"
      ),
      
      tags$hr(),
      # model selector (built dynamically based on model_set) -------
      uiOutput("model_dropdown"),
      
      tags$hr(),
      h4("Selected model info"),
      verbatimTextOutput("model_info"),
      
      tags$hr(),
      h4("Selected model summary"),
      verbatimTextOutput("model_summary"),
      
      tags$hr(),
      h4("Residuals vs Fitted"),
      plotOutput("resid_plot", height = "250px")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Model & diagnostics",
          h3("Model comparison table"),
          DTOutput("aic_tbl")
        ),
        
        tabPanel(
          "Effects & fit",
          fluidRow(
            column(
              width = 6,
              h3("ER → outcome (partial dependence)"),
              plotOutput("er_pdp", height = "300px")
            ),
            column(
              width = 6,
              h3("Temporal trend (predicted outcome by year)"),
              plotOutput("time_trend", height = "300px")
            )
          ),
          tags$hr(),
          fluidRow(
            column(
              width = 6,
              h3("Observed vs predicted"),
              plotOutput("fit_vs_obs", height = "300px")
            ),
            column(
              width = 6,
              h3("Coefficient forest plot"),
              plotOutput("coef_plot", height = "300px")
            )
          )
        ),
        
        tabPanel(
          "Diagnostics",
          h3("DHARMa residual diagnostics"),
          plotOutput("dharma_resid_plot", height = "500px"),
          br(),
          h4("DHARMa tests"),
          verbatimTextOutput("dharma_tests")
        )
      )
    )
  )
)

# ============================ SERVER ===============================

server <- function(input, output, session) {
  
  # ---------- Reactives to switch between Deaths vs ER sets --------
  current_aic_table <- reactive({
    if (input$model_set == "deaths") {
      aic_table_coef
    } else {
      aic_table_coef_ER
    }
  })
  
  current_fits <- reactive({
    if (input$model_set == "deaths") {
      fits
    } else {
      fits_ER
    }
  })
  
  current_df <- reactive({
    if (input$model_set == "deaths") {
      df
    } else {
      df
    }
  })
  
  baseline_row <- reactive({
    df_now <- current_df()
    df_now |>
      summarise(
        across(
          where(is.numeric),
          ~ median(.x, na.rm = TRUE)
        ),
        across(
          where(\(x) !is.numeric(x)),
          ~ dplyr::first(.x)
        )
      ) |>
      slice(1)
  })
  
  # -- dynamic model dropdown (depends on model_set) ----------------
  output$model_dropdown <- renderUI({
    at <- current_aic_table()
    req(nrow(at) > 0)
    default_model <- at$model[which.min(at$AIC)]
    selectInput(
      inputId  = "model_name",
      label    = "Select model:",
      choices  = at$model,
      selected = default_model
    )
  })
  
  # -- AIC table with clickable rows --------------------------------
  output$aic_tbl <- renderDT({
    at <- current_aic_table()
    datatable(
      at,
      filter    = "top",
      selection = "single",
      options   = list(
        pageLength = 50,
        autoWidth  = TRUE
      )
    )
  })
  
  # -- reactive: chosen model name (via table OR dropdown) ----------
  selected_model_name <- reactive({
    at      <- current_aic_table()
    row_idx <- input$aic_tbl_rows_selected
    if (!is.null(row_idx) && length(row_idx) == 1 && row_idx <= nrow(at)) {
      at$model[row_idx]
    } else {
      input$model_name
    }
  })
  
  # keep dropdown synced with table selection
  observeEvent(input$aic_tbl_rows_selected, {
    updateSelectInput(session, "model_name",
                      selected = selected_model_name())
  })
  
  # -- lookup sdmTMB object -----------------------------------------
  selected_fit <- reactive({
    current_fits()[[selected_model_name()]]
  })
  
  # -- compact info about AIC + ER coeff ----------------------------
  output$model_info <- renderText({
    mname <- selected_model_name()
    at    <- current_aic_table()
    row   <- at %>% filter(model == mname)
    if (nrow(row) == 0) return("No model selected.")
    paste0(
      "Model: ", mname,
      "\nAIC: ", round(row$AIC, 2),
      "\nΔAIC: ", round(row$DeltaAIC, 2),
      if ("coef_est" %in% names(row)) {
        paste0("\nER coef: ", row$coef_est,
               " (SE = ", row$coef_se, ")")
      } else ""
    )
  })
  
  # -- summary() of the selected model ------------------------------
  output$model_summary <- renderPrint({
    fit <- selected_fit()
    if (is.null(fit)) {
      cat("No fit found for model:", selected_model_name())
    } else {
      summary(fit)
    }
  })
  
  # Helper: safe prediction wrapper ---------------------------------
  safe_predict <- function(fit, newdata = NULL, type = "response", se_fit = FALSE) {
    p <- if (is.null(newdata)) {
      predict(fit, type = type, se_fit = se_fit)
    } else {
      predict(fit, newdata = newdata, type = type, se_fit = se_fit)
    }
    p_df <- as.data.frame(p)
    
    # Ensure 'est' column exists
    if (!"est" %in% names(p_df) && ncol(p_df) >= 1) {
      p_df$est <- as.numeric(p_df[[1]])
    }
    # Ensure 'est_se' column exists if needed
    if (se_fit && !"est_se" %in% names(p_df)) {
      if (ncol(p_df) >= 2) {
        p_df$est_se <- as.numeric(p_df[[2]])
      } else {
        p_df$est_se <- 0   # no SE provided; treat as 0 for plotting
      }
    }
    p_df
  }
  
  # -- Residual vs fitted plot --------------------------------------
  output$resid_plot <- renderPlot({
    fit <- selected_fit()
    req(fit)
    
    pred_df <- tryCatch(
      safe_predict(fit, type = "response", se_fit = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(pred_df)) {
      plot.new()
      title(main = paste("No fitted values for", selected_model_name()))
      return()
    }
    
    ff <- as.numeric(pred_df$est)  # fitted
    
    rr <- tryCatch(
      residuals(fit, type = "pearson"),
      error = function(e) residuals(fit)
    )
    rr <- as.numeric(rr)
    
    if (length(ff) != length(rr)) {
      plot.new()
      title(main = "Length mismatch between fitted values and residuals.")
      return()
    }
    
    plot(
      ff, rr,
      log  = "x",
      pch  = 21,
      bg   = "gray90",
      xlab = "Fitted values (log scale)",
      ylab = "Pearson residuals",
      main = paste("Residuals vs Fitted for", selected_model_name())
    )
    abline(h = 0, lty = 2)
  })
  
  # -- ER partial dependence plot -----------------------------------
  output$er_pdp <- renderPlot({
    fit <- selected_fit()
    req(fit)
    
    df_now <- current_df()
    req("ER_annual_percapita_z" %in% names(df_now))
    
    er_seq <- seq(
      min(df_now$ER_annual_percapita_z, na.rm = TRUE),
      max(df_now$ER_annual_percapita_z, na.rm = TRUE),
      length.out = 100
    )
    
    newdat <- baseline_row()[rep(1, length(er_seq)), ]
    newdat$ER_annual_percapita_z <- er_seq
    
    pred_df <- safe_predict(fit, newdata = newdat, type = "response", se_fit = TRUE)
    
    pd <- data.frame(
      er  = er_seq,
      est = as.numeric(pred_df$est),
      se  = as.numeric(pred_df$est_se)
    )
    
    ggplot(pd, aes(x = er, y = est)) +
      geom_line() +
      geom_ribbon(aes(ymin = est - 1.96 * se,
                      ymax = est + 1.96 * se),
                  alpha = 0.2) +
      labs(
        x = "ER annual per capita (z-score)",
        y = "Predicted response",
        title = "ER → outcome (partial dependence)"
      ) +
      theme_minimal()
  })
  
  # -- Temporal trend plot ------------------------------------------
  output$time_trend <- renderPlot({
    fit   <- selected_fit()
    req(fit)
    
    df_now <- current_df()
    req("year" %in% names(df_now), "time_index" %in% names(df_now))
    
    years <- sort(unique(df_now$year))
    n     <- length(years)
    
    newdat <- baseline_row()[rep(1, n), ]
    newdat$year       <- years
    newdat$time_index <- seq_len(n)  # adjust if your time coding differs
    
    pred_df <- safe_predict(fit, newdata = newdat, type = "response", se_fit = FALSE)
    
    td <- data.frame(
      year = years,
      est  = as.numeric(pred_df$est)
    )
    
    ggplot(td, aes(x = year, y = est)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Year",
        y = "Predicted response",
        title = "Temporal trend (holding covariates at baseline)"
      ) +
      theme_minimal()
  })
  
  # -- Observed vs predicted plot -----------------------------------
  output$fit_vs_obs <- renderPlot({
    fit <- selected_fit()
    req(fit)
    
    pred_df <- tryCatch(
      safe_predict(fit, type = "response", se_fit = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(pred_df)) {
      plot.new()
      title(main = paste("No predictions for", selected_model_name()))
      return()
    }
    
    df_now <- current_df()
    
    # Get the response variable name from the model formula
    resp_var <- all.vars(formula(fit))[1]
    validate(
      need(resp_var %in% names(df_now),
           paste("Response variable", resp_var, "not found in current data."))
    )
    
    df_plot <- data.frame(
      obs  = df_now[[resp_var]],
      pred = as.numeric(pred_df$est)
    )
    
    ggplot(df_plot, aes(x = obs, y = pred)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      labs(
        x = paste("Observed", resp_var),
        y = "Predicted",
        title = "Observed vs predicted"
      ) +
      theme_minimal()
  })
  
  # -- Coefficient forest plot --------------------------------------
  output$coef_plot <- renderPlot({
    fit <- selected_fit()
    req(fit)
    
    # 1. Get a tidy coefficient table ----------------------------------
    if (inherits(fit, "sdmTMB")) {
      coefs <- sdmTMB::tidy(fit, conf.int = TRUE)
    } else {
      coefs <- broom::tidy(fit, conf.int = TRUE)
    }
    
    validate(
      need(!is.null(coefs) && nrow(coefs) > 0,
           "No coefficients available for this model.")
    )
    
    # keep only fixed effects if sdmTMB adds random/smoother rows
    if ("effect" %in% names(coefs)) {
      coefs <- dplyr::filter(coefs, effect == "fixed")
    }
    
    validate(
      need(
        all(c("term", "estimate", "conf.low", "conf.high") %in% names(coefs)),
        "Tidy table is missing one of: term, estimate, conf.low, conf.high."
      )
    )
    
    coefs <- coefs %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(
        term = factor(term, levels = rev(unique(term)))
      )
    
    validate(
      need(nrow(coefs) > 0, "No non-intercept coefficients to plot.")
    )
    
    ggplot(coefs, aes(x = estimate, y = term)) +
      geom_point() +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
      geom_vline(xintercept = 0, linetype = 2) +
      labs(
        x = "Estimate",
        y = NULL,
        title = "Fixed-effect estimates with 95% CI"
      ) +
      theme_minimal()
  })
  
  # --- DHARMa residuals for sdmTMB (and others) --------------------
  dharma_object <- reactive({
    fit <- selected_fit()
    req(fit)
    
    set.seed(123)
    sims <- simulate(fit, nsim = 250, type = "mle-mvn")
    
    sdmTMB::dharma_residuals(
      simulated_response = sims,
      object             = fit,
      plot               = FALSE,
      return_DHARMa      = TRUE
    )
  })
  
  output$dharma_resid_plot <- renderPlot({
    dh <- dharma_object()
    plot(dh)  # DHARMa's S3 plot method
  })
  
  output$dharma_tests <- renderPrint({
    dh <- dharma_object()
    
    cat("Dispersion test:\n")
    print(DHARMa::testDispersion(dh))
    
    cat("\nUniformity (KS test):\n")
    print(DHARMa::testUniformity(dh))
    
    cat("\nZero-inflation test:\n")
    print(DHARMa::testZeroInflation(dh))
  })
  
}

# ============================ RUN APP ==============================

shinyApp(ui = ui, server = server)
