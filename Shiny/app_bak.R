# app.R -------------------------------------------------------------
library(shiny)
library(DT)
library(dplyr)
library(sdmTMB)
library(broom)

# ---- load precomputed model objects ----
fits <- readRDS("fits.rds")
aic_table_coef <- readRDS("aic_table_coef.rds")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Opioid ER–Death Models: Selection & Diagnostics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "model_name",
        label   = "Select model:",
        choices = aic_table_coef$model,
        selected = aic_table_coef$model[which.min(aic_table_coef$AIC)]
      ),
      h4("Selected model info"),
      verbatimTextOutput("model_info")
    ),
    
    mainPanel(
      h3("Model comparison table"),
      DTOutput("aic_tbl"),
      tags$hr(),
      
      h3("Selected model summary"),
      verbatimTextOutput("model_summary"),
      tags$hr(),
      
      h3("Selected model diagnostic plot"),
      plotOutput("model_plot", height = "400px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  output$aic_tbl <- renderDT({
    datatable(
      aic_table_coef,
      filter = "top",
      selection = "single",
      options = list(
        pageLength = 50,
        autoWidth = TRUE
      )
    )
  })
  
  selected_model <- reactive({
    row_idx <- input$aic_tbl_rows_selected
    if (length(row_idx) == 1) {
      aic_table_coef$model[row_idx]
    } else {
      input$model_name
    }
  })
  
  observeEvent(input$aic_tbl_rows_selected, {
    mname <- selected_model()
    updateSelectInput(session, "model_name", selected = mname)
  })
  
  output$model_info <- renderText({
    mname <- selected_model()
    row   <- aic_table_coef %>% filter(model == mname)
    if (nrow(row) == 0) return("No model selected.")
    
    paste0(
      "Model: ", mname,
      "\nAIC: ", round(row$AIC, 2),
      "\nΔAIC: ", round(row$DeltaAIC, 2),
      "\nER coef: ", row$coef_est,
      " (SE = ", row$coef_se, ")"
    )
  })
  
  output$model_summary <- renderPrint({
    mname <- selected_model()
    fit   <- fits[[mname]]
    if (is.null(fit)) {
      cat("No fit found for model:", mname)
    } else {
      summary(fit)
    }
  })
  
  output$model_plot <- renderPlot({
    mname <- selected_model()
    fit   <- fits[[mname]]
    req(fit)
    
    ff_raw <- tryCatch(fitted(fit), error = function(e) NULL)
    rr_raw <- tryCatch(residuals(fit, type = "pearson"), error = function(e) residuals(fit))
    
    if (is.null(ff_raw)) {
      plot.new()
      title(main = paste("No fitted values for", mname))
      return()
    }
    
    ff <- if (is.data.frame(ff_raw) || is.list(ff_raw)) {
      if ("est" %in% names(ff_raw)) as.numeric(ff_raw$est) else as.numeric(ff_raw[[1]])
    } else as.numeric(ff_raw)
    
    rr <- if (is.data.frame(rr_raw) || is.list(rr_raw)) {
      as.numeric(rr_raw[[1]])
    } else as.numeric(rr_raw)
    
    plot(
      ff, rr,
      log  = "x",
      pch  = 21, bg = "gray90",
      xlab = "Fitted values (log scale)",
      ylab = "Pearson residuals",
      main = paste("Residuals vs Fitted for", mname)
    )
    abline(h = 0, lty = 2)
  })
}

# ---- run app ----
shinyApp(ui = ui, server = server)
