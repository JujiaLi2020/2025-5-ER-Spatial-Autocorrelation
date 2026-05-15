library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(DHARMa)

# ------------------------------------------------------------------
# Load the specific best model
# ------------------------------------------------------------------
load("best_model.RData") # Loads 'final_fit' and 'df'

# ============================== UI =================================

ui <- fluidPage(
  titlePanel("Best Model Report: ER–Opioid Spatial Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Specification"),
      verbatimTextOutput("model_formula"),
      hr(),
      h4("Fixed Effects Summary"),
      verbatimTextOutput("model_summary"),
      width = 4
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Effect Estimates", 
                 h3("Coefficient Forest Plot"),
                 plotOutput("coef_plot", height = "350px"),
                 h3("Parameter Table"),
                 DTOutput("coef_table")),
        
        tabPanel("Spatial Results", 
                 h3("Spatial Random Effects (Omega)"),
                 plotOutput("spatial_map", height = "500px"),
                 helpText("Red areas show higher-than-average risk; blue areas show lower-than-average risk.")),
        
        tabPanel("Diagnostics", 
                 h3("DHARMa Residuals"),
                 plotOutput("dharma_plot", height = "500px"),
                 verbatimTextOutput("dharma_tests"))
      )
    )
  )
)

# ============================ SERVER ===============================

server <- function(input, output, session) {
  
  # 1. Formula Display
  output$model_formula <- renderPrint({
    final_fit$formula
  })
  
  # 2. Model Summary (Text)
  output$model_summary <- renderPrint({
    print(final_fit)
  })
  
  # 3. Coefficient Plot
  output$coef_plot <- renderPlot({
    tidy_df <- sdmTMB::tidy(final_fit, conf.int = TRUE)
    tidy_df %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(estimate, term)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      theme_minimal() +
      labs(x = "Estimate (95% CI)", y = NULL)
  })
  
  # 4. Coefficient Table
  output$coef_table <- renderDT({
    sdmTMB::tidy(final_fit, conf.int = TRUE) %>%
      datatable(options = list(dom = 't')) %>%
      formatRound(columns = 2:5, digits = 4)
  })
  
  # 5. Spatial Map (Omega_s)
  output$spatial_map <- renderPlot({
    # Generating spatial predictions
    pred <- predict(final_fit)
    
    # Note: Replace 'X' and 'Y' with your actual coordinate column names
    ggplot(pred, aes(X, Y, color = omega_s)) +
      geom_point(size = 2) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red") +
      theme_void() +
      labs(title = "Spatial Random Effects", color = "Deviance")
  })
  
  # 6. DHARMa Diagnostics
  output$dharma_plot <- renderPlot({
    set.seed(123)
    # Simulate for DHARMa
    sims <- simulate(final_fit, nsim = 250)
    dh <- sdmTMB::dharma_residuals(sims, final_fit, return_DHARMa = TRUE)
    plot(dh)
  })
  
  output$dharma_tests <- renderPrint({
    set.seed(123)
    sims <- simulate(final_fit, nsim = 250)
    dh <- sdmTMB::dharma_residuals(sims, final_fit, return_DHARMa = TRUE)
    cat("Dispersion Test:\n")
    print(DHARMa::testDispersion(dh))
    cat("\nZero-Inflation Test:\n")
    print(DHARMa::testZeroInflation(dh))
  })
}

# ============================ RUN APP ==============================
shinyApp(ui = ui, server = server)
