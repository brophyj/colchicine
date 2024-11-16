library(shiny)
library(ggplot2)

# Define the function to simulate Bayesian analysis and create plots
bayesian_analysis <- function(n1, e1, n2, e2, prior_n1, prior_e1, prior_n2, prior_e2) {
  # Beta parameters
  alpha1_prior <- prior_e1 + 1
  beta1_prior <- prior_n1 - prior_e1 + 1
  alpha2_prior <- prior_e2 + 1
  beta2_prior <- prior_n2 - prior_e2 + 1
  
  # Generate distributions
  prior_p1 <- rbeta(10000, alpha1_prior, beta1_prior)
  prior_p2 <- rbeta(10000, alpha2_prior, beta2_prior)
  like_p1 <- rbeta(10000, e1 + 1, n1 - e1 + 1)
  like_p2 <- rbeta(10000, e2 + 1, n2 - e2 + 1)
  post_p1 <- rbeta(10000, alpha1_prior + e1, beta1_prior + n1 - e1)
  post_p2 <- rbeta(10000, alpha2_prior + e2, beta2_prior + n2 - e2)
  
  # Calculate differences and ratios for posterior
  rd_posterior <- post_p1 - post_p2
  rr_posterior <- post_p1 / post_p2
  
  # Return results
  list(
    posterior_rd = rd_posterior,
    posterior_rr = rr_posterior,
    RD_Plot = create_triplot(prior_p1 - prior_p2, like_p1 - like_p2, rd_posterior, "Risk Difference"),
    RR_Plot = create_triplot(prior_p1 / prior_p2, like_p1 / like_p2, rr_posterior, "Relative Risk", c(NA, 10))
  )
}

# Create triplot function
create_triplot <- function(prior, likelihood, posterior, title, xlims = NULL) {
  data <- data.frame(
    Value = c(prior, likelihood, posterior),
    Group = factor(rep(c("Prior", "Likelihood", "Posterior"), each = length(prior)))
  )
  p <- ggplot(data, aes(x = Value, fill = Group)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Prior" = "blue", "Likelihood" = "green", "Posterior" = "red")) +
    labs(title = title, x = title, y = "Density") +
    theme_minimal() +
    guides(fill = guide_legend(title = "Distribution Type"))
  if (!is.null(xlims)) {
    p <- p + xlim(xlims)
  }
  p
}

# UI setup remains the same as previously defined
ui <- fluidPage(
  titlePanel("Bayesian Analysis of Binomial Data"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, wellPanel(
          h3("Prior Data"),
          numericInput("prior_n1", "Total in arm 1", 50),
          numericInput("prior_e1", "Events in arm 1", 15),
          numericInput("prior_n2", "Total in arm 2", 50),
          numericInput("prior_e2", "Events in arm 2", 5)
        )),
        column(6, wellPanel(
          h3("Current Data"),
          numericInput("n1", "Total in arm 1", 100),
          numericInput("e1", "Events in arm 1", 20),
          numericInput("n2", "Total in arm 2", 100),
          numericInput("e2", "Events in arm 2", 10)
        ))
      ),
      actionButton("go", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("analysisOutput")),
        tabPanel("Triplot RD", plotOutput("RD_Triplot")),
        tabPanel("Triplot RR", plotOutput("RR_Triplot")),
        tabPanel("Posterior RD Plot", plotOutput("posteriorRDPlot")),
        tabPanel("Posterior RR Plot", plotOutput("posteriorRRPlot"))
      )
    )
  )
)

# Server logic to generate outputs
server <- function(input, output) {
  observeEvent(input$go, {
    results <- bayesian_analysis(input$n1, input$e1, input$n2, input$e2,
                                 input$prior_n1, input$prior_e1, input$prior_n2, input$prior_e2)
    
    output$analysisOutput <- renderText({
      sprintf("Point estimate for RD is %.2f [95%% CrI: %.2f, %.2f]\nPoint estimate for RR is %.2f [95%% CrI: %.2f, %.2f]",
              mean(results$posterior_rd), quantile(results$posterior_rd, c(0.025, 0.975))[1], quantile(results$posterior_rd, c(0.025, 0.975))[2],
              mean(results$posterior_rr), quantile(results$posterior_rr, c(0.025, 0.975))[1], quantile(results$posterior_rr, c(0.025, 0.975))[2])
    })
    
    output$RD_Triplot <- renderPlot({ results$RD_Plot })
    output$RR_Triplot <- renderPlot({ results$RR_Plot })
    
    output$posteriorRDPlot <- renderPlot({
      ggplot(data.frame(Value = results$posterior_rd), aes(x = Value)) +
        geom_density(fill = "blue", alpha = 0.5) +
        labs(title = "Posterior Distribution of Risk Difference", x = "Risk Difference", y = "Density") +
        theme_minimal()
    })
    
    output$posteriorRRPlot <- renderPlot({
      ggplot(data.frame(Value = results$posterior_rr), aes(x = Value)) +
        geom_density(fill = "blue", alpha = 0.5) +
        labs(title = "Posterior Distribution of Relative Risk", x = "Relative Risk", y = "Density") +
        theme_minimal()
    })
  })
}

# Execute the Shiny app
shinyApp(ui = ui, server = server)
