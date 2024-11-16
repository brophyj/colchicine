library(shiny)
library(ggplot2)

# Function to simulate Bayesian analysis and generate detailed triplots
bayesian_analysis <- function(n1, e1, n2, e2, prior_n1, prior_e1, prior_n2, prior_e2) {
  set.seed(123)  # Set seed for reproducibility
  
  alpha1_prior <- prior_e1 + 1
  beta1_prior <- prior_n1 - prior_e1 + 1
  alpha2_prior <- prior_e2 + 1
  beta2_prior <- prior_n2 - prior_e2 + 1
  
  prior_p1 <- rbeta(10000, alpha1_prior, beta1_prior)
  prior_p2 <- rbeta(10000, alpha2_prior, beta2_prior)
  like_p1 <- rbeta(10000, e1 + 1, n1 - e1 + 1)
  like_p2 <- rbeta(10000, e2 + 1, n2 - e2 + 1)
  post_p1 <- rbeta(10000, alpha1_prior + e1, beta1_prior + n1 - e1)
  post_p2 <- rbeta(10000, alpha2_prior + e2, beta2_prior + n2 - e2)
  
  # Calculate risk difference and relative risk as intervention / control
  rd_posterior <- post_p2 - post_p1
  rr_posterior <- post_p2 / post_p1  # Change to intervention / control form
  
  return(list(
    posterior_rd = rd_posterior,
    posterior_rr = rr_posterior,
    RD_Plot = create_triplot(prior_p2 - prior_p1, like_p2 - like_p1, rd_posterior, "Risk difference (intervention arm - control arm)"),
    RR_Plot = create_triplot(prior_p2 / prior_p1, like_p2 / like_p1, rr_posterior, "Relative risk (intervention / control)", c(NA, 2.5)) # Truncated x-axis
  ))
}

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

# Define UI
ui <- fluidPage(
  titlePanel("Bayesian Analysis of Binomial Data"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, wellPanel(
          h3("Information"),
          p("This app performs a conjugate analysis using informative prior information expressed as binary outcome data with the results of the current trial to provide Bayesian posterior risk difference (RD) and risk ratio (RR) estimates with their accompanying probability density functions and plots. A vague prior can be used by entering '1' into all the cells for prior data. You can also set the minimal clinically important difference (MCID). To execute the analysis, scroll down and click on the 'Run Analysis' button.")
        ))
      ),
      fluidRow(
        column(6, wellPanel(
          h3("Prior Data"),
          numericInput("prior_n1", "Total in control arm", 50),
          numericInput("prior_e1", "Events in control arm", 15),
          numericInput("prior_n2", "Total in intervention arm", 50),
          numericInput("prior_e2", "Events in intervention arm", 5)
        )),
        column(6, wellPanel(
          h3("Current Data"),
          numericInput("n1", "Total in control arm", 100),
          numericInput("e1", "Events in control arm", 20),
          numericInput("n2", "Total in intervention arm", 100),
          numericInput("e2", "Events in intervention arm", 10)
        ))
      ),
      fluidRow(
        column(12, wellPanel(
          h3("Minimal Clinically Important Difference (MCID)"),
          numericInput("mcid_rd", "Risk Difference (RD)", value = 0.01),
          tags$p("Enter data as follows: if 1% RD is judged to be the MCID then enter 0.01"),
          numericInput("mcid_rr", "Risk Ratio (RR)", value = 0.1),
          tags$p("Enter data as follows: if 10% relative RR reduction is judged to be the MCID then enter 0.1")
        ))
      ),
      actionButton("go", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("analysisOutput")),
        tabPanel("Triplot RD", plotOutput("RD_Triplot")),
        tabPanel("Triplot RR", plotOutput("RR_Triplot")),
        tabPanel("Posterior RD Plot", 
                 plotOutput("posteriorRDPlot"), 
                 htmlOutput("aucDescriptionRD")),
        tabPanel("Posterior RR Plot", 
                 plotOutput("posteriorRRPlot"),
                 htmlOutput("aucDescriptionRR"))
      )
    )
  )
)

# Server function to process data and generate outputs
server <- function(input, output) {
  observeEvent(input$go, {
    results <- bayesian_analysis(input$n1, input$e1, input$n2, input$e2,
                                 input$prior_n1, input$prior_e1, input$prior_n2, input$prior_e2)
    
    mcid_rd <- abs(input$mcid_rd)  # Ensure MCID is positive for RD calculation
    mcid_rr <- abs(input$mcid_rr)  # Ensure MCID is positive for RR calculation
    
    # Calculate the probabilities relative to RD MCID
    prob_less_mcid_rd <- mean(results$posterior_rd < -mcid_rd) * 100
    prob_greater_mcid_rd <- mean(results$posterior_rd > mcid_rd) * 100
    prob_within_mcid_rd <- mean(results$posterior_rd >= -mcid_rd & results$posterior_rd <= mcid_rd) * 100
    
    # Calculate the probabilities relative to RR MCID
    prob_less_mcid_rr <- mean(results$posterior_rr < 1 - mcid_rr) * 100
    prob_greater_mcid_rr <- mean(results$posterior_rr > 1 + mcid_rr) * 100
    prob_within_mcid_rr <- mean(results$posterior_rr >= 1 - mcid_rr & results$posterior_rr <= 1 + mcid_rr) * 100
    
    output$analysisOutput <- renderText({
      sprintf(
        "Point estimate for RD is %.2f [95%% CrI: %.2f, %.2f]\nPoint estimate for RR is %.2f [95%% CrI: %.2f, %.2f]\n\nYou have selected an MCID RD of %.2f%%.\nThe probability of the risk difference being < -MCID = %.2f%%\nThe probability of the risk difference being between -MCID and +MCID \n(i.e., region of practical equivalence) = %.2f%%\nThe probability of the risk difference being > +MCID = %.2f%%\n\nYou have selected an MCID RR of %.2f.\nThe probability of the relative risk being < (1 - MCID) = %.2f%%\nThe probability of the relative risk being between (1 - MCID) and (1 + MCID) \n(i.e., region of practical equivalence) = %.2f%%\nThe probability of the relative risk being > (1 + MCID) = %.2f%%",
        mean(results$posterior_rd), quantile(results$posterior_rd, c(0.025, 0.975))[1], quantile(results$posterior_rd, c(0.025, 0.975))[2],
        mean(results$posterior_rr), quantile(results$posterior_rr, c(0.025, 0.975))[1], quantile(results$posterior_rr, c(0.025, 0.975))[2],
        mcid_rd * 100, prob_less_mcid_rd, prob_within_mcid_rd, prob_greater_mcid_rd,
        mcid_rr, prob_less_mcid_rr, prob_within_mcid_rr, prob_greater_mcid_rr
      )
    })
    
    output$RD_Triplot <- renderPlot({ results$RD_Plot })
    output$RR_Triplot <- renderPlot({ results$RR_Plot })
    
    output$posteriorRDPlot <- renderPlot({
      data <- data.frame(Value = results$posterior_rd)
      
      # Calculate density data for the entire distribution
      density_data <- density(data$Value)
      density_df <- data.frame(x = density_data$x, y = density_data$y)
      
      # Split density data into three regions: left of -MCID, between -MCID and +MCID, and right of +MCID
      left_region <- subset(density_df, x < -mcid_rd)
      middle_region <- subset(density_df, x >= -mcid_rd & x <= mcid_rd)
      right_region <- subset(density_df, x > mcid_rd)
      
      # Create the plot
      p <- ggplot() +
        geom_area(data = left_region, aes(x = x, y = y), fill = "green", alpha = 0.5) +
        geom_area(data = middle_region, aes(x = x, y = y), fill = "yellow", alpha = 0.5) +
        geom_area(data = right_region, aes(x = x, y = y), fill = "red", alpha = 0.5) +
        geom_line(data = density_df, aes(x = x, y = y), size = 1) +
        geom_vline(xintercept = -mcid_rd, color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = mcid_rd, color = "red", linetype = "dashed", size = 1) +
        labs(title = "Posterior Distribution of Risk Difference", x = "Risk difference (intervention arm - control arm)", y = "Density") +
        theme_minimal()
      p
    })
    
    output$aucDescriptionRD <- renderUI({
      HTML(sprintf("<p><strong style='color:green'>Green</strong> AUC to the left of 1st dashed line = P (< -MCID) = %.2f%% (i.e., intervention better than control)</p>
                          <p><strong style='color:yellow'>Yellow</strong> AUC between 2 dashed lines = %.2f%% (i.e., region of practical equivalence (ROPE))</p>
                          <p><strong style='color:red'>Red</strong> AUC to the right of 2nd dashed line = P (> +MCID) = %.2f%% (i.e., control better than intervention)</p>",
                   prob_less_mcid_rd, prob_within_mcid_rd, prob_greater_mcid_rd))
    })
    
    output$posteriorRRPlot <- renderPlot({
      data <- data.frame(Value = results$posterior_rr)
      
      # Calculate density data for the entire distribution
      density_data <- density(data$Value)
      density_df <- data.frame(x = density_data$x, y = density_data$y)
      
      # Split density data into three regions: left of (1 - MCID), between (1 - MCID) and (1 + MCID), and right of (1 + MCID)
      left_region <- subset(density_df, x < 1 - mcid_rr)
      middle_region <- subset(density_df, x >= 1 - mcid_rr & x <= 1 + mcid_rr)
      right_region <- subset(density_df, x > 1 + mcid_rr)
      
      # Create the plot
      p <- ggplot() +
        geom_area(data = left_region, aes(x = x, y = y), fill = "green", alpha = 0.5) +
        geom_area(data = middle_region, aes(x = x, y = y), fill = "yellow", alpha = 0.5) +
        geom_area(data = right_region, aes(x = x, y = y), fill = "red", alpha = 0.5) +
        geom_line(data = density_df, aes(x = x, y = y), size = 1) +
        geom_vline(xintercept = 1 - mcid_rr, color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = 1 + mcid_rr, color = "red", linetype = "dashed", size = 1) +
        labs(title = "Posterior Distribution of Relative Risk", x = "Relative risk (intervention / control)", y = "Density") +
        theme_minimal()
      p
    })
    
    output$aucDescriptionRR <- renderUI({
      HTML(sprintf("<p><strong style='color:green'>Green</strong> AUC to the left of 1st dashed line = P (RR < (1 - MCID)) = %.2f%% (i.e., intervention better than control)</p>
                          <p><strong style='color:yellow'>Yellow</strong> AUC between 2 dashed lines = P ((1 - MCID) < RR < (1 + MCID)) = %.2f%% (i.e., region of practical equivalence (ROPE))</p>
                          <p><strong style='color:red'>Red</strong> AUC to the right of 2nd dashed line = P (RR > (1 + MCID)) = %.2f%% (i.e., control better than intervention)</p>",
                   prob_less_mcid_rr, prob_within_mcid_rr, prob_greater_mcid_rr))
    })
  })
}

shinyApp(ui = ui, server = server)
