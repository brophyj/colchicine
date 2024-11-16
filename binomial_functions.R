# binomial congugate analysis function
library(ggplot2)
library(gridExtra)

# Bayesian analysis function with detailed outputs and plots
bayesian_analysis <- function(n1, e1, n2, e2, prior_n1, prior_e1, prior_n2, prior_e2, output_dir = getwd()) {
  # Set directory and check if it exists, create if not
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Prior parameters
  alpha1 <- prior_e1 + 1
  beta1 <- prior_n1 - prior_e1 + 1
  alpha2 <- prior_e2 + 1
  beta2 <- prior_n2 - prior_e2 + 1
  
  # Update priors with new data
  post_alpha1 <- alpha1 + e1
  post_beta1 <- beta1 + n1 - e1
  post_alpha2 <- alpha2 + e2
  post_beta2 <- beta2 + n2 - e2
  
  # Generate posterior samples
  post_p1 <- rbeta(10000, post_alpha1, post_beta1)
  post_p2 <- rbeta(10000, post_alpha2, post_beta2)
  
  # Compute differences and ratios
  diff_posterior <- post_p1 - post_p2
  rr_posterior <- post_p1 / post_p2
  
  # Calculate credible intervals and summaries
  rd_ci <- quantile(diff_posterior, probs = c(0.025, 0.975))
  rr_ci <- quantile(rr_posterior, probs = c(0.025, 0.975))
  
  # Print and save formatted results
  rd_result <- sprintf("Risk Difference: %.2f [95%% CrI: %.2f, %.2f]", mean(diff_posterior), rd_ci[1], rd_ci[2])
  rr_result <- sprintf("Relative Risk: %.2f [95%% CrI: %.2f, %.2f]", mean(rr_posterior), rr_ci[1], rr_ci[2])
  cat(rd_result, "\n")
  cat(rr_result, "\n")
  
  writeLines(c(rd_result, rr_result), file.path(output_dir, "Bayesian_Analysis_Results.txt"))
  
  # Create and save plots
  diff_plot <- ggplot() +
    geom_density(data = data.frame(diff = post_p1 - post_p2), aes(x = diff, fill = "Posterior"), alpha = 0.5) +
    geom_density(data = data.frame(diff = rbeta(10000, e1 + 1, n1 - e1 + 1) - rbeta(10000, e2 + 1, n2 - e2 + 1)), 
                 aes(x = diff, fill = "Likelihood"), alpha = 0.5) +
    geom_density(data = data.frame(diff = rbeta(10000, alpha1, beta1) - rbeta(10000, alpha2, beta2)), 
                 aes(x = diff, fill = "Prior"), alpha = 0.5) +
    scale_fill_manual(values = c("Posterior" = "red", "Likelihood" = "green", "Prior" = "blue")) +
    labs(title = "Risk Difference Analysis", x = "Risk Difference", y = "Density")
  
  rr_plot <- ggplot() +
    geom_density(data = data.frame(rr = post_p1 / post_p2), aes(x = rr, fill = "Posterior"), alpha = 0.5) +
    geom_density(data = data.frame(rr = rbeta(10000, e1 + 1, n1 - e1 + 1) / rbeta(10000, e2 + 1, n2 - e2 + 1)), 
                 aes(x = rr, fill = "Likelihood"), alpha = 0.5) +
    geom_density(data = data.frame(rr = rbeta(10000, alpha1, beta1) / rbeta(10000, alpha2, beta2)), 
                 aes(x = rr, fill = "Prior"), alpha = 0.5) +
    scale_fill_manual(values = c("Posterior" = "red", "Likelihood" = "green", "Prior" = "blue")) +
    labs(title = "Relative Risk Analysis", x = "Relative Risk", y = "Density")
  
  ggsave(filename = "RD_Plot.png", plot = diff_plot, path = output_dir)
  ggsave(filename = "RR_Plot.png", plot = rr_plot, path = output_dir)
  
  # Return plots for interactive use
  list(
    RD_Plot = diff_plot,
    RR_Plot = rr_plot
  )
}

# Example usage:
results <- bayesian_analysis(n1 = 100, e1 = 20, n2 = 100, e2 = 10, 
                             prior_n1 = 50, prior_e1 = 15, prior_n2 = 50, prior_e2 = 5, 
                             output_dir = "your/output/path")
print(results$RD_Plot)
print(results$RR_Plot)

##############

# Assuming the bayesian_analysis function is as previously confirmed
# We need to ensure it returns the necessary RD posterior data for further processing
library(ggplot2)

# Function to plot the posterior RD with specified AUC coloring as a single continuous PDF
plot_posterior_rd_colored <- function(post_rd) {
  # Ensure the RD data is numeric
  post_rd <- as.numeric(post_rd)
  
  # Calculate the density
  dens <- density(post_rd)
  # Convert density object to a data frame
  density_data <- data.frame(x = dens$x, y = dens$y)
  
  # Add a color group based on the x value (risk difference)
  density_data$color_group <- cut(density_data$x, breaks = c(-Inf, 0.1, 0.2, Inf),
                                  labels = c("less than 0.1", "0.1 to 0.2", "greater than 0.2"))
  
  # Create the RD plot with conditional coloring as a single continuous PDF
  rd_plot <- ggplot(density_data, aes(x = x, y = y, fill = color_group)) +
    geom_area(alpha = 0.5) +
    scale_fill_manual(values = c("less than 0.1" = "red", 
                                 "0.1 to 0.2" = "blue", 
                                 "greater than 0.2" = "green")) +
    labs(title = "Posterior Distribution of Risk Difference",
         x = "Risk Difference", y = "Density") +
    theme_minimal()
  
  # Display the plot
  print(rd_plot)
}

# Example usage:
# Assuming 'posterior_rd' contains numeric RD data from your analysis
posterior_rd <- rbeta(10000, 20, 80) - rbeta(10000, 10, 90)  # Hypothetical data generation for illustration
plot_posterior_rd_colored(posterior_rd)

#######
library(ggplot2)

# Function to plot the posterior RR with specified AUC coloring as a single continuous PDF
plot_posterior_rr_colored <- function(post_rr) {
  # Ensure the RR data is numeric
  post_rr <- as.numeric(post_rr)
  
  # Calculate the density
  dens <- density(post_rr)
  # Convert density object to a data frame
  density_data <- data.frame(x = dens$x, y = dens$y)
  
  # Add a color group based on the x value (relative risk)
  density_data$color_group <- cut(density_data$x, breaks = c(-Inf, 0.9, 1.1, Inf),
                                  labels = c("less than 0.9", "0.9 to 1.1", "greater than 1.1"))
  
  # Create the RR plot with conditional coloring as a single continuous PDF
  rr_plot <- ggplot(density_data, aes(x = x, y = y, fill = color_group)) +
    geom_area(alpha = 0.5) +
    scale_fill_manual(values = c("less than 0.9" = "red", 
                                 "0.9 to 1.1" = "blue", 
                                 "greater than 1.1" = "green")) +
    labs(title = "Posterior Distribution of Relative Risk",
         x = "Relative Risk", y = "Density") +
    theme_minimal()
  
  # Display the plot
  print(rr_plot)
}

# Example usage:
# Assuming 'posterior_rr' contains numeric RR data from your analysis
posterior_rr <- (rbeta(10000, 20, 80) + 1) / (rbeta(10000, 10, 90) + 1)  # Hypothetical data generation for illustration
plot_posterior_rr_colored(posterior_rr)
