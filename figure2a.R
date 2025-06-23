library(ggplot2)
library(dplyr)
library(tibble)

# Define base data WITH blank row inserted between "Overall" and "Predicted"
df <- tibble(
  study = c("CISR", "CLEAR", "COLCHICINE-PCI", "COLCOT", "COPS", "Deftereos_2013",
            "Lodoc_MI", "Okeefe", "Overall", " ", "Predicted"),
  rr_obs = c(0.25, 0.51, 0.99, 0.97, 0.90, 0.84, 0.25, 0.85, NA, NA, NA),
  ci_low_obs = c(0.10, 0.21, 0.85, 0.83, 0.55, 0.55, 0.02, 0.60, NA, NA, NA),
  ci_upp_obs = c(0.64, 0.87, 1.16, 1.13, 1.49, 1.28, 3.63, 1.20, NA, NA, NA),
  rr_post = c(0.23, 0.50, 0.97, 0.96, 0.88, 0.82, 0.69, 0.82, 0.69, NA, 0.78),
  ci_low_post = c(0.09, 0.20, 0.84, 0.82, 0.53, 0.52, 0.25, 0.59, 0.47, NA, 0.26),
  ci_upp_post = c(0.62, 0.86, 1.14, 1.12, 1.45, 1.26, 1.38, 1.11, 0.94, NA, 1.70)
)

# Build plotting dataframe
plot_df <- bind_rows(lapply(1:nrow(df), function(i) {
  study <- df$study[i]
  
  if (study %in% c("Overall", "Predicted", " ")) {
    return(tibble(
      y_label = study,
      rr = df$rr_post[i],
      ci_low = df$ci_low_post[i],
      ci_upp = df$ci_upp_post[i],
      value_label = if (!is.na(df$rr_post[i])) sprintf("%.2f [%.2f–%.2f]", df$rr_post[i], df$ci_low_post[i], df$ci_upp_post[i]) else "",
      type = if (!is.na(df$rr_post[i])) "Posterior" else "Spacer"
    ))
  } else {
    return(tibble(
      y_label = c(study, "", "", ""),
      rr = c(NA, df$rr_obs[i], df$rr_post[i], NA),
      ci_low = c(NA, df$ci_low_obs[i], df$ci_low_post[i], NA),
      ci_upp = c(NA, df$ci_upp_obs[i], df$ci_upp_post[i], NA),
      value_label = c(
        "",
        sprintf("%.2f [%.2f–%.2f]", df$rr_obs[i], df$ci_low_obs[i], df$ci_upp_obs[i]),
        sprintf("%.2f [%.2f–%.2f]", df$rr_post[i], df$ci_low_post[i], df$ci_upp_post[i]),
        ""
      ),
      type = c("Label", "Observed", "Posterior", "Spacer")
    ))
  }
}), .id = "group")

# Add vertical positions
plot_df$ypos <- rev(seq_len(nrow(plot_df)))

# Plot
ggplot(plot_df, aes(x = rr, y = ypos)) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_upp, color = type),
                 height = 0.2, na.rm = TRUE) +
  geom_point(aes(color = type), size = 2, na.rm = TRUE) +
  geom_text(aes(x = 1.8, label = value_label), hjust = 0, size = 3.5, na.rm = TRUE) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_manual(
    values = c("Observed" = "blue", "Posterior" = "darkgreen"),
    breaks = c("Observed", "Posterior"),
    labels = c("Observed", "Posterior"),
    name = NULL,
    na.translate = FALSE
  ) +
  scale_y_continuous(
    breaks = plot_df$ypos,
    labels = plot_df$y_label,
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  coord_cartesian(xlim = c(0, 2.5)) +
  labs(
    title = "Forest Plot: Bayesian Meta-Analysis (RR + 95% CIs)\nPost CLEAR",
    x = "Risk Ratio (RR)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 0),
    plot.margin = margin(10, 80, 10, 10)
  )
