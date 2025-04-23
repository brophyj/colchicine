ggplot() +
  stat_density(
    data     = draws_logrr,
    aes(x     = logRR,
        fill  = prior,
        color = prior,
        group = prior),
    geom     = "area",
    position = "identity",
    alpha    = 0.3,
    adjust   = 1
  ) +
  geom_line(
    data      = priors_df2,
    aes(x     = logRR,
        y     = density,
        color = prior),
    size = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  # Force line outlines to match the default fill colours
  scale_color_manual(values = c(
    "Meta-analysis" = "#F8766D",
    "COLCOT"        = "#00BA38",
    "Weak"          = "#619CFF"
  )) +
  
  labs(
    title   = "Prior and Posterior Distributions on log(RR) Scale",
    x       = "log(Risk Ratio)",
    y       = "Density",
    caption = "Lines = prior distributions; shaded areas = posterior distributions"
  ) +
  coord_cartesian(xlim = c(-1, 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box      = "vertical"
  )
