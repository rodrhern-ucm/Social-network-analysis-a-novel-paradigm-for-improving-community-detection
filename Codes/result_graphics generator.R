library(ggplot2)
library(dplyr)

# Add a fixed x-axis value for non-improvement cases
data_with_no_improvement_bar <- data %>%
  mutate(
    improvement_perc_adjusted = ifelse(improvement == 1, improvement_perc, -0.05)  # Assign -0.05 for non-improvement cases
  )

# Plot histogram with a bar for non-improvement cases
ggplot(data_with_no_improvement_bar, aes(x = improvement_perc_adjusted)) +
  geom_histogram(
    binwidth = 0.05,
    color = "black",
    fill = "blue",
    alpha = 0.7
  ) +
  scale_x_continuous(
    breaks = c(-0.05, seq(0, 1, by = 0.1)),  # Include break for non-improvement
    labels = c("No Improvement", seq(0, 1, by = 0.1))  # Label for non-improvement
  ) +
  labs(
    title = "Histogram with Non-Improvement Cases",
    x = "Improvement Percentage",
    y = "Number of Combinations"
  ) +
  facet_wrap(~algorithm, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Add a fixed x-axis value for non-improvement cases
data_with_no_improvement_bar <- data %>%
  mutate(
    improvement_perc_adjusted = ifelse(improvement == 1, improvement_perc, -0.05)  # Assign -0.05 for non-improvement cases
  )

# Plot histogram with non-improvement bar for each network
ggplot(data_with_no_improvement_bar, aes(x = improvement_perc_adjusted)) +
  geom_histogram(
    binwidth = 0.05,
    color = "black",
    fill = "green",
    alpha = 0.7
  ) +
  scale_x_continuous(
    breaks = c(-0.05, seq(0, 1, by = 0.1)),  # Include break for non-improvement
    labels = c("No Improvement", seq(0, 1, by = 0.1))  # Label for non-improvement
  ) +
  labs(
    title = "Histogram of Improvement Absolute Difference by Network",
    x = "Abs Difference",
    y = "Number of Combinations"
  ) +
  facet_wrap(~network, scales = "free_y", ncol = 6) +  # Adjust layout
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot combined histogram for algorithm and network with non-improvement bar
ggplot(data_with_no_improvement_bar, aes(x = improvement_perc_adjusted, fill = factor(algorithm))) +
  geom_histogram(
    binwidth = 0.05,
    color = "black",
    alpha = 0.7,
    position = "identity"  # Overlaid histograms
  ) +
  scale_x_continuous(
    breaks = c(-0.05, seq(0, 1, by = 0.1)),  # Include break for non-improvement
    labels = c("No Improvement", seq(0, 1, by = 0.1))  # Label for non-improvement
  ) +
  labs(
    title = "Histogram of Improvement Absolute Difference by Algorithm and Network",
    x = "Abs Difference",
    y = "Number of Combinations",
    fill = "Algorithm"
  ) +
  facet_wrap(~network, scales = "free_y", ncol = 6) +  # Adjust layout
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
