library(ggplot2)
library(readr)
library(dplyr)

# Set working directory if needed
setwd("/Users/veronicagosnell/Desktop/R/MD_plots/AB9")

# Define single file and label
rg_file <- "trRosetta/gyrate1.csv"
label <- "Replicate #1"

# Read and process the data
df <- read_csv(rg_file, col_names = FALSE, show_col_types = FALSE)

if (ncol(df) >= 5) {
  colnames(df)[1:5] <- c("Time_ps", "RgX", "RgY", "RgZ", "RgTotal")
  df <- df[, c("Time_ps", "RgTotal")]
  colnames(df) <- c("Time_ps", "Rg_nm")
  df$Run <- label
  rg_data <- df
} else {
  stop(paste("Skipping", rg_file, "- fewer than 5 columns"))
}

# Convert time
rg_data <- rg_data %>%
  mutate(Time_ns = Time_ps / 1000)

# Plot without average line
ggplot(rg_data, aes(x = Time_ns, y = Rg_nm, color = Run)) +
  geom_line(alpha = 0.4) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 1.2) +
  labs(
    title = "Radius of Gyration",
    x = "Time (ns)",
    y = "Rg (nm)",
    color = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 3)) +
  scale_color_manual(values = "darkblue") +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top",
    axis.line = element_line(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )
