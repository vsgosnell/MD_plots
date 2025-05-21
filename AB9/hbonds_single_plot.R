library(ggplot2)
library(readr)
library(dplyr)

# Set working directory if needed
setwd("/Users/veronicagosnell/Desktop/R/MD_plots/AB9")

# Define file and label
hb_file <- "trRosetta/hbnum1.csv"
label <- "Replicate #1"

# Read as comma-separated values
df <- read_csv(hb_file, col_names = FALSE, show_col_types = FALSE)

# Check columns
if (ncol(df) == 2) {
  colnames(df) <- c("Time_ps", "HBonds")
} else if (ncol(df) >= 3) {
  colnames(df)[1:3] <- c("Time_ps", "HB_Prot_Prot", "HB_Prot_Solv")
  df$HBonds <- df$HB_Prot_Prot
  df <- df[, c("Time_ps", "HBonds")]
} else {
  stop(paste("Unexpected number of columns in", hb_file, ":", ncol(df)))
}

# Add label and convert time
df$Run <- label
hb_data <- df %>%
  mutate(Time_ns = Time_ps / 1000)

# Plot
ggplot(hb_data, aes(x = Time_ns, y = HBonds, color = Run)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", size = 1.2) +
  labs(
    title = "Intramolecular Hydrogen Bonds",
    x = "Time (ns)",
    y = "Number of H-bonds",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = "darkblue")
