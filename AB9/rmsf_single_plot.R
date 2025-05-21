library(ggplot2)
library(readr)
library(dplyr)

# Set working directory if needed
setwd("/Users/veronicagosnell/Desktop/R/MD_plots/AB9")

# Define single file and label
rmsf_file <- "trRosetta/rmsf1.csv"
label <- "Replicate #1"

# Read the file (adjust to format: use read_csv if comma-separated, read_table if space/tab-separated)
df <- read_csv(rmsf_file, col_names = FALSE, show_col_types = FALSE)

# Check if there are at least 2 columns
if (ncol(df) < 2) {
  warning(paste("Skipping", label, "- only", ncol(df), "column(s) found"))
  rmsf_data <- NULL
} else {
  df <- df[, 1:2]
  colnames(df) <- c("Residue", "RMSF_nm")
  df$Run <- label
  rmsf_data <- df
}

# Convert to Ångstroms and clean data
rmsf_data <- rmsf_data %>%
  mutate(RMSF_A = RMSF_nm * 10) %>%
  filter(!is.na(Residue), !is.na(RMSF_A))

# Plot
ggplot(rmsf_data, aes(x = Residue, y = RMSF_A, color = Run)) +
  geom_line(size = 1) +
  labs(
    title = "Backbone RMSF",
    x = "Residue Index",
    y = "RMSF (Å)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = "darkblue")
