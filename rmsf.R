library(ggplot2)
library(readr)
library(dplyr)

# Set working directory if needed
setwd("/Users/veronicagosnell/Desktop/R/MD_plots/AB9")

rmsf_files <- list(
  "Replicate #1" = "run1/rmsf1.csv",
  "Replicate #2" = "run2/rmsf2.csv",
  "Replicate #3" = "run3/rmsf3.csv"
)

rmsf_data <- bind_rows(
  lapply(names(rmsf_files), function(label) {
    file <- rmsf_files[[label]]
    
    # Use read_table for whitespace-separated, read_csv for comma-separated
    if (label == "Replicate #3") {
      df <- read_csv(file, col_names = FALSE, show_col_types = FALSE)
    } else {
      df <- read_table(file, col_names = FALSE)
    }
    
    if (ncol(df) < 2) {
      warning(paste("Skipping", label, "- only", ncol(df), "column(s) found"))
      return(NULL)
    }
    
    df <- df[, 1:2]
    colnames(df) <- c("Residue", "RMSF_nm")
    df$Run <- label
    df
  })
)

# Convert nm to Å and remove bad rows
rmsf_data <- rmsf_data %>%
  mutate(RMSF_A = RMSF_nm * 10) %>%
  filter(!is.na(Residue), !is.na(RMSF_A))


ggplot(rmsf_data, aes(x = Residue, y = RMSF_A, color = Run)) +
  geom_line(size = 1) +
  labs(
    title = "Backbone RMSF",
    x = "Residue Index",
    y = "RMSF (Å)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2")
