library(ggplot2)
library(readr)
library(dplyr)

# Set working directory if needed
setwd("/Users/veronicagosnell/Desktop/R/MD_plots/AB9")

# Define file paths
rmsd_files <- list(
  "Replicate #1" = "run1/rmsd1.csv",
  "Replicate #2" = "run2/rmsd2.csv",
  "Replicate #3" = "run3/rmsd3.csv"
)

# Function to safely read CSV with or without header
read_rmsd_file <- function(file, label) {
  df <- read_csv(
    file,
    col_names = FALSE,
    col_types = cols(
      X1 = col_double(),
      X2 = col_double()
    ),
    show_col_types = FALSE
  )
  
  if (ncol(df) >= 2) {
    colnames(df)[1:2] <- c("Time_ps", "RMSD_nm")
    df$Run <- label
    return(df)
  } else {
    warning(paste("Skipping", file, "- insufficient columns"))
    return(NULL)
  }
}



# Read and combine
rmsd_data <- bind_rows(
  lapply(names(rmsd_files), function(label) {
    read_rmsd_file(rmsd_files[[label]], label)
  })
)


# Convert units
rmsd_data <- rmsd_data %>%
  mutate(Time_ns = Time_ps / 1000,
         RMSD_A = RMSD_nm * 10)

# Plot
ggplot(rmsd_data, aes(x = Time_ns, y = RMSD_A, color = Run)) +
  geom_line(size = 1) +
  labs(
    title = "Backbone RMSD",
    x = "Time (ns)",
    y = "Backbone RMSD (Å)",
    color = NULL
  ) +
  scale_y_continuous(limits = c(0, 20)) +  # 👈 sets Y-axis from 0 to 20 Å
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2")
