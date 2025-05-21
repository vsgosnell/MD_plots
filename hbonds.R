library(ggplot2)
library(readr)
library(dplyr)

hb_files <- list(
  "Replicate #1" = "run1/hbnum1.csv",
  "Replicate #2" = "run2/hbnum2.csv",
  "Replicate #3" = "run3/hbnum3.csv"
)

hb_data <- bind_rows(
  lapply(names(hb_files), function(label) {
    file <- hb_files[[label]]
    
    # Explicitly use read_table for whitespace-delimited files (run1 & run2)
    if (label %in% c("Replicate #1", "Replicate #2")) {
      df <- read_table(file, col_names = FALSE)
    } else {
      # Use read_csv for comma-delimited file (run3)
      df <- read_csv(file, col_names = FALSE, show_col_types = FALSE)
    }
    
    if (ncol(df) == 2) {
      colnames(df) <- c("Time_ps", "HBonds")
    } else if (ncol(df) >= 3) {
      colnames(df) <- c("Time_ps", "HB_Prot_Prot", "HB_Prot_Solv")
      df$HBonds <- df$HB_Prot_Prot
      df <- df[, c("Time_ps", "HBonds")]
    } else {
      warning(paste("Skipping", label, "- unexpected column count:", ncol(df)))
      return(NULL)
    }
    
    df$Run <- label
    df
  })
)



hb_data <- hb_data %>%
  mutate(Time_ns = Time_ps / 1000)

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
  scale_color_brewer(palette = "Dark2")

