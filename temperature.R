library(readr)
library(dplyr)
library(ggplot2)

temp_files <- list(
  "Replicate #1" = "run1/temperature1.csv",
  "Replicate #2" = "run2/temperature2.csv",
  "Replicate #3" = "run3/temperature3.csv"
)

temp_data <- bind_rows(
  lapply(names(temp_files), function(label) {
    file <- temp_files[[label]]
    
    # Try reading and skip if there's an error
    df <- tryCatch({
      if (label == "Replicate #3") {
        read_csv(file, col_names = FALSE, show_col_types = FALSE)
      } else {
        read_table(file, col_names = FALSE)
      }
    }, error = function(e) {
      warning(paste("Failed to read", label, ":", e$message))
      return(NULL)
    })
    
    if (is.null(df) || ncol(df) < 2) {
      warning(paste("Skipping", label, "- invalid or incomplete file"))
      return(NULL)
    }
    
    colnames(df) <- c("Time_ps", "Temperature_K")
    df$Run <- label
    df
  })
) %>%
  filter(!is.na(Time_ps), !is.na(Temperature_K)) %>%
  mutate(Time_ns = Time_ps / 1000)


# GAM smoother (recommended for large datasets)

ggplot(temp_data, aes(x = Time_ns, y = Temperature_K, color = Run)) +
  geom_line(alpha = 0.5) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = FALSE,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  labs(
    title = "Temperature Over Time",
    x = "Time (ns)",
    y = "Temperature (K)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2")


# LOESS smoothing per replicate (short runs, smooth line)
# this has been making R crash

# ggplot(temp_data, aes(x = Time_ns, y = Temperature_K, color = Run)) +
#   geom_line(alpha = 0.5) +
#   geom_smooth(se = FALSE, method = "loess", linewidth = 1.2, linetype = "dashed") +
#   labs(
#     title = "Temperature Over Time",
#     x = "Time (ns)",
#     y = "Temperature (K)",
#     color = NULL
#   ) +
#   theme_minimal(base_size = 14) +
#   scale_color_brewer(palette = "Dark2")
