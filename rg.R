library(ggplot2)
library(readr)
library(dplyr)

# Set working directory if needed
setwd("/Users/veronicagosnell/Desktop/R/MD_plots/AB9")

rg_files <- list(
  "Replicate #1" = "run1/gyrate1.csv",
  "Replicate #2" = "run2/gyrate2.csv",
  "Replicate #3" = "run3/gyrate3.csv"
)

rg_data <- bind_rows(
  lapply(names(rg_files), function(label) {
    df <- read_csv(rg_files[[label]], col_names = FALSE, show_col_types = FALSE)
    if (ncol(df) >= 5) {
      colnames(df)[1:5] <- c("Time_ps", "RgX", "RgY", "RgZ", "RgTotal")
      df <- df[, c("Time_ps", "RgTotal")]
      colnames(df) <- c("Time_ps", "Rg_nm")
      df$Run <- label
      return(df)
    } else {
      warning(paste("Skipping", rg_files[[label]], "- fewer than 5 columns"))
      return(NULL)
    }
  })
)

rg_data <- rg_data %>%
  mutate(Time_ns = Time_ps / 1000,
         Rg_nm = Rg_nm)  # This just keeps Rg_nm unchanged for clarity

avg_rg <- rg_data %>%
  group_by(Time_ns) %>%
  summarize(Rg_Avg = mean(Rg_nm, na.rm = TRUE))

ggplot(rg_data, aes(x = Time_ns, y = Rg_nm, color = Run)) +
  geom_line(alpha = 0.4) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 1.2) +
  geom_line(data = avg_rg, aes(x = Time_ns, y = Rg_Avg), 
            color = "black", linewidth = 1.3, linetype = "dashed") +
  labs(
    title = "Radius of Gyration",
    x = "Time (ns)",
    y = "Rg (nm)",
    color = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 3)) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top",
    axis.line = element_line(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )
