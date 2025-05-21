library(readr)

# General-purpose function to clean and convert any .xvg to .csv
convert_xvg_to_csv <- function(xvg_path, csv_path) {
  lines <- readLines(xvg_path)
  clean_lines <- lines[!grepl("^[@#]", lines)]
  
  data <- tryCatch({
    read_table(
      file = I(clean_lines),
      col_names = FALSE,
      col_types = cols(.default = col_double())
    )
  }, error = function(e) {
    warning(paste("Could not parse:", xvg_path))
    return(NULL)
  })
  
  if (!is.null(data)) {
    write_csv(data, csv_path, col_names = FALSE)  # <- no headers!
    message(paste("✅ Converted:", xvg_path, "→", csv_path))
  }
}




# optional: 
# Convert all .xvg files in a folder
# This helper will batch-convert all .xvg files in a directory:

convert_all_xvg_in_folder <- function(input_dir, output_dir = input_dir) {
  xvg_files <- list.files(input_dir, pattern = "\\.xvg$", full.names = TRUE)
  
  for (xvg_file in xvg_files) {
    # Construct output CSV path
    base <- tools::file_path_sans_ext(basename(xvg_file))
    csv_path <- file.path(output_dir, paste0(base, ".csv"))
    
    # Convert
    convert_xvg_to_csv(xvg_file, csv_path)
  }
}



# example usage:

# Convert one file
convert_xvg_to_csv("trRosetta/rmsf1.xvg", "trRosetta/rmsf1.csv")

# Convert all .xvg files in a folder
convert_all_xvg_in_folder("AB9/trRosetta/")
