#!/usr/bin/env Rscript

# Load necessary libraries
library(magick)
library(jsonlite)

# Function to count black pixels
count_black_pixels_r <- function(image_path, threshold = 30) {
  tryCatch({
    # Read the image
    img <- image_read(image_path)
    
    # Convert to grayscale
    img_grayscale <- image_convert(img, colorspace = "gray")
    
    # Get pixel data as a matrix
    # image_data returns raw bytes, we need to convert to integer values
    img_data <- image_data(img_grayscale, channels = "gray")
    
    # Convert to integer matrix (values are 0-255)
    img_matrix <- as.integer(img_data)
    
    # Count pixels below threshold (black)
    black_pixels <- sum(img_matrix < threshold)
    
    # Calculate percentage
    total_pixels <- length(img_matrix)
    black_percentage <- (black_pixels / total_pixels) * 100
    
    result <- list(
      status = "success",
      black_pixel_count = black_pixels,
      total_pixels = total_pixels,
      black_percentage = round(black_percentage, 2),
      threshold_used = threshold,
      original_file = image_path
    )
    return(result)
  }, error = function(e) {
    result <- list(
      status = "error",
      error = paste("Failed to process image:", e$message)
    )
    return(result)
  })
}

# Main entry point for the script
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 2) {
    result <- list(
      status = "error",
      message = "Usage: Rscript main.R <input_file> <params_file>"
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  }
  
  input_file <- args[1]
  params_file <- args[2]
  
  # Check if the input file exists
  if (!file.exists(input_file)) {
    result <- list(
      status = "error",
      message = paste("Error: Input file", input_file, "does not exist")
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  }
  
  # Check if the params file exists
  if (!file.exists(params_file)) {
    result <- list(
      status = "error",
      message = paste("Error: Parameters file", params_file, "does not exist")
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  }
  
  # Load the parameters
  tryCatch({
    params <- fromJSON(readLines(params_file))
  }, error = function(e) {
    result <- list(
      status = "error",
      message = paste("Error loading parameters:", e$message)
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  })
  
  # Get threshold parameter
  threshold <- ifelse(!is.null(params$threshold), as.integer(params$threshold), 30)
  
  # Process the image
  result <- count_black_pixels_r(input_file, threshold)
  
  # Add parameters applied to the result
  result$parameters_applied <- params
  
  # Output the result as JSON
  cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  return(result$status == "success")
}

# Run main function
if (sys.nframe() == 0) {
  success <- main()
  # Always exit with code 0, even if there are errors
  quit(status = 0)
}
