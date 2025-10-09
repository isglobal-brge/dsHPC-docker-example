#!/usr/bin/env Rscript

# Load necessary libraries
library(jsonlite)

# Function to analyze pixel count results
analyze_pixel_count <- function(pixel_count_json, darkness_threshold = 10.0) {
  tryCatch({
    # Parse the input if it's a string
    if (is.character(pixel_count_json)) {
      pixel_data <- fromJSON(pixel_count_json)
    } else {
      pixel_data <- pixel_count_json
    }
    
    # Extract relevant metrics
    black_percentage <- pixel_data$black_percentage
    total_pixels <- pixel_data$total_pixels
    black_pixels <- pixel_data$black_pixel_count
    threshold_used <- pixel_data$threshold_used
    
    # Categorize the image based on darkness
    darkness_category <- if (black_percentage < 5) {
      "very_light"
    } else if (black_percentage < 15) {
      "light"
    } else if (black_percentage < 30) {
      "medium"
    } else if (black_percentage < 50) {
      "dark"
    } else {
      "very_dark"
    }
    
    # Calculate additional statistics
    white_pixels <- total_pixels - black_pixels
    white_percentage <- 100 - black_percentage
    darkness_ratio <- black_pixels / max(1, white_pixels)
    
    # Determine if image meets darkness threshold
    is_dark_enough <- black_percentage >= darkness_threshold
    
    # Generate analysis summary
    summary_text <- sprintf(
      "Image contains %.2f%% black pixels (%d of %d). Category: %s. %s",
      black_percentage,
      black_pixels,
      total_pixels,
      darkness_category,
      if(is_dark_enough) "Meets darkness threshold." else "Below darkness threshold."
    )
    
    result <- list(
      status = "success",
      analysis = list(
        darkness_category = darkness_category,
        is_dark_enough = is_dark_enough,
        darkness_threshold = darkness_threshold,
        darkness_ratio = round(darkness_ratio, 4),
        white_percentage = round(white_percentage, 2),
        white_pixels = white_pixels,
        summary = summary_text
      ),
      input_data = list(
        black_percentage = black_percentage,
        black_pixels = black_pixels,
        total_pixels = total_pixels,
        threshold_used = threshold_used
      ),
      metadata = list(
        analysis_timestamp = Sys.time(),
        method = "analyze_pixel_count"
      )
    )
    
    return(result)
  }, error = function(e) {
    result <- list(
      status = "error",
      error = paste("Failed to analyze pixel count:", e$message)
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
  
  # Load the input data (should be JSON from count_black_pixels_r)
  tryCatch({
    input_content <- readLines(input_file, warn = FALSE)
    input_data <- fromJSON(paste(input_content, collapse = "\n"))
  }, error = function(e) {
    result <- list(
      status = "error",
      message = paste("Error loading input data:", e$message)
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  })
  
  # Load the parameters
  tryCatch({
    params <- fromJSON(readLines(params_file, warn = FALSE))
  }, error = function(e) {
    result <- list(
      status = "error",
      message = paste("Error loading parameters:", e$message)
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  })
  
  # Get darkness_threshold parameter
  darkness_threshold <- ifelse(!is.null(params$darkness_threshold), 
                               as.numeric(params$darkness_threshold), 10.0)
  
  # Analyze the pixel count data
  result <- analyze_pixel_count(input_data, darkness_threshold)
  
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
