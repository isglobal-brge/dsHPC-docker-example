#!/usr/bin/env Rscript

library(jsonlite)

generate_image_report <- function(input_json_path, include_recommendations = TRUE, 
                                  quality_threshold = 50.0) {
  tryCatch({
    input_data <- fromJSON(readLines(input_json_path))
    
    if (input_data$status != "success") {
      stop(paste("Input job failed:", input_data$error))
    }
    
    # Extract analysis data
    analysis <- input_data$analysis
    black_percentage <- analysis$black_percentage
    darkness_category <- analysis$darkness_category
    threshold_used <- analysis$darkness_threshold_used
    
    # Generate quality assessment
    quality_score <- 100 - black_percentage  # Higher score = more light = better quality
    
    quality_category <- "Unknown"
    if (quality_score >= quality_threshold) {
      quality_category <- "High"
    } else if (quality_score >= quality_threshold * 0.6) {
      quality_category <- "Medium"
    } else {
      quality_category <- "Low"
    }
    
    # Generate recommendations
    recommendations <- list()
    if (include_recommendations) {
      if (quality_category == "Low") {
        recommendations <- c(
          "Consider increasing image brightness",
          "Check exposure settings",
          "Verify image acquisition parameters"
        )
      } else if (quality_category == "Medium") {
        recommendations <- c(
          "Image quality is acceptable",
          "Minor adjustments may improve clarity"
        )
      } else {
        recommendations <- c(
          "Image quality is excellent",
          "No adjustments needed"
        )
      }
    }
    
    # Generate comprehensive report
    result <- list(
      status = "success",
      report = list(
        summary = list(
          black_pixels_percentage = black_percentage,
          darkness_category = darkness_category,
          quality_score = round(quality_score, 2),
          quality_category = quality_category
        ),
        thresholds = list(
          darkness_threshold = threshold_used,
          quality_threshold = quality_threshold
        ),
        recommendations = recommendations,
        metadata = list(
          report_generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          processing_chain_length = 3,
          analysis_complete = TRUE
        ),
        source_analysis = analysis  # Include original analysis for traceability
      )
    )
    
    return(result)
    
  }, error = function(e) {
    result <- list(
      status = "error",
      error = paste("Failed to generate image report:", e$message)
    )
    return(result)
  })
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 2) {
    result <- list(
      status = "error",
      message = "Usage: Rscript main.R <input_json_file> <params_file>"
    )
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  }
  
  input_file <- args[1]
  params_file <- args[2]
  
  if (!file.exists(input_file)) {
    result <- list(status = "error", message = paste("Error: Input file", input_file, "does not exist"))
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  }
  
  if (!file.exists(params_file)) {
    result <- list(status = "error", message = paste("Error: Parameters file", params_file, "does not exist"))
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  }
  
  tryCatch({
    params <- fromJSON(readLines(params_file))
  }, error = function(e) {
    result <- list(status = "error", message = paste("Error loading parameters:", e$message))
    cat(toJSON(result, auto_unbox = TRUE))
    return(FALSE)
  })
  
  # Extract parameters with defaults
  include_recommendations <- ifelse(!is.null(params$include_recommendations), 
                                   as.logical(params$include_recommendations), TRUE)
  quality_threshold <- ifelse(!is.null(params$quality_threshold), 
                             as.numeric(params$quality_threshold), 50.0)
  
  result <- generate_image_report(input_file, include_recommendations, quality_threshold)
  
  cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE))
  return(result$status == "success")
}

if (sys.nframe() == 0) {
  success <- main()
  quit(status = 0)
}

