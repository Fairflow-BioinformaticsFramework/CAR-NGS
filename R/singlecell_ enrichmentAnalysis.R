#' Enrichment Analysis Script
#'
#' @description Process the results of differential expression and performs pathway enrichment analysis
#' @param matrix_file name (with format) of a CSV file containing the results of differential expression analysis
#' @param species a character string indicating the species that is being analyzed (allowed values: hsapiens, mmusculus, dmelanogaster)
#' @param parent_folder path of the directory containing the csv file
#' @param source a character sting indicating the source of enrichment analysis
#' @param separator separator used in the count table
#' @param max_terms maximum number of enriched terms to display in the output plot
#' @return Results of the operation
#'
#' @export
singlecell_enrichmentAnalysis <- function(matrix_file,
species,
parent_folder,
source,
separator,
max_terms) {
  # Type validation
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  valid_species <- c("hsapiens", "mmusculus", "dmelanogaster")
  if (!is.character(species) || length(species) != 1 || !(species %in% valid_species)) {
    stop(paste0("species must be one of: ", paste(valid_species, collapse=", ")))
  }
  if (!is.character(parent_folder) || length(parent_folder) != 1) {
    stop("parent_folder must be a single character string")
  }
  if (!is.character(source) || length(source) != 1) {
    stop("source must be a single character string")
  }
  if (!is.character(separator) || length(separator) != 1) {
    stop("separator must be a single character string")
  }
  if (!is.numeric(max_terms) || length(max_terms) != 1 || max_terms != round(max_terms)) {
    stop("max_terms must be a single integer value")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", matrix_file)) {
    stop("Path traversal detected in matrix_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", parent_folder)) {
    stop("Path traversal detected in parent_folder")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", source)) {
    stop("Path traversal detected in source")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", separator)) {
    stop("Path traversal detected in separator")
  }
  
  # Check if directory exists
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(parent_folder)) {
      stop(paste("parent_folder:", parent_folder, "does not exist"))
    }
  }
  
  # Process file paths for Docker volume mounting
  # Process parent_folder for Docker
  parent_folder_abspath <- normalizePath(parent_folder, mustWork = FALSE)
  parent_folder_dir <- dirname(parent_folder_abspath)
  parent_folder_filename <- basename(parent_folder)
  
  # Main volume mount point
  main_mount_dir <- parent_folder_dir
  
  # Execute Docker container with error handling
  tryCatch({
    result <- rrundocker::run_in_docker(
      image_name = "repbioinfo/singlecelldownstream:latest",
      volumes = list(
        c(parent_folder_dir, "/scratch"),
      ),
      additional_arguments = c(
        "Rscript /home/enrichment_analysis.R",
        matrix_file,
        species,
        source,
        separator,
        as.character(max_terms),
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "singlecell_enrichmentAnalysis_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}
