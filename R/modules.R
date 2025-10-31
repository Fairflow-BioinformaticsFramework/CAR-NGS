#' Modules
#'
#' @description runs an RNA-seq data processing pipeline inside a Docker container. It analyzes a gene expression 
#'  count matrix and associated metadata to identify gene modules.
#' @param input_folder path of the directory containing the input files
#' @param matrix_file name (with format) of the count matrix file
#' @param organism a character string specifying the organism (allowed values: Homosapiens, Musmusculus, Drosophilamelanogaster)
#' @param metadata_file name (with format) of the metadata file
#' @param matrix_sep separator used in the count matrix file
#' @param meta_sep separator used in the metadata file
#' @return Results of the operation
#'
#' @export
modules <- function(input_folder,
matrix_file,
organism,
metadata_file,
matrix_sep,
meta_sep) {
  # Type validation
  if (!is.character(input_folder) || length(input_folder) != 1) {
    stop("input_folder must be a single character string")
  }
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  valid_organism <- c("Homosapiens", "Musmusculus", "Drosophilamelanogaster")
  if (!is.character(organism) || length(organism) != 1 || !(organism %in% valid_organism)) {
    stop(paste0("organism must be one of: ", paste(valid_organism, collapse=", ")))
  }
  if (!is.character(metadata_file) || length(metadata_file) != 1) {
    stop("metadata_file must be a single character string")
  }
  if (!is.character(matrix_sep) || length(matrix_sep) != 1) {
    stop("matrix_sep must be a single character string")
  }
  if (!is.character(meta_sep) || length(meta_sep) != 1) {
    stop("meta_sep must be a single character string")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", input_folder)) {
    stop("Path traversal detected in input_folder")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", matrix_file)) {
    stop("Path traversal detected in matrix_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", metadata_file)) {
    stop("Path traversal detected in metadata_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", matrix_sep)) {
    stop("Path traversal detected in matrix_sep")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", meta_sep)) {
    stop("Path traversal detected in meta_sep")
  }
  
  # Check if directory exists
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(input_folder)) {
      stop(paste("input_folder:", input_folder, "does not exist"))
    }
  }
  
  # Process file paths for Docker volume mounting
  # Process input_folder for Docker
  input_folder_abspath <- normalizePath(input_folder, mustWork = FALSE)
  input_folder_dir <- dirname(input_folder_abspath)
  input_folder_filename <- basename(input_folder)
  
  # Main volume mount point
  main_mount_dir <- input_folder_dir
  
  # Execute Docker container with error handling
  tryCatch({
    result <- rrundocker::run_in_docker(
      image_name = "repbioinfo/rnaseqbulkdownstreamunbias:latest",
      volumes = list(
        c(input_folder_dir, "/scratch"),
      ),
      additional_arguments = c(
        "Rscript /home/modules.R",
        organism,
        matrix_file,
        metadata_file,
        matrix_sep,
        meta_sep,
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "modules_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}
