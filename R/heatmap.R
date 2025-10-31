#' heatmap
#'
#' @description Generates heatmaps based on the filtered gene count matrix
#' @param matrix_file name of the count matrix file obtained after the genome indexing
#' @param metadata_file name of the metadata file obtained after the genome indexing
#' @param input_folder path of the directory containing the fastq files and the csv files obtained from the indexing
#' @return Results of the operation
#'
#' @export
heatmap <- function(matrix_file,
metadata_file,
input_folder) {
  # Type validation
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  if (!is.character(metadata_file) || length(metadata_file) != 1) {
    stop("metadata_file must be a single character string")
  }
  if (!is.character(input_folder) || length(input_folder) != 1) {
    stop("input_folder must be a single character string")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", matrix_file)) {
    stop("Path traversal detected in matrix_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", metadata_file)) {
    stop("Path traversal detected in metadata_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", input_folder)) {
    stop("Path traversal detected in input_folder")
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
      image_name = "repbioinfo/rnaseqstar_v2:latest",
      volumes = list(
        c(input_folder_dir, "/scratch"),
      ),
      additional_arguments = c(
        "Rscript /home/heatmap.R",
        matrix_file,
        metadata_file,
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "heatmap_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}

