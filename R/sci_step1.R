#' SCI_1
#'
#' @description Used in combination with SCI_2, from fastq files produces gene expression matrix and QC plots and statistics for SCI data
#' @param input_folder path of the directory containing the input file
#' @param sample_name a character string indicating the name of the experiment
#' @param UMI_cutoff minimum number of UMI per cell to consider the cell valid
#' @return Results of the operation
#'
#' @export
SCI_1 <- function(input_folder,
sample_name,
UMI_cutoff) {
  # Type validation
  if (!is.character(input_folder) || length(input_folder) != 1) {
    stop("input_folder must be a single character string")
  }
  if (!is.character(sample_name) || length(sample_name) != 1) {
    stop("sample_name must be a single character string")
  }
  if (!is.numeric(UMI_cutoff) || length(UMI_cutoff) != 1 || UMI_cutoff != round(UMI_cutoff)) {
    stop("UMI_cutoff must be a single integer value")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", input_folder)) {
    stop("Path traversal detected in input_folder")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", sample_name)) {
    stop("Path traversal detected in sample_name")
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
      image_name = "repbioinfo/sci_tomatrix_genome:latest",
      volumes = list(
        c(input_folder, "/data/scratch")
      ),
      additional_arguments = c(
        "/home/tomatrix.sh",
        sample_name,
        as.character(UMI_cutoff)
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "SCI_1_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}
