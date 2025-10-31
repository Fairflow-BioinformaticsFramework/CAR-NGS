#' Index and Alignment script
#'
#' @description Performs the alignment of RNA-Seq reads to the reference genome using STAR
#' @param input_folder path of a directory containing the fastq files to be analyzed
#' @param genome_folder path of a directory containing the fasta files of the genome
#' @return Results of the operation
#'
#' @export
index_align <- function(input_folder,
genome_folder) {
  # Type validation
  if (!is.character(input_folder) || length(input_folder) != 1) {
    stop("input_folder must be a single character string")
  }
  if (!is.character(genome_folder) || length(genome_folder) != 1) {
    stop("genome_folder must be a single character string")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", input_folder)) {
    stop("Path traversal detected in input_folder")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", genome_folder)) {
    stop("Path traversal detected in genome_folder")
  }
  
  # Check if directory exists
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(input_folder)) {
      stop(paste("input_folder:", input_folder, "does not exist"))
    }
  }
  
  # Check if directory exists
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(genome_folder)) {
      stop(paste("genome_folder:", genome_folder, "does not exist"))
    }
  }
  
  # Process file paths for Docker volume mounting
  # Process input_folder for Docker
  input_folder_abspath <- normalizePath(input_folder, mustWork = FALSE)
  input_folder_dir <- dirname(input_folder_abspath)
  input_folder_filename <- basename(input_folder)
  # Process genome_folder for Docker
  genome_folder_abspath <- normalizePath(genome_folder, mustWork = FALSE)
  genome_folder_dir <- dirname(genome_folder_abspath)
  genome_folder_filename <- basename(genome_folder)
  
  # Main volume mount point
  main_mount_dir <- input_folder_dir
  
  # Execute Docker container with error handling
  tryCatch({
    result <- rrundocker::run_in_docker(
      image_name = "repbioinfo/rnaseqstar_v2:lastest",
      volumes = list(
        c(input_folder_dir, "/scratch"),
        c(genome_folder_dir, "/genome"),
      ),
      additional_arguments = c(
        "/home/index_align.sh",
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "index_align_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}
