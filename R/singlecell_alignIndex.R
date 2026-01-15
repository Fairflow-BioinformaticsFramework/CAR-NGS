#' Index and Alignment Script for single cell analysis
#'
#' @description This script handles the alignment and indexing of the FASTQ
#' files generated from single-cell RNA-seq experiments using Cell Ranger.
#' @param input_folder path of the directory containing the fastq files
#' @param genome_folder path of the directory containing the fasta and gtf files of the genome
#' @param bamsave variable indicating if the BAM files are to be saved
#' @return Results of the operation
#'
#' @export
singlecell_alignIndex <- function(input_folder,
genome_folder,
bamsave) {
  # Type validation
  if (!is.character(input_folder) || length(input_folder) != 1) {
    stop("input_folder must be a single character string")
  }
  if (!is.character(genome_folder) || length(genome_folder) != 1) {
    stop("genome_folder must be a single character string")
  }
  if (!is.logical(bamsave) || length(bamsave) != 1) {
    stop("bamsave must be a single logical value (TRUE/FALSE)")
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
      image_name = "repbioinfo/carncellranger2:latest",
      volumes = list(
        c(input_folder, "/scratch"),
        c(genome_folder, "/genome")
      ),
      additional_arguments = c(
        "/home/index_align.sh",
        if(bamsave) "--true-flag" else character(0)
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "singlecell_alignIndex_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}

