#' DESeq2
#'
#' @description Performs differential expression analysis
#' @param matrix_file name of the count matrix file obtained after the genome indexing
#' @param metadata_file name of the metadata file obtained after the genome indexing
#' @param species name of the organism subject of the analysis (allowed values: Homosapiens, Musmusculus, Drosophilamelanogaster)
#' @param reference_group name of the reference group inside the metadata
#' @param input_folder path of the directory containing the fastq files and the csv files obtained from the indexing
#' @return An output directory inside input_dir_path containing CSV files with differential expression results,
#' named as DEG_<group>_vs_<reference_group>.csv, a filtered gene count matrix, and a venn diagram of significant genes.
#'
#' @export
DESeq2 <- function(matrix_file,
metadata_file,
species,
reference_group,
input_folder) {
  # Type validation
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  if (!is.character(metadata_file) || length(metadata_file) != 1) {
    stop("metadata_file must be a single character string")
  }
  valid_species <- c("Homosapiens", "Musmusculus", "Drosophilamelanogaster")
  if (!is.character(species) || length(species) != 1 || !(species %in% valid_species)) {
    stop(paste0("species must be one of: ", paste(valid_species, collapse=", ")))
  }
  if (!is.character(reference_group) || length(reference_group) != 1) {
    stop("reference_group must be a single character string")
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
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", reference_group)) {
    stop("Path traversal detected in reference_group")
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
        c(input_folder, "/scratch")
      ),
      additional_arguments = c(
        "Rscript /home/Deseq2.R",
        matrix_file,
        metadata_file,
        reference_group,
        species
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "DESeq2_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}


