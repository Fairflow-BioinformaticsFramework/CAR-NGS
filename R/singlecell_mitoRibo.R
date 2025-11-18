#' Mitochondrial and Riboomal Filtering Script
#'
#' @description Filtering cells based on their mitochondrial and ribosomal gene content, providing quality control in single-cell RNA-seq experiments
#' @param matrix_file name of the count matrix file, which can be both dense (.csv/.txt) or sparse (.mtx)
#' @param parent_folder path of the directory containing the matrix file
#' @param mitoMin start range for mitochondrial percentage, cells within the range are retained
#' @param mitoMax end range for mitochondrial percentage, cells within the range are retained
#' @param riboMin start range for ribosomal percentage, cells within the range are retained
#' @param riboMax end range for ribosomal percentage, cells within the range are retained
#' @param separator separator used in the count table, is "null" for sparse matrix analysis
#' @param genes_file name of the genes name files necessary for the analysis of a sparse matrix (*genes.tsv), "null" for dense matrix analysis
#' @param barcodes_file name of the barcodes file necessary for the analysis of a sparse matrix (*barcodes.tsv), "null" for dense matrix analysis
#' @return Results of the operation
#'
#' @export
singlecell_mitoRibo <- function(matrix_file,
parent_folder,
mitoMin,
mitoMax,
riboMin,
riboMax,
separator,
genes_file,
barcodes_file) {
  # Type validation
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  if (!is.character(parent_folder) || length(parent_folder) != 1) {
    stop("parent_folder must be a single character string")
  }
  if (!is.numeric(mitoMin) || length(mitoMin) != 1 || mitoMin != round(mitoMin)) {
    stop("mitoMin must be a single integer value")
  }
  if (!is.numeric(mitoMax) || length(mitoMax) != 1 || mitoMax != round(mitoMax)) {
    stop("mitoMax must be a single integer value")
  }
  if (!is.numeric(riboMin) || length(riboMin) != 1 || riboMin != round(riboMin)) {
    stop("riboMin must be a single integer value")
  }
  if (!is.numeric(riboMax) || length(riboMax) != 1 || riboMax != round(riboMax)) {
    stop("riboMax must be a single integer value")
  }
  if (!is.character(separator) || length(separator) != 1) {
    stop("separator must be a single character string")
  }
  if (!is.character(genes_file) || length(genes_file) != 1) {
    stop("genes_file must be a single character string")
  }
  if (!is.character(barcodes_file) || length(barcodes_file) != 1) {
    stop("barcodes_file must be a single character string")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", matrix_file)) {
    stop("Path traversal detected in matrix_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", parent_folder)) {
    stop("Path traversal detected in parent_folder")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", separator)) {
    stop("Path traversal detected in separator")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", genes_file)) {
    stop("Path traversal detected in genes_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", barcodes_file)) {
    stop("Path traversal detected in barcodes_file")
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
        c(parent_folder, "/scratch")
      ),
      additional_arguments = c(
        "Rscript /home/mitoRiboFilter.R",
        matrix_file,
        as.character(mitoMin),
        as.character(mitoMax),
        as.character(riboMin),
        as.character(riboMax),
        separator,
        genes_file,
        barcodes_file
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "singlecell_mitoRibo_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}

