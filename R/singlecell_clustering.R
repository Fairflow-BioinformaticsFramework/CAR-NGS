#' Clustering and Stability Analysis Script
#'
#' @description This script is used for clustering cells and assessing the stability of the clusters using Seurat and bootstrapping, 
#' ensuring that the identified clusters are consistent and reliable.
#' @param matrix_file name of the count matrix file, which can be both dense (.csv/.txt) or sparse (.mtx)
#' @param parent_folder path of the directory containing the matrix file
#' @param bootstrap_percentage percentage of cells to remove in each bootstrap iteration
#' @param stability_threshold minimum Jaccard Index value for a cluster to be considered stable
#' @param permutations number of bootstrap iterations to perform
#' @param separator separator used in the count table
#' @param genes_file name of the genes name files necessary for the analysis of a sparse matrix (*genes.tsv)
#' @param barcodes_file name of the barcodes file necessary for the analysis of a sparse matrix (*barcodes.tsv)
#' @param resolution resolution parameter for Seurat clustering
#' @return Results of the operation
#'
#' @export
singlecell_clustering <- function(matrix_file,
parent_folder,
bootstrap_percentage,
stability_threshold,
permutations,
separator,
genes_file,
barcodes_file,
resolution) {
  # Type validation
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  if (!is.character(parent_folder) || length(parent_folder) != 1) {
    stop("parent_folder must be a single character string")
  }
  if (!is.numeric(bootstrap_percentage) || length(bootstrap_percentage) != 1) {
    stop("bootstrap_percentage must be a single numeric value")
  }
  if (!is.numeric(stability_threshold) || length(stability_threshold) != 1) {
    stop("stability_threshold must be a single numeric value")
  }
  if (!is.numeric(permutations) || length(permutations) != 1 || permutations != round(permutations)) {
    stop("permutations must be a single integer value")
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
  if (!is.numeric(resolution) || length(resolution) != 1) {
    stop("resolution must be a single numeric value")
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
        c(parent_folder_dir, "/scratch"),
      ),
      additional_arguments = c(
        "Rscript /home/clustering.R",
        "matrixfile_name",
        as.character(bootstrap_percentage),
        as.character(stability_threshold),
        as.character(permutations),
        separator,
        "genesfile_name",
        "barcodesfile_name",
        as.character(resolution),
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "singlecell_clustering_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}
