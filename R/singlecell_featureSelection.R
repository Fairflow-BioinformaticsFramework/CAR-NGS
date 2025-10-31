#' Feature Selection Script
#'
#' @description Feature selection script to identify differentially expressed (DE) genes using various 
#' statistical methods like ANOVA, MAST, and edgeR
#' @param matrix_file name of the count matrix file, which can be both dense (.csv/.txt) or sparse (.mtx)
#' @param clustering_file name of the CSV file containing the clustering results
#' @param parent_folder path of the directory containing the matrix file
#' @param threshold the stability threshold for filtering cells based on their stability
#' @param log2fc the log2 fold change threshold for identifying DE genes
#' @param pvalue the p-value threshold for identifying DE genes
#' @param separator separator used in the count table
#' @param genes_file name of the genes name files necessary for the analysis of a sparse matrix (*genes.tsv)
#' @param barcodes_file name of the barcodes file necessary for the analysis of a sparse matrix (*barcodes.tsv)
#' @param heatmap option to generate an heatmap
#' @return Results of the operation
#'
#' @export
singlecell_featureSelection <- function(matrix_file,
clustering_file,
parent_folder,
threshold,
log2fc,
pvalue,
separator,
genes_file,
barcodes_file,
heatmap) {
  # Type validation
  if (!is.character(matrix_file) || length(matrix_file) != 1) {
    stop("matrix_file must be a single character string")
  }
  if (!is.character(clustering_file) || length(clustering_file) != 1) {
    stop("clustering_file must be a single character string")
  }
  if (!is.character(parent_folder) || length(parent_folder) != 1) {
    stop("parent_folder must be a single character string")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold != round(threshold)) {
    stop("threshold must be a single integer value")
  }
  if (!is.numeric(log2fc) || length(log2fc) != 1 || log2fc != round(log2fc)) {
    stop("log2fc must be a single integer value")
  }
  if (!is.numeric(pvalue) || length(pvalue) != 1) {
    stop("pvalue must be a single numeric value")
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
  if (!is.logical(heatmap) || length(heatmap) != 1) {
    stop("heatmap must be a single logical value (TRUE/FALSE)")
  }
  
  # Security checks
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", matrix_file)) {
    stop("Path traversal detected in matrix_file")
  }
  if (grepl("\\.\\./|\\.\\\\|\\/\\.\\./|\\\\\\.\\\\\\.\\\\", clustering_file)) {
    stop("Path traversal detected in clustering_file")
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
        "Rscript /home/featureSelection.R",
        matrix_file,
        clustering_file,
        as.character(threshold),
        as.character(log2fc),
        as.character(pvalue),
        separator,
        genes_file,
        barcodes_file,
        if(heatmap) "--true-flag" else character(0),
      )
    )
    
    # Process result
    return(list(
      status = "success",
      output_dir = file.path(main_mount_dir, "singlecell_featureSelection_results")
    ))
  }, error = function(e) {
    stop(paste("Docker execution failed:", e$message))
  })
}

