#' Feature Selection Script
#'
#' @description This script is used to identify differentially expressed (DE) genes using various statistical methods like ANOVA, MAST, and edgeR.
#' It returns an output directory inside the parent_folder containing: a CSV file containing DE gene results for each comparison, volcano plots and heatmaps.
#' $B{container(repbioinfo/singlecelldownstream:latest,docker);
#' command(Rscript /home/featureSelection.R $matrix_file $clustering_file $threshold $log2fc $pvalue $separator $genes_file $barcodes_file $heatmap);
#' volume($parent_folder:/scratch);id(featureselectionscript);name(Feature Selection Script)}
#' @param input_file_path a character string indicating the path of the count matrix file, which can be both dense (.csv/.txt) or sparse (.mtx)
#' $B{!;type(file)}
#' @param clustering_file a character string indicating the name of the CSV file containing the clustering results
#' $B{!;type(file)}
#' @param threshold the stability threshold for filtering cells based on their stability score
#' $B{!;type(integer)}
#' @param log2fc thelog2 fold change threshold for identifying DE genes
#' $B{!;type(integer)}
#' @param pvalue the p-value threshold for identifying DE genes
#' $B{!;type(float)}
#' @param separator separator used in the count table
#' $B{!;type(text)}
#' @param genes_file a character string indicating the name of the genes name files necessary for the analysis of a sparse matrix ("*genes.tsv")
#' $B{!;type(file)}
#' @param barcodes_file a character string indicating the name of the barcodes file necessary for the analysis of a sparse matrix ("*barcodes.tsv")
#' $B{!;type(file)}
#' @param heatmap option to generate an heatmap
#' $B{!;type(boolean)}
#' @author Luca Alessandri, Agata D'Onofrio
#'
#' @examples
#' \dontrun{
#' # Dense matrix analysis
#' singlecell_featureSelection(
#'   input_file_path = "/the/input/file.csv",
#'   clustering_file = "DENSE_Filtered_clustering_stability.output.csv",
#'   threshold = 0,
#'   log2fc = 1,
#'   pvalue = 0.05,
#'   separator = ","
#' )
#'
#' # Dense matrix analysis
#' singlecell_featureSelection(
#'   input_file_path = "/the/input/file.mtx",
#'   clustering_file = "combined_filtered_matrix_with_sample_clustering_stability.output.csv",
#'   threshold = 0,
#'   log2fc = 1,
#'   pvalue = 0.05,
#'   genes_file = "combined_filtered_with_sample_genes.tsv",
#'   barcodes_file = "combined_filtered_with_sample_barcodes.tsv"
#' )
#' }
#' @export
singlecell_featureSelection <- function(input_file_path, clustering_file, threshold, log2fc, pvalue, separator = NULL, genes_file = NULL, barcodes_file = NULL, heatmap = FALSE) {
  # Type checking.
  if (typeof(input_file_path) != "character") {
    stop(paste("input_file_path type is", paste0(typeof(input_file_path), "."), "It should be \"character\""))
  }
  if (typeof(clustering_file) != "character") {
    stop(paste("clustering_file type is", paste0(typeof(clustering_file), "."), "It should be \"character\""))
  }
  if (!is.numeric(threshold)) {
    stop(paste("threshold type is", paste0(typeof(threshold), "."), "It should be \"double\" or \"integer\""))
  }
  if (!is.numeric(log2fc)) {
    stop(paste("log2fc type is", paste0(typeof(log2fc), "."), "It should be \"double\" or \"integer\""))
  }
  if (!is.numeric(pvalue)) {
    stop(paste("pvalue type is", paste0(typeof(pvalue), "."), "It should be \"double\" or \"integer\""))
  }
  if (!is.null(separator) && typeof(separator) != "character") {
    stop(paste("separator type is", paste0(typeof(separator), "."), "It should be \"character\""))
  }
  if (!is.null(genes_file) && typeof(genes_file) != "character") {
    stop(paste("genes_file type is", paste0(typeof(genes_file), "."), "It should be \"character\""))
  }
  if (!is.null(barcodes_file) && typeof(barcodes_file) != "character") {
    stop(paste("barcodes_file type is", paste0(typeof(barcodes_file), "."), "It should be \"character\""))
  }
  if (typeof(heatmap) != "logical") {
    stop(paste("heatmap type is", paste0(typeof(heatmap), "."), "It should be \"logical\""))
  }

  # Check if the given paths exist
  if (!is_running_in_docker()) {
    if (!file.exists(input_file_path)) {
      stop(paste("input_file_path:", input_file_path, "does not exist."))
    }
  }

  # Obtain parent folder from input_file_path.
  parent_folder <- dirname(input_file_path)

  # Obtain matrix name and file format from input_file_path to create the variable matrix_file
][1]
  matrix_name <- input_file_path_parts[1]
  format <- input_file_path_parts[2] # Uses extension as an heuristic.
  matrix_file <- paste0(matrix_name, ".", format)

  # Setting the separator as NULL for sparse matrix analysis
  if (is.null(separator)) {
    separator <- "NULL"
  }

  # Executing the docker job
  rrundocker::run_in_docker(
    image_name = paste0("repbioinfo/singlecelldownstream:latest"),
    volumes = list(
      c(parent_folder, "/scratch")
    ),
    additional_arguments = c(
      "Rscript /home/featureSelection.R",
      matrix_file,
      clustering_file,
      threshold,
      log2fc,
      pvalue,
      separator,
      genes_file,
      barcodes_file,
      if (heatmap) "true" else "false"
    )
  )
}
