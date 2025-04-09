#' Clustering and Stability Analysis Script
#'
#' @description This script is used for clustering cells and assessing the stability of the clusters using Seurat and bootstrapping, ensuring that the identified clusters are consistent and reliable.
#' It returns an output directory inside the parent_folder containing: a CSV file containing cluster assignments, UMAP coordinates, and stability scores for each cell,
#' 2 UMAP plots (colored by cluster and stability scores) and a filtered matrix.
#' $B{container(repbioinfo/singlecelldownstream:latest,docker);
#' command(Rscript /home/clustering.R $/scratch/matrix_file $bootstrap_percentage $stability_threshold $permutations $separator $/scratch/genes_file $/scratch/barcodes_file $resolution);
#' volume($parent_folder:/scratch);id(clustering);name(Clustering and Stability Analysis Script)}
#' @param input_file_path a character string indicating the path of the count matrix file, which can be both dense (.csv/.txt) or sparse (.mtx)
#' $B{!;type(file)}
#' @param bootstrap_percentage percentage of cells to remove in each bootstrap iteration
#' $B{!;type(float)}
#' @param stability_threshold the minimum Jaccard Index value for a cluster to be considered stable
#' $B{!;type(float)}
#' @param permutations the number of bootstrap iterations to perform
#' $B{!;type(integer)}
#' @param separator, separator used in the count table
#' $B{!;type(text)}
#' @param genes_file, a character string indicating the name of the genes name files necessary for the analysis of a sparse matrix ("*genes.tsv")
#' $B{!;type(file)}
#' @param barcodes_file, a character string indicating the name of the barcodes file necessary for the analysis of a sparse matrix ("*barcodes.tsv")
#' $B{!;type(file)}
#' @param resolution, the resolution parameter for Seurat clustering, which controls the granularity of the clusters
#' $B{!;type(float)}
#' @author Luca Alessandri, alessandri [dot] luca1991 [at] gmail [dot] com, University of Torino
#'
#' @examples
#' \dontrun{
#' # Dense matrix analysis
#' singlecell_clustering(
#'   input_file_path = "/the/input/file.csv",
#'   bootstrap_percentage = 0.1,
#'   stability_threshold = 0.8,
#'   permutations = 10,
#'   separator = ",",
#'   resolution = 0.8
#' )
#'
#' # Dense matrix analysis
#' singlecell_clustering(
#'   input_file_path = "/the/input/file.mtx",
#'   bootstrap_percentage = 0.1,
#'   stability_threshold = 0.8,
#'   permutations = 10,
#'   genes_file = "combined_filtered_with_sample_genes.tsv",
#'   barcodes_file = "combined_filtered_with_sample_barcodes.tsv",
#'   resolution = 0.8
#' )
#' }
#' @export
singlecell_clustering <- function(input_file_path, bootstrap_percentage, stability_threshold, permutations, separator = NULL, genes_file = NULL, barcodes_file = NULL, resolution) {
  # Type checking.
  if (typeof(input_file_path) != "character") {
    stop(paste("input_file_path type is", paste0(typeof(input_file_path), "."), "It should be \"character\""))
  }
  if (!is.numeric(bootstrap_percentage)) {
    stop(paste("bootstrap_percentage type is", paste0(typeof(bootstrap_percentage), "."), "It should be \"double\" or \"integer\""))
  }
  if (!is.numeric(stability_threshold)) {
    stop(paste("stability_threshold type is", paste0(typeof(stability_threshold), "."), "It should be \"double\" or \"integer\""))
  }
  if (!is.numeric(permutations)) {
    stop(paste("permutations type is", paste0(typeof(permutations), "."), "It should be \"double\" or \"integer\""))
  }
  if (!is.numeric(resolution)) {
    stop(paste("resolution type is", paste0(typeof(resolution), "."), "It should be \"double\" or \"integer\""))
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

  # Check if the given paths exist
  if (!is_running_in_docker()) {
    if (!file.exists(input_file_path)) {
      stop(paste("input_file_path:", input_file_path, "does not exist."))
    }
  }

  # Obtain parent folder from input_file_path.
  parent_folder <- dirname(input_file_path)

  # Obtain matrix name and file format from input_file_path to create the variable matrix_file
  input_file_path_parts <- strsplit(basename(input_file_path), "\\.")[[1]]
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
      "Rscript /home/clustering.R",
      paste0("/scratch/", matrix_file),
      bootstrap_percentage,
      stability_threshold,
      permutations,
      separator,
      paste0("/scratch/", genes_file),
      paste0("/scratch/", barcodes_file),
      resolution
    )
  )
}
