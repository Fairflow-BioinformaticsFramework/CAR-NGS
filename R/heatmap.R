#' Heatmap
#'
#' @description This function generates heatmaps based on the filtered gene count matrix, visualizing the expression levels of significant genes across samples,
#' it returns an output directory inside input_dir_path containing a heatmap of significant genes based on filtered data.
#' $B{container(repbioinfo/rnaseqstar_v2:latest,docker);
#' command(Rscript /home/heatmap.R $countmatrix_name $metadata_name);
#' volume($input_dir_path:/scratch);id(heatmap);name(Heatmap)}
#' @param input_dir_path a character string indicating the path of the directory containing the fastq files and the csv files obtained from the indexing
#' $B{!;type(file)}
#' @param countmatrix_name name of the filtered gene count matrix file obtained after running DESeq
#' $B{!;type(file)}
#' @param metadata_name name of the metadata file obtained after the genome indexing
#' $B{!;type(file)}
#' @author Luca Alessandri, Agata D'Onofrio, Eliseo Martelli
#'
#' @examples
#' \dontrun{
#' heatmap(
#'   input_dir_path = "/the/input/dir",
#'   countmatrix_name = "filtered_count_matrix.csv",
#'   metadata_name = "Covariatesstat.csv"
#' )
#' }
#' @export
heatmap <- function(input_dir_path, countmatrix_name, metadata_name) {
  # Type checking.
  if (typeof(input_dir_path) != "character") {
    stop(paste("input_dir_path type is", paste0(typeof(input_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(countmatrix_name) != "character") {
    stop(paste("countmatrix_name type is", paste0(typeof(countmatrix_name), "."), "It should be \"character\""))
  }
  if (typeof(metadata_name) != "character") {
    stop(paste("metadata_name type is", paste0(typeof(metadata_name), "."), "It should be \"character\""))
  }

  # Check if input_dir_path exists
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(input_dir_path)) {
      stop(paste("input_dir_path:", input_dir_path, "does not exist."))
    }
  }

  # Executing the docker job
  rrundocker::run_in_docker(
    image_name = paste0("repbioinfo/rnaseqstar_v2:latest"),
    volumes = list(
      c(input_dir_path, "/scratch")
    ),
    additional_arguments = c(
      "Rscript /home/heatmap.R",
      countmatrix_name,
      metadata_name
    )
  )
}
