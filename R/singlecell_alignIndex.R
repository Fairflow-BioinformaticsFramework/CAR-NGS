#' Index and Alignment Script for single cell analysis
#'
#' @description This script handles the alignment and indexing of the FASTQ
#' files generated from single-cell RNA-seq experiments using Cell Ranger.
#' It returns a output directory inside input_dir_path containing aligned BAM
#' files and combined matrix of counts.
#' $B{
#' container(repbioinfo/carncellranger2:latest,docker);
#' command(/home/index_align.sh $bamsave);
#' volume($input_dir_path:/scratch);
#' volume($genome_dir_path:/genome);id(alignindex);name(Index and Alignment Script for single cell analysis)}
#'
#' @param input_dir_path a character string indicating the path of a directory
#' containing the fastq files to be analyzed
#' $B{!;type(file)}
#' @param genome_dir_path a character string indicating the path of a directory
#' containing the fasta and gtf files of the genome to be analyzed
#' $B{!;type(file)}
#' @param bamsave a boolean variable indicating if the BAM files are to be
#' saved or not 
#' $B{type(boolean);value(true)}
#' @author Luca Alessandri, Agata D'Onofrio
#'
#' @examples
#' \dontrun{
#' singlecell_alignIndex(
#'   input_dir_path = "/the/input/path",
#'   genome_dir_path = "/the/genome/path",
#' )
#' }
#' @export
singlecell_align_index <- function(
    input_dir_path, genome_dir_path, bamsave = TRUE) {
  # Type checking.
  if (typeof(input_dir_path) != "character") {
    stop(
      paste(
        "input_dir_path type is", paste0(typeof(input_dir_path), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(genome_dir_path) != "character") {
    stop(
      paste(
        "genome_dir_path type is", paste0(typeof(genome_dir_path), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(bamsave) != "logical") {
    stop(
      paste(
        "bamsave type is", paste0(typeof(bamsave), "."),
        "It should be \"logical\""
      )
    )
  }
  
  # Check if the paths in input exist
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(input_dir_path)) {
      stop(paste("input_dir_path:", input_dir_path, "does not exist."))
    }
    if (!dir.exists(genome_dir_path)) {
      stop(paste("genome_dir_path:", genome_dir_path, "does not exist."))
    }
  }
  
  # Executing the docker job
  rrundocker::run_in_docker(
    image_name = paste0("repbioinfo/carncellranger2:latest"),
    volumes = list(
      c(input_dir_path, "/scratch"),
      c(genome_dir_path, "/genome")
    ),
    additional_arguments = c(
      "/home/index_align.sh",
      if (bamsave) "true" else "false"
    )
  )
}
