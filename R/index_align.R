#' Index and Alignment Script
#'
#' @description This script performs the alignment of RNA-Seq reads to the reference genome using STAR and generates a gene count matrix.
#' It returns a output directory containing aligned BAM files, gene_count_matrix.csv (gene count matrix)
#' and Covariatesstat.csv (metadata file containing sample information)
#' $B{container(repbioinfo/rnaseqstar_v2:latest,docker);
#' command(/home/index_align.sh);
#' volume($input_dir_path:/scratch);
#' volume($genome_dir_path:/genome);id(index-and-alignment-script);name(Index and Alignment Script)}
#' @param input_dir_path, a character string indicating the path of a directory containing the fastq files to be analyzed
#' $B{!;type(file)}
#' @param genome_dir_path, a character string indicating the path of a directory containing the fasta files of the genome to be analyzed
#' $B{!;type(file)}
#' @author Luca Alessandri, Agata D'Onofrio
#'
#' @examples
#' \dontrun{
#' index_align(
#'   input_dir_path = "/the/input/path",
#'   genome_dir_path = "/the/genome/path"
#' )
#' }
#' @export
index_align <- function(input_dir_path, genome_dir_path) {
  # Type checking.
  if (typeof(input_dir_path) != "character") {
    stop(paste("input_dir_path type is", paste0(typeof(input_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(genome_dir_path) != "character") {
    stop(paste("genome_dir_path type is", paste0(typeof(genome_dir_path), "."), "It should be \"character\""))
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
    image_name = paste0("repbioinfo/rnaseqstar_v2:latest"),
    volumes = list(
      c(input_dir_path, "/scratch"),
      c(genome_dir_path, "/genome")
    ),
    additional_arguments = c(
      "/home/index_align.sh"
    )
  )
}
