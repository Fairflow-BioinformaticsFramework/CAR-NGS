#' Atac-seq
#'
#' @description This function is used to do a Assay for Transposase-Accessible Chromatin with high-throughput sequencing analysis,
#' it returns quality control files for each fastq (FastQC reports),
#' BAM files containing aligned reads in BAM format,
#' index files (.bai) for each BAM file,
#' Peak Calling results and BigWig files
#' $B{container(repbioinfo/atacseq:latest,docker);
#' command(/home/script.sh $nThreads);
#' volume($input_dir_path:/scratch);
#' volume($genome_dir_path:/genomes);id(Atac-seq);name(Atac-seq)}
#' @param input_dir_path a character string indicating the path of a directory containing the fastq files to be analyzed
#' $B{!;type(file)}
#' @param genome_dir_path a character string indicating the path of a directory containing the fasta files of the genome to be analyzed
#'$B{!;type(file)}
#' @param nThreads number of cores for parallelization
#' $B{type(integer);value(8)}
#' @author Luca Alessandri, Agata D'Onofrio
#'
#' @examples
#' \dontrun{
#' atacSeq(
#'   input_dir_path = "/the/input/path",
#'   genome_dir_path = "/the/genome/path"
#' )
#' }
#' @export
atacSeq <- function(input_dir_path, genome_dir_path, nThreads = 8) {
  # Type checking.
  if (typeof(input_dir_path) != "character") {
    stop(paste("input_dir_path type is", paste0(typeof(input_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(genome_dir_path) != "character") {
    stop(paste("genome_dir_path type is", paste0(typeof(genome_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(nThreads) != "double") {
    stop(paste("nThreads type is", paste0(typeof(nThreads), "."), "It should be \"double\""))
  }


  # Check if input_dir_path exists
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
    image_name = paste0("repbioinfo/atacseq:latest"),
    volumes = list(
      c(input_dir_path, "/scratch"),
      c(genome_dir_path, "/genomes"),
      c(paste0(input_dir_path, "/results"), "/scratch/results")
    ),
    additional_arguments = c(
      "/home/script.sh",
      nThreads
    )
  )
}
