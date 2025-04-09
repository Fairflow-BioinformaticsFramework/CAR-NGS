#' DetectSeq
#'
#' @description This function executes a genome-wide assessment of off-target effects associated with cytosine base editors (CBEs),
#' it returns BAM files, PMAT files, Log files, and Filtered Output Files (BED and WIG format files)
#' $B{container(repbioinfo/detectseq:latest,docker);
#' command(/home/detectSeq.sh $threshold $adapt1 $adapt2);
#' volume($genome_dir_path:/genome);
#' volume($output_dir_path:/scratch);
#' volume($fastq_dir_path:/scratch/raw.fastq:ro);id(detectseq);name(DetectSeq)}
#' @param genome_dir_path a character string indicating the path of the directory containing the genome fasta file and, if already obtained, the genome index
#' $B{!;type(file)}
#' @param output_dir_path a character string indicating the path of the directory in which the outputs will be saved
#' $B{!;type(file)}
#' @param fastq_dir_path a character string indicating the path of the directory containing the fastq files
#' $B{!;type(file)}
#' @param threshold the threshold value for filtering
#' $B{!;type(integer)}
#' @param adapt1, optional adapter sequence with default set at "AGATCGGAAGAGCACACGT"
#' $B{!;type(text)}
#' @param adapt2, optional adapter sequence with default set at "AGATCGGAAGAGCGTCGTG"
#' $B{!;type(text)}
#' @author Luca Alessandri, Agata D'Onofrio, Eliseo Martelli
#'
#' @examples
#' \dontrun{
#' detectSeq(
#'   genome_dir_path = "/the/genome/dir",
#'   output_dir_path = "/the/output/dir",
#'   fastq_dir_path = "/the/fastq/dir",
#'   threshold = "3"
#' )
#' }
#' @export
detectSeq <- function(genome_dir_path, output_dir_path, fastq_dir_path, threshold, adapt1 = "AGATCGGAAGAGCACACGT", adapt2 = "AGATCGGAAGAGCGTCGTG") {
  # Type checking.
  if (typeof(genome_dir_path) != "character") {
    stop(paste("genome_dir_path type is", paste0(typeof(genome_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(output_dir_path) != "character") {
    stop(paste("output_dir_path type is", paste0(typeof(output_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(fastq_dir_path) != "character") {
    stop(paste("fastq_dir_path type is", paste0(typeof(fastq_dir_path), "."), "It should be \"character\""))
  }
  if (typeof(threshold) != "character") {
    stop(paste("threshold type is", paste0(typeof(threshold), "."), "It should be \"character\""))
  }
  if (typeof(adapt1) != "character") {
    stop(paste("adapt1 type is", paste0(typeof(adapt1), "."), "It should be \"character\""))
  }
  if (typeof(adapt2) != "character") {
    stop(paste("adapt2 type is", paste0(typeof(adapt2), "."), "It should be \"character\""))
  }

  # Check if the directories in input exist
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(genome_dir_path)) {
      stop(paste("genome_dir_path:", genome_dir_path, "does not exist."))
    }
    if (!dir.exists(output_dir_path)) {
      stop(paste("output_dir_path:", output_dir_path, "does not exist."))
    }
    if (!dir.exists(fastq_dir_path)) {
      stop(paste("fastq_dir_path:", fastq_dir_path, "does not exist."))
    }
  }

  # Executing the docker job
  rrundocker::run_in_docker(
    image_name = paste0("repbioinfo/detectseq:latest"),
    volumes = list(
      c(genome_dir_path, "/genome"),
      c(output_dir_path, "/scratch"),
      c(fastq_dir_path, "/scratch/raw.fastq:ro")
    ),
    additional_arguments = c(
      "/home/detectSeq.sh",
      threshold,
      adapt1,
      adapt2
    )
  )
}
