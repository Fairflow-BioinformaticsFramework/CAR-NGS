#' Enrichment Analysis Script
#'
#' @description This script processes the results of differential expression and performs
#' pathway enrichment analysis.
#' $B{container(repbioinfo/singlecelldownstream:latest,docker);
#' command(Rscript/home/enrichment_analysis.R $matrix_file $species $source $separator $max_terms);
#' volume($parent_folder:/scratch);id(enrichment-analysis-script);name(Enrichment Analysis Script)}
#' It returns a output directory inside parent_folder containing a PDF file
#' containing a bar plot of the enriched terms.
#' @param input_file_path a character string indicating the path of a CSV file containing the results of differential expression analysis.
#' $B{!;type(file)}
#' @param species a character string indicating the species that is being analyzed (e.g. "hsapiens", "mmusculus" or "dmelanogaster")
#' $B{!;type(text)}
#' @param source a character sting indicating the source of enrichment analysis
#' $B{!;type(text)}
#' @param separator separator used in the count table
#' $B{!;type(text)}
#' @param max_terms the maximum number of enriched terms to display in the
#' $B{!;type(integer)}
#' output plot
#' @author Luca Alessandri, Agata D'Onofrio
#'
#' @examples
#' \dontrun{
#' enrichmentAnalysis(
#'   input_file_path = "/the/input/file/path",
#'   species = "dmelanogaster",
#'   source = "KEGG",
#'   separator = ",",
#'   max_terms = 20
#' )
#' }
#' @export
enrichment_analysis <- function(input_file_path,
                                species,
                                source,
                                separator,
                                max_terms) {
  # Type checking.
  if (typeof(input_file_path) != "character") {
    stop(
      paste(
        "input_file_path type is", paste0(typeof(input_file_path), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(species) != "character") {
    stop(
      paste(
        "species type is", paste0(typeof(species), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(source) != "character") {
    stop(
      paste(
        "source type is", paste0(typeof(source), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(separator) != "character") {
    stop(
      paste(
        "separator type is", paste0(typeof(separator), "."),
        "It should be \"character\""
      )
    )
  }
  if (!is.numeric(max_terms)) {
    stop(
      paste(
        "max_terms type is", paste0(typeof(max_terms), "."),
        "It should be \"double\" or \"integer\""
      )
    )
  }

  # Check if the given paths exist
  if (!rrundocker::is_running_in_docker()) {
    if (!file.exists(input_file_path)) {
      stop(paste("input_file_path:", input_file_path, "does not exist."))
    }
  }

  # Obtain parent folder from input_file_path.
  parent_folder <- dirname(input_file_path)

  # Obtain matrix name and file format from input_file_path to create the
  # variable matrix_file
  input_file_path_parts <- strsplit(basename(input_file_path), "\\.")[[1]]
  matrix_name <- input_file_path_parts[1]
  format <- input_file_path_parts[2] # Uses extension as an heuristic.
  matrix_file <- paste0(matrix_name, ".", format)

  # Executing the docker job
  rrundocker::run_in_docker(
    image_name = paste0("repbioinfo/singlecelldownstream:latest"),
    volumes = list(
      c(parent_folder, "/scratch")
    ),
    additional_arguments = c(
      "Rscript /home/enrichment_analysis.R",
      matrix_file,
      species,
      source,
      separator,
      max_terms
    )
  )
}
