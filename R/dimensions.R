#' Cells count table size
#'
#' @description Counts row and columns of a counts table, and provides a dimensions.txt
#' file inside result_dir_path.
#' $B{container(docker.io/repbioinfo/r332.2017.01:latest,docker $matrix_name $format "$separator");
#' command(Rscript /home/main.R);
#' volume($result_dir_path:/data);
#' volume($parent_folder:/scratch);id(cells-count-table-size);name(Cells count table size)}
#' @param result_dir_path The path where to save the result file.
#' $B{!;type(file)}
#' @param input_file_path The path of the input file.
#' $B{!;type(file)}
#' @param separator The separator used in the count table.
#' $B{!;type(text)}
#' @author Luca Alessandri, Sebastian Bucatariu, Agata D'Onofrio
#' @examples
#' \dontrun{
#' dimensions(
#'   result_dir_path = "/the/result/dir",
#'   input_file_path = "/the/input/file.csv",
#'   separator = ","
#' )
#' }
#' @export
dimensions <- function(result_dir_path, input_file_path, separator) {
  # Type checking.
  if (typeof(result_dir_path) != "character") {
    stop(
      paste(
        "result_dir_path type is",
        paste0(typeof(result_dir_path), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(input_file_path) != "character") {
    stop(
      paste(
        "input_file_path type is",
        paste0(typeof(input_file_path), "."),
        "It should be \"character\""
      )
    )
  }
  if (typeof(separator) != "character") {
    stop(
      paste(
        "separator type is",
        paste0(typeof(separator), "."),
        "It should be \"character\""
      )
    )
  }

  if (!rrundocker::is_running_in_docker()) {
    if (!file.exists(input_file_path)) {
      stop(paste("input_file_path:", input_file_path, "does not exist."))
    }
    if (!dir.exists(result_dir_path)) {
      stop(paste("result_dir_path:", result_dir_path, "does not exist."))
    }
  }

  # Obtain parent folder from input_file_path.
  parent_folder <- dirname(input_file_path)

  # Obtain matrix name and file format from input_file_path.
  input_file_path_parts <- strsplit(basename(input_file_path), "\\.")[[1]]
  matrix_name <- input_file_path_parts[1]
  format <- input_file_path_parts[2] # Uses extension as an heuristic.

  rrundocker::run_in_docker(
    image_name = "docker.io/repbioinfo/r332.2017.01:latest",
    volumes = list(
      c(result_dir_path, "/data"),
      c(parent_folder, "/scratch")
    ),
    additional_arguments = c(
      "Rscript /home/main.R",
      matrix_name,
      format,
      paste0('"', separator, '"')
    )
  )
}
