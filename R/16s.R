#' 16s
#'
#' @description This function is used to do an analysis of genomic bacteria, checking if
#' there is any genomic bacteria in a DNA sample and returning a html directory
#' $B{container(repbioinfo/qiime2023:latest,docker);
#' command(/home/qiime_full.sh);
#' volume($input_dir_path:/scratch);id(16s);name(16s)}
#' @param input_dir_path, a character string indicating the path of a directory
#' containing the fatq files to be analyzed
#' $B{!;type(file)}
#' @author Luca Alessandri, Agata D'Onofrio
#' @examples
#' \dontrun{
#' sixteenS(
#'   input_dir_path = "/the/input/dir"
#' )
#' }
#' @export
sixteenS <- function(input_dir_path) {
  # Type checking.
  if (typeof(input_dir_path) != "character") {
    stop(paste("input_dir_path type
    is", paste0(typeof(input_dir_path), "."), "It should be \"character\""))
  }

  # Check if input_dir_path exists
  if (!rrundocker::is_running_in_docker()) {
    if (!dir.exists(input_dir_path)) {
      stop(paste("input_dir_path:", input_dir_path, "does not exist."))
    }
  }

  # Executing the docker job
  rrundocker::run_in_docker(
    image_name = paste0("repbioinfo/qiime2023:latest"),
    volumes = list(
      c(input_dir_path, "/scratch")
    ),
    additional_arguments = c(
      "/home/qiime_full.sh"
    )
  )
}
