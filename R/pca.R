# ANSI color codes
RED    <- '\033[91m'
WHITE  <- '\033[97m'
YELLOW <- '\033[93m'
ORANGE <- '\033[38;5;208m'
GREEN  <- '\033[92m'
RESET  <- '\033[0m'

cat_col <- function(..., color = WHITE) {
  cat(color, ..., RESET, '\n', sep = '')
}

usage_str <- paste0(paste0("\033[93m<workdir>", RESET), ' ', paste0("\033[38;5;208m<matrix_file>", RESET), ' ', paste0("\033[38;5;208m<metadata_file>", RESET))

args_raw <- commandArgs(trailingOnly = TRUE)

if (length(args_raw) != 3) {
  cat(WHITE, 'Usage: Rscript pca.R ', usage_str, RESET, '\n\n', sep = '')
  cat_col("Performs Principal Component Analysis (PCA) on the gene count matrix", color = YELLOW)
  cat('\n')
  cat_col('Arguments:', color = WHITE)
  cat('\033[93mworkdir         [io]  Path to working directory containing the fastq files and csv files obtained from indexing', RESET, '\n', sep = '')
  cat('\033[38;5;208mmatrix_file     [cp]  Path to the count matrix file obtained after the genome indexing', RESET, '\n', sep = '')
  cat('\033[38;5;208mmetadata_file   [cp]  Path to the metadata file obtained after the genome indexing', RESET, '\n', sep = '')
  quit(status = 1)
}

# Parse positional arguments
args <- list()
args$workdir <- args_raw[1]
args$matrix_file <- args_raw[2]
args$metadata_file <- args_raw[3]

# --- Input validation ---
errors <- character(0)

if (!dir.exists(args$workdir)) {
  errors <- c(errors, paste0('Directory not found: workdir = ', args$workdir))
}
if (!file.exists(args$matrix_file)) {
  errors <- c(errors, paste0('File not found: matrix_file = ', args$matrix_file))
}
if (!file.exists(args$metadata_file)) {
  errors <- c(errors, paste0('File not found: metadata_file = ', args$metadata_file))
}

if (length(errors) > 0) {
  for (e in errors) cat(RED, 'ERROR: ', RESET, WHITE, e, RESET, '\n', sep = '')
  quit(status = 1)
}

# --- Scratch directory setup ---
n <- 1
repeat {
  if (dir.exists(file.path(normalizePath(args$workdir), paste0('scratch', n)))) {
    n <- n + 1
  } else {
    break
  }
}

scratch_path <- file.path(normalizePath(args$workdir), paste0('scratch', n))
dir.create(scratch_path, recursive = TRUE, showWarnings = FALSE)

# --- Build docker volume mounts ---
mounts      <- character(0)
docker_vals <- list()
service_idx <- 1

mounts <- c(mounts, paste0('-v "', scratch_path, ':/scratch"'))
docker_vals$workdir <- '/scratch'

# --- Bind files and service volumes ---
mounted_folders <- list()

src_matrix_file <- normalizePath(args$matrix_file)
file.copy(src_matrix_file, scratch_path)
docker_vals$matrix_file <- paste0('/scratch/', basename(src_matrix_file))

src_metadata_file <- normalizePath(args$metadata_file)
file.copy(src_metadata_file, scratch_path)
docker_vals$metadata_file <- paste0('/scratch/', basename(src_metadata_file))


# --- Assemble docker command ---
mount_str <- paste(mounts, collapse = ' ')
cmd <- paste('docker run --rm', mount_str, 'repbioinfo/rnaseqstar_v2 Rscript /home/pca.R <matrix_file> <metadata_file>')
placeholders <- regmatches(cmd, gregexpr('<[^>]+>', cmd))[[1]]
for (ph in placeholders) {
  key <- gsub('<|>', '', ph)
  val <- docker_vals[[key]]
  if (!is.null(val)) cmd <- gsub(ph, val, cmd, fixed = TRUE)
}
cat('\n', YELLOW, 'Running:\n', RESET, WHITE, cmd, RESET, '\n\n', sep = '')
log_path <- file.path(scratch_path, 'output_log.txt')
cat(YELLOW, 'Log: ', RESET, WHITE, log_path, RESET, '\n\n', sep = '')

con <- file(log_path, open = 'w')
p   <- pipe(paste(cmd, '2>&1'), open = 'r')
while (length(line <- readLines(p, n = 1, warn = FALSE)) > 0) {
  cat(line, '\n', sep = '')
  writeLines(line, con)
}
ret <- close(p)
close(con)

if (ret == 0) {
  cat('\n', GREEN, 'Done. Log saved to: ', log_path, RESET, '\n', sep = '')
} else {
  cat('\n', RED, 'Docker exited with code ', ret, '. See log: ', log_path, RESET, '\n', sep = '')
}
quit(status = ret)