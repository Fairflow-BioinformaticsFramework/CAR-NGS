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

usage_str <- paste0(paste0("\033[93m<workdir>", RESET), ' ', paste0("\033[93m<scratch>", RESET), ' ', paste0("\033[92m<organism>", RESET), ' ', paste0("\033[92m<matrix_file>", RESET), ' ', paste0("\033[92m<metadata_file>", RESET), ' ', paste0("\033[92m<matrix_sep>", RESET), ' ', paste0("\033[92m<meta_sep>", RESET))

args_raw <- commandArgs(trailingOnly = TRUE)

if (length(args_raw) != 7) {
  cat(WHITE, 'Usage: Rscript modules.R ', usage_str, RESET, '\n\n', sep = '')
  cat_col("runs an RNA-seq data processing pipeline inside a Docker container. It analyzes a gene expression count matrix and associated metadata to identify gene modules", color = YELLOW)
  cat('\n')
  cat_col('Arguments:', color = WHITE)
  cat('\033[93mworkdir         [io]  Path to working directory containing scratch folder', RESET, '\n', sep = '')
  cat('\033[93mscratch         [io]  path of the directory containing the input files', RESET, '\n', sep = '')
  cat('\033[92morganism              a character string specifying the organism', RESET, '\n', sep = '')
  cat('\033[92mmatrix_file           name (with format) of the count matrix fil', RESET, '\n', sep = '')
  cat('\033[92mmetadata_file         name (with format) of the metadata file', RESET, '\n', sep = '')
  cat('\033[92mmatrix_sep            separator used in the count matrix file', RESET, '\n', sep = '')
  cat('\033[92mmeta_sep              separator used in the metadata file', RESET, '\n', sep = '')
  quit(status = 1)
}

# Parse positional arguments
args <- list()
args$workdir <- args_raw[1]
args$scratch <- args_raw[2]
args$organism <- args_raw[3]
args$matrix_file <- args_raw[4]
args$metadata_file <- args_raw[5]
args$matrix_sep <- args_raw[6]
args$meta_sep <- args_raw[7]

# --- Input validation ---
errors <- character(0)

if (!dir.exists(args$workdir)) {
  errors <- c(errors, paste0('Directory not found: workdir = ', args$workdir))
}
if (!dir.exists(args$scratch)) {
  errors <- c(errors, paste0('Directory not found: scratch = ', args$scratch))
}
if (!args$organism %in% c("Homosapiens", "Musmusculus", "Drosophilamelanogaster")) {
  errors <- c(errors, paste0('Invalid value for organism: ', args$organism, '. Allowed: Homosapiens, Musmusculus, Drosophilamelanogaster'))
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

mounts <- c(mounts, paste0('-v "', scratch_path, ':/workDir"'))
docker_vals$workdir <- '/workDir'

# scratch: read-write directory [io]
mounts <- c(mounts, paste0('-v "', normalizePath(args$scratch), ':/scratch"'))
docker_vals$scratch <- '/scratch'

# --- Bind files and service volumes ---
mounted_folders <- list()

docker_vals$organism <- args$organism
docker_vals$matrix_file <- args$matrix_file
docker_vals$metadata_file <- args$metadata_file
docker_vals$matrix_sep <- args$matrix_sep
docker_vals$meta_sep <- args$meta_sep

# --- Assemble docker command ---
mount_str <- paste(mounts, collapse = ' ')
cmd <- paste('docker run --rm', mount_str, 'repbioinfo/rnaseqbulkdownstreamunbias Rscript /home/modules.R <organism> <matrix_file> <metadata_file> <matrix_sep> <meta_sep>')
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