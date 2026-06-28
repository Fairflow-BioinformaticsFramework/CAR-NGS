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

usage_str <- paste0(paste0("\033[93m<workdir>", RESET), ' ', paste0("\033[93m<scratch>", RESET), ' ', paste0("\033[92m<matrix_file>", RESET), ' ', paste0("\033[92m<species>", RESET), ' ', paste0("\033[92m<source>", RESET), ' ', paste0("\033[92m<separator>", RESET), ' ', paste0("\033[92m<max_terms>", RESET))

args_raw <- commandArgs(trailingOnly = TRUE)

if (length(args_raw) != 7) {
  cat(WHITE, 'Usage: Rscript singlecell_enrichmentanalysis.R ', usage_str, RESET, '\n\n', sep = '')
  cat_col("Process the results of differential expression and performs pathway enrichment analysis", color = YELLOW)
  cat('\n')
  cat_col('Arguments:', color = WHITE)
  cat('\033[93mworkdir         [io]  Path to working directory containing scratch folder', RESET, '\n', sep = '')
  cat('\033[93mscratch         [io]  Path to scratch folder containing the files to be analyzed', RESET, '\n', sep = '')
  cat('\033[92mmatrix_file           name of the count matrix file containing the results of differential expression analysis', RESET, '\n', sep = '')
  cat('\033[92mspecies               a character string indicating the species that is being analyzed', RESET, '\n', sep = '')
  cat('\033[92msource                a character sting indicating the source of enrichment analysis', RESET, '\n', sep = '')
  cat('\033[92mseparator             separator used in the count table', RESET, '\n', sep = '')
  cat('\033[92mmax_terms             maximum number of enriched terms to display in the output plot', RESET, '\n', sep = '')
  quit(status = 1)
}

# Parse positional arguments
args <- list()
args$workdir <- args_raw[1]
args$scratch <- args_raw[2]
args$matrix_file <- args_raw[3]
args$species <- args_raw[4]
args$source <- args_raw[5]
args$separator <- args_raw[6]
args$max_terms <- args_raw[7]

# --- Input validation ---
errors <- character(0)

if (!dir.exists(args$workdir)) {
  errors <- c(errors, paste0('Directory not found: workdir = ', args$workdir))
}
if (!dir.exists(args$scratch)) {
  errors <- c(errors, paste0('Directory not found: scratch = ', args$scratch))
}
if (!args$species %in% c("hsapiens", "mmusculus", "dmelanogaster")) {
  errors <- c(errors, paste0('Invalid value for species: ', args$species, '. Allowed: hsapiens, mmusculus, dmelanogaster'))
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

docker_vals$matrix_file <- args$matrix_file
docker_vals$species <- args$species
docker_vals$source <- args$source
docker_vals$separator <- args$separator
docker_vals$max_terms <- args$max_terms

# --- Assemble docker command ---
mount_str <- paste(mounts, collapse = ' ')
cmd <- paste('docker run --rm', mount_str, 'repbioinfo/singlecelldownstream Rscript /home/enrichment_analysis.R <matrix_file> <species> <source> <separator> <max_terms>')
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