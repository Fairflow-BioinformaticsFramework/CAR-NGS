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

usage_str <- paste0(paste0("\033[93m<workdir>", RESET), ' ', paste0("\033[93m<genome>", RESET), ' ', paste0("\033[93m<scratch>", RESET), ' ', paste0("\033[93m<outdir>", RESET), ' ', paste0("\033[92m<threshold>", RESET), ' ', paste0("\033[92m<adapt1>", RESET), ' ', paste0("\033[92m<adapt2>", RESET))

args_raw <- commandArgs(trailingOnly = TRUE)

if (length(args_raw) != 7) {
  cat(WHITE, 'Usage: Rscript detectseq.R ', usage_str, RESET, '\n\n', sep = '')
  cat_col("Executes a genome-wide assessment of off-target effects associated with cytosine base editors (CBEs)", color = YELLOW)
  cat('\n')
  cat_col('Arguments:', color = WHITE)
  cat('\033[93mworkdir         [io]  Path to working directory containing scratch folder', RESET, '\n', sep = '')
  cat('\033[93mgenome          [io]  Path to a directory containing the fasta files of the genome', RESET, '\n', sep = '')
  cat('\033[93mscratch         [io]  Path to scratch folder containing fastq files to be analyzed', RESET, '\n', sep = '')
  cat('\033[93moutdir          [out] path to output folder', RESET, '\n', sep = '')
  cat('\033[92mthreshold             threshold value for filtering', RESET, '\n', sep = '')
  cat('\033[92madapt1                first adapter sequence', RESET, '\n', sep = '')
  cat('\033[92madapt2                second adapter sequence', RESET, '\n', sep = '')
  quit(status = 1)
}

# Parse positional arguments
args <- list()
args$workdir <- args_raw[1]
args$genome <- args_raw[2]
args$scratch <- args_raw[3]
args$outdir <- args_raw[4]
args$threshold <- args_raw[5]
args$adapt1 <- args_raw[6]
args$adapt2 <- args_raw[7]

# --- Input validation ---
errors <- character(0)

if (!dir.exists(args$workdir)) {
  errors <- c(errors, paste0('Directory not found: workdir = ', args$workdir))
}
if (!dir.exists(args$genome)) {
  errors <- c(errors, paste0('Directory not found: genome = ', args$genome))
}
if (!dir.exists(args$scratch)) {
  errors <- c(errors, paste0('Directory not found: scratch = ', args$scratch))
}
if (!dir.exists(args$outdir)) {
  errors <- c(errors, paste0('Directory not found: outdir = ', args$outdir))
}

if (length(errors) > 0) {
  for (e in errors) cat(RED, 'ERROR: ', RESET, WHITE, e, RESET, '\n', sep = '')
  quit(status = 1)
}

# --- Scratch directory setup ---
n <- 1
repeat {
  if (dir.exists(file.path(normalizePath(args$workdir), paste0('scratch', n))) || dir.exists(file.path(normalizePath(args$outdir), paste0('scratch', n)))) {
    n <- n + 1
  } else {
    break
  }
}

scratch_path <- file.path(normalizePath(args$workdir), paste0('scratch', n))
dir.create(scratch_path, recursive = TRUE, showWarnings = FALSE)
scratch_out_path <- file.path(normalizePath(args$outdir), paste0('scratch', n))
dir.create(scratch_out_path, recursive = TRUE, showWarnings = FALSE)

# --- Build docker volume mounts ---
mounts      <- character(0)
docker_vals <- list()
service_idx <- 1

mounts <- c(mounts, paste0('-v "', scratch_path, ':/workDir"'))
docker_vals$workdir <- '/workDir'

host_out_base <- normalizePath(args$outdir)
mounts <- c(mounts, paste0('-v "', host_out_base, ':/scartch"'))
docker_vals$outdir <- paste0('/scartch/scratch', n)

# genome: read-write directory [io]
mounts <- c(mounts, paste0('-v "', normalizePath(args$genome), ':/genome"'))
docker_vals$genome <- '/genome'

# scratch: read-write directory [io]
mounts <- c(mounts, paste0('-v "', normalizePath(args$scratch), ':/scratch/raw.fastq:ro"'))
docker_vals$scratch <- '/scratch/raw.fastq:ro'

# --- Bind files and service volumes ---
mounted_folders <- list()

docker_vals$threshold <- args$threshold
docker_vals$adapt1 <- args$adapt1
docker_vals$adapt2 <- args$adapt2

# --- Assemble docker command ---
mount_str <- paste(mounts, collapse = ' ')
cmd <- paste('docker run --rm', mount_str, 'repbioinfo/detectseq:latest /home/detectSeq.sh <threshold> <adapt1> <adapt2>')
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