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

usage_str <- paste0(paste0("\033[93m<workdir>", RESET), ' ', paste0("\033[93m<scratch>", RESET), ' ', paste0("\033[92m<matrix_file>", RESET), ' ', paste0("\033[92m<clustering_file>", RESET), ' ', paste0("\033[92m<threshold>", RESET), ' ', paste0("\033[92m<log2fc>", RESET), ' ', paste0("\033[92m<pvalue>", RESET), ' ', paste0("\033[92m<separator>", RESET), ' ', paste0("\033[92m<genes_file>", RESET), ' ', paste0("\033[92m<barcodes_file>", RESET), ' ', paste0("\033[92m<heatmap>", RESET))

args_raw <- commandArgs(trailingOnly = TRUE)

if (length(args_raw) != 11) {
  cat(WHITE, 'Usage: Rscript singlecell_featureselection.R ', usage_str, RESET, '\n\n', sep = '')
  cat_col("Feature selection scipt to identify differentially expressed (DE) genes in single cell analysis", color = YELLOW)
  cat('\n')
  cat_col('Arguments:', color = WHITE)
  cat('\033[93mworkdir         [io]  Path to working directory containing scratch folder', RESET, '\n', sep = '')
  cat('\033[93mscratch         [io]  Path to scratch folder containing the files to be analyzed', RESET, '\n', sep = '')
  cat('\033[92mmatrix_file           name of the count matrix file, which can be both dense (.csv/.txt) or sparse (.mtx)', RESET, '\n', sep = '')
  cat('\033[92mclustering_file       name of the CSV file containing the clustering results', RESET, '\n', sep = '')
  cat('\033[92mthreshold             the stability threshold for filtering cells based on their stability', RESET, '\n', sep = '')
  cat('\033[92mlog2fc                the log2 fold change threshold for identifying DE genes', RESET, '\n', sep = '')
  cat('\033[92mpvalue                the p-value threshold for identifying DE genes', RESET, '\n', sep = '')
  cat('\033[92mseparator             separator used in the count table for dense matrix analysis, is \\"NULL\\" for sparse matrix analysis', RESET, '\n', sep = '')
  cat('\033[92mgenes_file            name of the genes name files necessary for the analysis of a sparse matrix (*genes.tsv), \\"NULL\\" for dense matrix analysis', RESET, '\n', sep = '')
  cat('\033[92mbarcodes_file         name of the barcodes file necessary for the analysis of a sparse matrix (*barcodes.tsv), \\"NULL\\" for dense matrix analysis', RESET, '\n', sep = '')
  cat('\033[92mheatmap               option to generate an heatmap', RESET, '\n', sep = '')
  quit(status = 1)
}

# Parse positional arguments
args <- list()
args$workdir <- args_raw[1]
args$scratch <- args_raw[2]
args$matrix_file <- args_raw[3]
args$clustering_file <- args_raw[4]
args$threshold <- args_raw[5]
args$log2fc <- args_raw[6]
args$pvalue <- args_raw[7]
args$separator <- args_raw[8]
args$genes_file <- args_raw[9]
args$barcodes_file <- args_raw[10]
args$heatmap <- args_raw[11]

# --- Input validation ---
errors <- character(0)

if (!dir.exists(args$workdir)) {
  errors <- c(errors, paste0('Directory not found: workdir = ', args$workdir))
}
if (!dir.exists(args$scratch)) {
  errors <- c(errors, paste0('Directory not found: scratch = ', args$scratch))
}
if (!args$heatmap %in% c("TRUE", "FALSE")) {
  errors <- c(errors, paste0('Invalid value for heatmap: ', args$heatmap, '. Allowed: TRUE, FALSE'))
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
docker_vals$clustering_file <- args$clustering_file
docker_vals$threshold <- args$threshold
docker_vals$log2fc <- args$log2fc
docker_vals$pvalue <- args$pvalue
docker_vals$separator <- args$separator
docker_vals$genes_file <- args$genes_file
docker_vals$barcodes_file <- args$barcodes_file
docker_vals$heatmap <- args$heatmap

# --- Assemble docker command ---
mount_str <- paste(mounts, collapse = ' ')
cmd <- paste('docker run --rm', mount_str, 'repbioinfo/singlecelldownstream Rscript /home/featureSelection.R <matrix_file> <clustering_file> <threshold> <log2fc> <pvalue> <separator> <genes_file> <barcodes_file> <heatmap>')
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