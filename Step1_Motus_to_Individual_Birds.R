################################################################################
## STEP 1: MOTUS → INDIVIDUAL BIRDS
##
## Purpose:
##   This script either:
##
##   (A) loads an example Motus detection dataset from an .RDS file, OR
##   (B) downloads a Motus project database, extracts the alltags table,
##
##   then applies the Motus filter and saves one file per MotusTagID × mfgID
##   dataset.
##
## In plain language:
##   Use this script when you want to turn one large Motus detection dataset into
##   separate filtered files for each tag dataset.
##
## Main output:
##   One folder per tag dataset:
##
##   <MotusTagID>_<mfgID>_<state_label>_<download_id>_MotusFiltered
##
## Each folder contains:
##   1. .RDS file for use in R
##   2. .csv file for viewing outside R
################################################################################
rm(list = ls())

# ==============================================================================
# 0) Project package environment
# ==============================================================================

# Activate project-specific renv library if available
if (file.exists("renv/activate.R")) {
  
  source("renv/activate.R")
  
} else {
  
  warning(
    "renv/activate.R not found.\n",
    "Packages will be loaded from the default R library."
  )
}

# ------------------------------------------------------------------------------
# Restore package versions recorded in renv.lock
# ------------------------------------------------------------------------------

if (requireNamespace("renv", quietly = TRUE)) {
  
  message("📦 Restoring project package environment with renv...")
  
  renv::restore(prompt = FALSE)
  
} else {
  
  warning(
    "Package 'renv' is not installed.\n",
    "Attempting to continue using the default R library."
  )
}

# ------------------------------------------------------------------------------
# Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "motus",
  "DBI",
  "RSQLite",
  "dplyr",
  "readr",
  "lubridate",
  "stringr",
  "here"
)

# ------------------------------------------------------------------------------
# Install any packages still missing
# ------------------------------------------------------------------------------

missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  
  message(
    "📦 Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  
  install.packages(
    missing_packages,
    repos = c(
      "https://steffilazerte.r-universe.dev",
      "https://cloud.r-project.org"
    )
  )
}


# ==============================================================================
# 1) Load packages
# ==============================================================================
invisible(
  lapply(required_packages, library, character.only = TRUE)
)

message("✅ Required packages successfully loaded.")

# ==============================================================================
# 2) User settings
# ==============================================================================

# ------------------------------------------------------------------------------
# RUN MODE
# ------------------------------------------------------------------------------

# Choose ONE option:
#
# "example"        = load the included example .RDS dataset
# "motus_download" = download/update a Motus project database and flatten alltags
# "existing_rds"  = load an already-existing alltags-style .RDS file
#
run_mode <- "example"

valid_run_modes <- c("example", "motus_download", "existing_rds")

if (!run_mode %in% valid_run_modes) {
  stop(
    "run_mode must be one of: ",
    paste(valid_run_modes, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# EXAMPLE DATA SETTINGS
# ------------------------------------------------------------------------------

example_rds <- here(
  "Sample_Data", "Raw", "Raw_Tower", 
  "Example_Allerton_WOTH_052525_060225.RDS"
)

# ------------------------------------------------------------------------------
# EXISTING RDS SETTINGS
# ------------------------------------------------------------------------------

# Use this when you already have a flattened Motus-style alltags .RDS file.
existing_alltags_rds <- here(
  "Sample_Data", "Raw", "Raw_Tower",
  "your_existing_alltags_file.RDS"
)

# ------------------------------------------------------------------------------
# MOTUS PROJECT SETTINGS
# ------------------------------------------------------------------------------

# Only used when run_mode <- "motus_download"
projRecv_id <- 787

# ------------------------------------------------------------------------------
# OUTPUT LABELS
# ------------------------------------------------------------------------------

# Short label used in output file names.
# Examples: "IL", "MX", "NSWO", "Ontario", "Spring2025"
state_label <- "IL"

# Project label used when saving the full flattened alltags file.
project_label <- "IL_WOTH"

# Optional automatic labels for example data
if (run_mode == "example") {
  state_label <- "ExampleAllerton"
  project_label <- "Example_Allerton_June2025"
}

# ------------------------------------------------------------------------------
# DATE LABEL FOR OUTPUT FILES
# ------------------------------------------------------------------------------

download_id <- format(Sys.Date(), "%m%d%y")

# ------------------------------------------------------------------------------
# FOLDER PATHS
# ------------------------------------------------------------------------------

# Folder where the .motus database OR full alltags .RDS file is stored.
motus_database_dir <- here("Sample_Data", "Raw", "Raw_Tower")

# Folder where individual Motus-filtered tag files will be saved.
filtered_indiv_dir <- here("Sample_Data", "Interim", "Motus_Tower_Data_Filtered")

dir.create(motus_database_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(filtered_indiv_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 3) Load Motus data
# ==============================================================================

if (run_mode == "example") {
  
  # ---------------------------------------------------------------------------
  # OPTION A: Load example RDS
  # ---------------------------------------------------------------------------
  
  message("📘 Loading example Motus dataset...")
  
  if (!file.exists(example_rds)) {
    stop("Example RDS file was not found at: ", example_rds)
  }
  
  df_alltags <- readRDS(example_rds)
  
  message("✅ Loaded example dataset: ", example_rds)
  
} else if (run_mode == "existing_rds") {
  
  # ---------------------------------------------------------------------------
  # OPTION B: Load existing alltags-style RDS
  # ---------------------------------------------------------------------------
  
  message("📘 Loading existing alltags-style RDS...")
  
  if (!file.exists(existing_alltags_rds)) {
    stop("Existing alltags RDS file was not found at: ", existing_alltags_rds)
  }
  
  df_alltags <- readRDS(existing_alltags_rds)
  
  message("✅ Loaded existing alltags dataset: ", existing_alltags_rds)
  
} else if (run_mode == "motus_download") {
  
  # ---------------------------------------------------------------------------
  # OPTION C: Download/update Motus database and flatten alltags table
  # ---------------------------------------------------------------------------
  
  message(
    "\n🔐 Motus login may be required.\n",
    "If prompted, enter your Motus username/email and password directly in the R console.\n"
  )
  
  motus_file <- file.path(
    motus_database_dir,
    paste0("project-", projRecv_id, ".motus")
  )
  
  create_new_db <- !file.exists(motus_file)
  
  tryCatch({
    
    tagme(
      projRecv = projRecv_id,
      dir = motus_database_dir,
      new = create_new_db
    )
    
  }, error = function(e) {
    
    stop(
      "\n❌ Unable to access Motus database.\n\n",
      "Possible causes:\n",
      "1. Incorrect Motus login credentials\n",
      "2. No access to this Motus project\n",
      "3. Network/VPN/firewall issues\n",
      "4. Cannot write to download directory\n\n",
      "Original error:\n",
      conditionMessage(e)
    )
  })
  
  if (!file.exists(motus_file)) {
    stop("Motus database was not found at: ", motus_file)
  }
  
  message("📦 Flattening alltags table...")
  
  con <- dbConnect(SQLite(), motus_file)
  
  df_alltags <- tbl(con, "alltags") %>%
    collect()
  
  dbDisconnect(con)
  
  if (nrow(df_alltags) == 0) {
    stop("The alltags table is empty.")
  }
  
  message("✅ Motus database flattened.")
}

# ------------------------------------------------------------------------------
# Standardize key columns after loading data from any source
# ------------------------------------------------------------------------------

df_alltags <- df_alltags %>%
  mutate(
    time = case_when(
      "time" %in% names(.) ~ as_datetime(time),
      "tsCorrected" %in% names(.) ~ as_datetime(tsCorrected),
      "ts" %in% names(.) ~ as_datetime(ts),
      TRUE ~ as_datetime(NA_real_)
    ),
    motusTagID = as.character(motusTagID),
    mfgID = as.character(mfgID)
  )

# ------------------------------------------------------------------------------
# Save alltags-style file for reproducibility
# ------------------------------------------------------------------------------

alltags_rds <- file.path(
  motus_database_dir,
  paste0(project_label, "_alltags_", download_id, ".RDS")
)

saveRDS(df_alltags, alltags_rds)

message("✅ Saved alltags-style file: ", alltags_rds)

# ==============================================================================
# 4) Check required columns
# ==============================================================================

required_cols <- c("motusTagID", "mfgID", "motusFilter")

missing_cols <- setdiff(required_cols, names(df_alltags))

if (length(missing_cols) > 0) {
  stop(
    "The dataset is missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    "\nThis script expects a Motus-style table with motusFilter included."
  )
}

# ==============================================================================
# 5) Apply Motus filter
# ==============================================================================

message("🧹 Applying motusFilter == 1...")

df_filtered <- df_alltags %>%
  filter(motusFilter == 1)

if (nrow(df_filtered) == 0) {
  stop("No detections remained after applying motusFilter == 1.")
}

message("Retained ", nrow(df_filtered), " detections after Motus filtering.")

# ==============================================================================
# 6) Identify unique tag datasets
# ==============================================================================

message("🦅 Identifying unique MotusTagID × mfgID datasets...")

tag_groups <- df_filtered %>%
  distinct(motusTagID, mfgID) %>%
  arrange(motusTagID, mfgID)

message("Found ", nrow(tag_groups), " unique tag datasets.")

# ==============================================================================
# 7) Split and save one filtered file per MotusTagID × mfgID
# ==============================================================================

message("💾 Saving individual Motus-filtered tag files...")

for (i in seq_len(nrow(tag_groups))) {
  
  tag_id <- tag_groups$motusTagID[i]
  mfg_id_original <- tag_groups$mfgID[i]
  
  mfg_id_for_filename <- mfg_id_original
  
  if (is.na(mfg_id_for_filename) || mfg_id_for_filename == "") {
    mfg_id_for_filename <- "unknownMFG"
  }
  
  if (is.na(mfg_id_original)) {
    
    tag_df <- df_filtered %>%
      filter(
        motusTagID == tag_id,
        is.na(mfgID)
      )
    
  } else {
    
    tag_df <- df_filtered %>%
      filter(
        motusTagID == tag_id,
        mfgID == mfg_id_original
      )
  }
  
  if (nrow(tag_df) == 0) next
  
  output_folder <- paste0(
    tag_id, "_",
    mfg_id_for_filename, "_",
    state_label, "_",
    download_id,
    "_MotusFiltered"
  )
  
  output_path <- file.path(filtered_indiv_dir, output_folder)
  
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

  rds_path <- file.path(output_path, paste0(output_folder, ".RDS"))
  csv_path <- file.path(output_path, paste0(output_folder, ".csv"))
  
  saveRDS(tag_df, rds_path)
  
  write_csv(tag_df, csv_path)
  
  message("✅ Saved filtered tag dataset: ", output_folder)
}

# ==============================================================================
# 8) Finish
# ==============================================================================

message("\n🎉 STEP 1 COMPLETE")
message("Output folder: ", filtered_indiv_dir)
