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
# 1) Libraries
# ==============================================================================

library(motus)

library(DBI)
library(RSQLite)

library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(here)

# ==============================================================================
# 2) User settings
# ==============================================================================

# ------------------------------------------------------------------------------
# RUN MODE
# ------------------------------------------------------------------------------

# Set to TRUE to use an example .RDS dataset.
# Set to FALSE to download and process a full Motus project.
use_example_data <- TRUE

# Path to example dataset.
# This example should be a Motus-style detection table that includes motusFilter.
example_rds <- here(
  "Sample_Data", "Raw", "Raw_Tower", 
  "Example_Allerton_WOTH_052525_060225.RDS"
)

# ------------------------------------------------------------------------------
# MOTUS PROJECT SETTINGS
# ------------------------------------------------------------------------------

# Motus project/receiver ID to download.
# Only used when use_example_data <- FALSE.
projRecv_id <- 787

# Short label used in output file names.
# Examples: "IL", "MX", "MO", "Ontario", "Spring2025"
state_label <- "IL"

# Project label used when saving the full flattened alltags file.
project_label <- "IL_WOTH"

# If running the example dataset, use example-specific output labels.
if (use_example_data) {
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

# Folder where the downloaded .motus database will be stored.
# Also where the full flattened alltags .RDS file will be saved.
motus_database_dir <- here("Sample_Data", "Raw", "Raw_Tower")

# Folder where individual Motus-filtered tag files will be saved.
filtered_indiv_dir <- here("Sample_Data","Interim", "Motus_Tower_Data_Filtered")

# Optional: if using example data, save to a separate example output folder.
if (use_example_data) {
  filtered_indiv_dir <- here("Sample_Data", "Interim", "Motus_Tower_Data_Filtered")
}

dir.create(motus_database_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(filtered_indiv_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# MOTUS LOGIN / DOWNLOAD CHECK
# ---------------------------------------------------------------------------

if (!use_example_data) {
  
  message(
    "\n🔐 Motus login required.\n",
    "If prompted, enter your Motus username/email and password directly in the R console.\n"
  )
  
  # Simple connection/download-location test before full download.
  tryCatch({
    
    tagme(
      projRecv = projRecv_id,
      dir = motus_database_dir,
      new = FALSE
    )
    
  }, error = function(e) {
    
    stop(
      "\n❌ Motus download check failed.\n\n",
      "Common causes:\n",
      "1. Incorrect Motus login credentials\n",
      "2. Interrupted login prompt\n",
      "3. No permission to access this Motus project\n",
      "4. The download folder cannot be written to\n",
      "5. Network/VPN/firewall issues\n\n",
      "Original error:\n",
      conditionMessage(e)
    )
    
  })
}

# ==============================================================================
# 3) Load or download Motus data
# ==============================================================================

if (use_example_data) {
  
  # ---------------------------------------------------------------------------
  # OPTION A: Load example RDS
  # ---------------------------------------------------------------------------
  
  message("📘 Loading example Motus dataset...")
  
  if (!file.exists(example_rds)) {
    stop("Example RDS file was not found at: ", example_rds)
  }
  
  df_alltags <- readRDS(example_rds) %>%
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
  
  message("✅ Loaded example dataset: ", example_rds)
  
  # Save a copy of the example alltags-style file for reproducibility.
  alltags_rds <- file.path(
    filtered_indiv_dir,
    paste0(project_label, "_alltags_", download_id, ".RDS")
  )
  
  saveRDS(df_alltags, alltags_rds)
  
  message("✅ Saved example alltags-style file: ", alltags_rds)
  
} else {
  
  # ---------------------------------------------------------------------------
  # OPTION B: Download Motus database
  # ---------------------------------------------------------------------------
  
  message("⬇️ Downloading Motus database...")
  
  tagme(
    projRecv = projRecv_id,
    dir = motus_database_dir,
    new = TRUE
  )
  
  motus_file <- file.path(
    motus_database_dir,
    paste0("project-", projRecv_id, ".motus")
  )
  
  if (!file.exists(motus_file)) {
    stop("Motus database was not found at: ", motus_file)
  }
  
  # ---------------------------------------------------------------------------
  # Flatten .motus database
  # ---------------------------------------------------------------------------
  
  message("📦 Flattening alltags table...")
  
  con <- dbConnect(SQLite(), motus_file)
  
  df_alltags <- tbl(con, "alltags") %>%
    collect() %>%
    mutate(
      time = as_datetime(ts),
      motusTagID = as.character(motusTagID),
      mfgID = as.character(mfgID)
    )
  
  dbDisconnect(con)
  
  if (nrow(df_alltags) == 0) {
    stop("The alltags table is empty.")
  }
  
  alltags_rds <- file.path(
    motus_database_dir,
    paste0(project_label, "_alltags_", download_id, ".RDS")
  )
  
  saveRDS(df_alltags, alltags_rds)
  
  message("✅ Saved flattened alltags file: ", alltags_rds)
}

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
