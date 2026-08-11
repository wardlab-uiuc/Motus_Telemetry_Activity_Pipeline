################################################################################
# STEP 1: MOTUS → INDIVIDUAL TAG DATASETS
#
# PURPOSE
# Load or download a Motus alltags-style detection dataset, retain detections
# accepted by the Motus filter, and split the resulting data into separate
# MotusTagID × mfgID datasets.
#
# INPUT OPTIONS
# 1. Included example dataset
# 2. Direct Motus project download
# 3. Existing Motus alltags-style RDS file
#
# OUTPUT
# One folder per MotusTagID × mfgID containing:
# • .RDS file for use in subsequent pipeline steps
# • .csv file for inspection outside R
#
# IMPORTANT
# Step 1 is the entry point for the pipeline. Before processing data, it:
#
# 1. Installs `renv` if needed
# 2. Activates the project-specific package environment
# 3. Restores package versions recorded in `renv.lock`
# 4. Verifies that packages required by Step 1 are available
#
# The first run may take several minutes while packages are installed.
# Later runs should be much faster because `renv` reuses packages that are
# already installed.
#
# After package setup, edit Section 1 ("USER SETTINGS") before running the
# remainder of the script.
#
# If run_mode = "motus_download", run this script interactively in R/RStudio.
# Motus authentication may be required before the project database can be
# downloaded or updated.
################################################################################


# ==============================================================================
# 0) PROJECT ENVIRONMENT AND PACKAGES
# ==============================================================================

# Motus timestamps are stored in UTC.
#
# Keep the R session in UTC until local time is explicitly assigned later
# in the activity-processing workflow.

Sys.setenv(TZ = "UTC")


# ------------------------------------------------------------------------------
# Install renv if needed
# ------------------------------------------------------------------------------

# `renv` manages the reproducible R package environment for this repository.
#
# The `renv.lock` file records the package versions used and tested with the
# pipeline. If `renv` itself is not installed, install it first.

if (!requireNamespace("renv", quietly = TRUE)) {
  
  message(
    "📦 Package 'renv' is not installed. Installing it now..."
  )
  
  install.packages(
    "renv",
    repos = "https://cloud.r-project.org"
  )
}


# Confirm that renv was installed successfully.

if (!requireNamespace("renv", quietly = TRUE)) {
  
  stop(
    "\n❌ Package 'renv' could not be installed.\n\n",
    "Install it manually with:\n\n",
    "  install.packages(\"renv\")\n\n",
    "Then restart R and rerun Step 1."
  )
}


# ------------------------------------------------------------------------------
# Activate project-specific renv environment
# ------------------------------------------------------------------------------

# When the repository contains renv/activate.R, activate the project library
# before restoring packages.

if (file.exists("renv/activate.R")) {
  
  source(
    "renv/activate.R"
  )
  
  message(
    "✅ Project renv environment activated."
  )
  
} else {
  
  warning(
    "`renv/activate.R` was not found.\n",
    "The reproducible project library cannot be activated.\n",
    "The script will continue using the current R library."
  )
}


# ------------------------------------------------------------------------------
# Restore package versions recorded in renv.lock
# ------------------------------------------------------------------------------

# Restore the complete package environment recorded for this repository.
#
# `renv::restore()` compares the current project library with `renv.lock`.
# Packages that already match the recorded versions are reused rather than
# reinstalled.
#
# This may take several minutes the first time the repository is run.

if (file.exists("renv.lock")) {
  
  message(
    "📦 Checking and restoring the package environment from renv.lock..."
  )
  
  tryCatch(
    {
      
      renv::restore(
        prompt = FALSE
      )
      
    },
    error = function(e) {
      
      stop(
        "\n❌ The project package environment could not be restored.\n\n",
        "Try running the following manually from the project root:\n\n",
        "  renv::restore()\n\n",
        "Original error:\n  ",
        conditionMessage(e)
      )
    }
  )
  
  message(
    "✅ Project package environment restored."
  )
  
} else {
  
  warning(
    "`renv.lock` was not found.\n",
    "Exact package versions cannot be restored.\n",
    "Required Step 1 packages will be installed from their repositories instead."
  )
}


# ------------------------------------------------------------------------------
# Required Step 1 packages
# ------------------------------------------------------------------------------

# These are the packages directly required by Step 1.
#
# When renv.lock is available, they should already have been restored above.

required_packages <- c(
  "motus",
  "DBI",
  "RSQLite",
  "dplyr",
  "readr",
  "lubridate",
  "here"
)


# ------------------------------------------------------------------------------
# Check required Step 1 packages
# ------------------------------------------------------------------------------

package_available <- vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

missing_packages <- required_packages[
  !package_available
]


# ------------------------------------------------------------------------------
# Fallback installation when renv.lock is unavailable
# ------------------------------------------------------------------------------

# Normally all required packages should be supplied by `renv::restore()`.
#
# If the repository does not contain renv.lock, install missing Step 1 packages
# directly so that the script can still run.

if (
  length(missing_packages) > 0 &&
  !file.exists("renv.lock")
) {
  
  message(
    "📦 Installing missing Step 1 package(s): ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
  
  install.packages(
    missing_packages,
    repos = c(
      "https://steffilazerte.r-universe.dev",
      "https://cloud.r-project.org"
    )
  )
}


# ------------------------------------------------------------------------------
# Confirm required packages are available
# ------------------------------------------------------------------------------

# Check again after renv restoration or fallback installation.

package_available <- vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

still_missing <- required_packages[
  !package_available
]


if (length(still_missing) > 0) {
  
  stop(
    "\n❌ Required Step 1 package(s) are still unavailable:\n  ",
    paste(
      still_missing,
      collapse = ", "
    ),
    "\n\nIf this repository contains renv.lock, run:\n\n",
    "  renv::restore()\n\n",
    "from the project root and rerun Step 1."
  )
}


# ------------------------------------------------------------------------------
# Load required Step 1 packages
# ------------------------------------------------------------------------------

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

message(
  "✅ Required Step 1 packages successfully loaded."
)


# ==============================================================================
# 1) USER SETTINGS — EDIT THIS SECTION
# ==============================================================================

# ------------------------------------------------------------------------------
# RUN MODE
# ------------------------------------------------------------------------------

# Choose ONE:
#
# "example"
# Run the included example dataset.
# No Motus login or external input file is required.
#
# "motus_download"
# Download or update a Motus project database directly from Motus.
# A valid Motus project receiver ID (`projRecv_id`) is required.
#
# IMPORTANT:
# Run the script interactively when using this mode.
#
# If you are not already authenticated with Motus, you may be prompted
# to enter your Motus username/email and password in the R console.
#
# Do NOT save Motus credentials in this script.
#
# "existing_rds"
# Load an existing flattened Motus alltags-style .RDS file.
# The file must contain the original Motus `ts` timestamp column.

run_mode <- "example"


# ------------------------------------------------------------------------------
# MOTUS PROJECT
# ------------------------------------------------------------------------------

# Used only when:
#
# run_mode <- "motus_download"
#
# Replace NA with the Motus project receiver ID for the project you want
# to access.
#
# Example:
#
# projRecv_id <- 787

projRecv_id <- NA_integer_


# ------------------------------------------------------------------------------
# EXISTING ALLTAGS RDS
# ------------------------------------------------------------------------------

# Used only when:
#
# run_mode <- "existing_rds"
#
# Supply the path to an existing flattened Motus alltags-style .RDS file.
#
# The dataset must contain:
#
# ts
# motusTagID
# mfgID
# motusFilter

existing_alltags_rds <- here::here(
  "Sample_Data",
  "Raw",
  "Raw_Tower",
  "your_existing_alltags_file.RDS"
)


# ------------------------------------------------------------------------------
# OUTPUT LABELS
# ------------------------------------------------------------------------------

# Short label added to output folder and file names.
#
# This can represent a location, species, project, or sampling period.
#
# Examples:
#
# "IL"
# "Ontario"
# "WOTH"
# "Spring2025"

dataset_label <- "IL"


# Label used when saving the complete flattened alltags dataset.

project_label <- "IL_WOTH"


# ------------------------------------------------------------------------------
# OUTPUT DIRECTORIES
# ------------------------------------------------------------------------------

# Directory where the downloaded .motus database and/or complete alltags
# dataset will be stored.

motus_database_dir <- here::here(
  "Sample_Data",
  "Raw",
  "Raw_Tower"
)


# Directory where individual MotusTagID × mfgID datasets will be saved.

filtered_indiv_dir <- here::here(
  "Sample_Data",
  "Interim",
  "Motus_Tower_Data_Filtered"
)


# ==============================================================================
# END USER SETTINGS
#
# Users should generally not need to edit anything below this point.
# ==============================================================================


# ==============================================================================
# 2) VALIDATE USER SETTINGS AND DEFINE DERIVED SETTINGS
# ==============================================================================


# ------------------------------------------------------------------------------
# Validate run mode
# ------------------------------------------------------------------------------

valid_run_modes <- c(
  "example",
  "motus_download",
  "existing_rds"
)


if (!run_mode %in% valid_run_modes) {
  
  stop(
    "`run_mode` must be one of: ",
    paste(
      valid_run_modes,
      collapse = ", "
    )
  )
}


# ------------------------------------------------------------------------------
# Define automatic date label
# ------------------------------------------------------------------------------

download_id <- format(
  Sys.Date(),
  "%m%d%y"
)


# ------------------------------------------------------------------------------
# Define included example dataset
# ------------------------------------------------------------------------------

# This path is internal to the repository and normally should not be edited.

example_rds <- here::here(
  "Sample_Data",
  "Raw",
  "Raw_Tower",
  "Example_Allerton_WOTH_052525_060225.RDS"
)


# Use standardized labels when running the included example.

if (run_mode == "example") {
  
  dataset_label <- "ExampleAllerton"
  
  project_label <- "Example_Allerton_June2025"
}


# ------------------------------------------------------------------------------
# Validate mode-specific settings
# ------------------------------------------------------------------------------

if (run_mode == "motus_download") {
  
  if (
    length(projRecv_id) != 1 ||
    is.na(projRecv_id) ||
    !is.numeric(projRecv_id)
  ) {
    
    stop(
      "`projRecv_id` must contain one valid Motus project receiver ID ",
      "when run_mode = \"motus_download\".\n\n",
      "Example:\n",
      "projRecv_id <- 787"
    )
  }
  
  
  message(
    "\n🔐 MOTUS AUTHENTICATION\n",
    "This run mode accesses a Motus project directly.\n",
    "If your current R session is not authenticated, Motus may prompt you ",
    "for your username/email and password in the console.\n",
    "Do not store Motus credentials in this script.\n"
  )
}


if (run_mode == "existing_rds") {
  
  if (!file.exists(existing_alltags_rds)) {
    
    stop(
      "The existing alltags-style RDS file was not found:\n  ",
      existing_alltags_rds
    )
  }
}


# ------------------------------------------------------------------------------
# Create required directories
# ------------------------------------------------------------------------------

dir.create(
  motus_database_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  filtered_indiv_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 3) LOAD MOTUS DATA
# ==============================================================================

if (run_mode == "example") {
  
  
  # ---------------------------------------------------------------------------
  # OPTION A: Included example dataset
  # ---------------------------------------------------------------------------
  
  message(
    "📘 Loading included example Motus dataset..."
  )
  
  
  if (!file.exists(example_rds)) {
    
    stop(
      "The included example RDS file was not found:\n  ",
      example_rds
    )
  }
  
  
  df_alltags <- readRDS(
    example_rds
  )
  
  
  message(
    "✅ Example dataset loaded."
  )
  
  
} else if (run_mode == "existing_rds") {
  
  
  # ---------------------------------------------------------------------------
  # OPTION B: Existing alltags-style RDS
  # ---------------------------------------------------------------------------
  
  message(
    "📘 Loading existing Motus alltags-style RDS..."
  )
  
  
  df_alltags <- readRDS(
    existing_alltags_rds
  )
  
  
  message(
    "✅ Existing alltags dataset loaded."
  )
  
  
} else if (run_mode == "motus_download") {
  
  
  # ---------------------------------------------------------------------------
  # OPTION C: Download/update Motus project
  # ---------------------------------------------------------------------------
  
  message(
    "📡 Downloading or updating Motus project ",
    projRecv_id,
    "..."
  )
  
  
  # Define the local Motus database path.
  
  motus_file <- file.path(
    motus_database_dir,
    paste0(
      "project-",
      projRecv_id,
      ".motus"
    )
  )
  
  
  # If the database does not already exist locally, create a new one.
  # Otherwise, update the existing database.
  
  create_new_db <- !file.exists(
    motus_file
  )
  
  
  tryCatch(
    {
      
      motus::tagme(
        projRecv = projRecv_id,
        dir = motus_database_dir,
        new = create_new_db
      )
      
    },
    error = function(e) {
      
      stop(
        "\n❌ Unable to access the Motus project.\n\n",
        "Possible causes:\n",
        "  1. Motus authentication is required or credentials are incorrect\n",
        "  2. Your Motus account does not have access to project ",
        projRecv_id,
        "\n",
        "  3. Internet, VPN, or firewall restrictions are blocking access\n",
        "  4. R cannot write to the database directory\n\n",
        "Database directory:\n  ",
        motus_database_dir,
        "\n\nOriginal error:\n  ",
        conditionMessage(e)
      )
    }
  )
  
  
  if (!file.exists(motus_file)) {
    
    stop(
      "The Motus database was not found after download/update:\n  ",
      motus_file
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Extract the alltags table
  # ---------------------------------------------------------------------------
  
  message(
    "📦 Reading the alltags table from the Motus database..."
  )
  
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    motus_file
  )
  
  
  df_alltags <- tryCatch(
    {
      
      dplyr::tbl(
        con,
        "alltags"
      ) %>%
        dplyr::collect()
      
    },
    finally = {
      
      DBI::dbDisconnect(
        con
      )
    }
  )
  
  
  if (nrow(df_alltags) == 0) {
    
    stop(
      "The Motus alltags table contains no detections."
    )
  }
  
  
  message(
    "✅ Motus project database loaded."
  )
}


message(
  "Loaded ",
  format(
    nrow(df_alltags),
    big.mark = ","
  ),
  " detections."
)


# ==============================================================================
# 4) VALIDATE AND STANDARDIZE INPUT DATA
# ==============================================================================

# Step 1 requires the following fields from a Motus alltags-style dataset:
#
# ts          = Motus detection timestamp
# motusTagID  = Motus tag identifier
# mfgID       = manufacturer identifier
# motusFilter = Motus false-positive filter

required_cols <- c(
  "ts",
  "motusTagID",
  "mfgID",
  "motusFilter"
)


missing_cols <- setdiff(
  required_cols,
  names(df_alltags)
)


if (length(missing_cols) > 0) {
  
  stop(
    "\nInput data are missing required column(s):\n  ",
    paste(
      missing_cols,
      collapse = ", "
    ),
    "\n\nStep 1 requires a Motus alltags-style dataset containing:\n",
    "  • ts\n",
    "  • motusTagID\n",
    "  • mfgID\n",
    "  • motusFilter"
  )
}


# ------------------------------------------------------------------------------
# Standardize timestamps and tag identifiers
# ------------------------------------------------------------------------------

# Motus stores `ts` as seconds since 1 January 1970 in UTC.
#
# The original numeric `ts` column is retained.
#
# A POSIXct `time` column is created for easier inspection and downstream use.
# Local time is NOT assigned here; local diel timing is calculated later from
# deployment coordinates.

df_alltags <- df_alltags %>%
  dplyr::mutate(
    time = lubridate::as_datetime(
      ts,
      tz = "UTC"
    ),
    motusTagID = as.character(
      motusTagID
    ),
    mfgID = as.character(
      mfgID
    )
  )


n_missing_time <- sum(
  is.na(df_alltags$time)
)


if (n_missing_time > 0) {
  
  warning(
    format(
      n_missing_time,
      big.mark = ","
    ),
    " detection timestamp(s) could not be converted from `ts`."
  )
}


message(
  "✅ Input data validated and timestamps standardized to UTC."
)


# ==============================================================================
# 5) SAVE COMPLETE ALLTAGS DATASET
# ==============================================================================

# Save a reproducible copy of the complete flattened alltags table after
# standardization.
#
# When an existing RDS is supplied, the original file is retained and is not
# duplicated.

if (run_mode != "existing_rds") {
  
  alltags_rds <- file.path(
    motus_database_dir,
    paste0(
      project_label,
      "_alltags_",
      download_id,
      ".RDS"
    )
  )
  
  
  saveRDS(
    df_alltags,
    alltags_rds
  )
  
  
  message(
    "✅ Complete alltags dataset saved:\n  ",
    alltags_rds
  )
  
  
} else {
  
  message(
    "⏭️ Existing alltags RDS retained without creating a duplicate copy."
  )
}


# ==============================================================================
# 6) APPLY MOTUS FILTER
# ==============================================================================

message(
  "🧹 Applying motusFilter == 1..."
)


df_filtered <- df_alltags %>%
  dplyr::filter(
    motusFilter == 1
  )


if (nrow(df_filtered) == 0) {
  
  stop(
    "No detections remained after applying motusFilter == 1."
  )
}


message(
  "✅ Retained ",
  format(
    nrow(df_filtered),
    big.mark = ","
  ),
  " of ",
  format(
    nrow(df_alltags),
    big.mark = ","
  ),
  " detections after Motus filtering."
)


# ==============================================================================
# 7) IDENTIFY UNIQUE TAG DATASETS
# ==============================================================================

message(
  "🦅 Identifying unique MotusTagID × mfgID datasets..."
)


tag_groups <- df_filtered %>%
  dplyr::distinct(
    motusTagID,
    mfgID
  ) %>%
  dplyr::arrange(
    motusTagID,
    mfgID
  )


if (nrow(tag_groups) == 0) {
  
  stop(
    "No MotusTagID × mfgID datasets were identified."
  )
}


message(
  "✅ Found ",
  nrow(tag_groups),
  " unique MotusTagID × mfgID datasets."
)


# ==============================================================================
# 8) SPLIT AND SAVE INDIVIDUAL TAG DATASETS
# ==============================================================================

message(
  "💾 Saving individual Motus-filtered tag datasets..."
)


for (i in seq_len(nrow(tag_groups))) {
  
  
  # ---------------------------------------------------------------------------
  # Identify current tag dataset
  # ---------------------------------------------------------------------------
  
  tag_id <- tag_groups$motusTagID[i]
  
  mfg_id_original <- tag_groups$mfgID[i]
  
  
  # Use a readable placeholder in file names when mfgID is missing.
  
  mfg_id_for_filename <- mfg_id_original
  
  
  if (
    is.na(mfg_id_for_filename) ||
    mfg_id_for_filename == ""
  ) {
    
    mfg_id_for_filename <- "unknownMFG"
  }
  
  
  # ---------------------------------------------------------------------------
  # Subset detections
  # ---------------------------------------------------------------------------
  
  if (
    is.na(mfg_id_original) ||
    mfg_id_original == ""
  ) {
    
    tag_df <- df_filtered %>%
      dplyr::filter(
        motusTagID == tag_id,
        is.na(mfgID) | mfgID == ""
      )
    
  } else {
    
    tag_df <- df_filtered %>%
      dplyr::filter(
        motusTagID == tag_id,
        mfgID == mfg_id_original
      )
  }
  
  
  if (nrow(tag_df) == 0) {
    
    next
  }
  
  
  # ---------------------------------------------------------------------------
  # Define output folder and file names
  # ---------------------------------------------------------------------------
  
  output_folder <- paste0(
    tag_id,
    "_",
    mfg_id_for_filename,
    "_",
    dataset_label,
    "_",
    download_id,
    "_MotusFiltered"
  )
  
  
  output_path <- file.path(
    filtered_indiv_dir,
    output_folder
  )
  
  
  dir.create(
    output_path,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  rds_path <- file.path(
    output_path,
    paste0(
      output_folder,
      ".RDS"
    )
  )
  
  
  csv_path <- file.path(
    output_path,
    paste0(
      output_folder,
      ".csv"
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Save RDS and CSV
  # ---------------------------------------------------------------------------
  
  saveRDS(
    tag_df,
    rds_path
  )
  
  
  readr::write_csv(
    tag_df,
    csv_path
  )
  
  
  message(
    "  ✅ ",
    output_folder,
    " | ",
    format(
      nrow(tag_df),
      big.mark = ","
    ),
    " detections"
  )
}


# ==============================================================================
# 9) FINISH
# ==============================================================================

message(
  "\n🎉 STEP 1 COMPLETE"
)


message(
  "Tag datasets saved: ",
  nrow(tag_groups)
)


message(
  "Output directory:\n  ",
  filtered_indiv_dir
)