################################################################################
# STEP 1: MOTUS → INDIVIDUAL TAG DATASETS
#
# PURPOSE
# Load or download a Motus alltags-style detection dataset, retain detections
# accepted by the Motus filter, and split the resulting data into separate
# MotusTagID × mfgID datasets.
#
# INPUT OPTIONS
#   1. Included example dataset
#   2. Direct Motus project download
#   3. Existing Motus alltags-style RDS file
#
# OUTPUT
# One folder per MotusTagID × mfgID containing:
#   • .RDS file for use in subsequent pipeline steps
#   • .csv file for inspection outside R
#
# IMPORTANT
# Edit Section 0 ("USER SETTINGS") before running the script.
#
# If run_mode = "motus_download", run this script interactively in R/RStudio.
# Motus authentication may be required before the project database can be
# downloaded or updated.
################################################################################


# ==============================================================================
# 0) USER SETTINGS — EDIT THIS SECTION
# ==============================================================================

# ------------------------------------------------------------------------------
# RUN MODE
# ------------------------------------------------------------------------------

# Choose ONE:
#
# "example"
#   Run the included example dataset.
#   No Motus login or external input file is required.
#
# "motus_download"
#   Download or update a Motus project database directly from Motus.
#   A valid Motus project receiver ID (`projRecv_id`) is required.
#
#   IMPORTANT:
#   Run the script interactively when using this mode.
#   If you are not already authenticated with Motus, you may be prompted
#   to enter your Motus username/email and password in the R console.
#   Do NOT save Motus credentials in this script.
#
# "existing_rds"
#   Load an existing flattened Motus alltags-style .RDS file.
#   The file must contain the original Motus `ts` timestamp column.

run_mode <- "example"


# ------------------------------------------------------------------------------
# MOTUS PROJECT
# ------------------------------------------------------------------------------

# Used only when:
# run_mode <- "motus_download"
#
# Replace with the Motus project receiver ID for the project you want to access.

projRecv_id <- 787


# ------------------------------------------------------------------------------
# EXISTING ALLTAGS RDS
# ------------------------------------------------------------------------------

# Used only when:
# run_mode <- "existing_rds"
#
# Supply the path to an existing flattened Motus alltags-style .RDS file.
# The dataset must contain:
#   ts
#   motusTagID
#   mfgID
#   motusFilter

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
#   "IL"
#   "Ontario"
#   "WOTH"
#   "Spring2025"

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
# 1) PROJECT ENVIRONMENT AND PACKAGES
# ==============================================================================

# Motus timestamps are stored in UTC.
# Keep the R session in UTC until local time is explicitly assigned later
# in the activity-processing workflow.

Sys.setenv(TZ = "UTC")


# ------------------------------------------------------------------------------
# Activate project-specific renv environment
# ------------------------------------------------------------------------------

# The repository includes an renv environment recording the package versions
# used with this workflow.
#
# Package restoration should normally be performed once after cloning or
# downloading the repository:
#
#   renv::restore()
#
# The script activates the project environment when available but does not
# reinstall packages each time it is run.

if (file.exists("renv/activate.R")) {
  
  source("renv/activate.R")
  
} else {
  
  warning(
    "`renv/activate.R` was not found.\n",
    "Packages will be loaded from the default R library."
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
  "here"
)


# Check that all required packages are available.

package_available <- vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

missing_packages <- required_packages[!package_available]


if (length(missing_packages) > 0) {
  
  stop(
    "\nRequired package(s) are not installed:\n  ",
    paste(missing_packages, collapse = ", "),
    "\n\nRun `renv::restore()` from the project root before running this script."
  )
}


# Load packages.

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

message("✅ Required packages successfully loaded.")



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
    paste(valid_run_modes, collapse = ", ")
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
    is.na(projRecv_id)
  ) {
    
    stop(
      "`projRecv_id` must contain one valid Motus project receiver ID ",
      "when run_mode = \"motus_download\"."
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
      "The existing alltags-style RDS file was not found:\n",
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
  
  message("📘 Loading included example Motus dataset...")
  
  if (!file.exists(example_rds)) {
    
    stop(
      "The included example RDS file was not found:\n",
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
      
      tagme(
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
      "The Motus database was not found after download/update:\n",
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
#   ts          = Motus detection timestamp
#   motusTagID  = Motus tag identifier
#   mfgID       = manufacturer identifier
#   motusFilter = Motus false-positive filter

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
# `ts` remains unchanged in the dataset.
#
# A POSIXct `time` column is created for downstream processing. Local time is
# NOT assigned here; local diel timing is calculated later using receiver
# location information.

df_alltags <- df_alltags %>%
  mutate(
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


# Warn if any timestamps could not be converted.

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
  filter(
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
  distinct(
    motusTagID,
    mfgID
  ) %>%
  arrange(
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