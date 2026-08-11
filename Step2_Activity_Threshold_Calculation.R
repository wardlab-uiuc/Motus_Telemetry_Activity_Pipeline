################################################################################
# STEP 2: ESTIMATE ACTIVITY THRESHOLDS
#
# PURPOSE
# Estimate proportional signal-change activity thresholds from periods of
# biologically low activity.
#
# For the included Wood Thrush example, nighttime detections are used as the
# inactive calibration period.
#
# WORKFLOW
#   1. Load Motus-filtered tag datasets created by Step 1
#   2. Match each tag dataset to deployment and receiver metadata
#   3. Assign detections to receiver hardware eras
#   4. Retain the strongest detection for each transmission event
#   5. Select the receiver contributing the most qualifying detections
#   6. Identify valid consecutive nighttime detections
#   7. Estimate proportional signal-change thresholds
#   8. Save threshold summaries, processed results, and diagnostic plots
#
# MAIN OUTPUTS
#   • CSV threshold summary table
#   • RDS list containing complete threshold results
#   • Diagnostic threshold histograms
#
# IMPORTANT
# Edit Section 0 ("USER SETTINGS") before running the script.
################################################################################


# ==============================================================================
# 0) USER SETTINGS — EDIT THIS SECTION
# ==============================================================================

# ------------------------------------------------------------------------------
# INPUT DATA
# ------------------------------------------------------------------------------

# Folder containing the Motus-filtered tag folders created by Step 1.

root_dir <- here::here(
  "Sample_Data",
  "Interim",
  "Motus_Tower_Data_Filtered"
)


# Deployment metadata.
#
# Required fields include:
#   motusTagID
#   mfgID
#   Band
#   Year
#   Lat
#   Lon
#   Burst_Interval

bird_metadata_path <- here::here(
  "Sample_Data",
  "Raw",
  "Metadata",
  "WOTH_IL_Metadata.csv"
)


# Receiver metadata.
#
# Required primary-system fields:
#   recvDeployName
#   DongleType_1
#   System1
#
# Optional second-system fields:
#   DongleType_2
#   System2
#   System1End
#
# The optional fields may be blank when receiver hardware did not change.

tower_metadata_path <- here::here(
  "Sample_Data",
  "Raw",
  "Metadata",
  "Tower_Metadata.csv"
)


# ------------------------------------------------------------------------------
# TRANSMITTER METADATA
# ------------------------------------------------------------------------------

# Column in bird_metadata_path containing the programmed transmitter burst
# interval in seconds.

required_duty_cycle_col <- "Burst_Interval"


# ------------------------------------------------------------------------------
# THRESHOLD SETTINGS
# ------------------------------------------------------------------------------

# Allowed timing deviation around the expected transmitter burst interval.
#
# Example:
# A 15-s burst interval with a tolerance of 0.3 s accepts consecutive
# transmission events separated by 14.7–15.3 s.

timing_tolerance <- 0.3


# Maximum timestamp difference used to identify detections belonging to the
# same transmitter burst.
#
# Multiple antennas or nearby receivers may record the same transmission at
# slightly different timestamps.

transmission_event_tolerance <- 0.3


# Minimum number of consecutive nighttime detections required to estimate an
# inactive baseline.

min_consecutive_night_detections <- 15


# Number of standard deviations used to define the lower and upper activity
# threshold limits.

threshold_sd_multiplier <- 2


# ------------------------------------------------------------------------------
# RECEIVER-SPECIFIC SIGNAL QUALITY
# ------------------------------------------------------------------------------

# Minimum signal-to-noise ratio (SNR) used for each receiver type.
#
# These are the default values used in this study. Researchers applying the
# framework to other receiver systems may evaluate alternative cutoffs.

parameter_lookup <- tibble::tribble(
  ~DongleType,   ~SNR_cutoff,
  "FUNcube",               6,
  "RTL",                  10,
  "SigmaEight",           12
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
# Local time is assigned explicitly later from deployment coordinates.

Sys.setenv(TZ = "UTC")


# ------------------------------------------------------------------------------
# Activate renv environment if available
# ------------------------------------------------------------------------------

if (file.exists(here::here("renv", "activate.R"))) {
  
  source(
    here::here(
      "renv",
      "activate.R"
    )
  )
  
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
  "ggplot2",
  "dplyr",
  "lubridate",
  "suncalc",
  "conflicted",
  "tidyr",
  "purrr",
  "readr",
  "stringr",
  "lutz",
  "here",
  "tibble"
)


package_available <- vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

missing_packages <- required_packages[
  !package_available
]


if (length(missing_packages) > 0) {
  
  stop(
    "\nRequired package(s) are not installed:\n  ",
    paste(missing_packages, collapse = ", "),
    "\n\nRun `renv::restore()` from the project root before running this script."
  )
}


invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)


conflicted::conflict_prefer(
  "filter",
  "dplyr"
)

conflicted::conflict_prefer(
  "lag",
  "dplyr"
)

conflicted::conflict_prefer(
  "select",
  "dplyr"
)


# ------------------------------------------------------------------------------
# Load shared activity functions
# ------------------------------------------------------------------------------

helper_file <- here::here(
  "Helper_Functions",
  "Activity_Timing_Functions.R"
)


if (!file.exists(helper_file)) {
  
  stop(
    "Required helper-function file was not found:\n",
    helper_file
  )
}


source(
  helper_file
)


message(
  "✅ Required packages and helper functions loaded."
)



# ==============================================================================
# 2) VALIDATE USER SETTINGS AND INPUT FILES
# ==============================================================================

if (!dir.exists(root_dir)) {
  
  stop(
    "Step 1 output directory was not found:\n",
    root_dir
  )
}


if (!file.exists(bird_metadata_path)) {
  
  stop(
    "Deployment metadata file was not found:\n",
    bird_metadata_path
  )
}


if (!file.exists(tower_metadata_path)) {
  
  stop(
    "Receiver metadata file was not found:\n",
    tower_metadata_path
  )
}


if (
  !is.numeric(timing_tolerance) ||
  length(timing_tolerance) != 1 ||
  is.na(timing_tolerance) ||
  timing_tolerance < 0
) {
  
  stop(
    "`timing_tolerance` must be one non-negative number."
  )
}


if (
  !is.numeric(transmission_event_tolerance) ||
  length(transmission_event_tolerance) != 1 ||
  is.na(transmission_event_tolerance) ||
  transmission_event_tolerance < 0
) {
  
  stop(
    "`transmission_event_tolerance` must be one non-negative number."
  )
}


if (
  min_consecutive_night_detections < 2
) {
  
  stop(
    "`min_consecutive_night_detections` must be at least 2."
  )
}



# ==============================================================================
# 3) HELPER FUNCTIONS
# ==============================================================================


# ------------------------------------------------------------------------------
# clean_dongle_type()
#
# Standardize receiver/dongle names used in tower metadata.
# ------------------------------------------------------------------------------

clean_dongle_type <- function(x) {
  
  x <- as.character(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("RTL", x, ignore.case = TRUE) ~ "RTL",
    grepl("FUNcube|Funcube", x, ignore.case = TRUE) ~ "FUNcube",
    grepl("Sigma", x, ignore.case = TRUE) ~ "SigmaEight",
    TRUE ~ x
  )
}



# ------------------------------------------------------------------------------
# standardize_tower_metadata()
#
# Prepare receiver metadata for downstream processing.
#
# In CSV files, optional columns containing only blank/NA values may be read as
# logical instead of character. Converting these fields here prevents failures
# when receivers do not have a second hardware system.
# ------------------------------------------------------------------------------

standardize_tower_metadata <- function(tower_metadata) {
  
  required_primary_cols <- c(
    "recvDeployName",
    "DongleType_1",
    "System1"
  )
  
  missing_primary_cols <- setdiff(
    required_primary_cols,
    names(tower_metadata)
  )
  
  if (length(missing_primary_cols) > 0) {
    
    stop(
      "Tower metadata are missing required column(s): ",
      paste(
        missing_primary_cols,
        collapse = ", "
      )
    )
  }
  
  
  # Optional receiver-era columns.
  # Create them when they are absent entirely.
  
  optional_cols <- c(
    "DongleType_2",
    "System2",
    "System1End"
  )
  
  for (nm in optional_cols) {
    
    if (!nm %in% names(tower_metadata)) {
      
      tower_metadata[[nm]] <- NA_character_
    }
  }
  
  
  # Receiver hardware fields must remain character even when every value is NA.
  
  character_cols <- c(
    "recvDeployName",
    "DongleType_1",
    "DongleType_2",
    "System1",
    "System2",
    "System1End"
  )
  
  
  tower_metadata <- tower_metadata %>%
    mutate(
      across(
        all_of(character_cols),
        as.character
      )
    ) %>%
    mutate(
      across(
        all_of(character_cols),
        ~ na_if(trimws(.x), "")
      )
    )
  
  
  tower_metadata
}



# ------------------------------------------------------------------------------
# get_local_timezone()
#
# Determine the local IANA timezone from deployment coordinates.
# ------------------------------------------------------------------------------

get_local_timezone <- function(lat, lon) {
  
  if (
    length(lat) != 1 ||
    length(lon) != 1 ||
    is.na(lat) ||
    is.na(lon)
  ) {
    
    stop(
      "Valid deployment latitude and longitude are required."
    )
  }
  
  
  tz_local <- lutz::tz_lookup_coords(
    lat = lat,
    lon = lon,
    method = "accurate"
  )
  
  
  if (
    length(tz_local) != 1 ||
    is.na(tz_local) ||
    tz_local == ""
  ) {
    
    stop(
      "Could not determine timezone from coordinates: ",
      "lat = ", lat,
      ", lon = ", lon
    )
  }
  
  
  tz_local
}



# ------------------------------------------------------------------------------
# get_deployment_duty_cycle()
#
# Retrieve the programmed transmitter burst interval from deployment metadata.
# ------------------------------------------------------------------------------

get_deployment_duty_cycle <- function(
    bird_row,
    duty_cycle_col
) {
  
  if (!duty_cycle_col %in% names(bird_row)) {
    
    stop(
      "Missing required burst-interval column in deployment metadata: ",
      duty_cycle_col
    )
  }
  
  
  duty_value <- suppressWarnings(
    as.numeric(
      bird_row[[duty_cycle_col]][1]
    )
  )
  
  
  if (
    is.na(duty_value) ||
    duty_value <= 0
  ) {
    
    band_label <- if (
      "Band" %in% names(bird_row)
    ) {
      bird_row$Band[1]
    } else {
      "unknown"
    }
    
    
    stop(
      "Invalid burst interval for Band ",
      band_label,
      ". Check `",
      duty_cycle_col,
      "` in deployment metadata."
    )
  }
  
  
  duty_value
}



# ------------------------------------------------------------------------------
# build_receiver_eras()
#
# Convert tower metadata to one row per receiver hardware era.
#
# Era 1 begins before the study and ends on System1End when a hardware change
# occurred. Era 2 begins the following day.
# ------------------------------------------------------------------------------

build_receiver_eras <- function(tower_metadata) {
  
  tower_metadata <- standardize_tower_metadata(
    tower_metadata
  )
  
  
  tower_metadata <- tower_metadata %>%
    mutate(
      DongleType_1_clean = clean_dongle_type(
        DongleType_1
      ),
      DongleType_2_clean = clean_dongle_type(
        DongleType_2
      ),
      System1End = as.Date(
        lubridate::parse_date_time(
          System1End,
          orders = c(
            "ymd",
            "mdy",
            "dmy"
          ),
          quiet = TRUE
        )
      )
    )
  
  
  invalid_second_era <- tower_metadata %>%
    filter(
      !is.na(System2),
      is.na(System1End)
    )
  
  
  if (nrow(invalid_second_era) > 0) {
    
    stop(
      "At least one receiver has `System2` but no valid `System1End` date.\n",
      "A hardware-change date is required when a second receiver system is supplied."
    )
  }
  
  
  era_1 <- tower_metadata %>%
    transmute(
      recvDeployName,
      DongleType_clean = DongleType_1_clean,
      System = System1,
      start_date = as.Date("1900-01-01"),
      end_date = dplyr::coalesce(
        System1End,
        as.Date("2100-12-31")
      )
    )
  
  
  era_2 <- tower_metadata %>%
    filter(
      !is.na(System2)
    ) %>%
    transmute(
      recvDeployName,
      DongleType_clean = DongleType_2_clean,
      System = System2,
      start_date = System1End + lubridate::days(1),
      end_date = as.Date("2100-12-31")
    )
  
  
  bind_rows(
    era_1,
    era_2
  ) %>%
    filter(
      !is.na(recvDeployName),
      !is.na(DongleType_clean),
      !is.na(System)
    ) %>%
    mutate(
      tower_type = paste(
        DongleType_clean,
        System,
        sep = "_"
      )
    )
}



# ------------------------------------------------------------------------------
# parse_individual_folders()
#
# Parse Step 1 output folders.
#
# Expected format:
#
# <MotusTagID>_<mfgID>_<dataset_label>_<MMDDYY>_MotusFiltered
#
# dataset_label may contain underscores.
# ------------------------------------------------------------------------------

parse_individual_folders <- function(root_dir) {
  
  folders <- list.dirs(
    root_dir,
    recursive = FALSE,
    full.names = TRUE
  )
  
  
  if (length(folders) == 0) {
    
    return(
      tibble::tibble()
    )
  }
  
  
  folder_info <- tibble::tibble(
    folder = folders,
    folder_name = basename(folders)
  ) %>%
    tidyr::extract(
      folder_name,
      into = c(
        "MotusTagID",
        "mfgID_raw",
        "dataset_label",
        "downloadID"
      ),
      regex = "^([^_]+)_([^_]+)_(.+)_([0-9]{6})_MotusFiltered$",
      remove = FALSE
    ) %>%
    filter(
      !is.na(MotusTagID),
      !is.na(downloadID)
    ) %>%
    mutate(
      MotusTagID = as.character(
        MotusTagID
      ),
      mfgID_raw = as.character(
        mfgID_raw
      ),
      mfgID_base = stringr::str_remove(
        mfgID_raw,
        "\\..*$"
      ),
      download_date = suppressWarnings(
        as.Date(
          downloadID,
          format = "%m%d%y"
        )
      )
    )
  
  
  # Step 1 downloads may be repeated over time.
  # Retain the most recent export for each MotusTagID × mfgID dataset to avoid
  # duplicating detections contained in successive project downloads.
  
  folder_info %>%
    group_by(
      MotusTagID,
      mfgID_base
    ) %>%
    arrange(
      desc(download_date),
      desc(downloadID)
    ) %>%
    slice_head(
      n = 1
    ) %>%
    ungroup()
}



# ------------------------------------------------------------------------------
# load_tag_files()
#
# Load the Step 1 RDS file contained in each selected tag folder.
# ------------------------------------------------------------------------------

load_tag_files <- function(
    tag_folders,
    bird_row
) {
  
  purrr::map_dfr(
    tag_folders,
    function(data_dir) {
      
      candidate_files <- list.files(
        data_dir,
        pattern = "_MotusFiltered\\.RDS$",
        full.names = TRUE
      )
      
      
      if (length(candidate_files) == 0) {
        
        message(
          "  Skipping folder with no MotusFiltered RDS: ",
          data_dir
        )
        
        return(
          NULL
        )
      }
      
      
      if (length(candidate_files) > 1) {
        
        warning(
          "More than one MotusFiltered RDS was found in:\n",
          data_dir,
          "\nUsing the first file."
        )
      }
      
      
      readRDS(
        candidate_files[1]
      ) %>%
        mutate(
          season = bird_row$Year[1]
        )
    }
  )
}


# ------------------------------------------------------------------------------
# get_tower_parameters()
#
# Retrieve the SNR cutoff associated with a receiver hardware configuration.
# ------------------------------------------------------------------------------

get_tower_parameters <- function(
    top_receiver,
    era_tower,
    tower_metadata_long,
    parameter_lookup
) {
  
  tower_metadata_long %>%
    filter(
      recvDeployName == top_receiver,
      tower_type == era_tower
    ) %>%
    slice_head(
      n = 1
    ) %>%
    left_join(
      parameter_lookup,
      by = c(
        "DongleType_clean" = "DongleType"
      )
    )
}



# ------------------------------------------------------------------------------
# calculate_signal_metrics()
#
# Calculate signal changes for valid pairs of consecutive transmission events.
#
# A pair is valid when:
#   • the elapsed time is within the allowed tolerance of the programmed
#     transmitter burst interval
#   • both detections exceed the receiver-specific SNR cutoff
# ------------------------------------------------------------------------------

calculate_signal_metrics <- function(
    data,
    duty_cycle,
    timing_tolerance,
    SNR_cutoff
) {
  
  data %>%
    arrange(
      date_time_local
    ) %>%
    mutate(
      SNR = sig - noise,
      lag_SNR = lag(
        SNR
      ),
      sig_lag = lag(
        sig
      ),
      time_numeric = as.numeric(
        date_time_local
      ),
      lag_time_numeric = lag(
        time_numeric
      ),
      time_dif = time_numeric - lag_time_numeric,
      
      sig_diff = if_else(
        !is.na(time_dif) &
          !is.na(sig_lag) &
          abs(time_dif - duty_cycle) <= timing_tolerance &
          !is.na(SNR) &
          !is.na(lag_SNR) &
          SNR >= SNR_cutoff &
          lag_SNR >= SNR_cutoff,
        sig - sig_lag,
        NA_real_
      ),
      
      sig_ratio = if_else(
        !is.na(sig_diff),
        10^(sig_diff / 10),
        NA_real_
      ),
      
      ln_sig_ratio = if_else(
        !is.na(sig_ratio) &
          sig_ratio > 0,
        log(
          sig_ratio
        ),
        NA_real_
      )
    )
}



# ------------------------------------------------------------------------------
# has_enough_consecutive_night_detections()
#
# Confirm that at least one nighttime sequence contains the required number of
# consecutive detections.
# ------------------------------------------------------------------------------

has_enough_consecutive_night_detections <- function(
    night_baseline_data,
    duty_cycle,
    timing_tolerance,
    min_consecutive_night_detections
) {
  
  if (nrow(night_baseline_data) == 0) {
    
    return(
      FALSE
    )
  }
  
  
  max_gap <- duty_cycle + timing_tolerance
  
  
  run_id <- cumsum(
    c(
      1,
      diff(
        as.numeric(
          night_baseline_data$date_time_local
        )
      ) > max_gap
    )
  )
  
  
  consecutive_runs <- night_baseline_data %>%
    mutate(
      run_id = run_id
    ) %>%
    group_by(
      run_id
    ) %>%
    summarise(
      n = n(),
      .groups = "drop"
    )
  
  
  max(
    consecutive_runs$n,
    na.rm = TRUE
  ) >= min_consecutive_night_detections
}



# ------------------------------------------------------------------------------
# count_night_runs()
#
# Count nighttime sequences meeting the minimum consecutive-detection length.
# Used only for diagnostics.
# ------------------------------------------------------------------------------

count_night_runs <- function(
    df_night,
    duty_cycle,
    timing_tolerance,
    min_run_length
) {
  
  if (nrow(df_night) == 0) {
    
    return(
      0
    )
  }
  
  
  max_gap <- duty_cycle + timing_tolerance
  
  
  run_id <- cumsum(
    c(
      1,
      diff(
        as.numeric(
          df_night$date_time_local
        )
      ) > max_gap
    )
  )
  
  
  df_night %>%
    mutate(
      run_id = run_id
    ) %>%
    group_by(
      run_id
    ) %>%
    summarise(
      n = n(),
      .groups = "drop"
    ) %>%
    summarise(
      n_runs = sum(
        n >= min_run_length,
        na.rm = TRUE
      )
    ) %>%
    pull(
      n_runs
    )
}



# ------------------------------------------------------------------------------
# calculate_thresholds()
#
# Estimate lower and upper proportional signal-change thresholds.
# ------------------------------------------------------------------------------

calculate_thresholds <- function(
    night_baseline_data,
    threshold_sd_multiplier
) {
  
  median_ratio <- median(
    night_baseline_data$sig_ratio,
    na.rm = TRUE
  )
  
  
  sigma_ln <- sd(
    night_baseline_data$ln_sig_ratio,
    na.rm = TRUE
  )
  
  
  if (
    is.na(median_ratio) ||
    is.na(sigma_ln) ||
    sigma_ln == 0
  ) {
    
    return(
      NULL
    )
  }
  
  
  lower_ratio <- median_ratio *
    exp(
      -threshold_sd_multiplier *
        sigma_ln
    )
  
  
  upper_ratio <- median_ratio *
    exp(
      threshold_sd_multiplier *
        sigma_ln
    )
  
  
  list(
    lower_ratio = lower_ratio,
    upper_ratio = upper_ratio,
    lower_db = 10 * log10(
      lower_ratio
    ),
    upper_db = 10 * log10(
      upper_ratio
    ),
    median_ratio = median_ratio,
    sigma_ln = sigma_ln
  )
}



# ------------------------------------------------------------------------------
# make_threshold_summary_table()
#
# Flatten the threshold-results list to one row per tag × receiver era.
# ------------------------------------------------------------------------------

make_threshold_summary_table <- function(
    all_results
) {
  
  purrr::imap_dfr(
    all_results,
    ~ tibble::tibble(
      bird = .x$bird,
      duty_cycle = .x$duty_cycle,
      top_receiver = .x$recvDeployName,
      tower_type = .x$tower_type,
      receiver_era_start_date = as.Date(
        .x$receiver_era_start_date
      ),
      receiver_era_end_date = as.Date(
        .x$receiver_era_end_date
      ),
      
      lower_ratio = .x$thresholds$lower_ratio,
      upper_ratio = .x$thresholds$upper_ratio,
      lower_db = .x$thresholds$lower_db,
      upper_db = .x$thresholds$upper_db,
      median_ratio = .x$thresholds$median_ratio,
      sigma_ln = .x$thresholds$sigma_ln,
      
      pct_within_threshold =
        .x$pct_within_threshold$pct_within,
      
      sample_size =
        .x$sample_size,
      
      pct_active =
        mean(
          .x$data_final$active,
          na.rm = TRUE
        ) * 100
    )
  )
}



# ------------------------------------------------------------------------------
# save_threshold_histograms()
#
# Save individual and combined threshold diagnostic histograms.
# ------------------------------------------------------------------------------

save_threshold_histograms <- function(
    all_results,
    all_signal_data_for_plots,
    output_dir,
    min_consecutive_night_detections
) {
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  combined_df <- purrr::imap_dfr(
    all_signal_data_for_plots,
    ~ .x %>%
      filter(
        is.finite(
          sig_diff
        )
      )
  )
  
  
  threshold_df <- purrr::imap_dfr(
    all_results,
    ~ {
      
      res <- .x
      
      tibble::tibble(
        bird = res$bird,
        top_receiver = res$recvDeployName,
        tower_type = res$tower_type,
        receiver_era_start_date =
          res$receiver_era_start_date,
        receiver_era_end_date =
          res$receiver_era_end_date,
        lower_db =
          res$thresholds$lower_db,
        upper_db =
          res$thresholds$upper_db
      )
    }
  )
  
  
  if (nrow(combined_df) == 0) {
    
    message(
      "⚠️ No finite signal differences were available for threshold histograms."
    )
    
    return(
      invisible(NULL)
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Individual tag × receiver-era histograms
  # ---------------------------------------------------------------------------
  
  for (result_name in names(all_results)) {
    
    res <- all_results[[result_name]]
    
    
    df_plot <- res$data_final %>%
      filter(
        is.finite(
          sig_diff
        )
      )
    
    
    if (nrow(df_plot) == 0) {
      
      next
    }
    
    
    lower_db <- res$thresholds$lower_db
    upper_db <- res$thresholds$upper_db
    
    
    df_night <- df_plot %>%
      filter(
        timing %in% c(
          "night_1",
          "night_2"
        )
      )
    
    
    night_stats <- df_night %>%
      summarise(
        n_total = n(),
        n_within = sum(
          sig_diff >= lower_db &
            sig_diff <= upper_db,
          na.rm = TRUE
        ),
        n_outside = sum(
          sig_diff < lower_db |
            sig_diff > upper_db,
          na.rm = TRUE
        )
      )
    
    
    pct_within_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_within /
        night_stats$n_total *
        100,
      NA_real_
    )
    
    
    pct_outside_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_outside /
        night_stats$n_total *
        100,
      NA_real_
    )
    
    
    runs_ge_min <- count_night_runs(
      df_night = df_night,
      duty_cycle = unique(
        df_plot$duty_cycle
      )[1],
      timing_tolerance = unique(
        df_plot$tolerance
      )[1],
      min_run_length =
        min_consecutive_night_detections
    )
    
    
    subtitle_text <- paste0(
      "Receiver: ",
      res$recvDeployName,
      " | Receiver type: ",
      res$tower_type,
      "\nEra: ",
      res$receiver_era_start_date,
      " to ",
      res$receiver_era_end_date,
      "\nSample size: ",
      nrow(df_plot),
      " | Night within: ",
      round(
        pct_within_night,
        1
      ),
      "% | Night outside: ",
      round(
        pct_outside_night,
        1
      ),
      "% | Night runs ≥",
      min_consecutive_night_detections,
      ": ",
      runs_ge_min,
      "\nLower: ",
      round(
        lower_db,
        2
      ),
      " dB | Upper: ",
      round(
        upper_db,
        2
      ),
      " dB"
    )
    
    
    p_hist <- ggplot(
      df_plot,
      aes(
        x = sig_diff
      )
    ) +
      geom_histogram(
        bins = 60
      ) +
      geom_vline(
        xintercept = lower_db,
        linetype = "dashed",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = upper_db,
        linetype = "dashed",
        linewidth = 1
      ) +
      labs(
        title = paste0(
          "Signal-change distribution — ",
          res$bird
        ),
        subtitle = subtitle_text,
        x = "Signal-strength change (dB)",
        y = "Count"
      ) +
      theme_bw() +
      theme(
        plot.subtitle = element_text(
          size = 9,
          lineheight = 1.1
        )
      )
    
    
    safe_receiver <- stringr::str_replace_all(
      res$recvDeployName,
      "[^A-Za-z0-9]+",
      "_"
    )
    
    
    safe_tower_type <- stringr::str_replace_all(
      res$tower_type,
      "[^A-Za-z0-9]+",
      "_"
    )
    
    
    ggsave(
      filename = paste0(
        res$bird,
        "_",
        safe_receiver,
        "_",
        safe_tower_type,
        "_threshold_histogram.png"
      ),
      plot = p_hist,
      path = output_dir,
      width = 7,
      height = 5
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Combined histograms by receiver × receiver type
  # ---------------------------------------------------------------------------
  
  receiver_groups <- combined_df %>%
    distinct(
      top_receiver,
      tower_type
    )
  
  
  for (
    j in seq_len(
      nrow(
        receiver_groups
      )
    )
  ) {
    
    receiver_name <-
      receiver_groups$top_receiver[j]
    
    tower_type_name <-
      receiver_groups$tower_type[j]
    
    
    df_receiver <- combined_df %>%
      filter(
        top_receiver == receiver_name,
        tower_type == tower_type_name
      )
    
    
    if (nrow(df_receiver) == 0) {
      
      next
    }
    
    
    receiver_thresholds <- threshold_df %>%
      filter(
        top_receiver == receiver_name,
        tower_type == tower_type_name
      )
    
    
    if (nrow(receiver_thresholds) == 0) {
      
      next
    }
    
    
    median_lower <- median(
      receiver_thresholds$lower_db,
      na.rm = TRUE
    )
    
    
    median_upper <- median(
      receiver_thresholds$upper_db,
      na.rm = TRUE
    )
    
    
    df_receiver_night <- df_receiver %>%
      filter(
        timing %in% c(
          "night_1",
          "night_2"
        )
      )
    
    
    night_stats <- df_receiver_night %>%
      summarise(
        n_total = n(),
        n_within = sum(
          sig_diff >= median_lower &
            sig_diff <= median_upper,
          na.rm = TRUE
        ),
        n_outside = sum(
          sig_diff < median_lower |
            sig_diff > median_upper,
          na.rm = TRUE
        )
      )
    
    
    pct_within_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_within /
        night_stats$n_total *
        100,
      NA_real_
    )
    
    
    pct_outside_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_outside /
        night_stats$n_total *
        100,
      NA_real_
    )
    
    
    subtitle_text <- paste0(
      "Receiver: ",
      receiver_name,
      " | Receiver type: ",
      tower_type_name,
      "\nTags included: ",
      dplyr::n_distinct(
        df_receiver$bird
      ),
      " | Total samples: ",
      nrow(
        df_receiver
      ),
      "\nNight within: ",
      round(
        pct_within_night,
        1
      ),
      "% | Night outside: ",
      round(
        pct_outside_night,
        1
      ),
      "%\nMedian lower threshold: ",
      round(
        median_lower,
        2
      ),
      " dB | Median upper threshold: ",
      round(
        median_upper,
        2
      ),
      " dB"
    )
    
    
    p_combined <- ggplot(
      df_receiver,
      aes(
        x = sig_diff
      )
    ) +
      geom_histogram(
        bins = 60
      ) +
      geom_vline(
        xintercept = median_lower,
        linetype = "dashed",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = median_upper,
        linetype = "dashed",
        linewidth = 1
      ) +
      labs(
        title = "Combined signal-change distribution",
        subtitle = subtitle_text,
        x = "Signal-strength change (dB)",
        y = "Count"
      ) +
      theme_bw() +
      theme(
        plot.subtitle = element_text(
          size = 9,
          lineheight = 1.1
        )
      )
    
    
    safe_receiver <- stringr::str_replace_all(
      receiver_name,
      "[^A-Za-z0-9]+",
      "_"
    )
    
    
    safe_tower_type <- stringr::str_replace_all(
      tower_type_name,
      "[^A-Za-z0-9]+",
      "_"
    )
    
    
    ggsave(
      filename = paste0(
        "Combined_",
        safe_receiver,
        "_",
        safe_tower_type,
        "_threshold_histogram.png"
      ),
      plot = p_combined,
      path = output_dir,
      width = 7,
      height = 5
    )
  }
  
  
  message(
    "✅ Threshold histograms saved to:\n  ",
    output_dir
  )
}



# ==============================================================================
# 4) LOAD AND VALIDATE METADATA
# ==============================================================================

message(
  "📘 Loading deployment and receiver metadata..."
)


Bird_metadata <- readr::read_csv(
  bird_metadata_path,
  show_col_types = FALSE
)


Tower_metadata <- readr::read_csv(
  tower_metadata_path,
  locale = readr::locale(
    encoding = "latin1"
  ),
  show_col_types = FALSE
)


# ------------------------------------------------------------------------------
# Validate deployment metadata
# ------------------------------------------------------------------------------

required_bird_cols <- c(
  "motusTagID",
  "mfgID",
  "Lat",
  "Lon",
  required_duty_cycle_col
)


missing_bird_cols <- setdiff(
  required_bird_cols,
  names(Bird_metadata)
)


if (length(missing_bird_cols) > 0) {
  
  stop(
    "Deployment metadata are missing required column(s): ",
    paste(
      missing_bird_cols,
      collapse = ", "
    )
  )
}


Bird_metadata <- Bird_metadata %>%
  mutate(
    motusTagID = as.character(
      motusTagID
    ),
    mfgID = as.character(
      mfgID
    ),
    mfgID_base = stringr::str_remove(
      mfgID,
      "\\..*$"
    )
  )


# ------------------------------------------------------------------------------
# Standardize receiver metadata and construct hardware eras
# ------------------------------------------------------------------------------

Tower_metadata <- standardize_tower_metadata(
  Tower_metadata
)


Tower_metadata_long <- build_receiver_eras(
  Tower_metadata
)


message(
  "✅ Metadata loaded and receiver eras prepared."
)



# ==============================================================================
# 5) IDENTIFY STEP 1 TAG DATASETS
# ==============================================================================

folder_info <- parse_individual_folders(
  root_dir
)


if (nrow(folder_info) == 0) {
  
  stop(
    "No Step 1 MotusFiltered folders were found in:\n",
    root_dir,
    "\n\nExpected folder format:\n",
    "<MotusTagID>_<mfgID>_<dataset_label>_<MMDDYY>_MotusFiltered"
  )
}


birds <- folder_info %>%
  distinct(
    MotusTagID,
    mfgID_base
  )


message(
  "✅ Found ",
  nrow(birds),
  " unique MotusTagID × mfgID datasets."
)



# ==============================================================================
# 6) CALCULATE ACTIVITY THRESHOLDS
# ==============================================================================

all_results <- list()

all_signal_data_for_plots <- list()


for (i in seq_len(nrow(birds))) {
  
  current_MotusTagID <-
    birds$MotusTagID[i]
  
  current_mfgID_base <-
    birds$mfgID_base[i]
  
  bird_id <- paste0(
    current_MotusTagID,
    "_",
    current_mfgID_base
  )
  
  
  message(
    "\n➡ Processing tag dataset ",
    bird_id
  )
  
  
  # ---------------------------------------------------------------------------
  # Identify Step 1 folder
  # ---------------------------------------------------------------------------
  
  bird_folders <- folder_info %>%
    filter(
      MotusTagID ==
        current_MotusTagID,
      mfgID_base ==
        current_mfgID_base
    ) %>%
    pull(
      folder
    )
  
  
  if (length(bird_folders) == 0) {
    
    message(
      "  Skipping: no Step 1 folder found."
    )
    
    next
  }
  
  
  # ---------------------------------------------------------------------------
  # Match deployment metadata
  # ---------------------------------------------------------------------------
  
  bird_row <- Bird_metadata %>%
    filter(
      motusTagID ==
        current_MotusTagID,
      mfgID_base ==
        current_mfgID_base
    )
  
  
  if (nrow(bird_row) == 0) {
    
    message(
      "  Skipping: no matching deployment metadata."
    )
    
    next
  }
  
  
  if (nrow(bird_row) > 1) {
    
    message(
      "  ℹ️ Multiple metadata rows matched; using the first row."
    )
  }
  
  
  bird_row <- bird_row[
    1,
  ]
  
  
  # ---------------------------------------------------------------------------
  # Retrieve transmitter and location information
  # ---------------------------------------------------------------------------
  
  duty_cycle <- get_deployment_duty_cycle(
    bird_row = bird_row,
    duty_cycle_col =
      required_duty_cycle_col
  )
  
  
  lat <- as.numeric(
    bird_row$Lat[1]
  )
  
  lon <- as.numeric(
    bird_row$Lon[1]
  )
  
  
  local_tz <- get_local_timezone(
    lat = lat,
    lon = lon
  )
  
  
  message(
    "  Burst interval: ",
    duty_cycle,
    " s"
  )
  
  message(
    "  Local timezone: ",
    local_tz
  )
  
  
  # ---------------------------------------------------------------------------
  # Load Step 1 detections
  # ---------------------------------------------------------------------------
  
  data_all <- load_tag_files(
    tag_folders = bird_folders,
    bird_row = bird_row
  )
  
  
  if (nrow(data_all) == 0) {
    
    message(
      "  Skipping: no detections loaded."
    )
    
    next
  }
  
  
  # Step 1 retains the original Motus `ts` field and creates `time`.
  # `ts` remains the authoritative UTC timestamp here.
  
  if (!"ts" %in% names(data_all)) {
    
    message(
      "  Skipping: required Motus `ts` column is missing."
    )
    
    next
  }
  
  
  # ---------------------------------------------------------------------------
  # Convert UTC timestamps to local time
  # ---------------------------------------------------------------------------
  
  data_clean <- data_all %>%
    mutate(
      date_time_utc =
        lubridate::as_datetime(
          ts,
          tz = "UTC"
        ),
      
      date_time_local =
        lubridate::with_tz(
          date_time_utc,
          tzone = local_tz
        ),
      
      detection_date =
        as.Date(
          date_time_local
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Assign receiver hardware eras
  # ---------------------------------------------------------------------------
  
  data_clean <- data_clean %>%
    left_join(
      Tower_metadata_long,
      by = "recvDeployName",
      relationship = "many-to-many"
    ) %>%
    filter(
      detection_date >= start_date,
      detection_date <= end_date
    )
  
  
  if (nrow(data_clean) == 0) {
    
    message(
      "  Skipping: no detections matched receiver metadata eras."
    )
    
    next
  }
  
  
  # ---------------------------------------------------------------------------
  # Collapse simultaneous receiver/antenna detections
  # ---------------------------------------------------------------------------
  
  data_clean <- select_strongest_transmission_events(
    data = data_clean,
    event_tolerance =
      transmission_event_tolerance
  )
  
  
  # ---------------------------------------------------------------------------
  # Select the receiver contributing the most retained transmission events
  # ---------------------------------------------------------------------------
  
  top_receiver <- data_clean %>%
    count(
      recvDeployName,
      sort = TRUE
    ) %>%
    slice_head(
      n = 1
    ) %>%
    pull(
      recvDeployName
    )
  
  
  if (
    length(top_receiver) == 0 ||
    is.na(top_receiver)
  ) {
    
    message(
      "  Skipping: no focal receiver could be identified."
    )
    
    next
  }
  
  
  message(
    "  Focal receiver: ",
    top_receiver
  )
  
  
  top_receiver_data <- data_clean %>%
    filter(
      recvDeployName ==
        top_receiver
    )
  
  
  # ---------------------------------------------------------------------------
  # Process each hardware era of the focal receiver separately
  # ---------------------------------------------------------------------------
  
  era_list <- top_receiver_data %>%
    group_by(
      tower_type,
      start_date,
      end_date
    ) %>%
    group_split()
  
  
  for (era_data in era_list) {
    
    era_tower <-
      era_data$tower_type[1]
    
    era_start <-
      era_data$start_date[1]
    
    era_end <-
      era_data$end_date[1]
    
    
    message(
      "  Receiver era: ",
      era_tower,
      " (",
      era_start,
      " to ",
      era_end,
      ")"
    )
    
    
    # -------------------------------------------------------------------------
    # Assign diel periods
    # -------------------------------------------------------------------------
    
    era_data <- info_fast(
      df = era_data,
      lat = lat,
      lon = lon,
      tz_local = local_tz
    )
    
    
    # -------------------------------------------------------------------------
    # Retrieve receiver-specific SNR cutoff
    # -------------------------------------------------------------------------
    
    tower_params <- get_tower_parameters(
      top_receiver =
        top_receiver,
      era_tower =
        era_tower,
      tower_metadata_long =
        Tower_metadata_long,
      parameter_lookup =
        parameter_lookup
    )
    
    
    if (
      nrow(tower_params) == 0 ||
      is.na(
        tower_params$SNR_cutoff[1]
      )
    ) {
      
      message(
        "    Skipping era: receiver-specific SNR parameters are missing."
      )
      
      next
    }
    
    
    SNR_cutoff <-
      tower_params$SNR_cutoff[1]
    
    
    # -------------------------------------------------------------------------
    # Calculate valid signal-change metrics
    # -------------------------------------------------------------------------
    
    era_threshold_data <- calculate_signal_metrics(
      data = era_data,
      duty_cycle = duty_cycle,
      timing_tolerance =
        timing_tolerance,
      SNR_cutoff =
        SNR_cutoff
    ) %>%
      mutate(
        duty_cycle =
          duty_cycle,
        tolerance =
          timing_tolerance,
        S2N_cutoff =
          SNR_cutoff
      )
    
    
    plot_data_name <- paste0(
      bird_id,
      "_",
      era_tower
    )
    
    
    all_signal_data_for_plots[[plot_data_name]] <- era_threshold_data %>%
      mutate(
        bird =
          bird_id,
        top_receiver =
          top_receiver,
        tower_type =
          era_tower,
        receiver_era_start_date =
          era_start,
        receiver_era_end_date =
          era_end
      )
    
    
    # -------------------------------------------------------------------------
    # Extract nighttime inactive baseline
    # -------------------------------------------------------------------------
    
    night_baseline_data <- era_threshold_data %>%
      filter(
        !is.na(
          sig_ratio
        ),
        timing %in% c(
          "night_1",
          "night_2"
        )
      ) %>%
      arrange(
        date_time_local
      )
    
    
    if (nrow(night_baseline_data) == 0) {
      
      message(
        "    Skipping era: no valid nighttime baseline detections."
      )
      
      next
    }
    
    
    # -------------------------------------------------------------------------
    # Require a sufficiently long consecutive nighttime sequence
    # -------------------------------------------------------------------------
    
    enough_night_data <-
      has_enough_consecutive_night_detections(
        night_baseline_data =
          night_baseline_data,
        duty_cycle =
          duty_cycle,
        timing_tolerance =
          timing_tolerance,
        min_consecutive_night_detections =
          min_consecutive_night_detections
      )
    
    
    if (!enough_night_data) {
      
      message(
        "    Skipping era: fewer than ",
        min_consecutive_night_detections,
        " consecutive nighttime detections."
      )
      
      next
    }
    
    
    # -------------------------------------------------------------------------
    # Estimate proportional signal-change thresholds
    # -------------------------------------------------------------------------
    
    thresholds <- calculate_thresholds(
      night_baseline_data =
        night_baseline_data,
      threshold_sd_multiplier =
        threshold_sd_multiplier
    )
    
    
    if (is.null(thresholds)) {
      
      message(
        "    Skipping era: threshold calculation failed."
      )
      
      next
    }
    
    
    # -------------------------------------------------------------------------
    # Apply preliminary activity classification for diagnostics
    #
    # The final deployment-level activity classification occurs in Step 3.
    # -------------------------------------------------------------------------
    
    era_activity_input <- era_threshold_data %>%
      mutate(
        lower_ratio =
          thresholds$lower_ratio,
        upper_ratio =
          thresholds$upper_ratio,
        lower_db =
          thresholds$lower_db,
        upper_db =
          thresholds$upper_db
      ) %>%
      pivot_wider(
        id_cols = c(
          date_time_local,
          timing,
          recvDeployName,
          duty_cycle,
          tolerance,
          S2N_cutoff,
          lower_ratio,
          upper_ratio,
          lower_db,
          upper_db
        ),
        names_from =
          port,
        values_from = c(
          sig,
          noise
        ),
        names_vary =
          "slowest"
      )
    
    
    era_activity_classified <- classify_activity(
      df =
        era_activity_input,
      duty_cycle =
        duty_cycle,
      lower_ratio =
        era_activity_input$lower_ratio,
      upper_ratio =
        era_activity_input$upper_ratio
    )
    
    
    # Preserve tag identifiers in the stored output.
    
    era_threshold_data <-
      era_activity_classified %>%
      mutate(
        MotusTagID =
          current_MotusTagID,
        mfgID_base =
          current_mfgID_base
      )
    
    
    # -------------------------------------------------------------------------
    # Store receiver-era result
    # -------------------------------------------------------------------------
    
    result_name <- paste0(
      bird_id,
      "_",
      era_tower
    )
    
    
    all_results[[result_name]] <- list(
      bird =
        bird_id,
      
      duty_cycle =
        duty_cycle,
      
      recvDeployName =
        top_receiver,
      
      tower_type =
        era_tower,
      
      receiver_era_start_date =
        era_start,
      
      receiver_era_end_date =
        era_end,
      
      threshold_scale =
        "ratio",
      
      data_final =
        era_threshold_data,
      
      thresholds =
        thresholds,
      
      pct_within_threshold =
        era_threshold_data %>%
        summarise(
          n_total =
            sum(
              !is.na(
                sig_ratio
              )
            ),
          
          n_within =
            sum(
              sig_ratio >=
                thresholds$lower_ratio &
                sig_ratio <=
                thresholds$upper_ratio,
              na.rm = TRUE
            ),
          
          pct_within =
            ifelse(
              n_total > 0,
              n_within /
                n_total *
                100,
              NA_real_
            )
        ),
      
      sample_size =
        nrow(
          era_threshold_data
        )
    )
    
    
    message(
      "    ✅ Thresholds calculated: ",
      round(
        thresholds$lower_ratio,
        3
      ),
      "–",
      round(
        thresholds$upper_ratio,
        3
      )
    )
  }
}



# ==============================================================================
# 7) SAVE THRESHOLD RESULTS
# ==============================================================================

if (length(all_results) == 0) {
  
  stop(
    "\nNo activity thresholds were calculated.\n",
    "Check deployment metadata, receiver metadata, nighttime detections, ",
    "burst intervals, and receiver-era definitions."
  )
}


summary_table <- make_threshold_summary_table(
  all_results
)


print(
  summary_table
)


dataset_labels <- paste(
  sort(
    unique(
      folder_info$dataset_label
    )
  ),
  collapse = "_"
)


summary_file <- file.path(
  root_dir,
  paste0(
    "all_birds_thresholds_summary_",
    dataset_labels,
    ".csv"
  )
)


results_rds_file <- file.path(
  root_dir,
  paste0(
    "all_birds_threshold_results_",
    dataset_labels,
    ".RDS"
  )
)


readr::write_csv(
  summary_table,
  summary_file
)


saveRDS(
  all_results,
  results_rds_file
)


message(
  "✅ Threshold summary saved:\n  ",
  summary_file
)


message(
  "✅ Full threshold results saved:\n  ",
  results_rds_file
)



# ==============================================================================
# 8) SAVE DIAGNOSTIC HISTOGRAMS
# ==============================================================================

threshold_plots_dir <- file.path(
  root_dir,
  "threshold_hist_plots"
)


save_threshold_histograms(
  all_results =
    all_results,
  all_signal_data_for_plots =
    all_signal_data_for_plots,
  output_dir =
    threshold_plots_dir,
  min_consecutive_night_detections =
    min_consecutive_night_detections
)



# ==============================================================================
# 9) FINISH
# ==============================================================================

message(
  "\n🎉 STEP 2 COMPLETE"
)


message(
  "Threshold estimates created: ",
  length(
    all_results
  )
)


message(
  "Threshold summary:\n  ",
  summary_file
)


message(
  "Diagnostic plots:\n  ",
  threshold_plots_dir
)