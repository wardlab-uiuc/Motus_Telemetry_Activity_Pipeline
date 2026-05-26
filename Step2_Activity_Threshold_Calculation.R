################################################################################
## ADAPTIVE THRESHOLD PIPELINE
## Wood Thrush telemetry
##
## Purpose:
##   Estimate activity thresholds from signal variation during a period of inactivity
##   (for Wood Thrushes, at night).
##
## This script:
##   1. Loads Motus-filtered individual bird/tag files
##   2. Combines all available seasons/downloads per MotusTagID × mfgID
##   3. Assigns receiver hardware eras
##   4. Selects each bird's top receiver
##   5. Estimates thresholds from nighttime detections
##   6. Saves threshold summaries and diagnostic plots
##
## Main output:
##   - CSV threshold summary table, one row per bird × receiver era
##   - RDS list of full per-era processed results
##   - Diagnostic threshold histograms
################################################################################

rm(list = ls())

# ==============================================================================
# 1) Setup
# ==============================================================================

# Load all packages needed for data wrangling, date/time handling, plotting,
# and applying custom functions. The install step is included so the script
# can be run on a new computer, but for long-term reproducibility it is often
# better to manage packages separately.

required_packages <- c(
  "ggplot2", "dplyr", "lubridate", "suncalc", "conflicted", "tidyr", "purrr",
  "readr", "stringr", "lutz", "here", "patchwork", "zoo", "scales", "vroom",
  "tibble", "magrittr"
)

installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(required_packages[!installed])
invisible(lapply(required_packages, library, character.only = TRUE))

# Explicitly resolve common function conflicts so that filter(), lag(), and
# select() always come from dplyr.
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

# Custom function file.
# This includes info_fast(), which assigns detections to diel timing periods, and classify_acivity(), which uses several
# parameters to estimate whether a bird was active between two consecutive detections.
source(here("Helper_Functions","Activity_Timing_Functions.R"))

# ==============================================================================
# 2) User settings
# ==============================================================================
# ---- Telemetry settings ----

# Expected interval between tag detections in seconds.
duty_cycle <- 15

# Allowed timing tolerance around the expected duty cycle, in seconds.
# For example, duty_cycle = 15 and tolerance = 0.3 allows intervals from
# 14.7 to 15.3 seconds.
tolerance <- 0.3

# Minimum number of consecutive nighttime detections required to estimate
# a stable inactive baseline. This prevents thresholds from being estimated
# from very short or fragmented nighttime detection sequences.
min_consecutive_night_detections <- 15

# Number of standard deviations used to define the threshold envelope around
# the inactive baseline. Larger values create wider, more conservative thresholds.
threshold_sd_multiplier <- 2

# ---- Paths ----

# Folder containing individual Motus-filtered bird/tag folders.
root_dir <- here("Sample_Data", "Interim", "Motus_Tower_Data_Filtered")

# Metadata files.
bird_metadata_path <- here("Sample_Data", "Raw", "Metadata", "WOTH_IL_Metadata.csv")
tower_metadata_path <- here("Sample_Data", "Raw", "Metadata", "Tower_Metadata.csv")

# ==============================================================================
# 3) Additional helper functions
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper: clean_dongle_type()
#
# Standardizes dongle names from tower metadata.
# This prevents slightly different labels, such as "RTL-SDR" vs. "RTL",
# from being treated as separate receiver types.
# ------------------------------------------------------------------------------

clean_dongle_type <- function(x) {
  dplyr::case_when(
    grepl("RTL", x, ignore.case = TRUE) ~ "RTL",
    grepl("Funcube", x, ignore.case = TRUE) ~ "Funcube",
    grepl("Sigma", x, ignore.case = TRUE) ~ "SigmaEight",
    TRUE ~ x
  )
}

# ------------------------------------------------------------------------------
# Helper: build_receiver_eras()
#
# Converts tower metadata into a long-format table where each receiver has one
# row per hardware era.
#
# A receiver era is a period when the tower hardware/system was consistent.
# If a tower changed from System1 to System2, the first era ends on System1End
# and the second era starts the next day.
#
# This matters because receiver hardware can affect signal strength, noise,
# and detection behavior. Thresholds should not combine detections across
# different hardware configurations.
# ------------------------------------------------------------------------------

build_receiver_eras <- function(tower_metadata) {
  tower_metadata %>%
    mutate(
      DongleType_1_clean = clean_dongle_type(DongleType_1),
      DongleType_2_clean = clean_dongle_type(DongleType_2),
      System1End = lubridate::ymd(System1End)
      ) %>%
    {
      bind_rows(
        transmute(
          .,
          recvDeployName,
          DongleType_clean = DongleType_1_clean,
          System = System1,
          start_date = as.Date("1900-01-01"),
          end_date = coalesce(System1End, as.Date("2100-12-31"))
        ),
        filter(., !is.na(System2)) %>%
          transmute(
            recvDeployName,
            DongleType_clean = DongleType_2_clean,
            System = System2,
            start_date = System1End + lubridate::days(1),
            end_date = as.Date("2100-12-31")
          )
      )
    } %>%
    mutate(
      tower_type = paste(DongleType_clean, System, sep = "_")
    )
}

# ------------------------------------------------------------------------------
# Helper: parse_individual_folders()
#
# Reads the individual-bird folder names and extracts identifiers from them.
#
# Expected folder naming convention:
#   <MotusTagID>_<mfgID>_<state>_<downloadID>_MotusFiltered
#
# The script keeps the most recent downloadID for each MotusTagID × mfgID_base.
# ------------------------------------------------------------------------------

parse_individual_folders <- function(root_dir) {
  list.dirs(root_dir, recursive = FALSE) %>%
    tibble(folder = .) %>%
    mutate(
      folder_name = basename(folder),
      parts = strsplit(folder_name, "_")
    ) %>%
    filter(lengths(parts) >= 4) %>%
    mutate(
      MotusTagID = as.numeric(map_chr(parts, 1)),
      mfgID_raw = map_chr(parts, 2),
      mfgID_base = stringr::str_remove(mfgID_raw, "\\..*$"),
      state = map_chr(parts, 3),
      downloadID = as.numeric(map_chr(parts, 4))
    ) %>%
    group_by(MotusTagID, mfgID_base) %>%
    slice_max(downloadID, n = 1) %>%
    ungroup()
}

# ------------------------------------------------------------------------------
# Helper: load_bird_files()
#
# Loads all Motus-filtered files for one MotusTagID × mfgID dataset.
# This allows seasons/downloads to be combined before threshold estimation.
#
# Thresholds are estimated across all available detections for the tag dataset,
# rather than separately by season, so the baseline is based on as much data as
# possible.
# ------------------------------------------------------------------------------

load_bird_files <- function(bird_folders, MotusTagID, bird_row) {
  purrr::map_dfr(bird_folders, function(data_dir) {
    parts <- strsplit(basename(data_dir), "_")[[1]]
    
    mfgID_raw <- parts[2]
    state <- parts[3]
    downloadID <- parts[4]
    
    file_name <- paste0(
      MotusTagID, "_", mfgID_raw, "_", state, "_",
      downloadID, "_MotusFiltered.RDS"
    )
    
    file_path <- file.path(data_dir, file_name)
    
    if (!file.exists(file_path)) {
      message("  Skipping missing file: ", file_path)
      return(NULL)
    }
    
    readRDS(file_path) %>%
      mutate(
        season = bird_row$Year[1],
        state = state
      )
  })
}

# ------------------------------------------------------------------------------
# Helper: clean_and_select_strongest_detections()
#
# Collapses detections that occur within a short time window on the same receiver.
# When duplicates occur, the strongest signal is retained.
#
# This reduces the chance that repeated detections from the same tag detection
# are treated as independent observations.
# ------------------------------------------------------------------------------

clean_and_select_strongest_detections <- function(data, tolerance) {
  data %>%
    arrange(recvDeployName, date_time_local) %>%
    group_by(recvDeployName) %>%
    mutate(
      time_diff = as.numeric(
        difftime(date_time_local, lag(date_time_local), units = "secs")
      ),
      group_id = cumsum(is.na(time_diff) | time_diff > tolerance)
    ) %>%
    group_by(recvDeployName, group_id) %>%
    slice_max(sig, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-time_diff, -group_id) %>%
    mutate(
      date_time_local = round_date(
        date_time_local,
        unit = seconds(tolerance)
      )
    )
}

# ------------------------------------------------------------------------------
# Helper: get_tower_parameters()
#
# Retrieves receiver-specific timing tolerance and S2N cutoff based on dongle type.
#
# S2N means signal-to-noise ratio, calculated as sig - noise.
# Low-S2N detections are more likely to reflect poor signal quality, where the
# tag signal is weak or difficult to distinguish from background noise.
# ------------------------------------------------------------------------------

get_tower_parameters <- function(top_receiver, era_tower, tower_metadata_long, parameter_lookup) {
  tower_metadata_long %>%
    filter(
      recvDeployName == top_receiver,
      tower_type == era_tower
    ) %>%
    slice_head(n = 1) %>%
    left_join(parameter_lookup, by = c("DongleType_clean" = "DongleType"))
}

# ------------------------------------------------------------------------------
# Helper: calculate_signal_metrics()
#
# Calculates signal differences only for valid consecutive detections.
#
# A pair of detections is valid when:
#   1. The time gap is close to the expected duty cycle
#   2. Both detections have adequate S2N
#   3. A previous signal value exists
#
# Signal differences are first calculated in dB, then converted to proportional
# signal ratios. Ratio-based change is used because it compares relative change
# between detections and partially reduces distance-related bias.
# ------------------------------------------------------------------------------

calculate_signal_metrics <- function(data, duty_cycle, tolerance, S2N_cutoff) {
  data %>%
    arrange(date_time_local) %>%
    mutate(
      S2N = sig - noise,
      lag_S2N = lag(S2N),
      sig_lag = lag(sig),
      time_dif = as.numeric(
        difftime(date_time_local, lag(date_time_local), units = "secs")
      ),
      
      sig_diff = if_else(
        !is.na(time_dif) &
          !is.na(sig_lag) &
          abs(time_dif - duty_cycle) <= tolerance &
          !is.na(S2N) &
          !is.na(lag_S2N) &
          S2N >= S2N_cutoff &
          lag_S2N >= S2N_cutoff,
        sig - sig_lag,
        NA_real_
      ),
      
      sig_ratio = if_else(
        !is.na(sig_diff),
        10^(sig_diff / 10),
        NA_real_
      ),
      
      ln_sig_ratio = if_else(
        !is.na(sig_ratio) & sig_ratio > 0,
        log(sig_ratio),
        NA_real_
      )
    )
}

# ------------------------------------------------------------------------------
# Helper: has_enough_consecutive_night_detections()
#
# Checks whether nighttime baseline detections include at least one long enough
# consecutive run.
#
# This prevents unstable thresholds from being estimated from sparse nighttime
# detections or isolated fragments.
# ------------------------------------------------------------------------------

has_enough_consecutive_night_detections <- function(
    night_baseline_data,
    duty_cycle,
    tolerance,
    min_consecutive_night_detections
) {
  if (nrow(night_baseline_data) == 0) return(FALSE)
  
  max_gap <- duty_cycle + tolerance
  
  run_id <- cumsum(
    c(1, diff(as.numeric(night_baseline_data$date_time_local)) > max_gap)
  )
  
  consecutive_runs <- night_baseline_data %>%
    mutate(run_id = run_id) %>%
    group_by(run_id) %>%
    summarise(n = n(), .groups = "drop")
  
  max(consecutive_runs$n, na.rm = TRUE) >= min_consecutive_night_detections
}

# ------------------------------------------------------------------------------
# Helper: count_night_runs()
#
# Counts nighttime detection runs that are at least a specified length.
# This is used only for plotting annotations and quality control summaries.
# ------------------------------------------------------------------------------

count_night_runs <- function(df_night, duty_cycle, tolerance, min_run_length = 15) {
  if (nrow(df_night) == 0) return(0)
  
  max_gap <- duty_cycle + tolerance
  
  run_id <- cumsum(c(1, diff(as.numeric(df_night$date_time_local)) > max_gap))
  
  df_night %>%
    mutate(run_id = run_id) %>%
    group_by(run_id) %>%
    summarise(n = n(), .groups = "drop") %>%
    summarise(n_runs = sum(n >= min_run_length, na.rm = TRUE)) %>%
    pull(n_runs)
}

# ------------------------------------------------------------------------------
# Helper: calculate_thresholds()
#
# Estimates lower and upper activity thresholds from nighttime baseline data.
#
# The median signal ratio represents the center of presumed inactive signal
# variation. The median is used instead of the mean because it is less affected
# by occasional noisy detections or brief movements.
#
# The spread is estimated from ln_sig_ratio. Thresholds are then calculated as
# median_ratio × exp(± threshold_sd_multiplier × sigma_ln).
# ------------------------------------------------------------------------------

calculate_thresholds <- function(night_baseline_data, threshold_sd_multiplier) {
  median_ratio <- median(night_baseline_data$sig_ratio, na.rm = TRUE)
  sigma_ln <- sd(night_baseline_data$ln_sig_ratio, na.rm = TRUE)
  
  if (is.na(median_ratio) || is.na(sigma_ln) || sigma_ln == 0) {
    return(NULL)
  }
  
  lower_ratio <- median_ratio * exp(-threshold_sd_multiplier * sigma_ln)
  upper_ratio <- median_ratio * exp( threshold_sd_multiplier * sigma_ln)
  
  list(
    lower_ratio = lower_ratio,
    upper_ratio = upper_ratio,
    lower_db = 10 * log10(lower_ratio),
    upper_db = 10 * log10(upper_ratio),
    median_ratio = median_ratio,
    sigma_ln = sigma_ln
  )
}

# ------------------------------------------------------------------------------
# Helper: classify_threshold_activity()
#
# Applies the proportional thresholds to flag preliminary activity.
#
# A detection is flagged as active only when:
#   1. Its proportional signal change falls outside the threshold envelope, and
#   2. Its signal quality meets the S2N cutoff.
#
# This is a preliminary activity flag used during threshold evaluation.
# The full activity classification in the later script also considers antenna
# switching, receiver switching, and dropout correction.
# ------------------------------------------------------------------------------

classify_threshold_activity <- function(data, thresholds, S2N_cutoff) {
  data %>%
    mutate(
      movement_ratio = if_else(
        !is.na(sig_ratio) &
          (sig_ratio < thresholds$lower_ratio | sig_ratio > thresholds$upper_ratio),
        TRUE,
        FALSE,
        missing = FALSE
      ),
      
      active_logic = if_else(
        !is.na(S2N) & S2N >= S2N_cutoff,
        TRUE,
        FALSE,
        missing = FALSE
      ),
      
      active = movement_ratio & active_logic
    )
}

# ------------------------------------------------------------------------------
# Helper: make_threshold_summary_table()
#
# Converts the nested all_results list into a flat summary table.
# Each row represents one bird × receiver era threshold estimate.
# ------------------------------------------------------------------------------

make_threshold_summary_table <- function(all_results) {
  purrr::imap_dfr(all_results, ~ tibble(
    bird = .x$bird,
    top_receiver = .x$recvDeployName,
    tower_type = .x$tower_type,
    receiver_era_start_date = as.Date(.x$receiver_era_start_date),
    receiver_era_end_date = as.Date(.x$receiver_era_end_date),
    
    lower_ratio = .x$thresholds$lower_ratio,
    upper_ratio = .x$thresholds$upper_ratio,
    lower_db = .x$thresholds$lower_db,
    upper_db = .x$thresholds$upper_db,
    median_ratio = .x$thresholds$median_ratio,
    sigma_ln = .x$thresholds$sigma_ln,
    
    pct_within_threshold = .x$pct_within_threshold$pct_within,
    sample_size = .x$sample_size,
    pct_active = mean(.x$data_final$active, na.rm = TRUE) * 100
  ))
}

# ------------------------------------------------------------------------------
# Helper: save_threshold_histograms()
#
# Saves all threshold histograms to one folder: threshold_hist_plots.
#
# This function automatically uses the receivers and tower types present in the
# threshold results. No tower names or tower types need to be entered manually.
# ------------------------------------------------------------------------------

save_threshold_histograms <- function(
    all_results,
    all_signal_data_for_plots,
    output_dir,
    duty_cycle,
    min_consecutive_night_detections
) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  combined_df <- purrr::imap_dfr(all_signal_data_for_plots, ~ {
    .x %>%
      filter(is.finite(sig_diff))
  })
  
  threshold_df <- purrr::imap_dfr(all_results, ~ {
    res <- .x
    
    tibble(
      bird = res$bird,
      top_receiver = res$recvDeployName,
      tower_type = res$tower_type,
      receiver_era_start_date = res$receiver_era_start_date,
      receiver_era_end_date = res$receiver_era_end_date,
      lower_db = res$thresholds$lower_db,
      upper_db = res$thresholds$upper_db
    )
  })
  
  if (nrow(combined_df) == 0) {
    message("⚠️ No finite sig_diff values available for threshold histograms.")
    return(invisible(NULL))
  }
  
  # ---------------------------------------------------------------------------
  # 1) Individual bird × receiver-era histograms
  # ---------------------------------------------------------------------------
  
  for (result_name in names(all_results)) {
    
    res <- all_results[[result_name]]
    
    df_plot <- res$data_final %>%
      filter(is.finite(sig_diff))
    
    if (nrow(df_plot) == 0) next
    
    lower_db <- res$thresholds$lower_db
    upper_db <- res$thresholds$upper_db
    
    df_night <- df_plot %>%
      filter(timing %in% c("night_1", "night_2"))
    
    night_stats <- df_night %>%
      summarise(
        n_total = n(),
        n_within = sum(sig_diff >= lower_db & sig_diff <= upper_db, na.rm = TRUE),
        n_outside = sum(sig_diff < lower_db | sig_diff > upper_db, na.rm = TRUE)
      )
    
    pct_within_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_within / night_stats$n_total * 100,
      NA_real_
    )
    
    pct_outside_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_outside / night_stats$n_total * 100,
      NA_real_
    )
    
    runs_ge_min <- count_night_runs(
      df_night = df_night,
      duty_cycle = duty_cycle,
      tolerance = unique(df_plot$tolerance)[1],
      min_run_length = min_consecutive_night_detections
    )
    
    subtitle_text <- paste0(
      "Receiver: ", res$recvDeployName, " | Tower type: ", res$tower_type, "\n",
      "Era: ", res$receiver_era_start_date, " to ", res$receiver_era_end_date, "\n",
      "Sample size: ", nrow(df_plot), " | ",
      "Night within: ", round(pct_within_night, 1), "% | ",
      "Night outside: ", round(pct_outside_night, 1), "% | ",
      "Night runs ≥", min_consecutive_night_detections, ": ", runs_ge_min, "\n",
      "Lower: ", round(lower_db, 2), " dB | ",
      "Upper: ", round(upper_db, 2), " dB"
    )
    
    p_hist <- ggplot(df_plot, aes(x = sig_diff)) +
      geom_histogram(bins = 60, fill = "darkseagreen2", color = "black") +
      geom_vline(xintercept = lower_db, linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = upper_db, linetype = "dashed", linewidth = 1) +
      labs(
        title = paste0("sig_diff Histogram — Bird ", res$bird),
        subtitle = subtitle_text,
        x = "sig_diff (dB)",
        y = "Count"
      ) +
      theme_bw() +
      theme(
        plot.subtitle = element_text(size = 9, lineheight = 1.1)
      )
    
    safe_receiver <- stringr::str_replace_all(res$recvDeployName, "[^A-Za-z0-9]+", "_")
    safe_tower_type <- stringr::str_replace_all(res$tower_type, "[^A-Za-z0-9]+", "_")
    
    ggsave(
      filename = paste0(
        res$bird, "_", safe_receiver, "_", safe_tower_type,
        "_threshold_histogram.png"
      ),
      plot = p_hist,
      path = output_dir,
      width = 7,
      height = 5
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2) Combined histograms by receiver × tower type
  # ---------------------------------------------------------------------------
  
  receiver_groups <- combined_df %>%
    distinct(top_receiver, tower_type)
  
  for (j in seq_len(nrow(receiver_groups))) {
    
    receiver_name <- receiver_groups$top_receiver[j]
    tower_type_name <- receiver_groups$tower_type[j]
    
    df_receiver <- combined_df %>%
      filter(
        top_receiver == receiver_name,
        tower_type == tower_type_name
      )
    
    if (nrow(df_receiver) == 0) next
    
    receiver_thresholds <- threshold_df %>%
      filter(
        top_receiver == receiver_name,
        tower_type == tower_type_name
      )
    
    if (nrow(receiver_thresholds) == 0) next
    
    median_lower <- median(receiver_thresholds$lower_db, na.rm = TRUE)
    median_upper <- median(receiver_thresholds$upper_db, na.rm = TRUE)
    
    df_receiver_night <- df_receiver %>%
      filter(timing %in% c("night_1", "night_2"))
    
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
      night_stats$n_within / night_stats$n_total * 100,
      NA_real_
    )
    
    pct_outside_night <- ifelse(
      night_stats$n_total > 0,
      night_stats$n_outside / night_stats$n_total * 100,
      NA_real_
    )
    
    subtitle_text <- paste0(
      "Receiver: ", receiver_name,
      " | Tower type: ", tower_type_name, "\n",
      "Birds included: ", dplyr::n_distinct(df_receiver$bird),
      " | Total samples: ", nrow(df_receiver), "\n",
      "Night within: ", round(pct_within_night, 1), "% | ",
      "Night outside: ", round(pct_outside_night, 1), "%\n",
      "Median lower threshold: ", round(median_lower, 2), " dB | ",
      "Median upper threshold: ", round(median_upper, 2), " dB"
    )
    
    p_combined <- ggplot(df_receiver, aes(x = sig_diff)) +
      geom_histogram(bins = 60, fill = "darkolivegreen", color = "black") +
      geom_vline(xintercept = median_lower, linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = median_upper, linetype = "dashed", linewidth = 1) +
      labs(
        title = "Combined sig_diff Histogram",
        subtitle = subtitle_text,
        x = "sig_diff (dB)",
        y = "Count"
      ) +
      theme_bw() +
      theme(
        plot.subtitle = element_text(size = 9, lineheight = 1.1)
      )
    
    safe_receiver <- stringr::str_replace_all(receiver_name, "[^A-Za-z0-9]+", "_")
    safe_tower_type <- stringr::str_replace_all(tower_type_name, "[^A-Za-z0-9]+", "_")
    
    ggsave(
      filename = paste0(
        "Combined_", safe_receiver, "_", safe_tower_type,
        "_threshold_histogram.png"
      ),
      plot = p_combined,
      path = output_dir,
      width = 7,
      height = 5
    )
  }
  
  message("✅ Threshold histograms saved to: ", output_dir)
}

# ==============================================================================
# 4) Load metadata and prepare receiver information
# ==============================================================================

# Bird metadata links MotusTagID × mfgID to deployment coordinates.
# Coordinates are needed to assign detections to diel periods using info_fast().

Bird_metadata <- readr::read_csv(bird_metadata_path, show_col_types = FALSE)

# Tower metadata identifies receiver hardware and receiver system changes.
# These data are used to assign each detection to the correct receiver era.

Tower_metadata <- readr::read_csv(
  tower_metadata_path,
  locale = readr::locale(encoding = "latin1"),
  show_col_types = FALSE
)

# Receiver-specific quality-control parameters.
# These values define acceptable timing jitter and minimum S2N by dongle type.

parameter_lookup <- tibble::tribble(
  ~DongleType,  ~tolerance, ~S2N_cutoff,
  "Funcube",             0.3,          6,
  "RTL",                 0.3,         10,
  "SigmaEight",          0.3,         12
)

Tower_metadata_long <- build_receiver_eras(Tower_metadata)

# ==============================================================================
# 5) Parse individual bird/tag folders
# ==============================================================================

# Folder names are used to identify MotusTagID, mfgID, state, and download date.
# The most recent download is retained for each MotusTagID × mfgID_base.

folder_info <- parse_individual_folders(root_dir)

birds <- folder_info %>%
  distinct(MotusTagID, mfgID_base)

if (nrow(birds) == 0) {
  stop("❌ No individual bird folders found in root_dir: ", root_dir)
}

message("Found ", nrow(birds), " MotusTagID × mfgID datasets.")

# ==============================================================================
# 6) Calculate thresholds
# ==============================================================================

all_results <- list()
all_signal_data_for_plots <- list()

for (i in seq_len(nrow(birds))) {
  
  MotusTagID <- birds$MotusTagID[i]
  mfgID_base <- birds$mfgID_base[i]
  bird_id <- paste0(MotusTagID, "_", mfgID_base)
  
  message("\n➡ Processing bird ", bird_id)
  
  # ---------------------------------------------------------------------------
  # Identify all folders for this bird/tag dataset.
  # ---------------------------------------------------------------------------
  
  bird_folders <- folder_info %>%
    filter(
      MotusTagID == !!MotusTagID,
      mfgID_base == !!mfgID_base
    ) %>%
    pull(folder)
  
  if (length(bird_folders) == 0) {
    message("  Skipping: no folders found.")
    next
  }
  
  mfgID_raw_vals <- unique(folder_info$mfgID_raw[
    folder_info$MotusTagID == MotusTagID &
      folder_info$mfgID_base == mfgID_base
  ])
  
  if (length(mfgID_raw_vals) > 1) {
    message("  ℹ️ Bird spans multiple raw Motus mfgIDs: ",
            paste(mfgID_raw_vals, collapse = ", "))
  }
  
  # ---------------------------------------------------------------------------
  # Match this tag dataset to bird metadata.
  # ---------------------------------------------------------------------------
  
  bird_row <- Bird_metadata %>%
    mutate(mfgID_base = as.character(mfgID)) %>%
    filter(
      motusTagID == !!MotusTagID,
      mfgID_base == !!mfgID_base
    )
  
  if (nrow(bird_row) == 0) {
    message("  Skipping: no matching bird metadata.")
    next
  }
  
  bird_row <- bird_row[1, ]
  
  lat <- bird_row$Lat
  lon <- bird_row$Lon
  local_tz <- "America/Chicago"
  
  message("  Metadata coordinates: lat = ", lat, ", lon = ", lon)
  message("  Local timezone: ", local_tz)
  
  # ---------------------------------------------------------------------------
  # Load and combine all available files for this bird/tag dataset.
  # ---------------------------------------------------------------------------
  
  data_all <- load_bird_files(
    bird_folders = bird_folders,
    MotusTagID = MotusTagID,
    bird_row = bird_row
  )
  
  if (nrow(data_all) == 0) {
    message("  Skipping: no data loaded.")
    next
  }
  
  # ---------------------------------------------------------------------------
  # Convert timestamps and assign receiver eras.
  # ---------------------------------------------------------------------------
  
  data_clean <- data_all %>%
    mutate(
      ts_utc = as.POSIXct(tsCorrected, origin = "1970-01-01", tz = "UTC"),
      date_time_local = with_tz(ts_utc, tzone = local_tz),
      detection_date = as.Date(date_time_local)
    ) %>%
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
    message("  Skipping: no detections matched receiver eras.")
    next
  }
  
  # ---------------------------------------------------------------------------
  # Deduplicate detections and retain strongest signal within short windows.
  # ---------------------------------------------------------------------------
  
  data_clean <- clean_and_select_strongest_detections(
    data = data_clean,
    tolerance = tolerance
  )
  
  # ---------------------------------------------------------------------------
  # Identify top receiver.
  #
  # Thresholds are estimated from the receiver with the most detections because
  # it is likely to provide the most stable and complete signal distribution.
  # ---------------------------------------------------------------------------
  
  top_receiver <- data_clean %>%
    count(recvDeployName, sort = TRUE) %>%
    slice_head(n = 1) %>%
    pull(recvDeployName)
  
  if (length(top_receiver) == 0 || is.na(top_receiver)) {
    message("  Skipping: no top receiver identified.")
    next
  }
  
  message("  Top receiver: ", top_receiver)
  
  top_receiver_data <- data_clean %>%
    filter(recvDeployName == top_receiver)
  
  # Split the top receiver detections by hardware era.
  era_list <- top_receiver_data %>%
    group_by(tower_type, start_date, end_date) %>%
    group_split()
  
  for (era_data in era_list) {
    
    era_tower <- era_data$tower_type[1]
    era_start <- era_data$start_date[1]
    era_end <- era_data$end_date[1]
    
    message("  Era: ", era_tower, " (", era_start, " to ", era_end, ")")
    
    # -------------------------------------------------------------------------
    # Assign detections to diel periods.
    #
    # Nighttime detections are used as the inactive baseline for threshold
    # estimation because Wood Thrushes are expected to be relatively inactive
    # at night outside of unusual movement events.
    # -------------------------------------------------------------------------
    
    era_data <- info_fast(
      df = era_data,
      lat = lat,
      lon = lon,
      tz_local = local_tz
    )
    
    # -------------------------------------------------------------------------
    # Retrieve receiver-specific timing and signal-quality parameters.
    # -------------------------------------------------------------------------
    
    tower_params <- get_tower_parameters(
      top_receiver = top_receiver,
      era_tower = era_tower,
      tower_metadata_long = Tower_metadata_long,
      parameter_lookup = parameter_lookup
    )
    
    if (
      nrow(tower_params) == 0 ||
      is.na(tower_params$tolerance[1]) ||
      is.na(tower_params$S2N_cutoff[1])
    ) {
      message("    Skipping era: missing tower parameters.")
      next
    }
    
    tolerance <- tower_params$tolerance[1]
    S2N_cutoff <- tower_params$S2N_cutoff[1]
    
    # -------------------------------------------------------------------------
    # Calculate valid signal differences and proportional signal ratios.
    # -------------------------------------------------------------------------
    
    era_threshold_data <- calculate_signal_metrics(
      data = era_data,
      duty_cycle = duty_cycle,
      tolerance = tolerance,
      S2N_cutoff = S2N_cutoff
    ) %>%
      mutate(
        tolerance = tolerance,
        S2N_cutoff = S2N_cutoff
      )
    
    plot_data_name <- paste0(bird_id, "_", era_tower)
    
    all_signal_data_for_plots[[plot_data_name]] <- era_threshold_data %>%
      mutate(
        bird = bird_id,
        top_receiver = top_receiver,
        tower_type = era_tower,
        receiver_era_start_date = era_start,
        receiver_era_end_date = era_end
      )
    
    # -------------------------------------------------------------------------
    # Extract nighttime baseline detections.
    # -------------------------------------------------------------------------
    
    night_baseline_data <- era_threshold_data %>%
      filter(
        !is.na(sig_ratio),
        timing %in% c("night_1", "night_2")
      ) %>%
      arrange(date_time_local)
    
    if (nrow(night_baseline_data) == 0) {
      message("    Skipping era: no valid nighttime baseline detections.")
      next
    }
    
    # -------------------------------------------------------------------------
    # Require enough consecutive nighttime detections.
    #
    # This prevents estimating thresholds from isolated nighttime detections,
    # which may not represent a stable inactive period.
    # -------------------------------------------------------------------------
    
    enough_night_data <- has_enough_consecutive_night_detections(
      night_baseline_data = night_baseline_data,
      duty_cycle = duty_cycle,
      tolerance = tolerance,
      min_consecutive_night_detections = min_consecutive_night_detections
    )
    
    if (!enough_night_data) {
      message("    Skipping era: fewer than ",
              min_consecutive_night_detections,
              " consecutive nighttime detections.")
      next
    }
    
    # -------------------------------------------------------------------------
    # Estimate thresholds.
    # -------------------------------------------------------------------------
    
    thresholds <- calculate_thresholds(
      night_baseline_data = night_baseline_data,
      threshold_sd_multiplier = threshold_sd_multiplier
    )
    
    if (is.null(thresholds)) {
      message("    Skipping era: threshold calculation failed.")
      next
    }
    
    # -------------------------------------------------------------------------
    # Apply preliminary activity classification for diagnostics.
    #
    # This uses the same classify_activity() logic used in Step 3, but only as a
    # diagnostic check for threshold behavior. It does not replace the final Step 3
    # deployment-level activity classification.
    # -------------------------------------------------------------------------
    
    era_activity_input <- era_threshold_data %>%
      mutate(
        lower_ratio = thresholds$lower_ratio,
        upper_ratio = thresholds$upper_ratio,
        lower_db = thresholds$lower_db,
        upper_db = thresholds$upper_db
      ) %>%
      pivot_wider(
        id_cols = c(
          date_time_local,
          timing,
          recvDeployName,
          tolerance,
          S2N_cutoff,
          lower_ratio,
          upper_ratio,
          lower_db,
          upper_db
        ),
        names_from = port,
        values_from = c(sig, noise),
        names_vary = "slowest"
      )
    
    era_activity_classified <- classify_activity(
      df = era_activity_input,
      duty_cycle = duty_cycle,
      lower_ratio = era_activity_input$lower_ratio,
      upper_ratio = era_activity_input$upper_ratio
    )
    
    era_threshold_data <- era_activity_classified %>%
      mutate(
        MotusTagID = MotusTagID,
        mfgID_raw = paste(mfgID_raw_vals, collapse = ","),
        mfgID_base = mfgID_base
      )
    
    # -------------------------------------------------------------------------
    # Store results for this bird × receiver era.
    # -------------------------------------------------------------------------
    
    result_name <- paste0(bird_id, "_", era_tower)
    
    all_results[[result_name]] <- list(
      bird = bird_id,
      recvDeployName = top_receiver,
      tower_type = era_tower,
      receiver_era_start_date = era_start,
      receiver_era_end_date = era_end,
      threshold_scale = "ratio",
      data_final = era_threshold_data,
      thresholds = thresholds,
      pct_within_threshold = era_threshold_data %>%
        summarise(
          n_total = sum(!is.na(sig_ratio)),
          n_within = sum(
            sig_ratio >= thresholds$lower_ratio &
              sig_ratio <= thresholds$upper_ratio,
            na.rm = TRUE
          ),
          pct_within = ifelse(n_total > 0, n_within / n_total * 100, NA_real_)
        ),
      sample_size = nrow(era_threshold_data)
    )
    
    message("    ✅ Thresholds calculated.")
  }
}

message("\n✅ Adaptive thresholds calculated.")

# ==============================================================================
# 7) Save summary outputs
# ==============================================================================

if (length(all_results) == 0) {
  stop("❌ No thresholds were calculated. Check metadata, nighttime detections, and receiver eras.")
}

summary_table <- make_threshold_summary_table(all_results)

print(summary_table)

state_names <- paste(unique(folder_info$state), collapse = "_")

summary_file <- file.path(
  root_dir,
  paste0("all_birds_thresholds_summary", state_names, ".csv")
)

results_rds_file <- file.path(
  root_dir,
  paste0("all_birds_threshold_results", state_names, ".RDS")
)

readr::write_csv(summary_table, summary_file)
saveRDS(all_results, results_rds_file)

message("✅ Summary table saved to: ", summary_file)
message("✅ Full results list saved to: ", results_rds_file)

# ==============================================================================
# 8) Threshold diagnostic histograms
# ==============================================================================

threshold_plots_dir <- file.path(root_dir, "threshold_hist_plots")

save_threshold_histograms(
  all_results = all_results,
  all_signal_data_for_plots = all_signal_data_for_plots,
  output_dir = threshold_plots_dir,
  duty_cycle = duty_cycle,
  min_consecutive_night_detections = min_consecutive_night_detections
)

message("\n🎉 STEP 2 COMPLETE")