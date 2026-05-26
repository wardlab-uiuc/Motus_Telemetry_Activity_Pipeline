###############################################################################
# FILE: Activity_Classification.R
#
# PURPOSE:
#   Classify biologically meaningful activity from Motus telemetry detections
#   while explicitly handling tag redeployments.
#
# This script processes one MotusTagID × mfgID dataset at a time. For each true
# biological deployment, defined by Band and deployment dates, it:
#
#   1. Loads detection data and metadata
#   2. Resolves redeployments using Band + Date_tagged + Date_end
#   3. Filters detections to the correct deployment window
#   4. Infers multi-receiver site membership
#   5. Cleans and deduplicates detections
#   6. Attaches receiver-type thresholds
#   7. Screens for possible stationary-tag / dropped-tag / mortality patterns
#   8. Classifies activity using proportional signal change, antenna switching,
#      receiver switching, S2N filtering, and dropout correction
#   9. Summarizes activity by hour and day
#  10. Saves classified tables, screening tables, and diagnostic plots
#
# Core biological assumption:
#   MotusTagID + mfgID identify a tag dataset, not necessarily one bird.
#   Redeployments are resolved using Band and deployment dates.
#
# Important note about the stationary-tag screen:
#   The stationary-tag screen is a conservative diagnostic tool only. A flagged
#   deployment should be interpreted as possible mortality, dropped tag,
#   stationary transmitter, or prolonged inactivity, and should always be checked
#   manually with maps, timelines, receiver metadata, and biological context.
###############################################################################

# ==============================================================================
# 1) Packages and environment
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
library(stringr)
library(here)
library(lutz)
library(slider)
library(ggplot2)
library(patchwork)
library(scales)
library(suncalc)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::select)
conflicts_prefer(readr::locale)
conflicts_prefer(dplyr::first)
conflicts_prefer(lubridate::hour)
conflicts_prefer(readr::cols)
conflicts_prefer(readr::col_date)
conflicts_prefer(readr::col_guess)

# ==============================================================================
# 2) Load custom functions
# ==============================================================================

# functions_activity.R should include:
#   - info_fast()
#   - classify_activity()
#
# plot_activity_suite.R should include:
#   - make_plot_id()
#   - make_deployment_subtitle()
#   - make_det_daily()
#   - make_det_daily_timing_by_tower_port()
#   - plot_hourly_activity()
#   - plot_daily_detected_vs_expected()
#   - plot_fraction_expected_tod()
#   - plot_duty_cycle()
#   - plot_signal_difference()
#   - plot_dropouts()
#   - plot_daily_daytime_activity()

source(here("Helper_Functions", "Activity_Timing_Functions.R"))
source(here("Helper_Functions", "Diagnostic_Plots_Functions.R"))

# ==============================================================================
# 3) User settings
# ==============================================================================

# Expected interval between detections.
duty_cycle <- 15

# Minimum fraction of expected detections required for an hourly activity estimate.
# With a 15-second duty cycle, 240 detections are expected per hour.
# A threshold of 0.25 requires at least 60 detections per hour.
sample_size_threshold <- 0.25
min_required_samples <- (3600 / duty_cycle) * sample_size_threshold

# Used for detection-time alignment and deduplication.
time_rounding <- 2

# ----------------------------------------------------------------------------
# Stationary-tag / dropped-tag screening settings
# ----------------------------------------------------------------------------

# Whether to run the possible stationary-tag screen.
# This is a diagnostic screen only and should not be treated as a confirmed
# mortality or dropped-tag classification.
run_stationary_tag_screen <- TRUE

# Number of final daytime hours used to evaluate whether the signal became
# unusually stationary near the end of the deployment.
stationary_late_window_hours <- 72

# Number of final daytime hours used to identify the focal receiver.
# The focal receiver is the receiver where the tag was most concentrated near
# the end of the track.
stationary_receiver_selection_hours <- 24

# Minimum number of valid consecutive signal-difference comparisons required
# during the final window. This prevents sparse late-track detections from being
# flagged too easily.
stationary_min_valid_late <- 30

# Minimum proportion of valid final-window signal differences that must stay
# within the expected inactive threshold range.
# Higher values make the screen stricter.
stationary_min_prop_within <- 0.95

# Maximum mean absolute signal difference allowed during the final window.
# Lower values make the screen stricter and require very little signal change.
stationary_max_mean_abs_sigdif <- 0.50

# Minimum proportion of final-window detections that must occur on the focal
# receiver. This reduces false flags caused by birds still moving among towers.
stationary_min_receiver_prop <- 0.80

# Folder containing one MotusTagID × mfgID dataset.
data_dir <- here(
  "Sample_Data", "Interim", "Motus_Tower_Data_Filtered",
  "84746_160_ExampleAllerton_051826_MotusFiltered"
)

bird_metadata_path <- here(
  "Sample_Data", "Raw", "Metadata", "WOTH_IL_Metadata.csv"
)

tower_metadata_path <- here(
  "Sample_Data", "Raw", "Metadata", "Tower_Metadata.csv"
)

threshold_dir <- here(
  "Sample_Data", "Interim", "Motus_Tower_Data_Filtered")

threshold_file_pattern <- "all_birds_thresholds_summary.*\\.csv$"

# ==============================================================================
# 4) Helper functions
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper: parse_dataset_folder()
#
# Extracts MotusTagID, mfgID, state, and date code from the folder name.
#
# Expected folder format:
#   <MotusTagID>_<mfgID>_<state>_<date_code>_MotusFiltered
# ------------------------------------------------------------------------------

parse_dataset_folder <- function(data_dir) {
  folder_name <- basename(data_dir)
  parts <- strsplit(folder_name, "_")[[1]]
  
  if (length(parts) < 4) {
    stop("❌ Folder name does not match expected format: ", folder_name)
  }
  
  list(
    folder_name = folder_name,
    MotusTagID = parts[1],
    mfgID = parts[2],
    MotusTagID_num = as.numeric(parts[1]),
    mfgID_num = as.numeric(parts[2]),
    state = parts[3],
    date_code = parts[4]
  )
}

# ------------------------------------------------------------------------------
# Helper: clean_dongle_family()
#
# Standardizes dongle type labels. This prevents small differences in metadata
# wording from creating separate receiver categories.
# ------------------------------------------------------------------------------

clean_dongle_family <- function(x) {
  case_when(
    grepl("^Funcube", x, ignore.case = TRUE) ~ "Funcube",
    grepl("^RTL", x, ignore.case = TRUE) ~ "RTL",
    grepl("^Sigma", x, ignore.case = TRUE) ~ "SigmaEight",
    TRUE ~ "Unknown"
  )
}

# ------------------------------------------------------------------------------
# Helper: clean_system_family()
#
# Standardizes receiver system labels. These system families are used with
# dongle family to define tower_type.
# ------------------------------------------------------------------------------

clean_system_family <- function(x) {
  case_when(
    grepl("CTT", x, ignore.case = TRUE) ~ "CTT",
    grepl("Sensorgnome", x, ignore.case = TRUE) ~ "Sensorgnome",
    TRUE ~ "Unknown"
  )
}

# ------------------------------------------------------------------------------
# Helper: build_tower_eras()
#
# Converts tower metadata into one row per receiver era.
#
# A receiver era is a period when a receiver had the same system and dongle type.
# If a receiver changed from System1 to System2, System1End is treated as the
# last date the first receiver type was used.
# ------------------------------------------------------------------------------

build_tower_eras <- function(Tower_metadata) {
  Tower_metadata %>%
    mutate(
      System1End = as.Date(
        parse_date_time(System1End, orders = c("ymd", "mdy"))
      )) %>%
    transmute(
      recvDeployName,
      DongleType = DongleType_1,
      System = System1,
      start_date = as.Date("1900-01-01"),
      end_date = coalesce(System1End, as.Date("2100-12-31"))
    ) %>%
    bind_rows(
      Tower_metadata %>%
        mutate(System1End = as.Date(lubridate::parse_date_time(
          System1End,
          orders = c("ymd", "mdy", "dmy")
        ))) %>%
        filter(!is.na(System2)) %>%
        transmute(
          recvDeployName,
          DongleType = DongleType_2,
          System = System2,
          start_date = System1End + 1,
          end_date = as.Date("2100-12-31")
        )
    ) %>%
    mutate(
      DongleType = if_else(is.na(DongleType), "Unknown", str_trim(DongleType)),
      System = if_else(is.na(System), "Unknown", str_trim(System)),
      DongleType_family = clean_dongle_family(DongleType),
      System_family = clean_system_family(System),
      tower_type = paste(DongleType_family, System_family, sep = "_")
    ) %>%
    distinct()
}

# ------------------------------------------------------------------------------
# Helper: load_threshold_table()
#
# Loads all threshold summary tables created by the threshold calculation script.
# These thresholds are later aggregated to receiver/tower type.
# ------------------------------------------------------------------------------

load_threshold_table <- function(threshold_dir, threshold_file_pattern) {
  threshold_tables <- list.files(
    threshold_dir,
    pattern = threshold_file_pattern,
    full.names = TRUE
  )
  
  if (length(threshold_tables) == 0) {
    stop("❌ No threshold tables found in: ", threshold_dir)
  }
  
  map_dfr(
    threshold_tables,
    ~ read_csv(
      .x,
      col_types = cols(
        receiver_era_start_date = col_date(),
        receiver_era_end_date = col_date(),
        .default = col_guess()
      ),
      show_col_types = FALSE
    )
  )
}

# ------------------------------------------------------------------------------
# Helper: build_tower_type_thresholds()
#
# Aggregates individual bird-era thresholds into receiver-type thresholds.
#
# Logic:
#   1. Calculate median thresholds per tower to reduce influence of outlier birds.
#   2. Calculate mean thresholds across towers within each hardware type.
#
# Ratio thresholds are used for classification.
# dB thresholds are retained for plotting and interpretation.
# ------------------------------------------------------------------------------

build_tower_type_thresholds <- function(threshold_table, tower_long) {
  tower_level_medians <- threshold_table %>%
    rename(tower_type_csv = tower_type) %>%
    left_join(
      tower_long,
      by = join_by(
        top_receiver == recvDeployName,
        receiver_era_start_date <= end_date,
        receiver_era_end_date >= start_date
      ),
      relationship = "many-to-many"
    ) %>%
    mutate(
      tower_type_final = coalesce(tower_type, tower_type_csv)
    ) %>%
    group_by(top_receiver, tower_type_final) %>%
    summarise(
      lower_ratio_median = median(lower_ratio, na.rm = TRUE),
      upper_ratio_median = median(upper_ratio, na.rm = TRUE),
      lower_db_median = median(lower_db, na.rm = TRUE),
      upper_db_median = median(upper_db, na.rm = TRUE),
      .groups = "drop"
    )
  
  tower_level_medians %>%
    group_by(tower_type_final) %>%
    summarise(
      lower_ratio = mean(lower_ratio_median, na.rm = TRUE),
      upper_ratio = mean(upper_ratio_median, na.rm = TRUE),
      lower_db = mean(lower_db_median, na.rm = TRUE),
      upper_db = mean(upper_db_median, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(tower_type = tower_type_final)
}

# ------------------------------------------------------------------------------
# Helper: get_receiver_thresholds()
#
# Matches each detection to the correct receiver era and tower_type threshold.
# This is date-aware, so detections are assigned to the receiver setup that was
# active when the detection occurred.
# ------------------------------------------------------------------------------

get_receiver_thresholds <- function(data, tower_long, tower_type_thresholds) {
  df_recv <- data %>%
    select(recvDeployName, date_time_local) %>%
    distinct() %>%
    mutate(det_date = as.Date(date_time_local))
  
  df_recv %>%
    left_join(
      tower_long,
      by = dplyr::join_by(
        recvDeployName == recvDeployName,
        between(det_date, start_date, end_date)
      ),
      relationship = "many-to-one"
    ) %>%
    left_join(tower_type_thresholds, by = "tower_type") %>%
    select(
      recvDeployName, date_time_local, tower_type,
      lower_ratio, upper_ratio,
      lower_db, upper_db
    )
}

# ------------------------------------------------------------------------------
# Helper: get_bird_deployments()
#
# Resolves true biological deployments for this MotusTagID × mfgID.
#
# This is essential because MotusTagID + mfgID identify a tag dataset, but a tag
# can be redeployed on multiple birds. Band and deployment dates identify the
# actual biological individual/deployment.
# ------------------------------------------------------------------------------

get_bird_deployments <- function(Bird_metadata, MotusTagID_num, mfgID) {
  Bird_metadata %>%
    filter(
      motusTagID == MotusTagID_num,
      as.character(mfgID) == mfgID
    ) %>%
    mutate(
      Date_tagged = as.Date(
        parse_date_time(Date_tagged, orders = c("ymd", "mdy"))
      ),
      
      Date_end = as.Date(
        parse_date_time(Date_end, orders = c("ymd", "mdy"))
      )
    ) %>%
    group_by(Band, Date_tagged) %>%
    summarise(
      Date_end = if (all(is.na(Date_end))) NA_Date_
      else max(Date_end, na.rm = TRUE),
      Lat = first(Lat),
      Lon = first(Lon),
      .groups = "drop"
    ) %>%
    arrange(Date_tagged) %>%
    mutate(deploy_index = row_number())
}

# ------------------------------------------------------------------------------
# Helper: get_deployment_end()
#
# Defines the end of a deployment.
#
# If Date_end exists, that is used. If the tag was redeployed later, the current
# deployment is forced to end before the next deployment starts. This prevents
# detections from one bird being assigned to a later redeployment.
# ------------------------------------------------------------------------------

get_deployment_end <- function(bird_deployments, bird_row, i, tz_local) {
  next_deploy_start <- bird_deployments %>%
    filter(deploy_index == i + 1) %>%
    pull(Date_tagged)
  
  deployment_end <- min(
    c(bird_row$Date_end, next_deploy_start - seconds(1)),
    na.rm = TRUE
  )
  
  if (is.infinite(deployment_end)) {
    return(NA)
  }
  
  deployment_end
}

# ------------------------------------------------------------------------------
# Helper: filter_to_deployment_window()
#
# Keeps only detections that occurred during the current biological deployment.
# ------------------------------------------------------------------------------

filter_to_deployment_window <- function(data_raw, bird_row, deployment_end, tz_local) {
  data_raw %>%
    mutate(
      ts_utc = as.POSIXct(tsCorrected, origin = "1970-01-01", tz = "UTC"),
      date_time_local = with_tz(ts_utc, tz_local)
    ) %>%
    filter(
      date_time_local >= as.POSIXct(bird_row$Date_tagged, tz = tz_local),
      is.na(deployment_end) |
        date_time_local <= as.POSIXct(deployment_end, tz = tz_local)
    )
}

# ------------------------------------------------------------------------------
# Helper: define_multi_receiver_sites()
#
# Defines receiver groups that should be treated as one biological site.
#
# This prevents false movement interpretation when a bird is detected by multiple
# nearby receivers that are part of the same study site.
# ------------------------------------------------------------------------------

define_multi_receiver_sites <- function() {
  tribble(
    ~site_id,    ~recvDeployName,
    "Allerton",  "Allerton",
    "Allerton",  "Allerton South",
    "Kennekuk",  "Kennekuk 1",
    "Kennekuk",  "Kennekuk 2",
    "Kennekuk",  "Kennekuk 3",
    "Kennekuk",  "Kennekuk 4",
    "Kennekuk",  "Kennekuk 5",
    "Kennekuk",  "Kennekuk 6",
    "Kennekuk",  "Kickapoo"
  )
}

# ------------------------------------------------------------------------------
# Helper: filter_to_site_receivers()
#
# Identifies the top receiver and keeps all receivers from the same site group.
# If the top receiver is not part of a defined multi-receiver site, only the top
# receiver is retained.
# ------------------------------------------------------------------------------

filter_to_site_receivers <- function(data, multi_receiver_sites) {
  receiver_counts <- data %>%
    count(recvDeployName, sort = TRUE)
  
  top_recv_name <- receiver_counts$recvDeployName[1]
  
  site_receivers <- multi_receiver_sites %>%
    filter(site_id %in%
             multi_receiver_sites$site_id[
               multi_receiver_sites$recvDeployName == top_recv_name
             ]) %>%
    pull(recvDeployName)
  
  if (length(site_receivers) == 0) {
    site_receivers <- top_recv_name
  }
  
  list(
    data = data %>% filter(recvDeployName %in% site_receivers),
    top_recv_name = top_recv_name,
    site_receivers = site_receivers
  )
}

# ------------------------------------------------------------------------------
# Helper: clean_and_select_strongest_detections()
#
# Aligns detections to the expected duty cycle and keeps the strongest detection.
#
# First, duplicates are collapsed within receiver and duty-time.
# Second, if multiple receivers detected the same expected detection, the strongest
# signal is retained.
# ------------------------------------------------------------------------------

clean_and_select_strongest_detections <- function(data, duty_cycle) {
  data %>%
    mutate(
      time_num = as.numeric(date_time_local),
      duty_align = round(time_num / duty_cycle) * duty_cycle,
      duty_time = as.POSIXct(
        duty_align,
        origin = "1970-01-01",
        tz = attr(date_time_local, "tzone")
      )
    ) %>%
    group_by(duty_time, recvDeployName) %>%
    slice_max(sig, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(duty_time) %>%
    slice_max(sig, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(date_time_local = round_date(date_time_local, "second"))
}

# ------------------------------------------------------------------------------
# Helper: check_missing_thresholds()
#
# Stops the script if any receiver lacks metadata or threshold values.
# This is intentionally strict so incomplete classifications are not produced.
# ------------------------------------------------------------------------------

check_missing_thresholds <- function(receiver_thresholds) {
  missing_receivers <- receiver_thresholds %>%
    filter(
      is.na(lower_ratio) | is.na(upper_ratio) |
        is.na(tower_type)
    ) %>%
    pull(recvDeployName) %>%
    unique()
  
  if (length(missing_receivers) > 0) {
    stop(
      paste0(
        "❌ Missing metadata or thresholds for receiver(s): ",
        paste(missing_receivers, collapse = ", "),
        ".\nNo activity classification performed."
      )
    )
  }
}

# ------------------------------------------------------------------------------
# Helper: attach_receiver_parameters()
#
# Adds tower type, threshold values, dongle family, timing tolerance, and S2N cutoff.
#
# S2N cutoff is used to avoid classifying poor-quality signals as movement.
# ------------------------------------------------------------------------------

attach_receiver_parameters <- function(data_clean, tower_long, tower_type_thresholds, parameter_lookup) {
  
  receiver_thresholds <- get_receiver_thresholds(
    data = data_clean,
    tower_long = tower_long,
    tower_type_thresholds = tower_type_thresholds
  )
  
  data_clean %>%
    left_join(
      receiver_thresholds,
      by = c("recvDeployName", "date_time_local")
    ) %>%
    mutate(
      DongleType_family = stringr::str_extract(tower_type, "^[^_]+")
    ) %>%
    left_join(
      parameter_lookup,
      by = "DongleType_family"
    )
}

# ------------------------------------------------------------------------------
# Helper: summarize_deployment_thresholds()
#
# Creates deployment-level median thresholds for plotting.
# Classification still uses row-level receiver thresholds.
# ------------------------------------------------------------------------------

summarize_deployment_thresholds <- function(data_clean) {
  data_clean %>%
    distinct(recvDeployName, lower_ratio, upper_ratio, lower_db, upper_db) %>%
    summarise(
      lower_ratio = median(lower_ratio, na.rm = TRUE),
      upper_ratio = median(upper_ratio, na.rm = TRUE),
      lower_db = median(lower_db, na.rm = TRUE),
      upper_db = median(upper_db, na.rm = TRUE)
    )
}

# ------------------------------------------------------------------------------
# Helper: screen_stationary_tag_deployment()
#
# Screens one biological deployment for possible stationary-tag behavior near the
# end of the detection record.
#
# This is intended to help identify deployments that may represent:
#   - mortality
#   - dropped tag
#   - stationary transmitter
#   - prolonged inactivity
#   - receiver-specific artifacts that need manual review
#
# Why this screen is separate from activity classification:
#   Activity classification estimates movement at each detection. The stationary
#   screen looks for a sustained end-of-track pattern: detections concentrated on
#   one receiver with very little valid signal change during the final daytime
#   window. That pattern is useful for flagging possible dropped tags, but it is
#   not direct evidence of mortality.
#
# Why daytime detections are used:
#   Nighttime inactivity is expected for diurnal Wood Thrushes, so nighttime-only
#   stillness should not automatically be treated as suspicious. Daytime stillness
#   near the end of a track is more informative.
#
# Why the final receiver is used:
#   A stationary transmitter is usually detected repeatedly by the same receiver
#   near the end of its record. Requiring concentration on one focal receiver
#   reduces false flags for birds still moving among receiver sites.
# ------------------------------------------------------------------------------

screen_stationary_tag_deployment <- function(
    data_clean,
    duty_cycle = 15,
    late_window_hours = 72,
    receiver_selection_hours = 24,
    min_valid_late = 30,
    min_prop_within = 0.95,
    max_mean_abs_sigdif = 0.50,
    min_receiver_prop = 0.80,
    Band,
    MotusTagID,
    mfgID
) {
  
  # Work from cleaned, deduplicated, threshold-attached detections.
  # At this point, data_clean should already contain:
  #   date_time_local, timing, recvDeployName, sig, noise,
  #   lower_db, upper_db, tolerance, S2N_cutoff, and tower_type.
  x <- data_clean %>%
    arrange(date_time_local)
  
  # If there are too few detections overall, do not attempt the screen.
  if (nrow(x) < 20) {
    return(NULL)
  }
  
  # Use only daytime detections to define the final time and evaluate stillness.
  # This avoids flagging normal nighttime roosting behavior.
  x_day <- x %>%
    filter(timing == "day")
  
  if (nrow(x_day) < 10) {
    return(NULL)
  }
  
  # The final daytime detection anchors the late-track screening window.
  final_day_time <- max(x_day$date_time_local, na.rm = TRUE)
  
  # Identify which receiver dominated detections near the end of the deployment.
  # This receiver becomes the focal receiver for the stationary screen.
  receiver_window <- x_day %>%
    filter(date_time_local >= final_day_time - hours(receiver_selection_hours))
  
  if (nrow(receiver_window) == 0) {
    return(NULL)
  }
  
  receiver_counts <- receiver_window %>%
    count(recvDeployName, sort = TRUE)
  
  focal_receiver <- receiver_counts$recvDeployName[1]
  focal_receiver_prop <- receiver_counts$n[1] / sum(receiver_counts$n)
  
  # Restrict to the focal receiver. This asks:
  #   Once the bird/tag was mostly detected at its final receiver, did the signal
  #   become unusually stable?
  x_focal <- x %>%
    filter(recvDeployName == focal_receiver) %>%
    arrange(date_time_local) %>%
    mutate(
      S2N = sig - noise,
      lag_time = lag(date_time_local),
      dt = as.numeric(difftime(date_time_local, lag_time, units = "secs")),
      sig_lag = lag(sig),
      lag_S2N = lag(S2N),
      
      # Calculate dB signal difference only for valid consecutive detections.
      # A comparison is valid only if:
      #   1. the interval is close to the tag duty cycle,
      #   2. the current detection has adequate S2N,
      #   3. the previous detection also had adequate S2N.
      sig_dif = if_else(
        !is.na(dt) &
          abs(dt - duty_cycle) <= tolerance &
          S2N >= S2N_cutoff &
          lag_S2N >= S2N_cutoff,
        sig - sig_lag,
        NA_real_
      )
    )
  
  # Pull the final daytime window and classify each valid signal difference as
  # inside or outside the expected inactive dB threshold range.
  late <- x_focal %>%
    filter(
      timing == "day",
      date_time_local >= final_day_time - hours(late_window_hours)
    ) %>%
    mutate(
      within_stationary_threshold =
        sig_dif >= lower_db &
        sig_dif <= upper_db
    )
  
  n_valid <- sum(!is.na(late$sig_dif))
  
  # If there are no valid signal comparisons, do not return a flag.
  if (n_valid == 0) {
    return(NULL)
  }
  
  # Summary metrics for the final window:
  #   prop_within: proportion of valid signal differences inside threshold bounds
  #   mean_abs_sigdif: average absolute dB change
  #   max_abs_sigdif: largest absolute dB change
  prop_within <- mean(late$within_stationary_threshold, na.rm = TRUE)
  mean_abs_sigdif <- mean(abs(late$sig_dif), na.rm = TRUE)
  max_abs_sigdif <- max(abs(late$sig_dif), na.rm = TRUE)
  
  # Conservative flag rule.
  # A deployment is flagged only if all conditions are met:
  #   1. enough valid late-track comparisons,
  #   2. most signal differences are inside inactive threshold bounds,
  #   3. average absolute signal change is very low,
  #   4. detections are concentrated on the final receiver.
  flagged <-
    n_valid >= min_valid_late &
    prop_within >= min_prop_within &
    mean_abs_sigdif <= max_mean_abs_sigdif &
    focal_receiver_prop >= min_receiver_prop
  
  summary <- tibble(
    MotusTagID = MotusTagID,
    mfgID = mfgID,
    Band = Band,
    focal_receiver = focal_receiver,
    focal_receiver_prop = focal_receiver_prop,
    tower_type = first(na.omit(late$tower_type)),
    lower_db = first(na.omit(late$lower_db)),
    upper_db = first(na.omit(late$upper_db)),
    S2N_cutoff = first(na.omit(late$S2N_cutoff)),
    late_window_hours = late_window_hours,
    receiver_selection_hours = receiver_selection_hours,
    n_valid_late = n_valid,
    prop_within_late = prop_within,
    mean_abs_sigdif = mean_abs_sigdif,
    max_abs_sigdif = max_abs_sigdif,
    min_valid_late = min_valid_late,
    min_prop_within = min_prop_within,
    max_mean_abs_sigdif = max_mean_abs_sigdif,
    min_receiver_prop = min_receiver_prop,
    flagged_possible_stationary_tag = flagged
  )
  
  # Diagnostic plot.
  # The shaded area represents the expected inactive signal-difference range.
  # Points inside this range are treated as consistent with stationary signal
  # behavior. A flagged deployment should still be manually reviewed.
  plot <- ggplot(late, aes(date_time_local, sig_dif)) +
    annotate(
      "rect",
      xmin = min(late$date_time_local, na.rm = TRUE),
      xmax = max(late$date_time_local, na.rm = TRUE),
      ymin = first(na.omit(late$lower_db)),
      ymax = first(na.omit(late$upper_db)),
      alpha = 0.15
    ) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_line(alpha = 0.7) +
    geom_point(aes(shape = within_stationary_threshold), size = 2) +
    labs(
      title = paste("Stationary-tag screen:", MotusTagID, mfgID, "Band", Band),
      subtitle = paste(
        "Focal receiver:", focal_receiver,
        "| Receiver concentration:", percent(focal_receiver_prop, accuracy = 1),
        "| Flagged:", flagged
      ),
      x = "Date/time",
      y = "Signal difference between valid consecutive detections, dB"
    ) +
    theme_bw()
  
  list(
    summary = summary,
    plot = plot,
    late_data = late
  )
}

# ------------------------------------------------------------------------------
# Helper: save_stationary_tag_screen()
#
# Saves the stationary-tag screening summary, the late-window detection data used
# by the screen, and the diagnostic plot.
#
# These files are saved separately from the main activity outputs so the screen
# can be reviewed without changing the activity classification itself.
# ------------------------------------------------------------------------------

save_stationary_tag_screen <- function(
    stationary_screen,
    output_dir,
    plot_dir,
    out_stem
) {
  if (is.null(stationary_screen)) {
    return(invisible(NULL))
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  write_csv(
    stationary_screen$summary,
    file.path(output_dir, paste0(out_stem, "_StationaryTagScreen.csv"))
  )
  
  write_csv(
    stationary_screen$late_data,
    file.path(output_dir, paste0(out_stem, "_StationaryTagScreen_LateData.csv"))
  )
  
  if (inherits(stationary_screen$plot, "ggplot")) {
    ggsave(
      filename = file.path(plot_dir, paste0(out_stem, "_stationary_tag_screen.png")),
      plot = stationary_screen$plot,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Helper: classify_detection_activity()
#
# Converts cleaned long-format detections to wide format by antenna port, then
# applies classify_activity().
#
# classify_activity() uses proportional signal change, antenna switching, receiver
# switching, valid timing, S2N filtering, and dropout correction.
# ------------------------------------------------------------------------------

classify_detection_activity <- function(data_clean, duty_cycle) {
  data_wide <- data_clean %>%
    pivot_wider(
      id_cols = c(
        date_time_local, timing,
        recvDeployName,
        tolerance, S2N_cutoff,
        lower_ratio, upper_ratio,
        lower_db, upper_db
      ),
      names_from = port,
      values_from = c(sig, noise),
      names_vary = "slowest"
    )
  
  df_classified <- classify_activity(
    df = data_wide,
    duty_cycle = duty_cycle,
    lower_ratio = data_wide$lower_ratio,
    upper_ratio = data_wide$upper_ratio
  )
  
  required_cols <- c(
    "top_port",
    "sig_diff",
    "sig_ratio",
    "within_threshold",
    "active"
  )
  
  stopifnot(all(required_cols %in% names(df_classified)))
  
  df_classified
}

# ------------------------------------------------------------------------------
# Helper: summarize_hourly_activity()
#
# Summarizes activity by date, hour, and diel timing category.
# Hours with insufficient detection effort are removed.
# ------------------------------------------------------------------------------

summarize_hourly_activity <- function(
    df_classified,
    min_required_samples,
    MotusTagID,
    mfgID,
    Band,
    state
) {
  df_classified %>%
    mutate(
      date = as.Date(date_time_local),
      hour = hour(date_time_local)
    ) %>%
    group_by(date, hour, timing) %>%
    summarise(
      sample_size = n(),
      n_active = sum(active, na.rm = TRUE),
      percent_activity = n_active / sample_size,
      .groups = "drop"
    ) %>%
    filter(sample_size >= min_required_samples) %>%
    mutate(
      MotusTagID = MotusTagID,
      mfgID = mfgID,
      Band = Band,
      state = state
    )
}

# ------------------------------------------------------------------------------
# Helper: summarize_hourly_across_days()
#
# Aggregates hourly activity across all retained days and estimates approximate
# 95% confidence intervals.
# ------------------------------------------------------------------------------

summarize_hourly_across_days <- function(activity_hourly) {
  activity_hourly %>%
    group_by(hour, timing) %>%
    summarise(
      sample_size = sum(sample_size),
      n_active = sum(n_active),
      percent_activity = n_active / sample_size,
      se = sqrt(percent_activity * (1 - percent_activity) / sample_size),
      ci_lower = percent_activity - 1.96 * se,
      ci_upper = percent_activity + 1.96 * se,
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# Helper: save_activity_tables()
#
# Saves all deployment-specific output tables.
# ------------------------------------------------------------------------------

save_activity_tables <- function(
    output_dir,
    out_stem,
    df_classified,
    activity_hourly,
    activity_hourly_summary
) {
  write_csv(
    df_classified,
    file.path(output_dir, paste0(out_stem, "_ActivityWide.csv"))
  )
  
  write_csv(
    activity_hourly,
    file.path(output_dir, paste0(out_stem, "_ActivityPerHourPerDay.csv"))
  )
  
  write_csv(
    activity_hourly_summary,
    file.path(output_dir, paste0(out_stem, "_ActivityPerHourSummary.csv"))
  )
}

# ------------------------------------------------------------------------------
# Helper: make_and_save_plots()
#
# Creates and saves the full diagnostic plotting suite for one deployment.
# ------------------------------------------------------------------------------

make_and_save_plots <- function(
    df_classified,
    activity_hourly,
    activity_hourly_summary,
    dominant_port,
    duty_cycle,
    lower_ratio,
    upper_ratio,
    MotusTagID,
    mfgID,
    deployment_suffix,
    bird_row,
    tz_local,
    plot_dir
) {
  plot_id <- make_plot_id(
    MotusTagID = MotusTagID,
    mfgID = mfgID,
    deployment_suffix = deployment_suffix
  )
  
  subtitle <- make_deployment_subtitle(
    Band = bird_row$Band,
    Date_tagged = bird_row$Date_tagged,
    Date_end = bird_row$Date_end
  )
  
  det_daily <- make_det_daily(
    data_clean = df_classified,
    dominant_port = dominant_port,
    duty_cycle = duty_cycle
  )
  
  det_daily_timing <- make_det_daily_timing_by_tower_port(
    df_classified = df_classified,
    duty_cycle = duty_cycle,
    lat = bird_row$Lat,
    lon = bird_row$Lon,
    tz_local = tz_local
  )
  
  data_top_finite <- df_classified %>%
    filter(
      top_port == dominant_port,
      is.finite(sig_diff)
    )
  
  plots <- list(
    hourly_activity =
      plot_hourly_activity(
        activity_hourly_summary = activity_hourly_summary,
        dominant_port = dominant_port,
        lower_ratio = lower_ratio,
        upper_ratio = upper_ratio,
        subtitle = subtitle
      ),
    
    daily_detected_vs_expected =
      plot_daily_detected_vs_expected(
        det_daily = det_daily,
        dominant_port = dominant_port,
        subtitle = subtitle
      ),
    
    fraction_expected_tod =
      plot_fraction_expected_tod(
        det_daily_timing = det_daily_timing,
        dominant_port = dominant_port,
        subtitle = subtitle
      ),
    
    duty_cycle =
      plot_duty_cycle(
        data_clean = df_classified,
        dominant_port = dominant_port,
        duty_cycle = duty_cycle,
        subtitle = subtitle
      ),
    
    signal_difference =
      plot_signal_difference(
        data_signal = data_top_finite,
        dominant_port = dominant_port,
        lower_db = lower_db,
        upper_db = upper_db,
        subtitle = subtitle
      ),
    
    dropouts =
      plot_dropouts(
        df_classified = df_classified,
        dominant_port = dominant_port,
        subtitle = subtitle
      ),
    
    daily_daytime_activity =
      plot_daily_daytime_activity(
        activity_hourly,
        subtitle
      )
  )
  
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  purrr::iwalk(plots, function(p, name) {
    if (inherits(p, "ggplot")) {
      ggsave(
        filename = file.path(plot_dir, paste0(plot_id, "_", name, ".png")),
        plot = p,
        width = 10,
        height = 6,
        dpi = 300
      )
    }
  })
}

# ==============================================================================
# 5) Parse input identifiers and load data
# ==============================================================================

ids <- parse_dataset_folder(data_dir)

MotusTagID <- ids$MotusTagID
mfgID <- ids$mfgID
MotusTagID_num <- ids$MotusTagID_num
mfgID_num <- ids$mfgID_num
state <- ids$state
date_code <- ids$date_code

file_path <- file.path(
  data_dir,
  paste0(
    MotusTagID, "_", mfgID, "_", state, "_", date_code,
    "_MotusFiltered.RDS"
  )
)

if (!file.exists(file_path)) {
  stop("❌ Detection file not found: ", file_path)
}

data_raw <- readRDS(file_path)

message("Loaded detection file: ", file_path)

# ==============================================================================
# 6) Load metadata and thresholds
# ==============================================================================

Bird_metadata <- read_csv(
  bird_metadata_path,
  show_col_types = FALSE
)

Tower_metadata <- read_csv(
  tower_metadata_path,
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

threshold_table <- load_threshold_table(
  threshold_dir = threshold_dir,
  threshold_file_pattern = threshold_file_pattern
)

parameter_lookup <- tibble::tribble(
  ~DongleType_family,  ~tolerance, ~S2N_cutoff,
  "Funcube",                 0.3,          6,
  "RTL",                     0.2,         10,
  "SigmaEight",              0.2,         12
)

tower_long <- build_tower_eras(Tower_metadata)

tower_type_thresholds <- build_tower_type_thresholds(
  threshold_table = threshold_table,
  tower_long = tower_long
)

# ==============================================================================
# 7) Resolve biological deployments
# ==============================================================================

bird_deployments <- get_bird_deployments(
  Bird_metadata = Bird_metadata,
  MotusTagID_num = MotusTagID_num,
  mfgID = mfgID
)

if (nrow(bird_deployments) == 0) {
  stop("❌ No matching bird metadata found.")
}

message("Found ", nrow(bird_deployments), " deployment(s) for this tag dataset.")

# ==============================================================================
# 8) Process each biological deployment
# ==============================================================================

for (i in seq_len(nrow(bird_deployments))) {
  
  bird_row <- bird_deployments[i, ]
  Band <- bird_row$Band
  deployment_suffix <- paste0("_Band", Band)
  
  message(
    "\n🐦 Processing MotusTagID ", MotusTagID,
    " | mfgID ", mfgID,
    " | Band ", Band
  )
  
  # ---------------------------------------------------------------------------
  # Determine local time zone for this deployment.
  # ---------------------------------------------------------------------------
  
  tz_local <- tz_lookup_coords(
    bird_row$Lat,
    bird_row$Lon,
    method = "accurate"
  )
  
  if (is.na(tz_local) || length(tz_local) != 1) {
    stop("Invalid timezone lookup for Band ", Band)
  }
  
  # ---------------------------------------------------------------------------
  # Filter detections to this deployment window.
  # ---------------------------------------------------------------------------
  
  deployment_end <- get_deployment_end(
    bird_deployments = bird_deployments,
    bird_row = bird_row,
    i = i,
    tz_local = tz_local
  )
  
  data <- filter_to_deployment_window(
    data_raw = data_raw,
    bird_row = bird_row,
    deployment_end = deployment_end,
    tz_local = tz_local
  )
  
  if (nrow(data) == 0) {
    message("  Skipping deployment: no detections within deployment window.")
    next
  }
  
  # ---------------------------------------------------------------------------
  # Keep receivers from the same multi-receiver site.
  # ---------------------------------------------------------------------------
  
  multi_receiver_sites <- define_multi_receiver_sites()
  
  site_result <- filter_to_site_receivers(
    data = data,
    multi_receiver_sites = multi_receiver_sites
  )
  
  data <- site_result$data
  
  message(
    "  Top receiver: ", site_result$top_recv_name,
    " | receivers retained: ",
    paste(site_result$site_receivers, collapse = ", ")
  )
  
  # ---------------------------------------------------------------------------
  # Clean and deduplicate detections.
  # ---------------------------------------------------------------------------
  
  data_clean <- clean_and_select_strongest_detections(
    data = data,
    duty_cycle = duty_cycle
  )
  
  # ---------------------------------------------------------------------------
  # Assign diel timing using sun/moon timing.
  # ---------------------------------------------------------------------------
  
  data_clean <- info_fast(
    data_clean,
    bird_row$Lat,
    bird_row$Lon,
    tz_local
  )
  
  # ---------------------------------------------------------------------------
  # Attach receiver thresholds and receiver-specific parameters.
  # ---------------------------------------------------------------------------
  
  receiver_thresholds <- get_receiver_thresholds(
    data = data_clean,
    tower_long = tower_long,
    tower_type_thresholds = tower_type_thresholds
  )
  
  check_missing_thresholds(receiver_thresholds)
  
  data_clean <- attach_receiver_parameters(
    data_clean = data_clean,
    tower_long = tower_long,
    tower_type_thresholds = tower_type_thresholds,
    parameter_lookup = parameter_lookup
  )
  
  stopifnot(!any(is.na(data_clean$lower_ratio)))
  stopifnot(!any(is.na(data_clean$upper_ratio)))
  
  # ---------------------------------------------------------------------------
  # Screen for possible mortality / dropped tag / stationary transmitter.
  # ---------------------------------------------------------------------------
  
  # This screen is run after cleaning, diel timing, receiver thresholds, and
  # receiver-specific S2N/tolerance values have been attached, because it needs
  # all of that information to evaluate whether the final signal pattern looks
  # unusually stationary.
  #
  # The output is only a flag for manual review. It does not change the activity
  # classification and should not be treated as confirmed mortality.
  
  stationary_screen <- NULL
  
  if (run_stationary_tag_screen) {
    stationary_screen <- screen_stationary_tag_deployment(
      data_clean = data_clean,
      duty_cycle = duty_cycle,
      late_window_hours = stationary_late_window_hours,
      receiver_selection_hours = stationary_receiver_selection_hours,
      min_valid_late = stationary_min_valid_late,
      min_prop_within = stationary_min_prop_within,
      max_mean_abs_sigdif = stationary_max_mean_abs_sigdif,
      min_receiver_prop = stationary_min_receiver_prop,
      Band = Band,
      MotusTagID = MotusTagID,
      mfgID = mfgID
    )
    
    if (!is.null(stationary_screen)) {
      if (isTRUE(stationary_screen$summary$flagged_possible_stationary_tag)) {
        message(
          "  ⚠️ Stationary-tag screen FLAGGED this deployment as possible mortality, dropped tag, or stationary transmitter. Manual review required."
        )
      } else {
        message(
          "  ✅ Stationary-tag screen did not flag this deployment."
        )
      }
    } else {
      message(
        "  ⚪ Stationary-tag screen skipped: not enough valid late-track daytime detections."
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Summarize deployment-level thresholds for plot labels.
  # ---------------------------------------------------------------------------
  
  threshold_summary <- summarize_deployment_thresholds(data_clean)
  
  lower_ratio <- threshold_summary$lower_ratio
  upper_ratio <- threshold_summary$upper_ratio
  lower_db <- threshold_summary$lower_db
  upper_db <- threshold_summary$upper_db
  
  # ---------------------------------------------------------------------------
  # Classify activity.
  # ---------------------------------------------------------------------------
  
  df_classified <- classify_detection_activity(
    data_clean = data_clean,
    duty_cycle = duty_cycle
  )
  
  # Reattach threshold summaries after classification for downstream plotting.
  df_classified <- df_classified %>%
    left_join(
      get_receiver_thresholds(
        data = df_classified,
        tower_long = tower_long,
        tower_type_thresholds = tower_type_thresholds
      ),
      by = c("recvDeployName", "date_time_local"),
      relationship = "many-to-many"
    ) %>%
    left_join(
      tower_type_thresholds,
      by = "tower_type",
      relationship = "many-to-one"
    )
  
  dominant_port <- df_classified %>%
    count(top_port, sort = TRUE) %>%
    dplyr::slice(1) %>%
    pull(top_port)
  
  # ---------------------------------------------------------------------------
  # Summarize activity.
  # ---------------------------------------------------------------------------
  
  activity_hourly <- summarize_hourly_activity(
    df_classified = df_classified,
    min_required_samples = min_required_samples,
    MotusTagID = MotusTagID,
    mfgID = mfgID,
    Band = Band,
    state = state
  )
  
  activity_hourly_summary <- summarize_hourly_across_days(activity_hourly)
  
  # ---------------------------------------------------------------------------
  # Build output names and save tables.
  # ---------------------------------------------------------------------------
  
  output_dir <- here(
    "Sample_Data", "Processed",
    paste0(
      MotusTagID, "_", mfgID,
      "_Band", Band,
      "_MotusFiltered_classified"
    )
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  raw_basename <- tools::file_path_sans_ext(basename(file_path))
  out_stem <- paste0(raw_basename, "_Band", Band)
  
  save_activity_tables(
    output_dir = output_dir,
    out_stem = out_stem,
    df_classified = df_classified,
    activity_hourly = activity_hourly,
    activity_hourly_summary = activity_hourly_summary
  )
  
  message("  ✅ Tables saved to: ", output_dir)
  
  # ---------------------------------------------------------------------------
  # Make and save diagnostic plots.
  # ---------------------------------------------------------------------------
  
  plot_dir <- file.path(output_dir, "plots")
  
  make_and_save_plots(
    df_classified = df_classified,
    activity_hourly = activity_hourly,
    activity_hourly_summary = activity_hourly_summary,
    dominant_port = dominant_port,
    duty_cycle = duty_cycle,
    lower_ratio = lower_ratio,
    upper_ratio = upper_ratio,
    MotusTagID = MotusTagID,
    mfgID = mfgID,
    deployment_suffix = deployment_suffix,
    bird_row = bird_row,
    tz_local = tz_local,
    plot_dir = plot_dir
  )
  
  message("  ✅ Plots saved to: ", plot_dir)
  
  # ---------------------------------------------------------------------------
  # Save stationary-tag screening outputs.
  # ---------------------------------------------------------------------------
  
  # If the screen ran successfully, this saves:
  #   1. A one-row summary table with the final-window metrics and flag result
  #   2. The late-window data used to make the decision
  #   3. A diagnostic plot showing final-window signal differences
  #
  # These outputs are saved after output_dir and plot_dir exist.
  
  if (run_stationary_tag_screen && !is.null(stationary_screen)) {
    save_stationary_tag_screen(
      stationary_screen = stationary_screen,
      output_dir = output_dir,
      plot_dir = plot_dir,
      out_stem = out_stem
    )
    
    message("  ✅ Stationary-tag screen outputs saved to: ", output_dir)
  }
}

message("\n✅ ALL DEPLOYMENTS COMPLETE — TABLES + PLOTS GENERATED")
