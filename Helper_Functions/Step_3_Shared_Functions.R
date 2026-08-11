################################################################################
# STEP 3 SHARED FUNCTIONS
#
# PURPOSE
# Functions used by both:
#   • Step3_Activity_Classification.R
#   • Step3_LOOP_Activity_Classification.R
#
# Keeping deployment processing in one shared file ensures that single-dataset
# and batch processing use identical activity-classification logic.
################################################################################


# ==============================================================================
# 1) DATASET-FOLDER FUNCTIONS
# ==============================================================================

parse_dataset_folder <- function(data_dir) {
  
  folder_name <- basename(data_dir)
  
  parsed <- stringr::str_match(
    folder_name,
    "^([^_]+)_([^_]+)_(.+)_([0-9]{6})_MotusFiltered$"
  )
  
  if (all(is.na(parsed))) {
    stop(
      "Folder name does not match the Step 1 output format:\n",
      folder_name,
      "\n\nExpected:\n",
      "<MotusTagID>_<mfgID>_<dataset_label>_<MMDDYY>_MotusFiltered"
    )
  }
  
  list(
    folder_name = folder_name,
    MotusTagID = parsed[, 2],
    mfgID = parsed[, 3],
    dataset_label = parsed[, 4],
    date_code = parsed[, 5],
    download_date = as.Date(
      parsed[, 5],
      format = "%m%d%y"
    )
  )
}


find_latest_dataset_folder <- function(
    data_parent_dir,
    target_MotusTagID,
    target_mfgID,
    target_dataset_label
) {
  
  all_dirs <- list.dirs(
    data_parent_dir,
    recursive = FALSE,
    full.names = TRUE
  )
  
  folder_info <- purrr::map_dfr(
    all_dirs,
    function(x) {
      
      out <- tryCatch(
        parse_dataset_folder(x),
        error = function(e) NULL
      )
      
      if (is.null(out)) {
        return(NULL)
      }
      
      tibble::tibble(
        data_dir = x,
        MotusTagID = out$MotusTagID,
        mfgID = out$mfgID,
        dataset_label = out$dataset_label,
        date_code = out$date_code,
        download_date = out$download_date
      )
    }
  )
  
  matches <- folder_info %>%
    dplyr::filter(
      MotusTagID == as.character(target_MotusTagID),
      mfgID == as.character(target_mfgID),
      dataset_label == target_dataset_label
    ) %>%
    dplyr::arrange(
      dplyr::desc(download_date)
    )
  
  if (nrow(matches) == 0) {
    stop(
      "No matching Step 1 dataset folder was found for:\n",
      "  MotusTagID: ", target_MotusTagID, "\n",
      "  mfgID: ", target_mfgID, "\n",
      "  dataset label: ", target_dataset_label
    )
  }
  
  matches$data_dir[1]
}


find_all_latest_dataset_folders <- function(data_parent_dir) {
  
  all_dirs <- list.dirs(
    data_parent_dir,
    recursive = FALSE,
    full.names = TRUE
  )
  
  folder_info <- purrr::map_dfr(
    all_dirs,
    function(x) {
      
      out <- tryCatch(
        parse_dataset_folder(x),
        error = function(e) NULL
      )
      
      if (is.null(out)) {
        return(NULL)
      }
      
      tibble::tibble(
        data_dir = x,
        MotusTagID = out$MotusTagID,
        mfgID = out$mfgID,
        dataset_label = out$dataset_label,
        date_code = out$date_code,
        download_date = out$download_date
      )
    }
  )
  
  if (nrow(folder_info) == 0) {
    return(folder_info)
  }
  
  # Repeated Motus downloads contain overlapping historical detections.
  # Retain only the newest Step 1 export for each tag dataset.
  
  folder_info %>%
    dplyr::group_by(
      MotusTagID,
      mfgID,
      dataset_label
    ) %>%
    dplyr::arrange(
      dplyr::desc(download_date)
    ) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
}



# ==============================================================================
# 2) METADATA FUNCTIONS
# ==============================================================================

standardize_tower_metadata <- function(tower_metadata) {
  
  required_cols <- c(
    "recvDeployName",
    "DongleType_1",
    "System1"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(tower_metadata)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "Tower metadata are missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
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
  
  character_cols <- c(
    "recvDeployName",
    "DongleType_1",
    "DongleType_2",
    "System1",
    "System2",
    "System1End"
  )
  
  tower_metadata %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(character_cols),
        as.character
      ),
      dplyr::across(
        dplyr::all_of(character_cols),
        ~ dplyr::na_if(trimws(.x), "")
      )
    )
}


clean_dongle_family <- function(x) {
  
  x <- as.character(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("FUNcube|Funcube", x, ignore.case = TRUE) ~ "FUNcube",
    grepl("RTL", x, ignore.case = TRUE) ~ "RTL",
    grepl("Sigma", x, ignore.case = TRUE) ~ "SigmaEight",
    TRUE ~ x
  )
}


clean_system_family <- function(x) {
  
  x <- as.character(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("CTT", x, ignore.case = TRUE) ~ "CTT",
    grepl("SensorGnome", x, ignore.case = TRUE) ~ "SensorGnome",
    grepl("Sigma", x, ignore.case = TRUE) ~ "SigmaEight",
    TRUE ~ x
  )
}


build_tower_eras <- function(tower_metadata) {
  
  tower_metadata <- standardize_tower_metadata(
    tower_metadata
  )
  
  tower_metadata <- tower_metadata %>%
    dplyr::mutate(
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
    dplyr::filter(
      !is.na(System2),
      is.na(System1End)
    )
  
  if (nrow(invalid_second_era) > 0) {
    stop(
      "At least one receiver has a System2 value but no System1End date."
    )
  }
  
  era_1 <- tower_metadata %>%
    dplyr::transmute(
      recvDeployName,
      DongleType = DongleType_1,
      System = System1,
      start_date = as.Date("1900-01-01"),
      end_date = dplyr::coalesce(
        System1End,
        as.Date("2100-12-31")
      )
    )
  
  era_2 <- tower_metadata %>%
    dplyr::filter(
      !is.na(System2)
    ) %>%
    dplyr::transmute(
      recvDeployName,
      DongleType = DongleType_2,
      System = System2,
      start_date = System1End + lubridate::days(1),
      end_date = as.Date("2100-12-31")
    )
  
  dplyr::bind_rows(
    era_1,
    era_2
  ) %>%
    dplyr::mutate(
      DongleType_family = clean_dongle_family(
        DongleType
      ),
      System_family = clean_system_family(
        System
      ),
      tower_type = paste(
        DongleType_family,
        System_family,
        sep = "_"
      )
    ) %>%
    dplyr::distinct()
}



# ==============================================================================
# 3) DEPLOYMENT FUNCTIONS
# ==============================================================================

get_deployment_duty_cycle <- function(
    bird_row,
    duty_cycle_col
) {
  
  if (!duty_cycle_col %in% names(bird_row)) {
    stop(
      "Missing required burst-interval column: ",
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
    stop(
      "Invalid transmitter burst interval."
    )
  }
  
  duty_value
}


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
      "Could not determine timezone from coordinates."
    )
  }
  
  tz_local
}


get_bird_deployments <- function(
    Bird_metadata,
    MotusTagID,
    mfgID
) {
  
  x <- Bird_metadata %>%
    dplyr::mutate(
      motusTagID = as.character(
        motusTagID
      ),
      mfgID = as.character(
        mfgID
      )
    ) %>%
    dplyr::filter(
      motusTagID == as.character(MotusTagID),
      mfgID == as.character(mfgID)
    )
  
  if (nrow(x) == 0) {
    return(x)
  }
  
  if (!"Time_tagged" %in% names(x)) {
    x$Time_tagged <- NA_character_
  }
  
  x %>%
    dplyr::mutate(
      Date_tagged = as.Date(
        lubridate::parse_date_time(
          Date_tagged,
          orders = c(
            "ymd",
            "mdy",
            "dmy"
          ),
          quiet = TRUE
        )
      ),
      Date_end = as.Date(
        lubridate::parse_date_time(
          Date_end,
          orders = c(
            "ymd",
            "mdy",
            "dmy"
          ),
          quiet = TRUE
        )
      ),
      Time_tagged = as.character(
        Time_tagged
      )
    ) %>%
    dplyr::group_by(
      Band,
      Date_tagged
    ) %>%
    dplyr::summarise(
      Time_tagged = dplyr::first(
        Time_tagged[
          !is.na(Time_tagged) &
            Time_tagged != ""
        ],
        default = NA_character_
      ),
      Date_end = if (
        all(is.na(Date_end))
      ) {
        as.Date(NA)
      } else {
        max(
          Date_end,
          na.rm = TRUE
        )
      },
      Lat = dplyr::first(Lat),
      Lon = dplyr::first(Lon),
      Burst_Interval = dplyr::first(
        Burst_Interval[
          !is.na(Burst_Interval)
        ],
        default = NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Time_tagged = dplyr::if_else(
        is.na(Time_tagged) |
          Time_tagged == "",
        "00:00:00",
        Time_tagged
      )
    ) %>%
    dplyr::arrange(
      Date_tagged
    ) %>%
    dplyr::mutate(
      deploy_index = dplyr::row_number()
    )
}


get_deployment_end <- function(
    bird_deployments,
    bird_row,
    i
) {
  
  explicit_end <- bird_row$Date_end[1]
  
  next_start <- bird_deployments %>%
    dplyr::filter(
      deploy_index == i + 1
    ) %>%
    dplyr::pull(
      Date_tagged
    )
  
  candidates <- as.Date(
    character()
  )
  
  if (!is.na(explicit_end)) {
    candidates <- c(
      candidates,
      explicit_end
    )
  }
  
  if (length(next_start) > 0) {
    candidates <- c(
      candidates,
      next_start - 1
    )
  }
  
  if (length(candidates) == 0) {
    return(
      as.Date(NA)
    )
  }
  
  min(
    candidates,
    na.rm = TRUE
  )
}


filter_to_deployment_window <- function(
    data_raw,
    bird_row,
    deployment_end,
    tz_local
) {
  
  if (!"ts" %in% names(data_raw)) {
    stop(
      "Required Motus `ts` column is missing."
    )
  }
  
  deployment_start <- lubridate::ymd_hms(
    paste(
      bird_row$Date_tagged,
      bird_row$Time_tagged
    ),
    tz = tz_local
  )
  
  x <- data_raw %>%
    dplyr::mutate(
      date_time_utc = lubridate::as_datetime(
        ts,
        tz = "UTC"
      ),
      date_time_local = lubridate::with_tz(
        date_time_utc,
        tzone = tz_local
      )
    ) %>%
    dplyr::filter(
      date_time_local >= deployment_start
    )
  
  if (!is.na(deployment_end)) {
    
    deployment_end_time <- lubridate::ymd_hms(
      paste(
        deployment_end,
        "23:59:59"
      ),
      tz = tz_local
    )
    
    x <- x %>%
      dplyr::filter(
        date_time_local <= deployment_end_time
      )
  }
  
  x
}



# ==============================================================================
# 4) SITE FUNCTION
# ==============================================================================

filter_to_site_receivers <- function(
    data,
    multi_receiver_sites
) {
  
  receiver_counts <- data %>%
    dplyr::count(
      recvDeployName,
      sort = TRUE
    )
  
  
  if (nrow(receiver_counts) == 0) {
    return(NULL)
  }
  
  
  top_recv_name <-
    receiver_counts$recvDeployName[1]
  
  
  matched_site_id <- multi_receiver_sites %>%
    dplyr::filter(
      recvDeployName ==
        top_recv_name
    ) %>%
    dplyr::pull(
      site_id
    )
  
  
  if (length(matched_site_id) == 0) {
    
    site_receivers <-
      top_recv_name
    
  } else {
    
    site_receivers <- multi_receiver_sites %>%
      dplyr::filter(
        site_id %in%
          matched_site_id
      ) %>%
      dplyr::pull(
        recvDeployName
      )
  }
  
  
  list(
    data =
      data %>%
      dplyr::filter(
        recvDeployName %in%
          site_receivers
      ),
    
    top_recv_name =
      top_recv_name,
    
    site_receivers =
      site_receivers
  )
}

# ==============================================================================
# 5) THRESHOLD FUNCTIONS
# ==============================================================================

load_threshold_table <- function(
    threshold_dir,
    threshold_file_pattern
) {
  
  threshold_tables <- list.files(
    threshold_dir,
    pattern = threshold_file_pattern,
    full.names = TRUE
  )
  
  if (length(threshold_tables) == 0) {
    stop(
      "No Step 2 threshold-summary tables were found in:\n",
      threshold_dir
    )
  }
  
  purrr::map_dfr(
    threshold_tables,
    ~ readr::read_csv(
      .x,
      col_types = readr::cols(
        receiver_era_start_date =
          readr::col_date(),
        receiver_era_end_date =
          readr::col_date(),
        .default =
          readr::col_guess()
      ),
      show_col_types = FALSE
    )
  )
}


build_tower_type_thresholds <- function(
    threshold_table,
    tower_long
) {
  
  # Stage 1:
  # Median across tag-specific estimates within each receiver/configuration.
  
  tower_level_medians <- threshold_table %>%
    dplyr::rename(
      tower_type_csv =
        tower_type
    ) %>%
    dplyr::left_join(
      tower_long,
      by = dplyr::join_by(
        top_receiver ==
          recvDeployName,
        receiver_era_start_date <=
          end_date,
        receiver_era_end_date >=
          start_date
      ),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      tower_type_final =
        dplyr::coalesce(
          tower_type,
          tower_type_csv
        )
    ) %>%
    dplyr::group_by(
      top_receiver,
      tower_type_final
    ) %>%
    dplyr::summarise(
      lower_ratio_median =
        median(
          lower_ratio,
          na.rm = TRUE
        ),
      upper_ratio_median =
        median(
          upper_ratio,
          na.rm = TRUE
        ),
      lower_db_median =
        median(
          lower_db,
          na.rm = TRUE
        ),
      upper_db_median =
        median(
          upper_db,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
  
  # Stage 2:
  # Mean of receiver-level medians within receiver type.
  
  tower_level_medians %>%
    dplyr::group_by(
      tower_type_final
    ) %>%
    dplyr::summarise(
      lower_ratio =
        mean(
          lower_ratio_median,
          na.rm = TRUE
        ),
      upper_ratio =
        mean(
          upper_ratio_median,
          na.rm = TRUE
        ),
      lower_db =
        mean(
          lower_db_median,
          na.rm = TRUE
        ),
      upper_db =
        mean(
          upper_db_median,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      tower_type =
        tower_type_final
    )
}


get_receiver_thresholds <- function(
    data,
    tower_long,
    tower_type_thresholds
) {
  
  df_recv <- data %>%
    dplyr::select(
      recvDeployName,
      date_time_local
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      det_date =
        as.Date(
          date_time_local
        )
    )
  
  df_recv %>%
    dplyr::left_join(
      tower_long,
      by = dplyr::join_by(
        recvDeployName,
        dplyr::between(
          det_date,
          start_date,
          end_date
        )
      ),
      relationship = "many-to-one"
    ) %>%
    dplyr::left_join(
      tower_type_thresholds,
      by = "tower_type"
    ) %>%
    dplyr::select(
      recvDeployName,
      date_time_local,
      tower_type,
      lower_ratio,
      upper_ratio,
      lower_db,
      upper_db
    )
}


attach_receiver_parameters <- function(
    data_clean,
    tower_long,
    tower_type_thresholds,
    parameter_lookup
) {
  
  receiver_thresholds <-
    get_receiver_thresholds(
      data = data_clean,
      tower_long = tower_long,
      tower_type_thresholds =
        tower_type_thresholds
    )
  
  missing_receivers <-
    receiver_thresholds %>%
    dplyr::filter(
      is.na(lower_ratio) |
        is.na(upper_ratio) |
        is.na(tower_type)
    ) %>%
    dplyr::pull(
      recvDeployName
    ) %>%
    unique()
  
  if (length(missing_receivers) > 0) {
    
    message(
      "  ⚠️ Missing receiver metadata/thresholds for: ",
      paste(
        missing_receivers,
        collapse = ", "
      )
    )
    
    return(NULL)
  }
  
  data_clean %>%
    dplyr::left_join(
      receiver_thresholds,
      by = c(
        "recvDeployName",
        "date_time_local"
      )
    ) %>%
    dplyr::mutate(
      DongleType_family =
        stringr::str_extract(
          tower_type,
          "^[^_]+"
        )
    ) %>%
    dplyr::left_join(
      parameter_lookup,
      by = "DongleType_family"
    )
}


# ==============================================================================
# 6) STATIONARY-TAG SCREENING FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# screen_stationary_tag_deployment()
#
# Screen a biological deployment for extended inactivity near the end of its
# detection record.
#
# The user chooses the diel period used for screening:
#   "day"   = daytime detections
#   "night" = night_1 and night_2 detections
#   "all"   = all diel periods
#
# The selected period should generally represent a time when the study species
# would normally be expected to show activity.
#
# This is a diagnostic flag only. It does not alter activity classifications and
# should not be interpreted as confirmed mortality or tag loss.
# ------------------------------------------------------------------------------

screen_stationary_tag_deployment <- function(
    data_clean,
    duty_cycle,
    screen_timing = "day",
    late_window_hours = 72,
    receiver_selection_hours = 24,
    min_valid_late = 30,
    min_prop_within = 0.80,
    max_mean_abs_sigdif = 2.5,
    min_receiver_prop = 0.50,
    Band,
    MotusTagID,
    mfgID
) {
  
  valid_screen_timings <- c(
    "day",
    "night",
    "all"
  )
  
  if (!screen_timing %in% valid_screen_timings) {
    stop(
      "`screen_timing` must be one of: ",
      paste(valid_screen_timings, collapse = ", ")
    )
  }
  
  
  x <- data_clean %>%
    dplyr::arrange(
      date_time_local
    )
  
  
  if (nrow(x) < 20) {
    return(NULL)
  }
  
  
  # ---------------------------------------------------------------------------
  # Select diel period used for screening
  # ---------------------------------------------------------------------------
  
  if (screen_timing == "day") {
    
    x_screen <- x %>%
      dplyr::filter(
        timing == "day"
      )
    
  } else if (screen_timing == "night") {
    
    x_screen <- x %>%
      dplyr::filter(
        timing %in% c(
          "night_1",
          "night_2"
        )
      )
    
  } else {
    
    x_screen <- x
  }
  
  
  if (nrow(x_screen) < 10) {
    return(NULL)
  }
  
  
  # Final detection within the selected diel period anchors the screen.
  
  final_screen_time <- max(
    x_screen$date_time_local,
    na.rm = TRUE
  )
  
  
  # ---------------------------------------------------------------------------
  # Identify focal receiver
  # ---------------------------------------------------------------------------
  
  receiver_window <- x_screen %>%
    dplyr::filter(
      date_time_local >=
        final_screen_time -
        lubridate::hours(
          receiver_selection_hours
        )
    )
  
  
  if (nrow(receiver_window) == 0) {
    return(NULL)
  }
  
  
  receiver_counts <- receiver_window %>%
    dplyr::count(
      recvDeployName,
      sort = TRUE
    )
  
  
  focal_receiver <-
    receiver_counts$recvDeployName[1]
  
  
  focal_receiver_prop <-
    receiver_counts$n[1] /
    sum(receiver_counts$n)
  
  
  # ---------------------------------------------------------------------------
  # Calculate valid signal differences on focal receiver
  # ---------------------------------------------------------------------------
  
  x_focal <- x %>%
    dplyr::filter(
      recvDeployName == focal_receiver
    ) %>%
    dplyr::arrange(
      date_time_local
    ) %>%
    dplyr::mutate(
      SNR = sig - noise,
      
      lag_SNR =
        dplyr::lag(
          SNR
        ),
      
      sig_lag =
        dplyr::lag(
          sig
        ),
      
      time_numeric =
        as.numeric(
          date_time_local
        ),
      
      lag_time_numeric =
        dplyr::lag(
          time_numeric
        ),
      
      dt =
        time_numeric -
        lag_time_numeric,
      
      sig_dif =
        dplyr::if_else(
          !is.na(dt) &
            abs(
              dt - duty_cycle
            ) <= tolerance &
            !is.na(SNR) &
            !is.na(lag_SNR) &
            SNR >= S2N_cutoff &
            lag_SNR >= S2N_cutoff,
          sig - sig_lag,
          NA_real_
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Apply the same diel-period restriction to the final screening window
  # ---------------------------------------------------------------------------
  
  if (screen_timing == "day") {
    
    late <- x_focal %>%
      dplyr::filter(
        timing == "day",
        date_time_local >=
          final_screen_time -
          lubridate::hours(
            late_window_hours
          )
      )
    
  } else if (screen_timing == "night") {
    
    late <- x_focal %>%
      dplyr::filter(
        timing %in% c(
          "night_1",
          "night_2"
        ),
        date_time_local >=
          final_screen_time -
          lubridate::hours(
            late_window_hours
          )
      )
    
  } else {
    
    late <- x_focal %>%
      dplyr::filter(
        date_time_local >=
          final_screen_time -
          lubridate::hours(
            late_window_hours
          )
      )
  }
  
  
  if (nrow(late) == 0) {
    return(NULL)
  }
  
  
  late <- late %>%
    dplyr::mutate(
      within_stationary_threshold =
        sig_dif >= lower_db &
        sig_dif <= upper_db
    )
  
  
  n_valid <- sum(
    !is.na(
      late$sig_dif
    )
  )
  
  
  if (n_valid == 0) {
    return(NULL)
  }
  
  
  prop_within <- mean(
    late$within_stationary_threshold,
    na.rm = TRUE
  )
  
  
  mean_abs_sigdif <- mean(
    abs(
      late$sig_dif
    ),
    na.rm = TRUE
  )
  
  
  max_abs_sigdif <- max(
    abs(
      late$sig_dif
    ),
    na.rm = TRUE
  )
  
  
  flagged <-
    n_valid >= min_valid_late &
    prop_within >= min_prop_within &
    mean_abs_sigdif <= max_mean_abs_sigdif &
    focal_receiver_prop >= min_receiver_prop
  
  
  summary <- tibble::tibble(
    MotusTagID = MotusTagID,
    mfgID = mfgID,
    Band = Band,
    screen_timing = screen_timing,
    focal_receiver = focal_receiver,
    focal_receiver_prop = focal_receiver_prop,
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
  
  
  plot <- ggplot2::ggplot(
    late,
    ggplot2::aes(
      date_time_local,
      sig_dif
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3
    ) +
    ggplot2::geom_line(
      alpha = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape =
          within_stationary_threshold
      ),
      size = 2
    ) +
    ggplot2::labs(
      title = paste(
        "Stationary-tag screen:",
        MotusTagID,
        mfgID,
        "Band",
        Band
      ),
      subtitle = paste(
        "Screen period:",
        screen_timing,
        "| Focal receiver:",
        focal_receiver,
        "| Receiver concentration:",
        scales::percent(
          focal_receiver_prop,
          accuracy = 1
        ),
        "| Flagged:",
        flagged
      ),
      x = "Date/time",
      y = "Signal-strength difference (dB)"
    ) +
    ggplot2::theme_bw()
  
  
  list(
    summary = summary,
    plot = plot,
    late_data = late
  )
}
# ------------------------------------------------------------------------------
# save_stationary_tag_screen()
#
# Save the stationary-screen summary, late-window data, and diagnostic plot.
# ------------------------------------------------------------------------------

save_stationary_tag_screen <- function(
    stationary_screen,
    output_dir,
    plot_dir,
    out_stem
) {
  
  if (is.null(stationary_screen)) {
    return(
      invisible(NULL)
    )
  }
  
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    plot_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  readr::write_csv(
    stationary_screen$summary,
    file.path(
      output_dir,
      paste0(
        out_stem,
        "_StationaryTagScreen.csv"
      )
    )
  )
  
  
  readr::write_csv(
    stationary_screen$late_data,
    file.path(
      output_dir,
      paste0(
        out_stem,
        "_StationaryTagScreen_LateData.csv"
      )
    )
  )
  
  
  if (
    inherits(
      stationary_screen$plot,
      "ggplot"
    )
  ) {
    
    ggplot2::ggsave(
      filename = file.path(
        plot_dir,
        paste0(
          out_stem,
          "_stationary_tag_screen.png"
        )
      ),
      plot =
        stationary_screen$plot,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
  
  invisible(NULL)
}

# ==============================================================================
# 7) ACTIVITY-CLASSIFICATION FUNCTIONS
# ==============================================================================

summarize_deployment_thresholds <- function(
    data_clean
) {
  
  data_clean %>%
    dplyr::distinct(
      recvDeployName,
      lower_ratio,
      upper_ratio,
      lower_db,
      upper_db
    ) %>%
    dplyr::summarise(
      lower_ratio =
        median(
          lower_ratio,
          na.rm = TRUE
        ),
      
      upper_ratio =
        median(
          upper_ratio,
          na.rm = TRUE
        ),
      
      lower_db =
        median(
          lower_db,
          na.rm = TRUE
        ),
      
      upper_db =
        median(
          upper_db,
          na.rm = TRUE
        )
    )
}


classify_detection_activity <- function(
    data_clean,
    duty_cycle
) {
  
  data_wide <- data_clean %>%
    dplyr::mutate(
      duty_cycle =
        duty_cycle
    ) %>%
    tidyr::pivot_wider(
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
  
  
  df_classified <- classify_activity(
    df =
      data_wide,
    duty_cycle =
      duty_cycle,
    lower_ratio =
      data_wide$lower_ratio,
    upper_ratio =
      data_wide$upper_ratio
  )
  
  
  required_cols <- c(
    "top_port",
    "sig_diff",
    "sig_ratio",
    "within_threshold",
    "active",
    "activity_denominator"
  )
  
  
  missing_cols <- setdiff(
    required_cols,
    names(df_classified)
  )
  
  
  if (length(missing_cols) > 0) {
    
    stop(
      "`classify_activity()` did not return required column(s): ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  df_classified
}

# ==============================================================================
# 8) ACTIVITY-SUMMARY FUNCTIONS
# ==============================================================================

summarize_hourly_activity <- function(
    df_classified,
    min_required_samples,
    MotusTagID,
    mfgID,
    Band,
    dataset_label
) {
  
  df_classified %>%
    dplyr::mutate(
      date =
        as.Date(
          date_time_local
        ),
      
      hour =
        lubridate::hour(
          date_time_local
        )
    ) %>%
    dplyr::group_by(
      date,
      hour,
      timing
    ) %>%
    dplyr::summarise(
      sample_size =
        sum(
          activity_denominator,
          na.rm = TRUE
        ),
      
      total_rows =
        dplyr::n(),
      
      n_active =
        sum(
          active &
            activity_denominator,
          na.rm = TRUE
        ),
      
      percent_activity =
        dplyr::if_else(
          sample_size > 0,
          n_active /
            sample_size,
          NA_real_
        ),
      
      .groups = "drop"
    ) %>%
    dplyr::filter(
      sample_size >=
        min_required_samples
    ) %>%
    dplyr::mutate(
      MotusTagID =
        MotusTagID,
      
      mfgID =
        mfgID,
      
      Band =
        Band,
      
      dataset_label =
        dataset_label
    )
}


summarize_hourly_across_days <- function(
    activity_hourly
) {
  
  activity_hourly %>%
    dplyr::group_by(
      hour,
      timing
    ) %>%
    dplyr::summarise(
      sample_size =
        sum(
          sample_size
        ),
      
      n_active =
        sum(
          n_active
        ),
      
      percent_activity =
        n_active /
        sample_size,
      
      se =
        sqrt(
          percent_activity *
            (1 - percent_activity) /
            sample_size
        ),
      
      ci_lower =
        pmax(
          0,
          percent_activity -
            1.96 * se
        ),
      
      ci_upper =
        pmin(
          1,
          percent_activity +
            1.96 * se
        ),
      
      .groups = "drop"
    )
}

# ==============================================================================
# 9) OUTPUT FUNCTIONS
# ==============================================================================

save_activity_tables <- function(
    output_dir,
    out_stem,
    df_classified,
    activity_hourly,
    activity_hourly_summary
) {
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  readr::write_csv(
    df_classified,
    file.path(
      output_dir,
      paste0(
        out_stem,
        "_ActivityWide.csv"
      )
    )
  )
  
  
  readr::write_csv(
    activity_hourly,
    file.path(
      output_dir,
      paste0(
        out_stem,
        "_ActivityPerHourPerDay.csv"
      )
    )
  )
  
  
  readr::write_csv(
    activity_hourly_summary,
    file.path(
      output_dir,
      paste0(
        out_stem,
        "_ActivityPerHourSummary.csv"
      )
    )
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
    lower_db,
    upper_db,
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
# 10) CORE TAG-DATASET PROCESSING FUNCTION
# ==============================================================================

# ------------------------------------------------------------------------------
# process_tag_dataset()
#
# Process one MotusTagID × mfgID dataset from beginning to end.
#
# A tag dataset may contain detections from more than one biological deployment
# if a transmitter was redeployed. Each deployment is therefore processed
# separately using Band, Date_tagged, and Date_end from the deployment metadata.
#
# This function is shared by both:
#   • Step3_Activity_Classification.R
#   • Step3_LOOP_Activity_Classification.R
#
# Keeping the full processing workflow here ensures that single-dataset and
# batch analyses use identical activity-classification logic.
# ------------------------------------------------------------------------------

process_tag_dataset <- function(
    data_dir,
    Bird_metadata,
    tower_long,
    tower_type_thresholds,
    parameter_lookup,
    multi_receiver_sites,
    processed_dir,
    required_duty_cycle_col,
    sample_size_threshold,
    transmission_event_tolerance,
    run_stationary_tag_screen,
    stationary_screen_timing,
    stationary_late_window_hours,
    stationary_receiver_selection_hours,
    stationary_min_valid_late,
    stationary_min_prop_within,
    stationary_max_mean_abs_sigdif,
    stationary_min_receiver_prop
) {
  
  # ============================================================================
  # 1) PARSE DATASET IDENTIFIERS
  # ============================================================================
  
  ids <- parse_dataset_folder(
    data_dir
  )
  
  MotusTagID <- ids$MotusTagID
  mfgID <- ids$mfgID
  dataset_label <- ids$dataset_label
  
  
  message(
    "\n============================================================",
    "\nPROCESSING TAG DATASET",
    "\nMotusTagID: ", MotusTagID,
    " | mfgID: ", mfgID,
    " | dataset: ", dataset_label,
    "\n============================================================"
  )
  
  
  # ============================================================================
  # 2) LOAD STEP 1 DETECTION DATA
  # ============================================================================
  
  rds_files <- list.files(
    data_dir,
    pattern = "_MotusFiltered\\.RDS$",
    full.names = TRUE
  )
  
  
  if (length(rds_files) == 0) {
    
    stop(
      "No MotusFiltered RDS file was found in:\n  ",
      data_dir
    )
  }
  
  
  if (length(rds_files) > 1) {
    
    stop(
      "More than one MotusFiltered RDS file was found in:\n  ",
      data_dir,
      "\nExpected exactly one."
    )
  }
  
  
  file_path <- rds_files[1]
  
  
  data_raw <- readRDS(
    file_path
  )
  
  
  if (!"ts" %in% names(data_raw)) {
    
    stop(
      "The Step 1 dataset does not contain the required Motus `ts` column:\n  ",
      file_path
    )
  }
  
  
  if (nrow(data_raw) == 0) {
    
    stop(
      "The Step 1 detection dataset contains no detections."
    )
  }
  
  
  message(
    "Loaded ",
    format(
      nrow(data_raw),
      big.mark = ","
    ),
    " detections."
  )
  
  
  # ============================================================================
  # 3) RESOLVE BIOLOGICAL DEPLOYMENTS
  # ============================================================================
  
  bird_deployments <- get_bird_deployments(
    Bird_metadata = Bird_metadata,
    MotusTagID = MotusTagID,
    mfgID = mfgID
  )
  
  
  if (nrow(bird_deployments) == 0) {
    
    stop(
      "No matching deployment metadata were found for ",
      "MotusTagID ", MotusTagID,
      " × mfgID ", mfgID,
      "."
    )
  }
  
  
  message(
    "Found ",
    nrow(bird_deployments),
    " biological deployment(s)."
  )
  
  
  # Store a compact processing summary for all deployments associated with
  # this tag dataset.
  
  deployment_results <- vector(
    "list",
    nrow(bird_deployments)
  )
  
  
  # ============================================================================
  # 4) PROCESS EACH BIOLOGICAL DEPLOYMENT
  # ============================================================================
  
  for (i in seq_len(nrow(bird_deployments))) {
    
    bird_row <- bird_deployments[
      i,
      ,
      drop = FALSE
    ]
    
    
    Band <- as.character(
      bird_row$Band[1]
    )
    
    
    message(
      "\n------------------------------------------------------------",
      "\nDeployment ",
      i,
      " of ",
      nrow(bird_deployments),
      " | Band ",
      Band,
      "\n------------------------------------------------------------"
    )
    
    
    # --------------------------------------------------------------------------
    # Retrieve transmitter burst interval
    # --------------------------------------------------------------------------
    
    duty_cycle <- tryCatch(
      {
        
        get_deployment_duty_cycle(
          bird_row = bird_row,
          duty_cycle_col =
            required_duty_cycle_col
        )
        
      },
      error = function(e) {
        
        message(
          "  ⚠️ Skipping deployment: ",
          conditionMessage(e)
        )
        
        NA_real_
      }
    )
    
    
    if (is.na(duty_cycle)) {
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "Invalid or missing burst interval"
      )
      
      next
    }
    
    
    # Expected number of valid intervals required to retain an hourly estimate.
    
    min_required_samples <-
      (3600 / duty_cycle) *
      sample_size_threshold
    
    
    message(
      "  Burst interval: ",
      duty_cycle,
      " s"
    )
    
    
    message(
      "  Minimum valid intervals per hour: ",
      ceiling(
        min_required_samples
      )
    )
    
    
    # --------------------------------------------------------------------------
    # Determine local timezone
    # --------------------------------------------------------------------------
    
    tz_local <- tryCatch(
      {
        
        get_local_timezone(
          lat = bird_row$Lat[1],
          lon = bird_row$Lon[1]
        )
        
      },
      error = function(e) {
        
        message(
          "  ⚠️ Skipping deployment: ",
          conditionMessage(e)
        )
        
        NA_character_
      }
    )
    
    
    if (is.na(tz_local)) {
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "Could not determine local timezone"
      )
      
      next
    }
    
    
    message(
      "  Local timezone: ",
      tz_local
    )
    
    
    # --------------------------------------------------------------------------
    # Determine deployment end date
    # --------------------------------------------------------------------------
    
    deployment_end <- get_deployment_end(
      bird_deployments =
        bird_deployments,
      bird_row =
        bird_row,
      i =
        i
    )
    
    
    # --------------------------------------------------------------------------
    # Restrict detections to this biological deployment
    # --------------------------------------------------------------------------
    
    data <- filter_to_deployment_window(
      data_raw =
        data_raw,
      bird_row =
        bird_row,
      deployment_end =
        deployment_end,
      tz_local =
        tz_local
    )
    
    
    if (nrow(data) == 0) {
      
      message(
        "  ⚠️ Skipping deployment: no detections within deployment window."
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "No detections within deployment window"
      )
      
      next
    }
    
    
    message(
      "  Detections within deployment: ",
      format(
        nrow(data),
        big.mark = ","
      )
    )
    
    
    # --------------------------------------------------------------------------
    # Restrict detections to the biological receiver site
    # --------------------------------------------------------------------------
    
    site_result <- filter_to_site_receivers(
      data =
        data,
      multi_receiver_sites =
        multi_receiver_sites
    )
    
    
    if (
      is.null(site_result) ||
      nrow(site_result$data) == 0
    ) {
      
      message(
        "  ⚠️ Skipping deployment: no receiver-site detections remained."
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "No receiver-site detections remained"
      )
      
      next
    }
    
    
    data <- site_result$data
    
    
    message(
      "  Focal receiver: ",
      site_result$top_recv_name
    )
    
    
    message(
      "  Receivers retained: ",
      paste(
        site_result$site_receivers,
        collapse = ", "
      )
    )
    
    
    # --------------------------------------------------------------------------
    # Collapse simultaneous detections into transmission events
    # --------------------------------------------------------------------------
    
    data_clean <- select_strongest_transmission_events(
      data =
        data,
      event_tolerance =
        transmission_event_tolerance
    )
    
    
    if (nrow(data_clean) == 0) {
      
      message(
        "  ⚠️ Skipping deployment: no transmission events remained."
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "No transmission events remained"
      )
      
      next
    }
    
    
    # --------------------------------------------------------------------------
    # Assign detections to diel periods
    # --------------------------------------------------------------------------
    
    data_clean <- info_fast(
      df =
        data_clean,
      lat =
        bird_row$Lat[1],
      lon =
        bird_row$Lon[1],
      tz_local =
        tz_local
    )
    
    
    # --------------------------------------------------------------------------
    # Attach receiver-type thresholds and quality-control parameters
    # --------------------------------------------------------------------------
    
    data_clean <- attach_receiver_parameters(
      data_clean =
        data_clean,
      tower_long =
        tower_long,
      tower_type_thresholds =
        tower_type_thresholds,
      parameter_lookup =
        parameter_lookup
    )
    
    
    if (is.null(data_clean)) {
      
      message(
        "  ⚠️ Skipping deployment because receiver thresholds or metadata are missing."
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "Missing receiver metadata or thresholds"
      )
      
      next
    }
    
    
    # Ensure receiver-specific classification parameters are available.
    
    required_parameter_cols <- c(
      "lower_ratio",
      "upper_ratio",
      "lower_db",
      "upper_db",
      "tolerance",
      "S2N_cutoff"
    )
    
    
    missing_parameter_cols <- required_parameter_cols[
      !required_parameter_cols %in%
        names(data_clean)
    ]
    
    
    if (length(missing_parameter_cols) > 0) {
      
      message(
        "  ⚠️ Skipping deployment: missing classification parameter column(s): ",
        paste(
          missing_parameter_cols,
          collapse = ", "
        )
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "Missing classification parameters"
      )
      
      next
    }
    
    
    if (
      any(is.na(data_clean$lower_ratio)) ||
      any(is.na(data_clean$upper_ratio))
    ) {
      
      message(
        "  ⚠️ Skipping deployment: one or more detections lack receiver thresholds."
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "Incomplete receiver thresholds"
      )
      
      next
    }
    
    
    # --------------------------------------------------------------------------
    # Optional stationary-tag screen
    # --------------------------------------------------------------------------
    
    stationary_screen <- NULL
    
    
    if (run_stationary_tag_screen) {
      
      stationary_screen <- screen_stationary_tag_deployment(
        data_clean =
          data_clean,
        
        duty_cycle =
          duty_cycle,
        
        screen_timing =
          stationary_screen_timing,
        
        late_window_hours =
          stationary_late_window_hours,
        
        receiver_selection_hours =
          stationary_receiver_selection_hours,
        
        min_valid_late =
          stationary_min_valid_late,
        
        min_prop_within =
          stationary_min_prop_within,
        
        max_mean_abs_sigdif =
          stationary_max_mean_abs_sigdif,
        
        min_receiver_prop =
          stationary_min_receiver_prop,
        
        Band =
          Band,
        
        MotusTagID =
          MotusTagID,
        
        mfgID =
          mfgID
      )
      
      
      if (is.null(stationary_screen)) {
        
        message(
          "  ⚪ Stationary-tag screen not evaluated: ",
          "insufficient qualifying detections."
        )
        
      } else if (
        isTRUE(
          stationary_screen$summary$
          flagged_possible_stationary_tag
        )
      ) {
        
        message(
          "  ⚠️ Stationary-tag screen FLAGGED this deployment. ",
          "Manual review is recommended."
        )
        
      } else {
        
        message(
          "  ✅ Stationary-tag screen did not flag this deployment."
        )
      }
    }
    
    
    # --------------------------------------------------------------------------
    # Summarize deployment thresholds for diagnostic plot labels
    # --------------------------------------------------------------------------
    
    threshold_summary <-
      summarize_deployment_thresholds(
        data_clean
      )
    
    
    lower_ratio <-
      threshold_summary$lower_ratio
    
    upper_ratio <-
      threshold_summary$upper_ratio
    
    lower_db <-
      threshold_summary$lower_db
    
    upper_db <-
      threshold_summary$upper_db
    
    
    # --------------------------------------------------------------------------
    # Classify activity
    # --------------------------------------------------------------------------
    
    df_classified <- classify_detection_activity(
      data_clean =
        data_clean,
      duty_cycle =
        duty_cycle
    )
    
    
    if (nrow(df_classified) == 0) {
      
      message(
        "  ⚠️ Skipping deployment: no classified detections were returned."
      )
      
      
      deployment_results[[i]] <- tibble::tibble(
        MotusTagID = MotusTagID,
        mfgID = mfgID,
        Band = Band,
        status = "skipped",
        message = "No classified detections returned"
      )
      
      next
    }
    
    
    # Reattach tower type for downstream diagnostics and plotting.
    
    receiver_info <- get_receiver_thresholds(
      data =
        df_classified,
      tower_long =
        tower_long,
      tower_type_thresholds =
        tower_type_thresholds
    ) %>%
      dplyr::select(
        recvDeployName,
        date_time_local,
        tower_type
      ) %>%
      dplyr::distinct()
    
    
    df_classified <- df_classified %>%
      dplyr::left_join(
        receiver_info,
        by = c(
          "recvDeployName",
          "date_time_local"
        ),
        relationship = "many-to-one"
      )
    
    
    # --------------------------------------------------------------------------
    # Identify dominant antenna port for diagnostic plots
    # --------------------------------------------------------------------------
    
    dominant_port <- df_classified %>%
      dplyr::filter(
        !is.na(top_port)
      ) %>%
      dplyr::count(
        top_port,
        sort = TRUE
      ) %>%
      dplyr::slice_head(
        n = 1
      ) %>%
      dplyr::pull(
        top_port
      )
    
    
    if (length(dominant_port) == 0) {
      
      dominant_port <- NA
    }
    
    
    # --------------------------------------------------------------------------
    # Summarize hourly activity
    # --------------------------------------------------------------------------
    
    activity_hourly <- summarize_hourly_activity(
      df_classified =
        df_classified,
      
      min_required_samples =
        min_required_samples,
      
      MotusTagID =
        MotusTagID,
      
      mfgID =
        mfgID,
      
      Band =
        Band,
      
      dataset_label =
        dataset_label
    )
    
    
    if (nrow(activity_hourly) == 0) {
      
      message(
        "  ⚠️ No hourly observations met the minimum coverage requirement."
      )
    }
    
    
    activity_hourly_summary <-
      summarize_hourly_across_days(
        activity_hourly
      )
    
    
    # --------------------------------------------------------------------------
    # Create output directory and file stem
    # --------------------------------------------------------------------------
    
    output_dir <- file.path(
      processed_dir,
      paste0(
        MotusTagID,
        "_",
        mfgID,
        "_Band",
        Band,
        "_",
        dataset_label,
        "_classified"
      )
    )
    
    
    dir.create(
      output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    
    out_stem <- paste0(
      MotusTagID,
      "_",
      mfgID,
      "_Band",
      Band,
      "_",
      dataset_label
    )    
    
    # --------------------------------------------------------------------------
    # Prepare classified detections for CSV export
    # --------------------------------------------------------------------------
    
    df_classified_export <- df_classified %>%
      dplyr::mutate(
        date_time_utc =
          format(
            lubridate::with_tz(
              date_time_local,
              "UTC"
            ),
            "%Y-%m-%d %H:%M:%OS3",
            tz = "UTC"
          ),
        
        date_time_local_readable =
          format(
            date_time_local,
            "%Y-%m-%d %H:%M:%OS3",
            tz = tz_local
          ),
        
        date_local =
          as.Date(
            date_time_local
          ),
        
        hour_local =
          lubridate::hour(
            date_time_local
          ),
        
        timezone_local =
          tz_local,
        
        MotusTagID =
          MotusTagID,
        
        mfgID =
          mfgID,
        
        Band =
          Band,
        
        dataset_label =
          dataset_label
      )
    
    
    # --------------------------------------------------------------------------
    # Save activity tables
    # --------------------------------------------------------------------------
    
    save_activity_tables(
      output_dir =
        output_dir,
      
      out_stem =
        out_stem,
      
      df_classified =
        df_classified_export,
      
      activity_hourly =
        activity_hourly,
      
      activity_hourly_summary =
        activity_hourly_summary
    )
    
    
    message(
      "  ✅ Activity tables saved."
    )
    
    
    # --------------------------------------------------------------------------
    # Save diagnostic plots
    # --------------------------------------------------------------------------
    
    plot_dir <- file.path(
      output_dir,
      "plots"
    )
    
    
    make_and_save_plots(
      df_classified =
        df_classified,
      
      activity_hourly =
        activity_hourly,
      
      activity_hourly_summary =
        activity_hourly_summary,
      
      dominant_port =
        dominant_port,
      
      duty_cycle =
        duty_cycle,
      
      lower_ratio =
        lower_ratio,
      
      upper_ratio =
        upper_ratio,
      
      lower_db =
        lower_db,
      
      upper_db =
        upper_db,
      
      MotusTagID =
        MotusTagID,
      
      mfgID =
        mfgID,
      
      deployment_suffix =
        paste0(
          "_Band",
          Band
        ),
      
      bird_row =
        bird_row,
      
      tz_local =
        tz_local,
      
      plot_dir =
        plot_dir
    )
    
    
    message(
      "  ✅ Diagnostic plots saved."
    )
    
    
    # --------------------------------------------------------------------------
    # Save stationary-screen outputs
    # --------------------------------------------------------------------------
    
    if (
      run_stationary_tag_screen &&
      !is.null(stationary_screen)
    ) {
      
      save_stationary_tag_screen(
        stationary_screen =
          stationary_screen,
        
        output_dir =
          output_dir,
        
        plot_dir =
          plot_dir,
        
        out_stem =
          out_stem
      )
      
      
      message(
        "  ✅ Stationary-tag screening outputs saved."
      )
    }
    
    
    # --------------------------------------------------------------------------
    # Record successful deployment
    # --------------------------------------------------------------------------
    
    deployment_results[[i]] <- tibble::tibble(
      MotusTagID =
        MotusTagID,
      
      mfgID =
        mfgID,
      
      Band =
        Band,
      
      dataset_label =
        dataset_label,
      
      duty_cycle =
        duty_cycle,
      
      n_raw_deployment_detections =
        nrow(data),
      
      n_transmission_events =
        nrow(data_clean),
      
      n_classified_rows =
        nrow(df_classified),
      
      n_retained_hourly_periods =
        nrow(activity_hourly),
      
      stationary_screen_evaluated =
        !is.null(stationary_screen),
      
      stationary_screen_flagged =
        if (
          is.null(stationary_screen)
        ) {
          NA
        } else {
          isTRUE(
            stationary_screen$summary$
              flagged_possible_stationary_tag
          )
        },
      
      status =
        "completed",
      
      message =
        NA_character_,
      
      output_dir =
        output_dir
    )
    
    
    message(
      "  ✅ Deployment complete."
    )
  }
  
  
  # ============================================================================
  # 5) RETURN TAG-DATASET PROCESSING SUMMARY
  # ============================================================================
  
  processing_summary <- dplyr::bind_rows(
    deployment_results
  )
  
  
  if (nrow(processing_summary) == 0) {
    
    warning(
      "No biological deployments were successfully evaluated for ",
      "MotusTagID ", MotusTagID,
      " × mfgID ", mfgID,
      "."
    )
    
    return(
      processing_summary
    )
  }
  
  
  message(
    "\n✅ Finished tag dataset ",
    MotusTagID,
    " × ",
    mfgID,
    "."
  )
  
  
  message(
    "   Completed deployments: ",
    sum(
      processing_summary$status ==
        "completed"
    )
  )
  
  
  message(
    "   Skipped deployments: ",
    sum(
      processing_summary$status ==
        "skipped"
    )
  )
  
  
  processing_summary
}