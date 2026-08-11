###############################################################################
# FILE: functions_activity.R
#
# PURPOSE:
#   1) Functions to calculate activity and timing
#
# AUTHOR: Lauren Brunk
###############################################################################

# FUNCTION: classify_activity
# ==============================================================================
# PURPOSE
# ==============================================================================
# Classify biologically meaningful activity from Motus telemetry data using:
#   (1) proportional signal-strength change beyond receiver-specific thresholds,
#   (2) antenna (port) switching,
#   (3) receiver switching,
#
# while guarding against false movement caused by:
#   - missed detections,
#   - poor signal quality,
#   - short-term signal dropouts.
#
# DESIGN PRINCIPLES
# ------------------------------------------------------------------------------
# • Classification is ONLY evaluated on valid consecutive detections
# • Structural changes (port / receiver) require timing validity
# • Dropouts are corrected conservatively after classification
# • All derived columns are retained for diagnostics & plotting
# • sig_diff (dB) is retained for interpretability and plots
# • sig_ratio is used for the actual threshold classification
# ==============================================================================

classify_activity <- function(df,
                              duty_cycle,
                              upper_ratio,
                              lower_ratio) {
  
  # ============================================================================
  # PHASE 0 — SEMANTIC CONSTANTS & COLUMN DISCOVERY
  # ============================================================================
  
  sig_cols   <- grep("^sig_",   names(df), value = TRUE)
  noise_cols <- grep("^noise_", names(df), value = TRUE)
  
  if (length(sig_cols) == 0)
    stop("❌ No sig_X columns found in dataframe.")
  
  # ============================================================================
  # PHASE 1 — SORT BY TIME
  # ============================================================================
  
  df <- df %>%
    arrange(date_time_local)
  
  # ============================================================================
  # PHASE 2 — STRONGEST ANTENNA RESOLUTION (PER detection)
  # ============================================================================
  
  sig_matrix   <- as.matrix(df[sig_cols])
  noise_matrix <- as.matrix(df[noise_cols])
  
  sig_matrix[!is.finite(sig_matrix)] <- -Inf
  
  top_port <- max.col(sig_matrix, ties.method = "first")
  
  df <- df %>%
    mutate(
      top_port = top_port,
      Signal   = sig_matrix[cbind(seq_len(n()), top_port)],
      Noise    = noise_matrix[cbind(seq_len(n()), top_port)]
    )
  
  # ============================================================================
  # PHASE 3 — TEMPORAL STRUCTURE
  # ============================================================================
  
  df <- df %>%
    mutate(
      time_num = as.numeric(date_time_local),
      lag_time_num = lag(time_num),
      time_dif = time_num - lag_time_num
    )
  
  # ============================================================================
  # PHASE 4 — VALID CONSECUTIVE PAIRS
  # ============================================================================
  
  df <- df %>%
    mutate(
      tolerance_allowed = if (!"tolerance" %in% names(.)) {
        stop("❌ Missing required tolerance column.")
      } else {
        tolerance
      },
      valid_pair =
        !is.na(time_dif) &
        abs(time_dif - duty_cycle) <= tolerance_allowed
    )
  
  # ============================================================================
  # PHASE 5 — SIGNAL QUALITY
  # ============================================================================
  
  df <- df %>%
    mutate(
      lag_Signal   = lag(Signal),
      lag_Noise    = lag(Noise),
      lag_port     = lag(top_port),
      lag_receiver = lag(recvDeployName),
      
      S2N     = Signal - Noise,
      lag_S2N = lag(S2N),
      
      good_signal =
        valid_pair &
        !is.na(lag_Signal) &
        !is.na(S2N) &
        !is.na(lag_S2N) &
        S2N >= S2N_cutoff &
        lag_S2N >= S2N_cutoff,
      
      activity_denominator = good_signal
    )
  
  # ============================================================================
  # PHASE 6 — dB DIFFERENCE, THEN PROPORTIONAL CHANGE
  # ============================================================================
  # This mirrors the threshold-calculation code:
  #   1) calculate sig_diff in dB
  #   2) convert to proportional change
  #   3) compare proportional change to lower_ratio / upper_ratio
  
  df <- df %>%
    mutate(
      sig_diff = if_else(
        good_signal,
        Signal - lag_Signal,
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
  
  # ============================================================================
  # PHASE 7 — PROPORTIONAL-THRESHOLD CLASSIFICATION
  # ============================================================================
  
  df <- df %>%
    mutate(
      within_threshold = if_else(
        !is.na(sig_ratio),
        sig_ratio >= lower_ratio & sig_ratio <= upper_ratio,
        FALSE,
        missing = FALSE
      ),
      
      movement_ratio = if_else(
        !is.na(sig_ratio) & (sig_ratio < lower_ratio | sig_ratio > upper_ratio),
        TRUE,
        FALSE,
        missing = FALSE
      )
    )
  
  # ============================================================================
  # PHASE 8 — STRUCTURAL MOVEMENT INDICATORS
  # ============================================================================
  
  df <- df %>%
    mutate(
      port_change =
        good_signal &
        !is.na(lag_port) &
        top_port != lag_port,
      
      receiver_change =
        valid_pair &
        !is.na(lag_receiver) &
        recvDeployName != lag_receiver
    )
  
  # ============================================================================
  # PHASE 9 — INITIAL ACTIVITY DECISION
  # ============================================================================
  
  df <- df %>%
    mutate(
      active =
        coalesce(movement_ratio, FALSE) |
        coalesce(port_change, FALSE) |
        coalesce(receiver_change, FALSE)
    )
  
  # ============================================================================
  # PHASE 10 — DROPOUT CORRECTION USING PROPORTIONAL THRESHOLDS
  # ============================================================================
  # Goal:
  #   Reclassify obvious one-detection anomalies as inactive when the neighboring
  #   detections are stable and from the same antenna/receiver context.
  #
  # Logic:
  #   - look one detection backward and forward
  #   - compare neighbors directly on the proportional scale
  #   - if neighbors are similar enough, treat the middle detection as a dropout
  #
  # This mirrors the spirit of your earlier conservative dropout correction,
  # but now uses proportional change rather than dB thresholds.
  
  df <- df %>%
    mutate(
      lead_Signal   = lead(Signal),
      lead_port     = lead(top_port),
      lead_receiver = lead(recvDeployName),
      
      # proportional change between the two neighboring detections
      neighbor_ratio = if_else(
        !is.na(lag_Signal) & !is.na(lead_Signal),
        10^((lead_Signal - lag_Signal) / 10),
        NA_real_
      ),
      
      # neighbors are considered stable if their proportional difference
      # falls within the same proportional threshold envelope
      neighbors_stable = if_else(
        !is.na(neighbor_ratio),
        neighbor_ratio >= lower_ratio & neighbor_ratio <= upper_ratio,
        FALSE,
        missing = FALSE
      ),
      
      dropout_fix =
        (
          # current detection is missing/invalid OR differs from both neighbors in port
          !is.finite(Signal) |
            (
              !is.na(lag_port) & !is.na(lead_port) &
                top_port != lag_port &
                top_port != lead_port
            )
        ) &
        # neighbors agree structurally
        !is.na(lag_port) & !is.na(lead_port) &
        lag_port == lead_port &
        !is.na(lag_receiver) & !is.na(lead_receiver) &
        lag_receiver == lead_receiver &
        # neighbors are also stable on the proportional scale
        neighbors_stable,
      
      active = if_else(dropout_fix, FALSE, active),
      active = if_else(lag(dropout_fix, default = FALSE), FALSE, active)
    )
  
  # ============================================================================
  # PHASE 11 — TEMPORAL METADATA
  # ============================================================================
  
  df <- df %>%
    mutate(
      hour = lubridate::hour(date_time_local),
      date = as.Date(date_time_local)
    )
  
  return(df)
}

# ------------------------------------------------------------------------------
# FUNCTION: info_fast
# ------------------------------------------------------------------------------
#
# PURPOSE
# ------------------------------------------------------------------------------
# Assign biologically meaningful diel timing categories to detections based on:
#
#   • deployment latitude
#   • deployment longitude
#   • local time zone
#   • daily sun position
#
# The function calculates civil dawn, nautical dawn, nautical dusk,
# and civil dusk for each date, then assigns every detection to a
# standardized diel period.
#
# TIMING DEFINITIONS
# ------------------------------------------------------------------------------
# dawn
#   civil dawn → nautical dawn
#
# day
#   nautical dawn → nautical dusk
#
# dusk
#   nautical dusk → civil dusk
#
# night_1
#   civil dusk → midnight
#
# night_2
#   midnight → civil dawn
#
# Night is intentionally split into two categories so detections remain
# chronologically ordered across midnight while still representing one
# biological nighttime period.
#
# DESIGN PRINCIPLES
# ------------------------------------------------------------------------------
# • Timing is calculated in the bird's local time zone
# • Solar events are calculated separately for each date
# • Nighttime is split into night_1 and night_2 to preserve temporal order
# • Moon information is retained for potential downstream analyses
# • Original detection timestamps are preserved
#
# OUTPUT
# ------------------------------------------------------------------------------
# Returns the original dataframe plus:
#
#   date
#   civilDawn
#   nauticalDawn
#   nauticalDusk
#   civilDusk
#   fraction      (moon illumination fraction)
#   altitude      (moon altitude)
#   timing        (dawn/day/dusk/night_1/night_2)
#
# ------------------------------------------------------------------------------

info_fast <- function(df, lat, lon, tz_local) {
  
  # ============================================================================
  # PHASE 1 — STANDARDIZE LOCAL DATETIME
  # ============================================================================
  # Convert timestamps into the deployment's local time zone and
  # create a local date for solar calculations.
  
  df2 <- df %>%
    mutate(
      date_time_local = lubridate::with_tz(date_time_local, tzone = tz_local),
      localtime = date_time_local,
      date = as.Date(date_time_local, tz = tz_local)
    )
  
  # ============================================================================
  # PHASE 2 — CALCULATE DAILY SUN & MOON INFORMATION
  # ============================================================================
  # Solar events are calculated once per date and later joined back
  # to the detection-level dataframe.
  
  unique_dates <- unique(df2$date)
  
  sun_moon_df <- tibble(date = unique_dates) %>%
    mutate(
      
      # ------------------------------------------------------------------------
      # Solar timing
      # ------------------------------------------------------------------------
      
      sun = purrr::map(
        date,
        ~ getSunlightTimes(
          date = .x,
          lat  = lat,
          lon  = lon,
          keep = c(
            "dawn",          # civil dawn
            "nauticalDawn",
            "nauticalDusk",
            "dusk"           # civil dusk
          ),
          tz = tz_local
        ) %>%
          dplyr::select(-date)
      ),
      
      # ------------------------------------------------------------------------
      # Lunar information (retained for downstream analyses)
      # ------------------------------------------------------------------------
      
      moon = purrr::map(date, ~ getMoonIllumination(.x)),
      moonpos = purrr::map(date, ~ getMoonPosition(.x, lat = lat, lon = lon))
    ) %>%
    
    tidyr::unnest(sun) %>%
    
    mutate(
      
      # ------------------------------------------------------------------------
      # Convert all solar events into local timezone
      # ------------------------------------------------------------------------
      
      civilDawn    = lubridate::with_tz(dawn, tz_local),
      nauticalDawn = lubridate::with_tz(nauticalDawn, tz_local),
      nauticalDusk = lubridate::with_tz(nauticalDusk, tz_local),
      civilDusk    = lubridate::with_tz(dusk, tz_local),
      
      # ------------------------------------------------------------------------
      # Extract moon metrics
      # ------------------------------------------------------------------------
      
      fraction = purrr::map_dbl(moon, ~ .x$fraction),
      altitude = purrr::map_dbl(moonpos, ~ .x$altitude)
    ) %>%
    
    select(
      date,
      civilDawn,
      nauticalDawn,
      nauticalDusk,
      civilDusk,
      fraction,
      altitude
    )
  
  # ============================================================================
  # PHASE 3 — JOIN SOLAR DATA TO DETECTIONS
  # ============================================================================
  # Each detection receives the solar and lunar information associated
  # with its local date.
  
  df3 <- df2 %>%
    left_join(sun_moon_df, by = "date")
  
  # ============================================================================
  # PHASE 4 — ASSIGN DIEL TIMING CATEGORIES
  # ============================================================================
  # Timing periods:
  #
  # dawn    = nautical dawn → civil dawn 
  # day     = civil dawn → civil dusk
  # dusk    = civil dusk → nautical dusk
  # night_1 = nautical dusk → midnight
  # night_2 = midnight → nautical dawn
  
  df3 <- df3 %>%
    mutate(
      timing = case_when(
        
        date_time_local >= nauticalDawn &
          date_time_local < civilDawn ~ "dawn",
        
        date_time_local >= civilDawn &
          date_time_local < civilDusk ~ "day",
        
        date_time_local >= civilDusk &
          date_time_local < nauticalDusk ~ "dusk",
        
        date_time_local >= nauticalDusk ~ "night_1",
        
        date_time_local < nauticalDawn ~ "night_2",
        
        TRUE ~ NA_character_
      )
    )

  # ============================================================================
  # PHASE 5 — QUALITY CHECK
  # ============================================================================
  # Any NA timing values usually indicate timezone problems,
  # coordinate issues, or missing solar calculations.
  
  if (any(is.na(df3$timing))) {
    warning(
      "⚠️ Some rows have NA timing. Check date_time_local, ",
      "coordinates, timezone, or solar calculations."
    )
  }
  
  # ============================================================================
  # PHASE 6 — RETURN RESULTS
  # ============================================================================
  
  return(df3)
}

# ==============================================================================
# TRANSMISSION-EVENT PROCESSING
# ==============================================================================

# ------------------------------------------------------------------------------
# select_strongest_transmission_events()
#
# Group nearly simultaneous detections of the same transmitter burst and retain
# the detection with the strongest received signal.
#
# Automated receiver networks may record the same transmitter burst on multiple
# antennas or nearby receivers at slightly different timestamps. These duplicate
# detections should represent one transmission event rather than independent
# observations.
#
# Event grouping is anchored to the first detection in each event. A subsequent
# detection is assigned to the current event only when its timestamp occurs
# within `event_tolerance` seconds of that event's first detection.
#
# Anchoring events this way prevents "chaining." For example, with a 0.3-s
# tolerance, detections at 0.00, 0.25, and 0.50 s will not all be grouped into
# one event simply because each adjacent pair is separated by only 0.25 s.
#
# After events are assigned, only the detection with the greatest signal
# strength (`sig`) is retained from each event.
#
# Parameters
# ----------
# data :
#   Detection data containing `date_time_local` and `sig`.
#
# event_tolerance :
#   Maximum elapsed time, in seconds, from the first detection in an event.
#
# Returns
# -------
# One row per transmission event, ordered chronologically.
# ------------------------------------------------------------------------------

select_strongest_transmission_events <- function(
    data,
    event_tolerance = 0.3
) {
  
  # ---------------------------------------------------------------------------
  # Basic validation
  # ---------------------------------------------------------------------------
  
  required_cols <- c(
    "date_time_local",
    "sig"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(data)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "`select_strongest_transmission_events()` requires column(s): ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  if (
    length(event_tolerance) != 1 ||
    !is.numeric(event_tolerance) ||
    is.na(event_tolerance) ||
    event_tolerance < 0
  ) {
    stop(
      "`event_tolerance` must be one non-negative number in seconds."
    )
  }
  
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  
  # Remove rows lacking a usable timestamp.
  #
  # These cannot be assigned to transmission events.
  
  n_missing_time <- sum(
    is.na(
      data$date_time_local
    )
  )
  
  if (n_missing_time > 0) {
    warning(
      n_missing_time,
      " detection(s) with missing `date_time_local` were removed ",
      "before transmission-event grouping."
    )
  }
  
  
  x <- data %>%
    dplyr::filter(
      !is.na(
        date_time_local
      )
    ) %>%
    dplyr::arrange(
      date_time_local
    )
  
  
  if (nrow(x) == 0) {
    return(x)
  }
  
  
  # ---------------------------------------------------------------------------
  # Assign transmission events
  # ---------------------------------------------------------------------------
  
  event_time <- as.numeric(
    x$date_time_local
  )
  
  event_id <- integer(
    length(event_time)
  )
  
  
  # First detection begins the first event.
  
  current_event <- 1L
  
  event_id[1] <- current_event
  
  event_start_time <- event_time[1]
  
  
  if (length(event_time) > 1) {
    
    for (i in 2:length(event_time)) {
      
      # Begin a new event when the current detection occurs more than the
      # permitted tolerance after the FIRST detection in the current event.
      
      if (
        is.na(event_time[i]) ||
        event_time[i] - event_start_time > event_tolerance
      ) {
        
        current_event <- current_event + 1L
        
        event_start_time <- event_time[i]
      }
      
      
      event_id[i] <- current_event
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Retain strongest detection from each event
  # ---------------------------------------------------------------------------
  
  x %>%
    dplyr::mutate(
      transmission_event = event_id
    ) %>%
    dplyr::group_by(
      transmission_event
    ) %>%
    dplyr::slice_max(
      order_by = sig,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      date_time_local
    ) %>%
    dplyr::select(
      -transmission_event
    )
}