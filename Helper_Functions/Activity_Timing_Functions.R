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
      time_dif = as.numeric(difftime(
        date_time_local,
        lag(date_time_local),
        units = "secs"
      ))
    )
  
  # ============================================================================
  # PHASE 4 — VALID CONSECUTIVE PAIRS
  # ============================================================================
  
  df <- df %>%
    mutate(
      valid_pair =
        !is.na(time_dif) &
        abs(time_dif - duty_cycle) <= tolerance
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
        lag_S2N >= S2N_cutoff
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
        valid_pair &
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

info_fast <- function(df, lat, lon, tz_local) {
  
  df2 <- df %>%
    mutate(
      localtime = as.POSIXlt(date_time_local, tz = tz_local),
      date      = as.Date(localtime)
    )
  
  unique_dates <- unique(df2$date)
  
  sun_moon_df <- tibble(date = unique_dates) %>%
    mutate(
      sun = purrr::map(date, ~ getSunlightTimes(
        date = .x,
        lat  = lat,
        lon  = lon,
        keep = c("nauticalDawn", "sunrise", "sunset", "nauticalDusk"),
        tz   = tz_local
      )),
      moon = purrr::map(date, ~ getMoonIllumination(.x)),
      moonpos = purrr::map(date, ~ getMoonPosition(.x, lat = lat, lon = lon))
    ) %>%
    mutate(
      nauticalDawn = map_chr(sun, ~ as.character(.x$nauticalDawn)),
      sunrise      = map_chr(sun, ~ as.character(.x$sunrise)),
      sunset       = map_chr(sun, ~ as.character(.x$sunset)),
      nauticalDusk = map_chr(sun, ~ as.character(.x$nauticalDusk)),
      fraction     = map_dbl(moon,    ~ .x$fraction),
      altitude     = map_dbl(moonpos, ~ .x$altitude)
    ) %>%
    select(-sun, -moon, -moonpos) %>%
    mutate(across(
      c(nauticalDawn, sunrise, sunset, nauticalDusk),
      ~ as.POSIXct(.x, tz = tz_local)
    ))
  
  df3 <- df2 %>%
    left_join(sun_moon_df, by = "date")
  
  df3 <- df3 %>%
    mutate(
      timing = case_when(
        localtime >= nauticalDawn & localtime <  sunrise      ~ "dawn",
        localtime >= sunrise      & localtime <= sunset       ~ "day",
        localtime >  sunset       & localtime <= nauticalDusk ~ "dusk",
        localtime >  nauticalDusk                             ~ "night_1",
        localtime <  nauticalDawn                             ~ "night_2",
        TRUE ~ NA_character_
      )
    )
  
  if (any(is.na(df3$timing))) {
    warning("⚠️ Some rows have NA timing (missing sun/moon times).")
  }
  
  return(df3)
}