###############################################################################
# FILE: plot_activity_suite.R
#
# PURPOSE:
#   Centralized plotting utilities for Motus activity classification outputs,
#   using proportional signal change for all threshold-based signal plots.
#
# AUTHOR: Lauren Brunk
###############################################################################

# =============================================================================
# PHASE 0 — LIBRARIES
# =============================================================================

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(purrr)
library(suncalc)

# =============================================================================
# PHASE 1 — GLOBAL CONSTANTS
# =============================================================================

SECONDS_PER_DAY  <- 86400
SECONDS_PER_HOUR <- 3600

TIMING_LEVELS <- c(
  "night_2",
  "dawn",
  "day",
  "dusk",
  "night_1"
)

# =============================================================================
# PHASE 2 — SHARED THEME
# =============================================================================

theme_woth <- function() {
  theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 15),
      plot.subtitle = element_text(hjust = 0, size = 11),
      plot.caption = element_text(size = 9, hjust = 0),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold")
    )
}

# =============================================================================
# PHASE 3 — HELPER FUNCTIONS
# =============================================================================

snap_interval <- function(x, centers, tol = 2) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    diffs <- abs(v - centers)
    if (min(diffs, na.rm = TRUE) <= tol) centers[which.min(diffs)] else NA_real_
  })
}

make_plot_id <- function(MotusTagID, mfgID, deployment_suffix) {
  paste0(MotusTagID, "_", mfgID, deployment_suffix)
}

make_deployment_subtitle <- function(Band, Date_tagged, Date_end) {
  start_str <- format(Date_tagged, "%b %d, %Y")
  end_str <- ifelse(
    is.na(Date_end),
    "present",
    format(Date_end, "%b %d, %Y")
  )
  paste0(
    "Band ", Band,
    " | Deployment: ",
    start_str, " – ", end_str
  )
}

make_det_daily <- function(data_clean, dominant_port, duty_cycle) {
  
  data_clean %>%
    filter(top_port == dominant_port) %>%
    mutate(date = as.Date(date_time_local)) %>%
    distinct(date_time_local, .keep_all = TRUE) %>%
    group_by(date) %>%
    summarise(
      observed = n(),
      .groups = "drop"
    ) %>%
    mutate(
      expected = SECONDS_PER_DAY / duty_cycle
    )
}

# -----------------------------------------------------------------------------
# Build proportional-change plotting data
# -----------------------------------------------------------------------------
make_signal_ratio_by_tower_port <- function(df_classified) {
  
  df_classified %>%
    filter(
      is.finite(sig_ratio),
      !is.na(top_port),
      !is.na(recvDeployName)
    ) %>%
    mutate(
      tower_port = paste(recvDeployName, top_port, sep = " : ")
    )
}

# =============================================================================
# daily timing fractions by receiver × selected port
# =============================================================================
make_det_daily_timing_by_tower_port <- function(
    df_classified,
    duty_cycle,
    lat,
    lon,
    tz_local
) {
  
  df <- df_classified %>%
    filter(!is.na(recvDeployName), !is.na(top_port)) %>%
    mutate(
      date = lubridate::as_date(date_time_local, tz = tz_local),
      timing = as.character(timing),
      tower_port = paste(recvDeployName, top_port, sep = " : ")
    )
  
  if (nrow(df) == 0) return(df[0, ])
  
  unique_dates <- df %>%
    distinct(date) %>%
    pull(date)
  
  tower_ports <- df %>%
    distinct(recvDeployName, top_port, tower_port)
  
  sun_df <- tibble(date = unique_dates) %>%
    mutate(
      sun = purrr::map(
        date,
        ~ getSunlightTimes(
          date = .x,
          lat = lat,
          lon = lon,
          keep = c("nauticalDawn", "sunrise", "sunset", "nauticalDusk"),
          tz = tz_local
        ) %>%
          select(-date)
      )
    ) %>%
    tidyr::unnest(sun) %>%
    mutate(
      local_midnight = lubridate::ymd_hms(
        paste(date, "00:00:00"),
        tz = tz_local
      ),
      next_midnight = local_midnight + lubridate::days(1),
      
      day_seconds = as.numeric(difftime(sunset, sunrise, units = "secs")),
      dawn_seconds = as.numeric(difftime(sunrise, nauticalDawn, units = "secs")),
      dusk_seconds = as.numeric(difftime(nauticalDusk, sunset, units = "secs")),
      night1_seconds = as.numeric(difftime(next_midnight, nauticalDusk, units = "secs")),
      night2_seconds = as.numeric(difftime(nauticalDawn, local_midnight, units = "secs"))
    ) %>%
    select(date, day_seconds, dawn_seconds, dusk_seconds, night1_seconds, night2_seconds) %>%
    pivot_longer(
      cols = ends_with("_seconds"),
      names_to = "timing",
      values_to = "seconds_available"
    ) %>%
    mutate(
      timing = case_when(
        timing == "day_seconds" ~ "day",
        timing == "dawn_seconds" ~ "dawn",
        timing == "dusk_seconds" ~ "dusk",
        timing == "night1_seconds" ~ "night_1",
        timing == "night2_seconds" ~ "night_2",
        TRUE ~ NA_character_
      ),
      timing = factor(timing, levels = TIMING_LEVELS),
      expected = seconds_available / duty_cycle
    )
  
  observed <- df %>%
    count(date, timing, recvDeployName, top_port, tower_port, name = "observed") %>%
    mutate(timing = factor(timing, levels = TIMING_LEVELS))
  
  tidyr::crossing(
    date = unique_dates,
    timing = factor(TIMING_LEVELS, levels = TIMING_LEVELS)
  ) %>%
    left_join(sun_df, by = c("date", "timing")) %>%
    tidyr::crossing(tower_ports) %>%
    left_join(
      observed,
      by = c("date", "timing", "recvDeployName", "top_port", "tower_port")
    ) %>%
    mutate(
      observed = replace_na(observed, 0),
      frac = if_else(expected > 0, observed / expected, NA_real_),
      frac = pmin(frac, 1)
    )
}

# =============================================================================
# Helper: get sunrise/sunset times per day
# =============================================================================
make_day_night_windows <- function(df_classified, lat, lon, tz_local) {
  
  df_classified %>%
    mutate(date = as.Date(date_time_local)) %>%
    distinct(date) %>%
    rowwise() %>%
    mutate(
      sun = list(
        getSunlightTimes(
          date = date,
          lat = lat,
          lon = lon,
          keep = c("sunrise", "sunset"),
          tz = tz_local
        ) %>% select(-date)
      )
    ) %>%
    unnest_wider(sun) %>%
    ungroup() %>%
    mutate(
      day_seconds = as.numeric(difftime(sunset, sunrise, units = "secs")),
      night_seconds = 86400 - day_seconds
    )
}

# =============================================================================
# PHASE 4 — HOURLY ACTIVITY (MEAN ± 95% CI)
# =============================================================================

plot_hourly_activity <- function(
    activity_hourly_summary,
    dominant_port,
    lower_ratio,
    upper_ratio,
    subtitle
) {
  
  if (nrow(activity_hourly_summary) == 0) {
    return(
      ggplot() +
        labs(
          title = "No data available",
          subtitle = "activity_hourly_summary is empty",
          x = NULL, y = NULL
        ) +
        theme_void()
    )
  }
  
  ggplot(
    activity_hourly_summary,
    aes(hour, percent_activity, fill = timing)
  ) +
    geom_col(position = position_dodge(0.9)) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(0.9),
      width = 0.2
    ) +
    scale_x_continuous(breaks = 0:23) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.6)
    ) +
    labs(
      title = "Hourly activity pattern",
      subtitle = subtitle,
      caption = sprintf(
        "Bars show the proportion of retained detections classified as active. Error bars show approximate 95%% confidence intervals. Thresholds: %.3f–%.3f proportional signal change.",
        lower_ratio, upper_ratio
      ),
      x = "Hour of day, local time",
      y = "Detections classified as active",
      fill = "Diel period"
    ) +
    theme_woth()
}

# =============================================================================
# PHASE 5 — DAILY DETECTED VS EXPECTED
# =============================================================================

plot_daily_detected_vs_expected <- function(
    det_daily,
    dominant_port,
    subtitle
) {
  
  if (nrow(det_daily) == 0) return(invisible())
  
  ggplot(det_daily, aes(as.Date(date))) +
    geom_col(aes(y = expected), fill = "grey85") +
    geom_col(aes(y = observed), fill = "peru") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(
      title = "Daily detection coverage",
      subtitle = subtitle,
      caption = paste(
        "Grey bars show expected detections if the tag was continuously detected.",
        "Colored bars show observed detections.",
        sprintf("Expected detections are calculated as %s seconds per day / duty cycle.", SECONDS_PER_DAY),
        sep = "\n"
      ),
      x = "Date",
      y = "Number of detections"
    ) +
    theme_woth()
}

# =============================================================================
# PHASE 6 — FRACTION OF EXPECTED BY TIME OF DAY
# =============================================================================

plot_fraction_expected_tod <- function(
    det_daily_timing,
    dominant_port,
    subtitle
) {
  
  if (nrow(det_daily_timing) == 0) return(invisible())
  
  det_daily_timing <- det_daily_timing %>%
    mutate(
      date   = as.Date(date),
      timing = factor(timing, levels = TIMING_LEVELS),
      frac = pmin(frac, 1)
    )
  
  gray_bg <- det_daily_timing %>%
    distinct(date, timing) %>%
    mutate(frac = 1)
  
  ggplot(det_daily_timing, aes(date, frac, fill = timing)) +
    geom_col(
      data = gray_bg,
      aes(y = frac),
      fill = "gray92",
      position = "identity"
    ) +
    geom_col(position = "identity") +
    facet_wrap(~ timing, nrow = 1, scales = "free_y") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b"
    ) +
    labs(
      title = "Detection coverage by diel period",
      subtitle = "Observed detections divided by the number expected for each diel period",
      caption = subtitle,
      x = "Date",
      y = "Fraction of expected detections",
      fill = "Diel period"
    ) +
    theme_woth()
}

# =============================================================================
# PHASE 7 — DUTY CYCLE DIAGNOSTICS
# =============================================================================

plot_duty_cycle <- function(
    data_clean,
    dominant_port,
    duty_cycle,
    subtitle
) {
  
  data_duty <- data_clean %>%
    filter(top_port == dominant_port) %>%
    arrange(date_time_local) %>%
    mutate(
      dt = as.numeric(
        difftime(date_time_local, lag(date_time_local), units = "secs")
      ),
      snapped = snap_interval(
        dt,
        centers = seq(duty_cycle, 60, by = duty_cycle)
      )
    ) %>%
    filter(!is.na(snapped), snapped <= 60)
  
  if (nrow(data_duty) == 0) return(invisible())
  
  ggplot(
    data_duty,
    aes(factor(snapped, levels = seq(duty_cycle, 60, by = duty_cycle)))
  ) +
    geom_bar(fill = "forestgreen", color = "black") +
    labs(
      title = "Detection intervals relative to expected duty cycle",
      subtitle = subtitle,
      x = sprintf("Time between detections, seconds; expected interval = %s s", duty_cycle),
      y = "Number of detection intervals"
    ) +
    theme_woth()
}

# =============================================================================
# PHASE 8 — dB SIGNAL-DIFFERENCE DENSITY
# =============================================================================

plot_signal_difference <- function(
    data_signal,
    dominant_port,
    lower_db,
    upper_db,
    subtitle
) {
  
  data_signal <- data_signal %>%
    filter(is.finite(sig_diff))
  
  if (nrow(data_signal) == 0) return(invisible())
  
  ggplot(data_signal, aes(sig_diff, fill = timing)) +
    geom_density(alpha = 0.6) +
    geom_vline(
      xintercept = c(lower_db, upper_db),
      linetype = "dashed"
    ) +
    facet_wrap(~ timing, scales = "free_y") +
    coord_cartesian(
      xlim = c(
        quantile(data_signal$sig_diff, 0.01, na.rm = TRUE),
        quantile(data_signal$sig_diff, 0.99, na.rm = TRUE)
      )
    ) +
    labs(
      title = "Signal-change distribution by diel period",
      subtitle = subtitle,
      caption = sprintf(
        "Dashed lines show inactive threshold bounds: %.2f to %.2f dB. Values outside these bounds are classified as active.",
        lower_db, upper_db
      ),
      x = "Signal change between valid consecutive detections, dB",
      y = "Density"
    ) +
    theme_woth()
}

# =============================================================================
# PHASE 9 — DROPOUTS
# =============================================================================

plot_dropouts <- function(
    df_classified,
    dominant_port,
    subtitle
) {
  
  df_daily <- df_classified %>%
    filter(top_port == dominant_port) %>%
    mutate(date = as.Date(date_time_local)) %>%
    group_by(date) %>%
    summarise(
      total_detections = n(),
      dropouts = sum(dropout_fix, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(df_daily) == 0) return(invisible())
  
  drop_ratio <- sum(df_daily$dropouts) / sum(df_daily$total_detections)
  
  ggplot(df_daily, aes(date)) +
    geom_col(aes(y = total_detections), fill = "grey80") +
    geom_col(aes(y = dropouts), fill = "steelblue") +
    labs(
      title = "Corrected single-detection receiver or antenna switches",
      subtitle = subtitle,
      caption = sprintf(
        "Blue bars show detections corrected by the dropout filter. Grey bars show total detections. Overall corrected proportion: %.2f%%.",
        drop_ratio * 100
      ),
      x = "Date",
      y = "Number of detections"
    ) +
    theme_woth()
}

# =============================================================================
# PHASE 10 — AVERAGE DAYTIME ACTIVITY PER DAY OVER TIME
# =============================================================================

plot_daily_daytime_activity <- function(activity_hourly, subtitle = NULL) {
  
  if (nrow(activity_hourly) == 0) return(invisible())
  
  df_day <- activity_hourly %>%
    filter(timing == "day") %>%
    group_by(date) %>%
    summarise(
      avg_percent_activity = mean(percent_activity, na.rm = TRUE),
      se = sqrt(avg_percent_activity * (1 - avg_percent_activity) / n()),
      ci_lower = avg_percent_activity - 1.96 * se,
      ci_upper = avg_percent_activity + 1.96 * se,
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = pmax(ci_lower, 0),
      ci_upper = pmin(ci_upper, 1)
    )
  
  first_date <- min(df_day$date)
  last_date  <- max(df_day$date)
  
  ggplot(df_day, aes(x = date, y = avg_percent_activity, color = date)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 0.50)
    ) +
    scale_x_date(
      breaks = c(min(df_day$date), max(df_day$date)),
      labels = scales::label_date("%Y-%m-%d"),
      expand = expansion(add = c(0, 0))
    ) +
    scale_color_viridis_c(
      option = "plasma",
      begin = 0.2, end = 0.8,
      name = "Date",
      breaks = c(first_date, last_date),
      labels = c(format(first_date, "%b %d"), format(last_date, "%b %d"))
    ) +
    labs(
      title = "Average Daytime Activity per Day",
      subtitle = subtitle,
      x = "Date",
      y = "Avg % Active (day only)",
      caption = "Daytime hours only"
    ) +
    theme_woth()
}