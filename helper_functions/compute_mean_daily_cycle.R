compute_mean_daily_cycle = function(df, model_name, Q_col) 
{
  df %>%
    mutate(
      # Convert from mm/day to m3/s
      Q_val = (.data[[Q_col]] * 178) / 86.4,
      Date = as.Date(Date),
      
      # Hydrological year (Sept–Aug)
      WaterYear = ifelse(month(Date) >= 9, year(Date) + 1, year(Date)),
      
      # Hydrological day of year (Sept 1 = 1)
      HydroDOY = ifelse(
        month(Date) >= 9,
        yday(Date) - yday(as.Date(paste0(year(Date), "-09-01"))) + 1,
        yday(Date) + (365 - yday(as.Date(paste0(year(Date) - 1, "-09-01"))) + 1)
      )
    ) %>%
    group_by(HydroDOY) %>%
    summarise(
      Model = model_name,
      MeanDailyQ = mean(Q_val, na.rm = TRUE),
      .groups = "drop"
    )
}