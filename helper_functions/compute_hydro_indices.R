compute_hydro_indices = function(df, model_name, Q_col) 
{
  df %>%
    mutate(
      # Revert selected Q from mm to m3/s (assuming area is 178 km2)
      # .data[[...]] allows us to reference the column dynamically
      Q_val = (.data[[Q_col]] * 178) / 86.4,
      Date = as.Date(Date),
      # Define Water Year (Sept 1st)
      WaterYear = ifelse(month(Date) >= 9, year(Date) + 1, year(Date))
    ) %>%
    group_by(WaterYear) %>%
    summarise(
      Model = model_name,
      AnMean = mean(Q_val, na.rm = TRUE),
      Amax   = max(Q_val, na.rm = TRUE),
      minVCN7 = min(rollapply(Q_val, width = 7, FUN = mean, fill = NA), na.rm = TRUE),
      .groups = "drop"
    )
}