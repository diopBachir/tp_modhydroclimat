calc_hargreaves = function(df, lat = 44.2) 
{
  # Convert Latitude to Radians
  phi = lat * pi / 180
  
  # Get Day of Year from the Date column
  doy = yday(df$Date)
  
  # 1. Inverse relative distance Earth-Sun (dr)
  dr = 1 + 0.033 * cos(2 * pi * doy / 365)
  
  # 2. Solar declination (delta)
  delta = 0.409 * sin(2 * pi * doy / 365 - 1.39)
  
  # 3. Sunset hour angle (ws)
  # This handles the geometry of the Earth's curve at 44.2 degrees
  ws = acos(-tan(phi) * tan(delta))
  
  # 4. Extraterrestrial radiation (Ra) in MJ/m2/day
  Ra = (24 * 60 / pi) * 0.0820 * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
  
  # 5. Convert Ra to evaporation equivalent (mm/day)
  # 1 MJ/m2/day is approx 0.408 mm/day
  Ra_mm = Ra * 0.408
  
  # 6. Final Hargreaves Calculation
  T_mean = (df$TMAX + df$TMIN) / 2
  # pmax ensures we don't get NaNs if Tmin > Tmax (though rare)
  PET = 0.0023 * Ra_mm * (T_mean + 17.8) * sqrt(pmax(0, df$TMAX - df$TMIN))
  
  return(PET)
}