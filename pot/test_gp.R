library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(trend)
library(extRemes)

filename = './data/discharge.csv'
Qdata = read.csv(filename, col.names = c("Year", "Month", "Day", "Q"))  

# construct time serie
Qdata$Date = paste(Qdata[["Year"]], Qdata[["Month"]], Qdata[["Day"]], sep="-")
Qdata$Date  = ymd(Qdata$Date)

# final time serie
Qdata = Qdata[, c("Date", "Q")]


thres = 10   # threshold value
area = 194   # catchment area


all_exceedances = Qdata |>
                  filter(Q > thres) |>
                  arrange(Date)



time_sep_declust = function(data, threshold, Area) 
{  
  # convert Area from km²to Miles²
  A_miles = Area*0.386

  # compute the number of days required (Ts)
  Ts = ceiling(5 + log(A_miles))

  # Filter flow values above the threshold and sort by date
  exceedances = data |>
                filter(Q > threshold) |>
                arrange(Date)
  
  # initialize first exceedance as the first independent peak
  kept_peaks = exceedances[1, , drop=FALSE]
  last_peak_date = exceedances$Date[1]
  
  # Loop through remaining exceedances to apply the time-separation rule
  for (i in 2:nrow(exceedances)) 
  {
    current_date = exceedances$Date[i]
    if (as.numeric(current_date - last_peak_date) >= Ts) {
      kept_peaks = bind_rows(kept_peaks, exceedances[i, , drop=FALSE])
      last_peak_date = current_date
    }
  }

  # Return the de-clustered peaks
  return(kept_peaks)

}


peaks_time_declust = time_sep_declust(data=Qdata, threshold=thres, Area=area)


magnitude_diff_declust = function(data, original_data, frac = 2/3) 
{
  # Return as-is if there is only one or no peak
  if (nrow(data) <= 1) return(data)
  
  # Logical vector to mark which peaks to keep
  keep = rep(TRUE, nrow(data))
  
  # Iterate through peaks, comparing each with the previous one
  for (i in 2:nrow(data)) {
    peak1_date = data$Date[i - 1]
    peak2_date = data$Date[i]
    
    # Extract the flow values between two peaks from the full time series
    in_between = original_data |>
      filter(Date > peak1_date, Date < peak2_date)
    
    # If no data in between, keep the current peak
    if (nrow(in_between) == 0) next
    
    # Safely calculate minimum flow in the interval
    flows_between = in_between$Flow
    flows_between = flows_between[!is.na(flows_between)]
    
    # Skip if no valid flow values between peaks
    if (length(flows_between) == 0) next
    
    Xmin = min(flows_between)
    Xs1 = data$Flow[i - 1]
    
    # Skip if previous peak flow is NA or missing
    if (is.na(Xs1)) next
    
    # Discard current peak if in-between flow is too high
    if (Xmin > frac * Xs1) {
      keep[i] = FALSE
    }
  }
  
  # Return only the peaks that meet the magnitude difference criterion
  return(data[keep, ])
}


peaks_final = magnitude_diff_declust(data=peaks_time_declust, original_data=Qdata)


# compute the average number of threshold exceedances per year
peaks_final$year <- year(peaks_final$Date)
n_years <- length(unique(peaks_final$year))
n_events <- nrow(peaks_final)
units <- n_events / n_years

# Optional: print it nicely
cat("Average number of events per year:", round(units, 2), "\n")


library(extRemes)

rperiods = c(2, 5, 10, 20, 50, 100)

thres = 10
x = peaks_final$Q
# sgp_fit = fevd(x, type="GP", method="GMLE", threshold=thres, time.units = "2.56/year")


# plot(sgp_fit)

# rlevels_gp = return.level(sgp_fit, return.period=rperiods, do.ci=TRUE)
# rlevels_gp

# Excédents
# Estimation GPD
fit <- gpd.fit(x_exc, threshold = 0)
shape <- fit$mle[1]
scale <- fit$mle[2]

# Quantile de période de retour
Q100_gpd <- 10 + (4.8 / 0.05) * (((2.56 * T)^0.05) - 1)
Q100_gpd

# Pour comparaison : quantile GEV
fit_gev <- fgev(x)
Q100_gev <- qgev(1 - 1/100,
                 loc = fit_gev$estimate[1],
                 scale = fit_gev$estimate[2],
                 shape = fit_gev$estimate[3])
Q100_gev