#| echo: true
#| eval: true
#| warning: false
#| message: false
#| code-fold: true
#| code-summary: Show the code

#' ---------------Function to prepare climate data-------------------
#' @param model_prefix Character. The name of the climate model.
#' @param base_path Character. Path to the folder containing model subfolders.
#' @param h_start Date. Start of the historical period .
#' @param h_end Date. End of the historical period.
#' @param f_start Date. Start of the future projection period.
#' @param f_end Date. End of the future projection period.
#'
#' @return A list with three elements:
#' hist: A tibble with historical PR, TMAX, TMIN data.
#' rcp45: A tibble with future RCP 4.5 PR, TMAX, TMIN data.
#' rcp85: A tibble with future RCP 8.5 PR, TMAX, TMIN data.
#'
#' @details
#' The function assumes each CSV file is named following the convention:
#' [model]_data[Variable]_[Scenario].csv.
#' Dates must be in a format that as.Date() can parse (e.g., "YYYY-MM-DD").
#' 
#' @examples
#' climate_data = prepare_climate_data(
#'   model_prefix = "CLM",
#'   base_path = "data/",
#'   h_start = as.Date("1977-01-01"),
#'   h_end   = as.Date("2005-12-31"),
#'   f_start = as.Date("2070-01-01"),
#'   f_end   = as.Date("2099-12-31")
#' )
#' 
#' names(climate_data) # returns "hist", "rcp45", "rcp85"
#'
#' @export
prepare_climate_data = function(model_prefix, 
                                base_path, 
                                h_start, 
                                h_end, 
                                f_start, 
                                f_end) 
{
  
  # Helper function to load and filter a single variable CSV
  load_var = function(var_name, scenario, start_date, end_date) 
  {
    
    # Construct file path based on naming convention
    fname = paste0(tolower(model_prefix), "_", tolower(var_name), "_", scenario, ".csv")
    file_path = file.path(base_path, toupper(model_prefix), fname)
    
    # Load CSV
    df = read.csv(
        file_path, 
        col.names = c("Date", toupper(var_name)),
        colClasses = c("character", "numeric")
    )
    
    # Convert Date column to Date type
    df$Date = as.Date(df$Date, format = "%Y%m%d")
    
    # Filter dates within range and rename the variable column
    df %>%
      filter(Date >= start_date & Date <= end_date) %>%
      rename(!!var_name := 2)  # Rename 2nd column to PR, TMAX, or TMIN
  }

  # --- 1. HISTORICAL PERIOD ---
  hist_pr   = load_var("PR",   "hist", h_start, h_end)
  hist_tmax = load_var("TMAX", "hist", h_start, h_end)
  hist_tmin = load_var("TMIN", "hist", h_start, h_end)
  
  # Merge historical variables by Date
  hist_all  = hist_pr %>%
    inner_join(hist_tmax, by = "Date") %>%
    inner_join(hist_tmin, by = "Date")

  # --- 2. FUTURE RCP 4.5 ---
  fut45_pr   = load_var("PR",   "rcp45", f_start, f_end)
  fut45_tmax = load_var("TMAX", "rcp45", f_start, f_end)
  fut45_tmin = load_var("TMIN", "rcp45", f_start, f_end)
  
  fut45_all  = fut45_pr %>%
    inner_join(fut45_tmax, by = "Date") %>%
    inner_join(fut45_tmin, by = "Date")

  # --- 3. FUTURE RCP 8.5 ---
  fut85_pr   = load_var("PR",   "rcp85", f_start, f_end)
  fut85_tmax = load_var("TMAX", "rcp85", f_start, f_end)
  fut85_tmin = load_var("TMIN", "rcp85", f_start, f_end)
  
  fut85_all  = fut85_pr %>%
    inner_join(fut85_tmax, by = "Date") %>%
    inner_join(fut85_tmin, by = "Date")

  # Return all data as a structured list
  return(list(
    hist  = hist_all,
    rcp45 = fut45_all,
    rcp85 = fut85_all
  ))
}