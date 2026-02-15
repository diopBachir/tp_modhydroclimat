# @param obs: Data frame/matrix of historical observations
# @param mod_hist: Data frame/matrix of historical model outputs
# @param mod_proj: Data frame/matrix of future model projections (RCP)#| code-fold: true
# @param var_names: Vector of column names to process
seasonalMBCn = function(obs, 
                        mod_hist, 
                        mod_proj, 
                        var_names = c("PR", "PET")) 
{
  # helper function to add date parts
  add_date_parts = function(df) 
  {
    df$Year  = as.numeric(format(df$Date, "%Y"))
    df$Month = as.numeric(format(df$Date, "%m"))
    df$Day   = as.numeric(format(df$Date, "%d"))

    df
  }

  # add date parts
  obs       = add_date_parts(obs)
  mod_hist  = add_date_parts(mod_hist)
  mod_proj  = add_date_parts(mod_proj)
  
  # Lists to store the corrected chunks for each month
  hist_list = list()
  proj_list = list()
  
  # Loop through each month (1 = Jan, ..., 12 = Dec)
  for (m in 1:12) {

    # Subset data for the current month
    s_obs  = obs[obs$Month == m, ]
    s_hist = mod_hist[mod_hist$Month == m, ]
    s_proj = mod_proj[mod_proj$Month == m, ]
    
    # 2. Convert to matrices for MBCn
    m_obs  = as.matrix(s_obs[, var_names])
    m_hist = as.matrix(s_hist[, var_names])
    m_proj = as.matrix(s_proj[, var_names])
    
    # Run MBCn
    capture.output({ # stop the MBCn from printing the iteration progress
      fit = MBCn(
        o.c = m_obs,
        m.c = m_hist,
        m.p = m_proj,
        iter = 100,
        ratio.seq = c(TRUE, TRUE), 
        trace = .01
      )
    })
    
    # Prepare Adjusted Chunks
    s_hist_adj = as.data.frame(fit$mhat.c)
    colnames(s_hist_adj) = paste0(var_names, "_adj")
    hist_list[[m]] = cbind(s_hist, s_hist_adj)
    
    # Projection
    s_proj_adj = as.data.frame(fit$mhat.p)
    colnames(s_proj_adj) = paste0(var_names, "_adj")
    proj_list[[m]] = cbind(s_proj, s_proj_adj)
  }
  
  # Recombine all months and restore chronological order
  # We use do.call(rbind, ...) to stack the 12 data frames
  final_hist = do.call(rbind, hist_list)
  final_proj = do.call(rbind, proj_list)
  
  # Sort by Year, Month and Dayto keep time series linear
  final_hist = final_hist[
    order(final_hist$Year, final_hist$Month, final_hist$Day), 
  ][,c("Date", "PR", "PR_adj", "PET", "PET_adj")]
  final_proj = final_proj[
    order(final_proj$Year, final_proj$Month, final_proj$Day), 
  ][,c("Date", "PR", "PR_adj", "PET", "PET_adj")]
  
  list(hist = final_hist, proj = final_proj)
}