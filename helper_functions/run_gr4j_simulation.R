run_gr4j_simulation = function(data, Pvarname, PETvarname, Qobs, params) 
{
  
  data = data[complete.cases(data[, c(Pvarname, PETvarname)]), ]
  data[[Pvarname]] = ifelse(data[[Pvarname]] < 0, 0, data[[Pvarname]])
  data[[PETvarname]] = ifelse(data[[PETvarname]] < 0, 0, data[[PETvarname]])

  # Ensure Date format (POSIXt required by airGR) ---
  data[["Date"]] = as.POSIXct(data[["Date"]])
  
  # Create model inputs ---
  InputsModel = CreateInputsModel(
    FUN_MOD  = RunModel_GR4J,
    DatesR   = data[["Date"]],
    Precip   = data[[Pvarname]],
    PotEvap  = data[[PETvarname]]
  )
  
  # Create run options (no warm-up) ---
  RunOptions = CreateRunOptions(
    FUN_MOD = RunModel_GR4J,
    InputsModel = InputsModel,
    IndPeriod_Run = seq_len(length(data[["Date"]]))
  )
  
  # Run GR4J simulation ---
  OutputsModel = RunModel_GR4J(
    InputsModel = InputsModel,
    RunOptions  = RunOptions,
    Param       = params
  )
  
  # Return simulation outputs ---
  data.frame(
    Date = data$Date,
    Qobs = Qobs,
    Qsim = OutputsModel$Qsim
  )
}