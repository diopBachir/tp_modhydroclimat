files <- list.files("data/CNRM", pattern = "^cnrm_.*\\.csv$", full.names = T)

for (f in files) {

  df <- read.csv(f, header = FALSE)
  colnames(df) <- c("year", "month", "day", "value")

  date <- sprintf("%04d%02d%02d", df$year, df$month, df$day)
  out <- data.frame(date, df$value)

  write.table(out,
              file = f,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
}


files <- list.files("data/SMHI_MPI", pattern = "^smhi_mpi_.*\\.csv$", full.names = T)

for (f in files) {

  df <- read.csv(f, header = FALSE)
  colnames(df) <- c("year", "month", "day", "value")

  date <- sprintf("%04d%02d%02d", df$year, df$month, df$day)
  out <- data.frame(date, df$value)

  write.table(out,
              file = f,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
}