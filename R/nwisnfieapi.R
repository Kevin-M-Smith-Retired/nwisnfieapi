GetFlowAtStation <- function(dir, station, begin_utc, end_utc, remove.na = TRUE) {
  
  files <- .SelectFiles(dir = dir, begin_utc = begin_utc, end_utc = end_utc)
  
  flows <- lapply(files, .GetFlowFromFileForStation, stationid = station)[[1]]
  
  if (remove.na) {
    flows <- flows[!is.na(flows[,2]),]
  }
  
  epoch <- base::as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  
  begin_utc_seconds <- base::as.numeric(
    base::difftime(time1 = base::as.POSIXct(begin_utc, tz = "UTC"), 
                   time2 = epoch, 
                   units="secs"
    )
  )
  
  end_utc_seconds <- base::as.numeric(
    base::difftime(time1 = base::as.POSIXct(end_utc, tz = "UTC"), 
                   time2 = epoch, 
                   units="secs"
    )
  )
  
  
  flows <- flows[which(flows[, 1] >= begin_utc_seconds),]
  flows <- flows[which(flows[, 1] < end_utc_seconds),]
  
  return(flows)
}


.GetFlowFromFileForStation <- function(file, stationid) {
  
  times <- NA
  flows <- NA
  
  if (file.exists(file)) {
    nc <- ncdf4::nc_open(filename = file)
    stations <- ncdf4::ncvar_get(nc = nc, varid = "site_no")
    times <- ncdf4::ncvar_get(nc = nc, varid = "time")
    
    if (stationid %in% stations){
      matches <- which(stationid == stations)
      
      flows <- ncdf4::ncvar_get(nc = nc, varid = "v00060_value")[matches,]
      
      flows <- flows[rowSums(is.na(flows)) != ncol(flows), ]
      
      if(!is.null(dim(flows))){
        flows <- flows[1, ]
      }
      
    } else {
      flows <- rep(NA, length(times))
    }
    
    ncdf4::nc_close(nc = nc)
    
  } 
  
  return(cbind(times, flows))
}

.SelectFiles <- function(dir, begin_utc, end_utc){
  begin_cst = lubridate::ymd_hms(begin_utc) - lubridate::hours(6)
  begin_cst_day = lubridate::floor_date(begin_cst, unit = "day")
  
  end_cst = lubridate::ymd_hms(end_utc) - lubridate::hours(6)
  end_cst_day = lubridate::floor_date(end_cst, unit = "day")
  
  dates <- seq(as.Date(begin_cst_day), 
               as.Date(end_cst_day), 
               by = "day")
  
  dir_builder <- function(date){
    paste0(dir, date, "/national_", date, ".nc") 
  }
  
  return(unlist(lapply(dates, dir_builder)))
}