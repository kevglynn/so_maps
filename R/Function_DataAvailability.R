#' @export
Data.Availability <- function(data.tm = NULL, file = NULL, digits = 3){
  
  oldoptions <- options(digits = digits)
  on.exit(oldoptions)
  
  data.profiling <- as.data.frame(matrix(NA, nrow = ncol(data.tm), ncol = 14))
  colnames(data.profiling) <- c("variable" ,"n", "missing", "empty", "available", "unique", 
                                "min", "q25", "median", "q75", "max",
                                "mean", "sd", "type") 
  
  data.profiling$variable <- colnames(data.tm)
  
  # Identify class
  class.vec <- vector(mode = "character", length = ncol(data.tm))
  for (i in 1:ncol(data.tm)) {
    class.vec[i] <- class(data.tm[[i]])[1]
  }
  data.profiling$type <- class.vec
  
  #data.profiling[data.profiling$type == "integer" ,"type"] <- "numeric" 
  
  #data.tm[,data.profiling$type == "character", drop = FALSE] <- trim(data.tm[,data.profiling$type == "character"])
  
  
  # Subsetting data
  data.numeric   <- data.tm[,data.profiling$type %in% c("numeric","integer"), drop = FALSE]
  data.character <- data.tm[,data.profiling$type == "character", drop = FALSE]
  data.factor    <- data.tm[,data.profiling$type %in% c("factor"), drop = FALSE]
  data.date      <- data.tm[,data.profiling$type %in% c("date","POSIXct"), drop = FALSE]
  
  # trim White Space
  data.character <- as.data.frame(apply(data.character, 2, trimws))
  
  data.profiling$n <- nrow(data.tm)
  
  n         <- nrow(data.tm)
  NAs       <- apply(data.tm, 2, is.na)
  missing   <- apply(NAs, 2, sum)
  empty       <- apply(data.tm, 2, function(x) sum(nchar(gsub("[^[:alnum:]///' ]", "", x)[!is.na(x)])==0))
  available <- dim(data.tm)[1] - missing - empty
  unique    <- apply(data.tm, 2, function(x) length(unique(x[!is.na(x)])))
  
  data.profiling[,c("missing", "empty", "available", "unique")] <- c(missing, empty, available, unique)
  
  ### numeric -----------------------------
  if(ncol(data.numeric) > 0){
    mean      <- apply(data.numeric, 2, function(x) mean(as.numeric(x[!is.na(x)])))
    sd        <- apply(data.numeric, 2, function(x) sd(x[!is.na(x)]))
    quantiles <- apply(data.numeric, 2, function(x) quantile(x[!is.na(x)]))
    min    <- quantiles[1,]
    q25    <- quantiles[2,]
    median <- quantiles[3,]
    q75    <- quantiles[4,]
    max    <- quantiles[5,]
    
    data.profiling[data.profiling$type %in% c("numeric","integer"),c("min", "q25", "median", "q75", "max", "mean", "sd")] <- as.character(round(c(min, q25, median, q75, max, mean, sd),digits= digits))
  }
  
  ### date --------------------------------
  if(ncol(data.date) > 0){
    range      <- apply(data.date, 2, function(x) range((x[!is.na(x)])))
    
    data.profiling[data.profiling$type %in% c("date","POSIXct"),c("min","max")] <- c(range)
  }
  
  # Export csv
  if(!is.null(file)){
    write.csv(data.profiling, paste(file,".csv",sep=""))
    cat("--- DATA PROFILING SUCCESFUL\n RESULTS SAVED IN: ",getwd(), "\n")
  }
  
  return(data.profiling)
  
} #End function
