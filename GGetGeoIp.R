library(rjson)
library(RCurl)
#library(dplyr)

freegeoip <- function(df = NULL) { 
  
  if (is.data.frame(df) == FALSE) { 
    warning('This function only works with data.frames. Please provide a data.frame.') 
  }
  
  else {  
    final <- data.frame()
    for (i in 1:nrow(df)) {
      ip <- as.character(df[i,])
      if (!substring(ip,3) == "10." & !is.na(ip)) {
        url <- paste("http://ip-api.com/json/", c(as.character(df[i,])), sep="")
        line <- tryCatch( 
          data.frame(fromJSON(getURLContent(url))), 
          error = function(e) e
          )
        if(inherits(line, "error")) 
          {line <- c(NA)} 
        else {
          if (nrow(final) == 0) {final <- line} else {
            if (line$status == "success") {final <- rbind(final, line)}
          }
        }
      }
#      final <- rbind(final, line)
    }
    return(final)
  }
}
df <- unique(as.data.frame(dataset$`Dirección IP`))
localizacion1<-freegeoip(df)

split.screen(c(2,1))

