# =========================================================================== 
# SCRIPT THAT DECODES A STRING THAT HAS SPECIAL CODIFICATION FOR TRANSFORMING
# GENERIC DATES INTO ACTUAL DATES FORMATS
#
# Date: 30/ago/2017
# Author: Juan G. Alvarez
#
# decodeName(input_string, workDate)
#
#   input_string: Any character string to be decoded
#   workDate:     Date ("yyyy-mm-dd") that will be used to decode the string
#
# Examples:
# decodeName("d:/&yyyy&", "2016-01-01") results in "d:/2016"
# decodeName("d:/&yy&&mm&&dd&/subdir", "2016-01-01") results in "d:/160101/subdir"
# =========================================================================== 

# Just for tests
# input_string <- "D:/dir/&yy&&mm&"
# workDate <- "2017-08-30"

decodeName <- function(input_string, workDate) {

  library(lubridate)
  library(stringr)
  
  #   La codificacion es asi
  
  # 1) &yyyy&     formato 4 digitos anio
  # 2) &yy&       formato 2 digitos anio
  # 3) &mm&       formato 2 digitos mes
  # 4) &m&        formato 1 o 2 digitos mes
  # 5) mesC&      formato texto mes Corto (e.g. ene, feb, ...)
  # 6) &mesL&     formato texto mes Largo (e.g. enero, febrero, ...)
  # 7) &monthS&   formato texto Month Short (e.g. jan, feb, mar, ...)
  # 8) &monthL&   formato texto Month Long (e.g. january, february, ...)
  # 9) &dd&       formato 2 digitos dia (e.g. 01, 02, .. 10, 11,)
  # 10) &d&       formato 1 o 2 digitos dia (e.g. 1,2,...10,11, ...)
  # 11) &diaC&    formato texto dia corto (e.g. lun,mar,mie,jue,vie,sab,dom)
  # 12) &diaL&    formato texto dia Largo (e.g. lunes,martes,...,domingo)
  # 13) &dayS&    formato texto Day Short (e.g. mon, tue, wed, thu, fri, sat, sun)
  # 14) &dayL&    formato texto Day Long (e.g. monday, tuesday,...,sunday)
  
  
  # Detect a list of codes inside the input string
  list_codes <- str_match_all(input_string, "&(.*?)&")
  
  # Changing each code with the actual value
  for (i in seq_along(list_codes[[1]][,1])) {
    code <- list_codes[[1]][i,2]
    decodestring <- unitDecodeconverter(code, workDate)
    
    input_string <- str_replace(string = input_string,
                                pattern = list_codes[[1]][i,1], 
                                replacement = decodestring)
  }
  return(input_string)
}

# ---------------------------------------------------------------------------
# Unit function to convert date-like string to the equivalent actual values
# ---------------------------------------------------------------------------

unitDecodeconverter <- function(code, workDate) {

  typesCodes <- c("yyyy",
                  "yy",
                  "mm",
                  "m",
                  "mesC",
                  "mesL",
                  "monthS",
                  "monthL",
                  "dd",
                  "d",
                  "diaC",
                  "diaL",
                  "dayS",
                  "dayL")

  # build a pattern with the exact code to be searched
  pattern <- paste0("^", code, "$")
    
  typeDetected <- grep(pattern, typesCodes, ignore.case = T)
  
  if (length(typeDetected) == 0) {
    errormsg <- sprintf("ERROR: code type %s doesn't exist", code)
    cat(errormsg)
    quit(save ="no")
  }
  
  if (typeDetected == 1) {
    # style yyyy
    strDecoded <- as.character(year(workDate)) 
  } else if (typeDetected == 2) {
    # style yy
    strDecoded <- str_sub(string = year(workDate), start = 3, end = 4)
  } else if (typeDetected == 3) {
    # Style mm
    strDecoded <- sprintf("%02d", month(workDate) )
  } else if (typeDetected == 4) {
    # Style m
    strDecoded <- sprintf("%d", month(workDate) )
  } else if (typeDetected == 5) {
    # Style mesC
    switch(month(workDate),
           strDecoded <-  "ene",
           strDecoded <-  "feb", 
           strDecoded <-  "mar", 
           strDecoded <-  "abr", 
           strDecoded <-  "may", 
           strDecoded <-  "jun", 
           strDecoded <-  "jul", 
           strDecoded <-  "ago", 
           strDecoded <-  "sep", 
           strDecoded <-  "oct", 
           strDecoded <-  "nov", 
           strDecoded <-  "dic") 
  } else if (typeDetected == 6 ) {
    # Style mesL
    switch(month(workDate),
           strDecoded <-  "enero",
           strDecoded <-  "febrero", 
           strDecoded <-  "marzo", 
           strDecoded <-  "abril", 
           strDecoded <-  "mayo", 
           strDecoded <-  "junio", 
           strDecoded <-  "julio", 
           strDecoded <-  "agosto", 
           strDecoded <-  "septiembre", 
           strDecoded <-  "octubre", 
           strDecoded <-  "noviembre", 
           strDecoded <-  "diciembre") 
  } else if (typeDetected == 7) {
    # style monthS
    switch(month(workDate),
           strDecoded <-  "jan",
           strDecoded <-  "feb", 
           strDecoded <-  "mar", 
           strDecoded <-  "apr", 
           strDecoded <-  "may", 
           strDecoded <-  "jun", 
           strDecoded <-  "jul", 
           strDecoded <-  "aug", 
           strDecoded <-  "sep", 
           strDecoded <-  "oct", 
           strDecoded <-  "nov", 
           strDecoded <-  "dec") 
  } else if (typeDetected == 8) {
    # Style monthL
    switch(month(workDate),
           strDecoded <-  "january",
           strDecoded <-  "february", 
           strDecoded <-  "march", 
           strDecoded <-  "april", 
           strDecoded <-  "may", 
           strDecoded <-  "jun", 
           strDecoded <-  "july", 
           strDecoded <-  "august", 
           strDecoded <-  "september", 
           strDecoded <-  "october", 
           strDecoded <-  "november", 
           strDecoded <-  "december") 
  } else if (typeDetected == 9) {
    # Style dd
    strDecoded <- sprintf("%02d", day(workDate))
  } else if (typeDetected == 10) {
    # Style d
    strDecoded <- sprintf("%d", day(workDate))
  } else if (typeDetected == 11) {
    # Style diaC
    strDecoded <- c("dom", "lun", "mar", "mie", "jue", 
                   "vie", "sab")[as.POSIXlt(workDate)$wday + 1]
  } else if (typeDetected == 12) {
    # Style diaL
    strDecoded <- c("domingo", "lunes", "martes", "miercoles", "jueves", 
                   "viernes", "sabado")[as.POSIXlt(workDate)$wday + 1]
  } else if (typeDetected == 13) {
    # Style dayS
    strDecoded <- c("Sun", "Mon", "Tue", "Wed", "Thu", 
                   "Fri", "Sat")[as.POSIXlt(workDate)$wday + 1]
  } else if (typeDetected == 14) {
    # Style dayL
    strDecoded <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
      "Friday", "Saturday")[as.POSIXlt(workDate)$wday + 1]
  }
         
  return(strDecoded)
}

