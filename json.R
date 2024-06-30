library(tidyverse)
library(jsonlite)
library(lubridate)

jsons <- readClipboard()

output <- tibble(
  
  running        = character(),
  startTime      = double(),
  lastStartTime  = double(),
  pauseStartTime = double(),
  pauseLength    = integer(),
  notified       = logical(),
  poissa         = integer(),
  sairas         = integer(),
  loma           = integer()
  
)

for ( json in jsons ) {
  
  json_tibble <- json %>% fromJSON() %>% as_tibble() %>% mutate_at(1, as.character)
  
  output <- output %>% add_row(json_tibble)
  
}

output

# Date.now()
time <- Sys.time() %>% as.numeric() %>% magrittr::multiply_by(1000)

# The POSIXct format stores date and time in seconds with the number of seconds beginning at January 1, 1970
limit <- "16.1.2024" %>% lubridate::dmy() %>% as.POSIXct( origin = "1970-01-01", tz = Sys.timezone() ) %>% as.numeric() %>% magrittr::multiply_by(1000)

updates %>% keep( \(update) update >= limit )

time %>% magrittr::divide_by(1000) %>% as.POSIXct( origin = "1970-01-01", tz = Sys.timezone() ) %>% ymd_hms()
