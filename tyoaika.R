library(tidyverse)

source("functions.R")

seq( lubridate::dmy("26.1.2024"), lubridate::dmy("25.02.2024"), by = "day" ) %>%
  discard( \(day) weekdays(day) %in% c( "Saturday", "Sunday" ) ) %>%
  format("%d.%m.%Y") %>%
  str_remove("^0") %>%
  str_remove("(?<=\\.)0") %>%
  quotemarks() %>%
  str_flatten_comma() %>%
  writeLines( con = "clipboard", sep = "" )


# Lomat -------------------------------------------------------------------

pvms <- seq( lubridate::dmy("1.07.2024"), lubridate::dmy("28.07.2024"), by = "day" ) %>%
  discard( \(day) weekdays(day) %in% c( "Sunday" ) ) %>%
  format("%d.%m.%Y") %>%
  str_remove("^0") %>%
  str_remove("(?<=\\.)0")

nums <- 0 %>% rep(45) %>% append(1) %>% str_flatten_comma()

str_glue( "( 'Simo Korpela', '{pvms}', {nums} ), " ) %>% writeClipboard()
