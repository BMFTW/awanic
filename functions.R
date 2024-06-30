# Packages ----------------------------------------------------------------

library(tidyverse)

# Set working directory ---------------------------------------------------

setwd("C:/Users/simok/Desktop")

# Set locale --------------------------------------------------------------

Sys.setlocale("LC_CTYPE", "english")

# Seconds to hh:mm:ss -----------------------------------------------------

s_to_hms <- function(input) {
  
  h <- floor( input / 3600 )
  m <- floor( ( input - 3600 * h ) / 60 )
  s <- input - 3600 * h - 60 * m
  
  h <- ifelse( h < 10, paste0(0, h), h )
  m <- ifelse( m < 10, paste0(0, m), m )
  s <- ifelse( s < 10, paste0(0, s), s )
  
  output <- paste0(h, ":", m, ":", s)
  
  return(output)
  
}

s_to_hms2 <- function(input) {
  
  h <- floor(input / 3600)
  m <- floor(input / 60) %% 60
  s <- floor(input / 1) %% 60
  
  h <- ifelse( h < 10, paste0(0, h), h )
  m <- ifelse( m < 10, paste0(0, m), m )
  s <- ifelse( s < 10, paste0(0, s), s )
  
  output <- paste0(h, ":", m, ":", s)
  
  return(output)
  
}

# hh:mm:ss to seconds -----------------------------------------------------

hms_to_s <- function(input) {
  
  hms <- strsplit(input, ":")[[1]]
  
  h <- as.numeric(hms[1])
  m <- as.numeric(hms[2])
  s <- as.numeric(hms[3])
  
  output <- 3600 * h + 60 * m + s
  
  return(output)
  
}

# Expand grid -------------------------------------------------------------

expand.grid( x = 1:100, y = 1 ) %>% 
  as_tibble() %>% 
  mutate( z = paste(x, y, sep = ", ") %>% paste0("(", ., ")") ) %>%
  pull(z) %>% 
  str_flatten_comma() %>% 
  writeClipboard()

# Quotemarks --------------------------------------------------------------

quotemarks <- function(input) {
  return( input %>% paste("'", ., "'", sep = "") )
}

# Parentheses -------------------------------------------------------------

parentheses <- function(input) {
  return( input %>% paste("(", ., ")", sep = "") )
}