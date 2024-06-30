# Packages ----------------------------------------------------------------

library(DBI)
library(odbc)
library(tidyverse)

# Database connection -----------------------------------------------------

# conn <- dbConnect(
#   odbc(),
#   Driver = "SQL Server",
#   Server = "83.150.87.73",
#   Database = "haipro",
#   uid = "haipro",
#   pwd = "haipro",
#   encoding = "latin1"
# )

# haipro_kohteet <- conn %>% dbReadTable("haipro_kohteet") %>% as_tibble()

# HaiPro-kohteet ----------------------------------------------------------

haipro_kohteet <- "G:/My Drive/Työ/Awanic Oy/haipro_kohteet.csv" %>%
  read_delim( delim = ";", col_names = FALSE, show_col_types = FALSE ) %>% 
  rename( id = X1, kanta = X2, www = X3 ) %>%
  mutate( kanta = tolower(kanta) ) %>%
  arrange(id)

hva     <- c( 10, 22, 29, 35, 112, 175, 220, 242, 243, 246, 248, 249, 250, 251, 252, 254, 255, 256, 257, 258, 259 )
wb      <- c( 24, 107, 144, 161, 178, 189, 209, 221, 224, 236, 239, 264, 265, 266, 267, 268, 269, 270, 272, 274, 286, 289 )
kempro  <- c( 10, 32, 69, 139, 149, 153, 164, 180, 186, 252, 258, 260, 261, 271, 277 )
eki     <- c( 99, 109, 114, 120, 142, 149, 152, 162, 181, 221, 223, 225, 239, 241, 244, 252, 254, 265, 269, 271, 276, 281, 282, 284, 285 )
posipro <- c( 15, 22, 26, 28, 29, 30, 41, 42, 58, 162, 175, 180, 196, 200, 201, 202, 211, 221, 223, 224, 239, 246, 248, 250, 251, 257, 258, 259, 260, 264, 267, 283, 284 )

"%!in%" <- Negate("%in%")

# data <- haipro_kohteet %>% filter( www == "www" )
data <- haipro_kohteet %>% filter( www == "www2", kanta %in% c("haipro2", "haipro3", "haipro4") )
# data <- haipro_kohteet %>% filter( www == "www3" )

# Generate SQL script -----------------------------------------------------

n <- nrow(data)

sql_script <- character(n)

for ( i in 1:n ) {
  
  id    <- data[i,1]
  kanta <- data[i,2]
  
  table <- str_glue( "[{kanta}].[haipro].[lomake_objektit_{id}]" )
  
  # row <- str_glue( "SELECT * FROM {table}" )
  row <- str_glue( "IF EXISTS ( SELECT * FROM {table} WHERE nayttonimi LIKE '%xxxxxxxxxxxxxxxxxx%' ) PRINT {id};" )
  # row <- paste0( "IF COL_LENGTH('", table, "', 'SpostiVastaanottajaEKIYT') IS NULL ALTER TABLE", table, " ADD SpostiVastaanottajaEKIYT tinyint;" )
  # row <- paste0( "IF COL_LENGTH('", table, "', 'SpostiVastaanottajaEKIYT') IS NULL PRINT ", id )
  
  
  sql_script[i] <- row
  
}sss

sql_script %>% writeClipboard()