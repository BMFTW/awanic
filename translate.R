# Packages ----------------------------------------------------------------

library(tidyverse)
library(deeplr)

# Auth key ----------------------------------------------------------------

my_key <- "8e4ce3c1-c7b4-de8f-89b4-8f3701d4f1da:fx"

# Change working directory ------------------------------------------------

setwd("C:/Users/simok/Desktop/R")

# Set locale --------------------------------------------------------------

Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Sys.setlocale("LC_ALL", "English")

# Read data ---------------------------------------------------------------

kaannokset <- "kaannokset.rds" %>% read_rds() %>% as_tibble()

kaannetyt <- kaannokset %>% pull(FIN)

new_data <- readClipboard()
new_data %>% setdiff(kaannetyt)

new_data_df <- new_data %>% setdiff(kaannetyt) %>% as_tibble() %>% rename( FIN = value )

# Translate data ----------------------------------------------------------

translated_data <- new_data_df %>%
  mutate( SWE = deeplr::translate2( FIN, target_lang = "SV", source_lang = "FI", auth_key = my_key  ) ) %>% 
  mutate( ENG = deeplr::translate2( FIN, target_lang = "EN", source_lang = "FI", auth_key = my_key  ) )

# Export ------------------------------------------------------------------

# kaannokset %>% saveRDS("kaannokset.rds")
kaannokset %>% add_row(translated_data) %>% saveRDS("kaannokset.rds")
kaannokset <- "kaannokset.rds" %>% read_rds() %>% as_tibble()

# kaannokset <- kaannokset %>% mutate( SWE = ifelse( FIN == "Neste", "V?tska", SWE ) ) %>% mutate( ENG = ifelse( FIN == "Neste", "Liquid", ENG ) )

kaannokset %>%
  filter( FIN %in% new_data ) %>%
  mutate( across( everything(), \(x) str_replace_all( x, "'", "''" ) ) ) %>%
  mutate( sqlUpdate = str_glue( "UPDATE [haipro2].[haipro].[lomake_objektit_260] SET NayttonimiSwe = '{SWE}', NayttonimiEng = '{ENG}' WHERE Nayttonimi = '{FIN}'" ) ) %>%
  pull(sqlUpdate) %>%
  writeClipboard()

kaannokset %>%
  filter( FIN %in% new_data ) %>%
  mutate( across( everything(), \(x) str_replace_all( x, "'", "''" ) ) ) %>%
  mutate( sqlUpdate = str_glue( "UPDATE [haipro2].[haipro].[objektit_sisallot_180] SET sisaltoSwe = '{SWE}', sisaltoEng = '{ENG}' WHERE sisalto = '{FIN}'" ) ) %>%
  pull(sqlUpdate) %>%
  writeClipboard()

# ---

"kaannokset.rds" %>% read_rds() %>% openxlsx::write.xlsx( "kaannokset.xlsx", colNames = FALSE, sheetName = "kaannokset" )
"kaannokset.rds" %>% read_rds() %>% write.csv( "kaannokset.csv", row.names = FALSE )