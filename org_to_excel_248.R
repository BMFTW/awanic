library(tidyverse)
library(openxlsx)

setwd("C:/Users/simok/Desktop")

# file <- "org_249.csv"
# data <- file %>% readr::read_delim( ";", col_names = FALSE )

org_data <- clipr::read_clip_tbl( header = FALSE ) %>%
  as_tibble() %>% 
  rename( org_ID = V1, org_taso = V2, indeksi = V3, org_lyhyt_nimi = V4, org_nimi = V5, rooli = V6, sposti = V7 ) %>% 
  mutate( org_lyhyt_nimi = org_lyhyt_nimi %>% str_remove("NULL") )

org_41 <- clipr::read_clip_tbl( header = FALSE ) %>%
  as_tibble() %>% 
  rename( org_ID = V1, org_taso = V2, indeksi = V3, org_lyhyt_nimi = V4, org_nimi = V5, rooli = V6, sposti = V7 ) %>% 
  mutate( org_lyhyt_nimi = org_lyhyt_nimi %>% str_remove("NULL") )

orgid_to_indeksi <- function(id) { org_data %>% filter( org_ID == id ) %>% select(indeksi) %>% pull() %>% unique() }

createWhitespace <- function(n, string) { strrep(" ", ( as.numeric(n)-1 ) * 20 ) %>% paste(string, sep = "") }
createWhitespace <- Vectorize(createWhitespace)

org_data_0 <- org_data %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )

org_data_1 <- org_data %>% filter( indeksi >= orgid_to_indeksi(51) & indeksi < orgid_to_indeksi(12) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_2 <- org_data %>% filter( indeksi >= orgid_to_indeksi(12) & indeksi < orgid_to_indeksi(13) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_3 <- org_data %>% filter( indeksi >= orgid_to_indeksi(13) & indeksi < orgid_to_indeksi(10) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_4 <- org_data %>% filter( indeksi > orgid_to_indeksi(10) & indeksi < orgid_to_indeksi(16) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_5 <- org_data %>% filter( indeksi >= orgid_to_indeksi(18) & indeksi < orgid_to_indeksi(19) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_6 <- org_data %>% filter( indeksi >= orgid_to_indeksi(19) & indeksi <= orgid_to_indeksi(2543) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_7 <- org_data %>% filter( indeksi > orgid_to_indeksi(2543) & indeksi < orgid_to_indeksi(21) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_8 <- org_data %>% filter( indeksi >= orgid_to_indeksi(21) & indeksi < orgid_to_indeksi(20) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )
org_data_9 <- org_data %>% filter( indeksi >= orgid_to_indeksi(20) & indeksi < orgid_to_indeksi(2) ) %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )

org_41 <- org_41 %>% mutate( org_nimi = createWhitespace( org_taso, org_nimi ) ) %>% select( org_lyhyt_nimi:sposti )

# Create workbook
wb <- createWorkbook()

# Add sheets to the workbook
addWorksheet(wb, "PSHVA")
addWorksheet(wb, "Ikääntyneiden palvelut")
addWorksheet(wb, "Perhe- ja vammaispalvelut")
addWorksheet(wb, "Yleisten palvelujen toimiala")
addWorksheet(wb, "Pelastustoimi ja turvall.")
addWorksheet(wb, "Hallintopalvelut")
addWorksheet(wb, "Henkilöstöpalvelut")
addWorksheet(wb, "HVA Konsernihallinto")
addWorksheet(wb, "Strategia- ja kehittäminen")
addWorksheet(wb, "Talouspalvelut")
addWorksheet(wb, "KYS")

# Write data to each sheet
writeData(wb, "PSHVA", org_data_0 )
writeData(wb, "Ikääntyneiden palvelut", org_data_1)
writeData(wb, "Perhe- ja vammaispalvelut", org_data_2)
writeData(wb, "Yleisten palvelujen toimiala", org_data_3)
writeData(wb, "Pelastustoimi ja turvall.", org_data_4)
writeData(wb, "Hallintopalvelut", org_data_5)
writeData(wb, "Henkilöstöpalvelut", org_data_6)
writeData(wb, "HVA Konsernihallinto", org_data_7)
writeData(wb, "Strategia- ja kehittäminen", org_data_8)
writeData(wb, "Talouspalvelut", org_data_9)
writeData(wb, "KYS", org_41)

# Save the workbook
saveWorkbook(wb, "org_248.xlsx", overwrite = TRUE)
