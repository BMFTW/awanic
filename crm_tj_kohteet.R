# TODO: str_pad()

library(tidyverse)
library(lubridate)
library(openxlsx)

setwd("C:/Users/simok/Desktop")

input  <- "tj_kohteet.csv"
output <- "crm_tj_kohteet.xlsx"

input %>%
  read_delim( col_names = FALSE, show_col_types = FALSE ) %>% 
  rename( Kohde_ID = X1, Kohde_Nimi = X2, kanta = X3, www_palvelin = X4, LuontiPvm = X5) %>% 
  mutate( kanta = tolower(kanta), LuontiPvm = paste( day(LuontiPvm), month(LuontiPvm), year(LuontiPvm), sep = "." ) ) %>% 
  openxlsx::write.xlsx(output)
