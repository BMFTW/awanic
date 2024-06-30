# https://lecy.github.io/Open-Data-for-Nonprofit-Research/Quick_Guide_to_XML_in_R.html

library(tidyverse)
library(xml2)

setwd("C:/Users/simok/Desktop/R/IP")

ip_addresses <- "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"

ips <- readClipboard() %>% str_extract_all(ip_addresses) %>% unlist() %>% unique()

sort_ips <- function(ip_list) {
  
  sort_ips_0 <- function(ip) { ip %>% str_split_1("\\.") %>% str_pad( 3, pad = "0" ) %>% str_flatten() %>% as.numeric() }
  sort_ips_0 <- Vectorize(sort_ips_0)
  
  i <- ip_list %>% sort_ips_0() %>% order()
  
  ip_list <- ip_list[i]
  
  return(ip_list)
  
}

ips_sorted <- ips %>% sort_ips()

# data <- tibble( ips1 = ips ) %>% mutate( ips2 = ips1 %>% sort_ips() )
# data %>% view()

ip_list <- tibble( www = character(), id = integer(), ip = character() )

file_names <- c( "www.txt", "www2.txt", "www3.txt" )

for ( file_name in file_names ) {
  
  xml <- file_name %>% read_xml()
  
  locations <- xml %>% xml_find_all("//location") %>% .[ str_detect( xml_attr(., "path"), "(?i)^Default Web Site/HaiPro/\\d+$" ) ]
  
  ip_list_id <- tibble( id = integer(), ip = character() )
  
  for ( location in locations ) {
    
    id <- location %>% xml_attr("path") %>% str_extract("\\d+$") %>% as.integer()
    
    ips <- location %>% xml_find_all(".//add") %>% as.character()
    
    new_ips <- tibble( id = id, ip = ips )
    
    ip_list_id <- ip_list_id %>% add_row(new_ips)
    
  }
  
  www_palvelin = file_name %>% sub(".txt", "", .)
  
  ip_list_id <- tibble( www = www_palvelin, ip_list_id )
  
  ip_list <- ip_list %>% add_row(ip_list_id)
  
}

# haipro_kohteet <- "G:/My Drive/Ty?/Awanic Oy/haipro_kohteet.csv" %>%
#   read_delim( delim = ";", col_names = FALSE, show_col_types = FALSE ) %>% 
#   rename( id = X1, kanta = X2, www = X3 ) %>%
#   mutate( kanta = tolower(kanta) ) %>%
#   arrange(id)
# 
# ip_list <- ip_list %>%
#   inner_join( haipro_kohteet, by = c( "www",  "id" ) ) %>%
#   select(id, ip) %>%
#   arrange(id)

a <- ip_list %>% filter( www == "www", id == 252 ) %>% pull(ip)
b <- ip_list %>% filter( www == "www2", id == 252 ) %>% pull(ip)

a %>% setdiff(b)