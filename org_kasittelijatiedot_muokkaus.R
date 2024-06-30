library(tidyverse)

setwd("C:/Users/simok/Desktop")

data <- readxl::read_xlsx( "Varha_temp.xlsx", col_names = FALSE )

get_name <- function(x) { x %>% str_extract(".+(?=@)") %>% str_replace_all( "\\.", " " ) %>% str_to_title() }
get_name <- Vectorize(get_name)

swap <- function(x) {
  
    if ( is.na(x) ) return("")
  
    xx <- x %>% str_split_1(" ");
    output <- paste( xx[2], xx[1] );
    return(output)

}

swap <- Vectorize(swap)

data %>% 
  unite( "org_nimi_koko", 1:7, na.rm = TRUE ) %>% 
  mutate (
    org_lyhyt_nimi = org_nimi_koko %>% str_extract( "(?<=\\[)[^\\]]+" ) %>% trimws(),
    org_nimi = org_nimi_koko %>% trimws() %>% str_remove(".+\\] ") %>% str_remove("\\s\\(\\d+\\)$"),
    name8 = get_name(...8) %>% swap(),
    name9 = get_name(...9) %>% swap(),
    name10 = get_name(...10) %>% swap(),
    name11 = get_name(...11) %>% swap(),
    name12 = get_name(...12) %>% swap(),
    name13 = get_name(...13) %>% swap(),
    name14 = get_name(...14) %>% swap(),
    name15 = get_name(...15) %>% swap(),
  ) %>% 
  select( org_lyhyt_nimi, org_nimi, name8, ...8 , name9, ...9 , name10, ...10 , name11, ...11 , name12, ...12 , name13, ...13 , name14, ...14 , name15, ...15 ) %>%
  rename( nimi1 = name8, sposti1 = ...8, nimi2 = name9, sposti2 = ...9, nimi3 = name10, sposti3 = ...10, nimi4 = name11, sposti4 = ...11, nimi5 = name12, sposti5 = ...12, nimi6 = name13, sposti6 = ...13, nimi7 = name14, sposti7 = ...14, nimi8 = name15, sposti8 = ...15 ) %>% 
  openxlsx::write.xlsx("Varha_temp_jalostettu.xlsx")
