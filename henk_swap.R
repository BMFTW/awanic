library(tidyverse)

henk_22 <- clipr::read_clip_tbl( header = TRUE ) %>% as_tibble()

swap <- function(name) {
  
  name_parts <- name %>% str_split_1(" ")
  
  n <- name_parts %>% length()
  
  if ( n == 1 ) return(name)
  
  surname <- name_parts[n]
  first_names <- name_parts %>% .[1:(n-1)] %>% str_flatten(" ")
  
  output <- paste( surname, first_names )
  
  return(output)
  
}

swap <- Vectorize(swap)

henk_22 %>%
  mutate( etunimi = nimi %>% str_extract("[a-öA-Ö\\-]+") %>% tolower() %>% str_replace_all( c( "ä" = "a", "ö" = "o" ) ) ) %>% 
  mutate( sposti_etunimi = sposti %>% str_extract("[^\\.]+")  ) %>% 
  filter( etunimi == sposti_etunimi ) %>% 
  mutate( uusi_nimi = nimi %>% swap() ) %>% 
  mutate( sqlUpdate = paste0( "UPDATE [haipro4].[haipro].[henk_22] SET nimi = '", uusi_nimi, "' WHERE nimi = '", nimi, "'" ) ) %>%
  pull(sqlUpdate) %>% 
  writeClipboard()
