library(tidyverse)

# -------------------------------------------------------------------------

setwd("C:/Users/simok/Desktop")

# -------------------------------------------------------------------------

excel_242 <- "242.xlsx" %>% readxl::read_excel() %>% as_tibble() %>% select( matches("(?i)name") ) %>% rename( org_nimi = 1 )
excel_252 <- "252.xlsx" %>% readxl::read_excel() %>% as_tibble() %>% select( matches("(?i)name") ) %>% rename( org_nimi = 1 )
excel_257 <- "257.xlsx" %>% readxl::read_excel() %>% as_tibble() %>% select( matches("(?i)name") ) %>% rename( org_nimi = 1 )
excel_258 <- "258.xlsx" %>% readxl::read_excel() %>% as_tibble() %>% select( matches("(?i)name") ) %>% rename( org_nimi = 1 )

# -------------------------------------------------------------------------

org_242 <- clipr::read_clip_tbl( header = FALSE ) %>% as_tibble() %>% rename( org_ID = V1, org_lyhyt_nimi = V2, org_nimi = V3 )
org_252 <- clipr::read_clip_tbl( header = FALSE ) %>% as_tibble() %>% rename( org_ID = V1, org_lyhyt_nimi = V2, org_nimi = V3 )
org_257 <- clipr::read_clip_tbl( header = FALSE ) %>% as_tibble() %>% rename( org_ID = V1, org_lyhyt_nimi = V2, org_nimi = V3 )
org_258 <- clipr::read_clip_tbl( header = FALSE ) %>% as_tibble() %>% rename( org_ID = V1, org_lyhyt_nimi = V2, org_nimi = V3 )

# -------------------------------------------------------------------------

ci_str_detect <- function(x, y) { str_detect( x, regex(y, ignore_case = TRUE) ) }
fuzzy_left_join( org_242, match_fun = ci_str_detect, by = c("org_nimi" = "org_nimi") )

# -------------------------------------------------------------------------

b <- excel_252 %>%
  mutate( org_nimi = str_replace( org_nimi, "(?<=\\d),", "" ) ) %>% 
  separate( org_nimi, c( "org_lyhyt_nimi", "org_nimi" ), sep = "(?<=\\d{3})\\s", fill = "left" ) %>% 
  mutate( org_lyhyt_nimi = org_lyhyt_nimi %>% gsub(" ", "", .) )

c <- b %>% left_join(org_252, by = "org_lyhyt_nimi", na_matches = "never" ) %>% mutate( id = row_number() ) %>% relocate(id)

multiples <- c %>% count(org_nimi.x) %>% filter( n > 1 ) %>% pull(org_nimi.x)

c %>% filter( org_nimi.x %in% multiples ) %>% print( n = Inf ); remove <- c( 57, 58, 108:110, 112:114, 603:604 )

output <- c %>% filter( !( id %in% remove ) ) %>% select(-id) %>% mutate( id = row_number() ) %>% relocate(id)

output %>% select( org_ID, org_nimi.x ) %>% clipr::write_clip()

# -------------------------------------------------------------------------

new <- "252.xlsx" %>% readxl::read_excel() %>% as_tibble()
final <- new %>% filter( str_detect(...7, "\\d") ) %>% select(2,7) %>% rename( yksi = 1, kaksi = 2 )

final %>% pull(yksi) %>% paste0("(", ., ")") %>% paste(collapse = ", ") %>% writeClipboard()

final %>%
  select(kaksi) %>%
  mutate( id = row_number() ) %>%
  mutate( z = paste(kaksi, id, sep = ", ") %>%
  paste0("(", ., ")") ) %>%
  pull(z) %>% 
  paste(collapse = ", ") %>%
  writeClipboard()
