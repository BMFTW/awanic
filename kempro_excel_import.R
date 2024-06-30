library(tidyverse)

setwd("C:/Users/simok/Desktop")

data <- "Kiinteist?nhuolto.xlsx" %>% openxlsx::read.xlsx() %>% as_tibble()

data2 <- data %>%
  janitor::remove_empty("cols") %>%
  mutate( across( everything(), \(x) gsub("\n", "", x) ) ) %>%
  mutate( across( everything(), \(x) if_else( is.na(x), "", x ) ) ) %>% 
  mutate( across( everything(), \(x) paste0("'", x, "'") ) ) %>% 
  select( -Artikkelinumero, -Aineen.tai.seoksen.luokitus, -`Aineen.ja/tai.seoksen.k?ytt?tapa`) %>% 
  mutate( Tuotekuvaus                             = paste( "10, ''", Tuotekuvaus, sep = ", "  ) ) %>% 
  mutate( K?ytt?turvallisuustiedotteen.p?iv?m??r? = paste( "12, ''", K?ytt?turvallisuustiedotteen.p?iv?m??r?, sep = ", "  ) ) %>% 
  mutate( Symbolit                                = paste( "19, ''", Symbolit, sep = ", "  ) ) %>% #GHS
  mutate( Huomiosana                              = paste( "18", Huomiosana, "''", sep = ", "  ) ) %>% mutate( Huomiosana = str_replace(Huomiosana, "Vaara", "185"), Huomiosana = str_replace(Huomiosana, "Varoitus", "186") ) %>% 
  mutate( `H/EUH-lausekkeet`                      = paste( "13, ''", `H/EUH-lausekkeet`, sep = ", "  ) ) %>%  # H
  mutate( `P-lauseke`                             = paste( "14, ''", `P-lauseke`, sep = ", "  ) ) %>% #
  mutate( Osasto                                  = "2, '', 116" ) %>% 
  mutate( kemikaaliID = 3:( nrow(data) + 2 ) ) %>% 
  mutate( lomakeID = 6:( nrow(data) + 5 ) ) %>%
  relocate( kemikaaliID ) %>%
  relocate( lomakeID )

# mutate( across( everything(), \(x) if_else( str_detect(x, "''$"), "", x ) ) ) %>% 

data2 %>%
  mutate(
    across( Tuotekuvaus:Osasto, \(x) paste( lomakeID, x, kemikaaliID, sep = ", " ) ),
    across( Tuotekuvaus:Osasto, \(x) paste( "(", x, ")" ) ),
    across( everything(), \(x) if_else( str_detect(x, ", ,"), NA, x ) ),
    across( Tuotekuvaus:Osasto, \(x) if_else( !is.na(x), paste( "INSERT INTO [haipro2].[haipro].[kempro_lomake_valinnat_153] ( lomakeID, ObjektiID, sisaltoIDt, SisaltoTxt, kemikaaliID ) VALUES ", x ), x ) )
  ) %>%
  unite( "output", Tuotekuvaus:Osasto, sep = ";\n", na.rm = TRUE ) %>%
  pull(output) %>%
  writeClipboard()
  
# mutate( `Aineen.tai.seoksen.luokitus`           = paste( "xx", `Aineen.tai.seoksen.luokitus`, sep = ", "  ) ) %>% 
# mutate( `Aineen.ja/tai.seoksen.k?ytt?tapa`      = paste( "xx", `Aineen.ja/tai.seoksen.k?ytt?tapa`, sep = ", "  ) ) %>%

sisaltoid <- readClipboard()
sisalto <- readClipboard() %>% str_remove_all("\\s")
 
foo <- function(xx) { x <- sisaltoid[ which( tolower(sisalto) == tolower(xx) ) ]; ifelse( length(x) != 0, x, NA ) }; foo <- Vectorize(foo)
foo2 <- function(x) { x %>% str_split_1(", *") %>% foo() %>% str_flatten( ",", na.rm = TRUE ) }; foo2 <- Vectorize(foo2)
foo3 <- function(x) { x %>% str_extract_all("(H|EUH|GHS|R|S)\\d+") %>% unlist() %>% str_flatten(",") }; foo3 <- Vectorize(foo3);
foo3 <- function(x) { x %>% str_extract_all( "P\\d{2,}(\\s*\\+\\s*P\\d{2,})*") %>% unlist() %>% str_flatten(",") }; foo3 <- Vectorize(foo3);

data <- readClipboard() %>% as_tibble()

data %>%
  mutate( value2 = foo3(value) %>% str_remove_all("\\s") ) %>%
  mutate( value3 = foo2(value2) ) %>% 
  filter( str_count(value2, ",") == str_count(value3, ","), value != "" ) %>%
  mutate( sql = paste0( "UPDATE [haipro].[haipro].[kempro_lomake_valinnat_10] SET sisaltoidt = '", value3, "' WHERE sisaltoTxt = '", value, "' AND ObjektiID IN ( 13, 14, 19 )"  )  ) %>% 
  pull(sql) %>% 
  writeClipboard()


# --------

data <- clipr::read_clip_tbl( header = FALSE ) %>% as_tibble()

data %>%
  select(V1) %>%
  mutate(
    V1 = V1 %>% as.character() %>% if_else( is.na(.), "", . ) %>% trimws() %>% str_replace_all( c( "\\s{2,}" = " ", "\n" = " ", "'" = "''" ) ),
    z = paste0( "( ", 10:( nrow(.) + 9 ), ", 10, ", quotemarks(V1), ", ", 7:( nrow(.) + 6 ) , " )" )
  ) %>% 
  pull(z) %>%
  str_flatten(",\n") %>%
  writeClipboard()
