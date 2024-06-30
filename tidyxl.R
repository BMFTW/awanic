library(tidyverse)
library(tidyxl)

data <- tidyxl::xlsx_cells("C:/Users/simok/Desktop/bb.xlsx") 
formats <- tidyxl::xlsx_formats("C:/Users/simok/Desktop/bb.xlsx") 

data <- bb$data[[1]] %>% as_tibble()

data %>% 
  filter( local_format_id %in% which( formats$local$font$color$rgb == "FFFF0000" ) ) %>% 
  filter( !is.na(character) ) %>% 
  pull(character) %>% 
  str_extract( "(?<=\\[)[^\\]]+" )

