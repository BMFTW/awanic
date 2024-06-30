library(tidyverse)

data <- readClipboard()
data2 <- data %>% keep( \(line) str_detect( line, "Directory of" ) || str_detect( line, "\\d File" ) ) %>% as_tibble()
data3 <- data2 %>% mutate( y = lead(value) ) %>% filter( str_detect( value, "Directory of" ) ) %>% mutate( value = str_remove(value, "Directory of") %>% trimws(), y = str_extract(y, "\\d+") )
final <- data3 %>% mutate( id = str_extract(value, "\\d+") %>% as.numeric(), y = as.numeric(y) ) %>% group_by(id) %>% summarise(sum_value = sum(y))

data
