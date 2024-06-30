# Packages
library(tidyverse)
library(openxlsx)

# Read data
data <- clipr::read_clip_tbl( header = FALSE ) %>%
  as_tibble() %>%
  rename( org_taso = V1, org_lyhyt_nimi = V2, org_nimi = V3, rooli = V4, kasittelija = V5 )

# createWhitespace function
createWhitespace <- function(n, string) { strrep(" ", ( as.numeric(n)-1 ) * 20 ) %>% paste(string, sep = "") };
createWhitespace <- Vectorize(createWhitespace)

# Modify data
data <- raw_data %>%
  mutate( org_lyhyt_nimi = org_lyhyt_nimi %>% str_remove("NULL") ) %>% 
  mutate( org_nimi = createWhitespace(org_taso, org_nimi) ) %>% 
  select( -org_taso )

# Create a workbook and add a worksheet
wb <- createWorkbook()
addWorksheet( wb, "Org-rakenne" )

# Write data to the worksheet
writeData( wb, sheet = "Org-rakenne", x = data )

# Create a style and apply
style <- createStyle( fgFill = "#c4d79b" )
addStyle( wb, sheet = "Org-rakenne", style = style, rows = 1, cols = 1:4 )

# Auto-fit column widths
setColWidths( wb, sheet = "Org-rakenne", cols = 1:4, widths = "auto" )

# Save the workbook to a file
saveWorkbook( wb, "C:/Users/simok/Desktop/haipro_org_248.xlsx", overwrite = TRUE )

# Legacy ------------------------------------------------------------------

# data %>%
#   mutate( org_lyhyt_nimi = org_lyhyt_nimi %>% str_remove("NULL") ) %>% 
#   mutate( org_nimi = createWhitespace(org_taso, org_nimi) ) %>% 
#   select(-org_taso) %>% 
#   write.xlsx("C:/Users/simok/Desktop/org_248.xlsx")


# pivot_wider -------------------------------------------------------------

# Read data

data <- clipr::read_clip_tbl( header = FALSE ) %>%
  as_tibble() %>%
  rename( org_id = V1, org_taso = V2, indeksi = V3, org_lyhyt_nimi = V4, org_nimi = V5, rooli = V6, kasittelija = V7 )

# Transform data

data %>%
  pivot_wider( names_from = rooli, values_from = kasittelija, values_fn = str_flatten_comma ) %>%
  mutate( across( where(is.character), \(x) replace( x, is.na(x) | x == "NULL", "" ) ) ) %>%
  mutate(
    "Level 1" = ifelse( org_taso == 1, org_nimi, "" ),
    "Level 2" = ifelse( org_taso == 2, org_nimi, "" ),
    "Level 3" = ifelse( org_taso == 3, org_nimi, "" ),
    "Level 4" = ifelse( org_taso == 4, org_nimi, "" ),
    "Level 5" = ifelse( org_taso == 5, org_nimi, "" ),
    "Level 6" = ifelse( org_taso == 6, org_nimi, "" ),
    "Level 7" = ifelse( org_taso == 7, org_nimi, "" ),
    "Level 8" = ifelse( org_taso == 8, org_nimi, "" ),
    "Level 9" = ifelse( org_taso == 9, org_nimi, "" ),
    "Level 10" = ifelse( org_taso == 10, org_nimi, "" )
  ) %>%
  rename( Code = org_lyhyt_nimi ) %>% 
  relocate( starts_with("Level"), .after = Code ) %>% 
  arrange(indeksi) %>% 
  select( -org_id, -org_taso, -indeksi, -org_nimi )
