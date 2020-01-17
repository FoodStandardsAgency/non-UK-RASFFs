# Non-UK RASFFs model construction.
library(lubridate)
source('helpers.r')

# Load the data.
## Columns to be collected.
data_columns1 <- 'classification, dateOfCase, reference, notifyingCountry,'
data_columns2 <- 'subject, productCategory, type' 
## Take the data from the database.
df_data_raw <- load_rasffs(
  columns=paste(data_columns1, data_columns2),
  table='rasff_data'
  )
df_details_raw <- load_rasffs(
  columns='reference, product, hyperlink',
  table='rasff_details'
  )
df_countries_raw <- load_rasffs(
  columns='reference, country',
  table='rasff_countries'
  )
df_hazards_raw <- load_rasffs(
  columns='reference, hazard, hazardType',
  table='rasff_hazards'
  )

# Edit tables prior to amalgamating.
df_origins <- df_countries_raw %>%
  dplyr::filter(
    stringr::str_detect(
      string=country,
      pattern=paste(c('(O)', '(D/O)'), collapse='|')
      )
  ) %>%
  dplyr::mutate(
    country=stringr::str_remove(
      string=country,
      pattern=paste(c('\\(O\\)', '\\(D/O\\)'), collapse='|')
      )
    )

# Create a full dataset.
df_full_raw <- dplyr::left_join(
  x=unique(df_data_raw),
  y=unique(df_details_raw),
  by='reference'
  ) %>%
  dplyr::mutate(date=as.Date(dateOfCase, format='%d/%m/%Y')) %>%
  dplyr::mutate(month=lubridate::month(date)) %>%
  dplyr::mutate(days_from_start=date - lubridate::ymd(19790101)) %>%
  dplyr::left_join(x=., y=unique(df_origins), by='reference') %>%
  dplyr::rename(origin_country=country)