# Non-UK RASFF helpers.
library(tidyverse)
# DB9_connection.R provides database access.

load_rasffs <- function(columns='*', table=''){
  # The data dataframe.
  data_tibble <- dplyr::as_tibble(
    DBI::dbGetQuery(
      conn=con,
      statement= paste0(
        'SELECT ', columns, ' FROM [SPRINT_RiskyFoods].[dbo].[', table, ']'
      )
    )
  )
  return(data_tibble)
}




