# Non-UK RASFF helpers.
library(tidyverse)

# Database connection.
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver='SQL Server',
  Database='TEST',
  Server='VMP1WSQLDB9',
  port=1433
  )

df_data_raw <- DBI::dbGetQuery(
  conn=con,
  statement='
    SELECT *
    FROM [SPRINT_RiskyFoods].[dbo].[rasff_data]
  '
  )

