# Non-UK RASFF helpers.
library(tidyverse)
library(bnlearn)
library(svMisc)
source('ignore.r')
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


estimate_probabilities <- function(
  data_frame, bayesian_network, predict_column
  ){
    probs <- c()
    for (r in 1:dim(data_frame)[1]){
      per_cent_done <- round(100 * (r / dim(data_frame)[1]))
      svMisc::progress(value=per_cent_done, progress.bar=F)
      p <- bnlearn::cpquery(
        fitted=bayesian_network,
        event=(uk_rasff_soon=='1'),
        evidence=(as.list(data_frame[r, c(-predict_column)])),
        method='lw',
        n=1e5
      )
      probs <- c(probs, p)
    }
    return(probs)
    }

