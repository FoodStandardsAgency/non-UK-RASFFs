library(tidyverse)
library(lubridate)
source('helpers.r')

etl <- function(){
  # Number of days considered to be the near-term.
  days <- 28
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
  ## Identify origin countries.
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
  ## Identify UK distribution.
  df_dist_uk <- df_countries_raw %>%
    dplyr::filter(
      stringr::str_detect(
        string=country,
        pattern=paste(c('United Kingdom', 'UK'), collapse='|')
        )
      ) %>%
    dplyr::filter(
      stringr::str_detect(string=country, pattern='\\(O\\)', negate=T)
      )
  # Create a full dataset.
  df_full <- dplyr::left_join(
    x=unique(df_data_raw),
    y=unique(df_details_raw),
    by='reference'
    ) %>%
    dplyr::mutate(date=as.Date(dateOfCase, format='%d/%m/%Y')) %>%
    dplyr::mutate(month=lubridate::month(date)) %>%
    dplyr::mutate(days_from_start=date - lubridate::ymd(19790101)) %>%
    dplyr::left_join(x=., y=unique(df_origins), by='reference') %>%
    dplyr::rename(origin_country=country) %>%
    dplyr::mutate(
      dist_uk=dplyr::if_else(
        condition=reference %in% df_dist_uk$reference,
        true=dist_uk<-1,
        false=dist_uk<-0)
      ) %>%
    dplyr::left_join(x=., y=unique(df_hazards_raw), by='reference') %>%
    dplyr::mutate(uk_rasff_soon=0)
  # Find the UK report days and  associated near-term window.
  df_uk_report_days <- df_full %>%
    dplyr::filter(notifyingCountry=='United Kingdom') %>%
    dplyr::select(product, origin_country, hazard, days_from_start) %>% 
    #dplyr::select(productCategory, origin_country, hazard, days_from_start) %>% 
    dplyr::mutate(days_before=days_from_start - days)
  dates_near_uk_rasff <- c()
  references_uk_soon <- c()
  for (r in 1:dim(df_uk_report_days)[1]){
    df_filter <- df_full %>%
      dplyr::filter(
        product == df_uk_report_days$product[r] &
          #productCategory == df_uk_report_days$productCategory[r] &
          origin_country == df_uk_report_days$origin_country[r] &
          #hazard == df_uk_report_days$hazard &
          dplyr::between(
            x=days_from_start,
            left=df_uk_report_days$days_before[r], 
            right=df_uk_report_days$days_from_start[r]-1
            )
        ) %>%
      dplyr::mutate(df_uk_rasff_soon=1)
    references_uk_soon <- c(
      references_uk_soon,
      dplyr::pull(df_filter, var=reference)
      )
    }
  # Mutate uk_rasff_soon to reflect imminent UK RASFFs and filter out.
  df_full <- dplyr::mutate(
    df_full, uk_rasff_soon=dplyr::if_else(
      condition=df_full$reference %in% references_uk_soon,
      true=df_full$uk_rasff_soon<-1,
      false=df_full$uk_rasff_soon<-0
      )
    )
  # Re-order the columns in the dataframe.
  col_order <- c(
    'reference', 'dateOfCase', 'date', 'month', 'days_from_start','subject',
    'type', 'classification', 'productCategory', 'product', 'hazardType',
    'hazard', 'uk_rasff_soon', 'notifyingCountry', 'origin_country',
    'dist_uk'
    )
  df_full <- df_full[, col_order] %>%
    dplyr::rename(date_of_case=dateOfCase) %>%
    dplyr::rename(product_category=productCategory) %>%
    dplyr::rename(hazard_type=hazardType) %>%
    dplyr::rename(notifying_country=notifyingCountry)
  # Use a restricted set of hazards.
  df_filtered_hazards <- standardise_hazards(data_frame=df_full)
  df_filtered <- standardise_products(data_frame=df_filtered_hazards)
  # Build and fit a Bayesian network.
  df_features <- df_filtered %>%
    dplyr::select(
      -c(
        reference, date_of_case, date, days_from_start, classification, type,
        subject, original_hazard, original_product
        )
      ) %>%
    dplyr::mutate(month=as.factor(month)) %>%
    dplyr::mutate(product_category=as.factor(product_category)) %>%
    dplyr::mutate(product=as.factor(product)) %>%
    dplyr::mutate(hazard_type=as.factor(hazard_type)) %>%
    dplyr::mutate(hazard=as.factor(hazard)) %>%
    dplyr::mutate(uk_rasff_soon=as.factor(uk_rasff_soon)) %>%
    dplyr::mutate(notifying_country=as.factor(notifying_country)) %>%
    dplyr::mutate(origin_country=as.factor(origin_country)) %>%
    dplyr::mutate(dist_uk=as.factor(dist_uk)) %>%
    na.omit() %>%
    droplevels()
  df_features <- data.frame(df_features)
  return(df_features)
}