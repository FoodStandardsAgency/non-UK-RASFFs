# Non-UK RASFF helpers.
library(tidyverse)
library(bnlearn)
library(svMisc)
source('ignore.r')


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
      per_cent_done <- 100 * (r / dim(data_frame)[1])
      svMisc::progress(value=per_cent_done, progress.bar=FALSE)
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

estimates_vs_reality <- function(estimates, reality){
  df <- dplyr::tibble(estimates, reality) %>%
    dplyr::mutate(estimates=round(estimates, digits=2)) %>%
    dplyr::mutate(reality=as.numeric(reality)-1) %>%
    dplyr::mutate(p_group=as.factor(estimates))
  averages <- df %>%
    group_by(p_group) %>% 
    dplyr::summarise(mean=mean(reality, na.rm=TRUE), n=n()) %>%
    dplyr::filter(n >= 5)
  return(averages)
  }

plot_estimates_vs_reality <- function(data_frame){
  plt <- ggplot2::ggplot(
    data=data_frame,
    aes(x=as.numeric(as.character(data_frame$p_group)), y=data_frame$mean)
  ) +
    geom_abline(intercept=0, slope=1, colour='grey50', linetype='dashed') +
    geom_point(lwd=2) +
    xlab('Predicted Probability') +
    ylab('Realised Proportion of Events') +
    xlim(low=0, high=1) +
    ylim(low=0, high=1) +
  theme_bw() 
  return(plt)
  }

standardise_hazards <- function(data_frame){
  df <- data_frame %>%
    mutate(original_hazard=hazard) %>%
    mutate(
      hazard=replace(hazard, str_detect(hazard, 'aflatoxin'), 'Aflatoxins')
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'almonella'), 'Salmonella spp.'
        )
      ) %>%
    mutate(
      hazard=replace(hazard, str_detect(hazard, 'mercury'), 'Mercury')
      ) %>%
    mutate(
      hazard=replace(hazard, str_detect(hazard, 'isteria'), 'Listeria spp.')
      ) %>%
    mutate(
      hazard=replace(hazard, str_detect(hazard, 'scherichia coli'), 'E. coli')
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'sulphite unauthorised'), 'Sulphite'
        )
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'sulphite undeclared'), 'Sulphite'
        )
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'too high content of sulphite'), 'Sulphite')
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'cadmium'), 'Cadmium')
      ) %>%
    mutate(
      hazard=replace(
        hazard, 
        str_detect(hazard, 'genetically modified'),
        'Genetically Modified'
        )
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'ochratoxin A'), 'Ochratoxin A'
        )
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'histamine'), 'Histamine'
        )
      ) %>%
    mutate(
      hazard=replace(
        hazard, str_detect(hazard, 'fipronil'), 'Fipronil'
        )
      )
  hazards_selection <- c('Aflatoxins', 'Salmonella spp.', 'Listeria spp.',
                         'Mercury', 'E. coli', 'Sulphite', 'Cadmium',
                         'Genetically Modified', 'Ochratoxin A', 'Histamine',
                         'Fipronil')
  df <- df %>%
    dplyr::filter(hazard %in% hazards_selection)
  return(df)
  }