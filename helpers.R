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
    dplyr::mutate(original_hazard=hazard) %>%
    dplyr::mutate(
      hazard=base::replace(
        x=hazard,
        list=stringr::str_detect(string=hazard, pattern='aflatoxin'),
        values='Aflatoxins'
        )
      ) %>%
    dplyr::mutate(
      hazard=base::replace(
        x=hazard,
        list=stringr::str_detect(string=hazard, pattern='almonella'),
        values='Salmonella spp.'
        )
      ) %>%
    dplry::mutate(
      hazard=base::replace(
        x=hazard, 
        list=stringr::str_detect(string=hazard, pattern='mercury'),
        values='Mercury'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard,
        list=stringr::str_detect(string=hazard, pattern='isteria'),
        values='Listeria spp.'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(string=hazard, pattern='scherichia coli'), 
        values='E. coli'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(
          string=hazard, 
          pattern='sulphite unauthorised'
          ), 
        values='Sulphite'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard,
        list=stringr::str_detect(string=hazard, pattern='sulphite undeclared'),
        values='Sulphite'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(
          string=hazard,
          pattern='too high content of sulphite'
          ), 
        values='Sulphite'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(string=hazard, pattern='cadmium'), 
        values='Cadmium'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(
          string=hazard,
          pattern='genetically modified'
          ),
        values='Genetically Modified'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(string=hazard, pattern='ochratoxin A'),
        values='Ochratoxin A'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard, 
        list=stringr::str_detect(string=hazard, pattern='histamine'), 
        values='Histamine'
        )
      ) %>%
    mutate(
      hazard=replace(
        x=hazard,
        list=stringr::str_detect(string=hazard, pattern='fipronil'),
        values='Fipronil'
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

standardise_products <- function(data_frame){
  df <- data_frame %>%
    dplyr::mutate(original_product=product) %>%
    dplyr::mutate(
      product=base::replace(
        x=product, 
        list=stringr::str_detect(string=product, pattern='food supplement'),
        values='Food Supplement'
        )
      ) %>%
    dplyr::mutate(
      product=base::replace(
        x=product,
        list=stringr::str_detect(string=product, pattern='Pistachio'),
        values='Pistachios'
        )
      ) %>%
    dplyr::mutate(
      product=replace(
        x=product,
        list=stringr::str_detect(string=product, pattern='pistachio'),
        values='Pistachios')) %>%
    dplyr::mutate(
      product=replace(
        x=product, 
        list=stringr::str_detect(string=product, pattern='dried figs'),
        values='Dried Figs'
        )
      ) %>%
    mutate(product=replace(product, str_detect(product, 'dried  figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'drieg figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'Dried figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried mini figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried garland figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried organic figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried Lerida figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried lerida figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried baglama figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried Protoben figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried fig cubes'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried diced figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried Layer figs'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'dried fig garlands'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'figs - dried'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'Figs - dried'), 'Dried Figs')) %>%
    mutate(product=replace(product, str_detect(product, 'groundnut'), 'Peanuts')) %>%
    mutate(product=replace(product, str_detect(product, 'peanut'), 'Peanuts')) %>%
    mutate(product=replace(product, str_detect(product, 'Peanut'), 'Peanuts')) %>%
    mutate(product=replace(product, str_detect(product, 'hazelnut'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - shelled'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnut kernels without shell'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - Natural'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnut kernels,shelled'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnut kernels'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - shelled kernels'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - shelled'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - peeled'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - minced'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts kernels'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts kernels - ground'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - kernels'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts kernels - broken'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - ground'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts - broken'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'Hazelnuts powder'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'peeled hazel nuts'), 'Hazelnuts'))%>%
    mutate(product=replace(product, str_detect(product, 'soybean meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soybeanmeal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soybean pellets'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soybean pellets'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soybeanmeal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soy bean meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soya bean meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soya pellets'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soya meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soyabean meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soy pellets in bulk'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'organic soy beans'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'organic soya'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soyabean meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soy meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soya'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soymeal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soybean'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'soy-bean meal'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'organic soy bean'), 'Soybean Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'fish meal'), 'Fish Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'fishmeal'), 'Fish Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'Fish meal'), 'Fish Meal')) %>%
    mutate(product=replace(product, str_detect(product, 'almonds'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'almond kernels'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'whole almond kernels with skin'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'sweet almond kernels'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'almond kernels with skin'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'almond meal'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'sweet shelled almond kernels'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'almond'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'almond flakes'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'almond powder'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'diced almond'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'diced almond kernels'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'half almond kernels with skin'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'sweet almond grains'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'unpasteurised almond kernels'), 'Almonds')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'salmon - smoked'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon - smoked'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon - smoked kodiak wild'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked Norwegian salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked sliced salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled smoked Norwegian salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled smoked sliced salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled smoked vacuum-packed salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon - smoked Atlantic'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked and graved salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked and salted salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked atlantic salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Smoked salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled smoked Norvegian salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled smoked Scottish salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'frozen smoked vacuum packed salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon - fillets smoked scotch'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon - Norwegian smoked'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon - Sliced salmo salar smoked'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon smoked'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Salmon smokes - (salmo salar)'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked Atlantic salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked farmed salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked organic atlantic salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'Smoked Salmon trimmings'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'smoked wild salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'vacuum packed smoked Atlantic salmon'), 'Smoked Salmon')) %>%
    mutate(product=replace(product, str_detect(product, 'sesame seed'), 'Sesame Seeds')) %>%
    mutate(product=replace(product, str_detect(product, 'Sesame seed'), 'Sesame Seeds')) %>%
    mutate(product=replace(product, str_detect(product, 'hulled sesame'), 'Sesame Seeds')) %>%
    mutate(product=replace(product, str_detect(product, 'sesam seed'), 'Sesame Seeds')) %>%
    mutate(product=replace(product, str_detect(product, 'whole chicken'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'whole hens'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'whole broiler chicken'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'whole frozen chicken without giblets'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'frozen whole raw chicken'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'whole frozen chicken'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'whole fresh chicken'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'frozen whole eviscerated chicken'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'Chicken - whole frozen'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken half breasts'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken breast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'Chicken breast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chickenbreast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'Chickenbreast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken boneless skinless'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken boneless skinless breast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken half breast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken half breast'), 'Chicken')) %>%
    mutate(product=replace(product, str_detect(product, 'chicken'), 'Chicken')) %>% ### this might be a bit rough
    mutate(product=replace(product, str_detect(product, 'wordfish'), 'Swordfish')) %>%
    mutate(product=replace(product, str_detect(product, 'word fish'), 'Swordfish')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Raw Milk cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Raw milk cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk reblochon'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "raw milk goat's cheese"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "raw milk sheep's cheeses"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk sheep cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk camembert'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk brie cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "raw milk sheep's cheese"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk Neufchâtel cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'organic raw milk camembert'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk camembert cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk reblochon cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled sheep cheese made from raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk Reblochon'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'cheese made from raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Cheese made from raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'sliced raw milk camembert'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk raclette cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk soft cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "milk cheese made with raw milk"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'chilled raw milk brie'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'cheese from raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk raclett'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'camembert made from raw mil'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'raw milk goat cheese'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "cow's milk cheese made"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "raw milk's cheese"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "raw milk's cheese"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "Camembert cheese made"), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Cheese - made from raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Cheese produced with raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Cheese - semi-soft raw milk rind washed'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'Cheese - raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, 'soft cheese produced from raw milk'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, "(produced from raw milk)"), 'Raw Milk Cheese')) %>% # is cheese-only
    mutate(product=replace(product, str_detect(product, 'raw milk brie and camembert'), 'Raw Milk Cheese')) %>%
    mutate(product=replace(product, str_detect(product, regex('dried apricot', ignore_case=TRUE)), 'Dried Apricot')) %>%
    mutate(product=replace(product, str_detect(product, regex('dried pitted apricot', ignore_case=TRUE)), 'Dried Apricot')) %>%
    mutate(product=replace(product, str_detect(product, regex('dried and pitted apricot', ignore_case=TRUE)), 'Dried Apricot')) %>%
    mutate(product=replace(product, str_detect(product, regex('brazil nut', ignore_case=TRUE)), 'Brazil Nuts')) %>%
    mutate(product=replace(product, str_detect(product, regex('brazilnut', ignore_case=TRUE)), 'Brazil Nuts')) %>%
    mutate(product=replace(product, str_detect(product, regex('brazil nust', ignore_case=TRUE)), 'Brazil Nuts')) %>%
    mutate(product=replace(product, str_detect(product, regex('rice noodles', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('rice spaghetti', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('rice pasta', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('rice vermicelli', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('vermicelli rice', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('rice macaroni', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('rice-flour noodles', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('organic brown rice ramen noodles', ignore_case=TRUE)), 'Rice Noodles')) %>%
    mutate(product=replace(product, str_detect(product, regex('raisins', ignore_case=TRUE)), 'Raisins')) %>%
    mutate(product=replace(product, str_detect(product, regex('tuna', ignore_case=TRUE)), 'Tuna')) %>%
    mutate(product=replace(product, str_detect(product, regex('beef', ignore_case=TRUE)), 'Beef')) %>%##  rough?
    mutate(product=replace(product, str_detect(product, regex('rump steak', ignore_case=TRUE)), 'Beef')) %>% ##  rough?
    mutate(product=replace(product, str_detect(product, regex('black angus steak', ignore_case=TRUE)), 'Beef')) %>% ##  rough?
    mutate(product=replace(product, str_detect(product, regex('pork', ignore_case=TRUE)), 'Pork')) %>% ## rough
    mutate(product=replace(product, str_detect(product, regex('pigeon', ignore_case=TRUE)), 'p_i_g_eon')) %>% ## rough
    mutate(product=replace(product, str_detect(product, regex('pig', ignore_case=TRUE)), 'Pork')) %>%
    mutate(product=replace(product, str_detect(product, regex('p_i_g_eon', ignore_case=TRUE)), 'pigeon'))
  products_selection <- c('Peanuts',
                          'Pistachios',
                          'Dried Figs', 
                          'Hazelnuts',
                          'Chicken Breast',
                          'Sesame Seeds',
                          'Soybean Meal',
                          'Almonds',
                          'Smoked Salmon',
                          'Fish Meal',
                          'Swordfish',
                          'Raw Milk Cheese',
                          'Dried Apricot',
                          'Brazil Nuts',
                          'Rice Noodles',
                          'Raisins',
                          'Tuna',
                          'Beef',
                          'Pork'
                          )
  df <- df %>%
    dplyr::filter(product %in% products_selection)
  return(df)
  }