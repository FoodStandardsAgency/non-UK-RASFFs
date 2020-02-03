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
      statement=base::paste0(
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


mutate_replace_hazard <- function(
  .data, old_string, new_string, hazard=hazard
  ){
  dplyr::mutate(.data,
    hazard=base::replace(
      x=hazard,
      list=stringr::str_detect(
        string=hazard,
        pattern=stringr::regex(
          pattern=old_string, ignore_case=TRUE
          )
        ),
      values=new_string
      )
    )
}


mutate_replace_product <- function(
  .data, old_string, new_string, product=product
  ){
  dplyr::mutate(.data,
                product=base::replace(
                  x=product,
                  list=stringr::str_detect(
                    string=product, 
                    pattern=stringr::regex(
                      pattern=old_string, ignore_case=TRUE
                      )
                    ),
                  values=new_string
                  )
                )
  }


standardise_hazards <- function(data_frame){
  df <- data_frame %>%
    dplyr::mutate(original_hazard=hazard) %>%
    mutate_replace_hazard(old_string='aflatoxin', new_string='Aflatoxins') %>%
    mutate_replace_hazard(
      old_string='almonella', new_string='Salmonella spp.'
      ) %>%
    mutate_replace_hazard(old_string='mercury', new_string='Mercury') %>%
    mutate_replace_hazard(old_string='isteria', new_string='Listeria spp.') %>%
    mutate_replace_hazard(
      old_string='scherichia coli', new_string='E. coli'
      ) %>%
    mutate_replace_hazard(
      old_string='sulphite unauthorised', new_string='Sulphite'
      ) %>%
    mutate_replace_hazard(
      old_string='sulphite undeclared', new_string='Sulphite'
      ) %>%
    mutate_replace_hazard(
      old_string='too high content of sulphite', new_string='Sulphite'
      ) %>%
    mutate_replace_hazard(old_string='cadmium', new_string='Cadmium') %>%
    mutate_replace_hazard(
      old_string='genetically modified', new_string='Genetically Modified'
      ) %>%
    mutate_replace_hazard(
      old_string='ochratoxin A', new_string='Ochratoxin A'
      ) %>%
    mutate_replace_hazard(
      old_string='histamine', new_string='Histamine'
      ) %>%
    mutate_replace_hazard(
      old_string='fipronil', new_string='Fipronil')
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
    mutate_replace_product(
      old_string='food supplement', new_string='Food Supplement'
      ) %>%
    mutate_replace_product(old_string='Pistachio', new_string='Pistachios') %>%
    mutate_replace_product(
      old_string='dried fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried  fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried mini fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried garland fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried organic fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried lerida fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried baglama fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried Protoben fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried fig cube', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried diced fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried layer fig', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='dried fig garland', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(
      old_string='figs - dried', new_string='Dried Figs'
      ) %>%
    mutate_replace_product(old_string='groundnut', new_string='Peanuts') %>%
    mutate_replace_product(old_string='peanut', new_string='Peanuts') %>%
    mutate_replace_product(old_string='hazelnut', new_string='Hazelnuts') %>%
    mutate_replace_product(
      old_string='hazelnuts - shelled', new_string='Hazelnuts'
      ) %>%
    mutate_replace_product(
      old_string='hazelnut kernels without shell', new_string='Hazelnuts'
      ) %>%
    mutate_replace_product(
      old_string='hazel nut', new_string='Hazelnuts'
      ) %>%
    mutate_replace_product(
      old_string='soybean meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soybeanmeal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soyabeanmeal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soybean pellet', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soy bean meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soya bean meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soya meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soya pellets', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soy pellets in bulk', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='organic soya', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soyabean meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soy meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soymeal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(
      old_string='soy-bean meal', new_string='Soybean Meal'
      ) %>%
    mutate_replace_product(old_string='fish meal', new_string='Fish Meal') %>%
    mutate_replace_product(old_string='fishmeal', new_string='Fish Meal') %>%
    mutate_replace_product(old_string='almonds', new_string='Almonds') %>%
    mutate_replace_product(
      old_string='almond kernels', new_string='Almonds'
      ) %>%
    mutate_replace_product(old_string='almond meal', new_string='Almonds') %>%
    mutate_replace_product(
      old_string='almond flakes', new_string='Almonds'
      ) %>%
    mutate_replace_product(
      old_string='almond powder', new_string='Almonds'
      ) %>%
    mutate_replace_product(
      old_string='diced almond', new_string='Almonds'
      ) %>%
    mutate_replace_product(
      old_string='half almond kernels with skin', new_string='Almonds'
      ) %>%
    mutate_replace_product(
      old_string='sweet almond grains', new_string='Almonds'
      ) %>%
    mutate_replace_product(
      old_string='smoked salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='salmon smoked', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='salmon smokes', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='salmon - smoked', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked Norwegian salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked sliced salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='chilled smoked vacuum-packed salmon',
      new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked atlantic salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='Salmon - smoked Atlantic', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked and graved salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked and salted salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='chilled smoked Scottish salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='frozen smoked vacuum packed salmon',
      new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='Salmon - fillets smoked scotch', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='Salmon - Sliced salmo salar smoked',
      new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked farmed salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked organic atlantic salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='smoked wild salmon', new_string='Smoked Salmon'
      ) %>%
    mutate_replace_product(
      old_string='sesame seed', new_string='Sesame Seeds'
      ) %>%
    mutate_replace_product(
      old_string='hulled sesame', new_string='Sesame Seeds'
      ) %>%
    mutate_replace_product(
      old_string='sesam seed', new_string='Sesame Seeds'
      ) %>%
    mutate_replace_product(
      old_string='whole chicken', new_string='Chicken'
      ) %>%
    mutate_replace_product(
      old_string='whole hens', new_string='Chicken'
      ) %>%
    mutate_replace_product(
      old_string='whole broiler chicken', new_string='Chicken'
      ) %>%
    mutate_replace_product(
      old_string='whole frozen chicken', new_string='Chicken'
      ) %>%
    mutate_replace_product(
      old_string='whole raw chicken', new_string='Chicken'
      ) %>%
    mutate_replace_product(
      old_string='whole fresh chicken', new_string='Chicken'
      ) %>%
    mutate_replace_product(
      old_string='frozen whole eviscerated chicken', new_string='Chicken'
      ) %>%  
    mutate_replace_product(
      old_string='Chicken - whole frozen', new_string='Chicken'
      ) %>% 
    mutate_replace_product(
      old_string='chicken half breasts', new_string='Chicken'
      ) %>% 
    mutate_replace_product(
      old_string='chicken breast', new_string='Chicken'
      ) %>% 
    mutate_replace_product(
      old_string='chickenbreast', new_string='Chicken'
      ) %>% 
    mutate_replace_product(
      old_string='chicken boneless skinless', new_string='Chicken'
      ) %>% 
    
    mutate(product=replace(product, str_detect(product, 'swordfish'), 'Swordfish')) %>%
    mutate(product=replace(product, str_detect(product, 'sword fish'), 'Swordfish')) %>%
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
                          'Chicken',
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
