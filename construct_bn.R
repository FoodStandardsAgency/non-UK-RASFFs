# code to fit Bayesian networks to RASFF data

#----------
# packages
#----------
library(tidyverse)
library(bnlearn)
library(bnviewer)
library(caret)
library(OneR)
#library(svMisc)

#----------
# load data
#----------
d <- readRDS('RASFF_080819.rds')

#------------
# filter data
#------------
hazard_frequencies <- d %>% 
  group_by(hazard) %>% 
  summarise(freq=n()) %>%
  arrange(desc(freq))

hazards_selection <- hazard_frequencies$hazard[1:100]

d_hazards <- d %>%
  filter(hazard %in% hazards_selection)

#saveRDS(object=d_hazards, file='RASFF_010819_20hazards.rds')


#--------------------------------
# identify when RASFF were raised
#--------------------------------
# find soon UK reports
d_hazards <- mutate(d_hazards, uk_report_soon=0)
DAYS <- 28
## find the days the UK reported and work out a window of interest
uk_report_days <- d_hazards %>%
  filter(notifyingCountry=='United Kingdom') %>%
  select(product_level2, days_from_start) %>%
  mutate(days_before=days_from_start-DAYS)
## mutate uk_report_soon if uk reports for same product_level2 in DAYS
references_uk_soon <- c()
for (r in 1:dim(uk_report_days)[1]){
  d_filter <- d_hazards %>%
    filter(product_level2==uk_report_days$product_level2[r]&
             between(x=days_from_start, left=uk_report_days$days_before[r], 
                     right=uk_report_days$days_from_start[r]-1)) %>% #-1 to avoid same day alerts
    mutate(uk_report_soon=1)
  references_uk_soon <- c(references_uk_soon,
                          pull(d_filter, reference))
}
d_hazards <- mutate(d_hazards, 
                    uk_report_soon=if_else(condition=d_hazards$reference%in%references_uk_soon,
                                           true=d_hazards$uk_report_soon<-1,
                                           false=d_hazards$uk_report_soon<-0
                    )
)

## what percentage of reports have a UK RASFF soon after?
round(100*sum(d_hazards$uk_report_soon)/length(d_hazards$uk_report_soon),1)

# find days when UK distributed to
d_hazards <- mutate(d_hazards, uk_listed_soon=0)
uk_listed_days <- d_hazards %>%
  filter(dist_uk=='1') %>%
  select(product_level2, days_from_start) %>%
  mutate(days_before=days_from_start-DAYS)
## mutate uk_report_soon if uk reports for same product_level2 in DAYS
references_uk_listed_soon <- c()
for (r in 1:dim(uk_listed_days)[1]){
  d_filter <- d_hazards %>%
    filter(product_level2==uk_listed_days$product_level2[r]&
             between(x=days_from_start, left=uk_listed_days$days_before[r], 
                     right=uk_listed_days$days_from_start[r]-1)) %>% # -1 to avoid same day reports
    mutate(uk_listed_soon=1)
  references_uk_listed_soon <- c(references_uk_listed_soon,
                                 pull(d_filter, reference))
}

d_hazards <- mutate(d_hazards, uk_listed_soon=if_else(condition=d_hazards$reference%in%references_uk_listed_soon,
                                                      true=d_hazards$uk_listed_soon<-1,
                                                      false=d_hazards$uk_listed_soon<-0
)
)


#--------------
# analysis data
#--------------
d_analysis <- d_hazards %>%
  replace_na(list(origin_country='Unknown')) %>%
  mutate(origin_country=replace(origin_country, 
                                str_detect(origin_country, 'unknown'), 'Unknown')) %>%
  mutate(uk_rasff_soon=if_else(condition=uk_report_soon==1|uk_listed_soon==1,
                               true=uk_rasff_soon <- 1,
                               false=uk_rasff_soon <- 0)) %>%
  select(reference, date, subject,
         month, type,
         classification, distributionStatus,
         origin_country, notifyingCountry,
         hazard, product_level1, product_level2,
         riskDecision, action, dist_uk, uk_report_soon, uk_listed_soon,
         uk_rasff_soon) %>%
  mutate(month=as.factor(month)) %>%
  mutate(type=as.factor(type)) %>%
  mutate(classification=as.factor(classification)) %>%
  mutate(distributionStatus=as.factor(distributionStatus)) %>%
  mutate(origin_country=as.factor(origin_country)) %>%
  mutate(notifyingCountry=as.factor(notifyingCountry)) %>%
  mutate(hazard=as.factor(hazard)) %>%
  mutate(product_level1=as.factor(product_level1)) %>%
  mutate(product_level2=as.factor(product_level2)) %>%
  mutate(riskDecision=as.factor(riskDecision)) %>%
  mutate(action=as.factor(action)) %>%
  mutate(dist_uk=if_else(condition=dist_uk=='1',
                         true='Yes',
                         false='No'
  )) %>%
  mutate(dist_uk=as.factor(dist_uk)) %>%
  mutate(uk_report_soon=as.factor(uk_report_soon)) %>%
  mutate(uk_listed_soon=as.factor(uk_listed_soon)) %>%
  mutate(uk_rasff_soon=as.factor(uk_rasff_soon)) %>%
  mutate(year=lubridate::year(date)) #%>%
#select(-date, -year)
d_analysis <- unique(d_analysis)
glimpse(d_analysis)


#-------------------------
# create Bayesian networks
#-------------------------
# make some datasets
set.seed(54321)
d_analysis_cropped <- d_analysis %>% select(month,
                                            #classification, 
                                            #type
                                            #, riskDecision,
                                            #distributionStatus,
                                            origin_country, notifyingCountry,  
                                            dist_uk, hazard, 
                                            product_level1,
                                            #product_level1, product_level2,
                                            uk_rasff_soon
) 

train_index <- sample(1:dim(d_analysis_cropped)[1], floor(dim(d_analysis_cropped)[1]*0.8))
#test_index <- c(1:(0.1*(dim(d_analysis_cropped)[1])) )
d_train <- d_analysis_cropped[train_index,]
d_test <- d_analysis_cropped[-train_index,]

#d_train_os <- ovun.sample(formula=uk_rasff_soon~., 
#                          data=d_train, 
#                          method='under',
#                          N=dim(d_train)[1])

bn_tan <- tree.bayes(data.frame(d_train),
                     'uk_rasff_soon')
bn_tan_fit <- bn.fit(bn_tan,
                     data.frame(d_train),
                     method='mle'
)
bn_tan_pred <- predict(bn_tan_fit,
                       node='uk_rasff_soon', ## knows what node to predict as specified when making TAN
                       data=data.frame(d_test))
confusionMatrix(bn_tan_pred, d_test$uk_rasff_soon)

#------------------------------
# Checking outputs, uncertainty
#------------------------------

outcome_examples <- data.frame(d_test[sample(1:dim(d_test)[1]),])

start <- Sys.time()
probs <- vector()
for (i in 1:dim(d_test)[1]){
  Sys.sleep(0.01)
  print(paste0(round(100*i/dim(d_test)[1],2), '% done'))
  probs[i] <- mean(
    #replicate(100,
    cpquery(fitted=bn_tan_fit, 
            event=(uk_rasff_soon=='1'),
            evidence=(as.list(d_test[i,c(-7)])), # index used here
            method='lw',
            n=1e5)
    #)
  )
}
end <- Sys.time(); end-start

plot(probs, jitter(as.numeric(d_test$uk_rasff_soon)-1),
     xlim=c(0,1), ylim=c(0,1),
     xlab='Predicted', ylab='Actual')

pred_actual <- data.frame(as.numeric(d_test$uk_rasff_soon)-1, probs) %>%
  drop_na()
names(pred_actual) <- c('Actual', 'Predicted')
pred_actual$bins <- bin(pred_actual$Predicted, nbins=10,
                        lables<-c('L1','L2','L3','L4','L5',
                                  'L6','L7','L8','L9','L10'))


l1 <- pred_actual %>% filter(bins=='L1'); l1_prop <- sum(l1$Actual)/length(l1$Actual)
l2 <- pred_actual %>% filter(bins=='L2'); l2_prop <- sum(l2$Actual)/length(l2$Actual)
l3 <- pred_actual %>% filter(bins=='L3'); l3_prop <- sum(l3$Actual)/length(l3$Actual)
l4 <- pred_actual %>% filter(bins=='L4'); l4_prop <- sum(l4$Actual)/length(l4$Actual)
l5 <- pred_actual %>% filter(bins=='L5'); l5_prop <- sum(l5$Actual)/length(l5$Actual)
l6 <- pred_actual %>% filter(bins=='L6'); l6_prop <- sum(l6$Actual)/length(l6$Actual)
l7 <- pred_actual %>% filter(bins=='L7'); l7_prop <- sum(l7$Actual)/length(l7$Actual)
l8 <- pred_actual %>% filter(bins=='L8'); l8_prop <- sum(l8$Actual)/length(l8$Actual)
l9 <- pred_actual %>% filter(bins=='L9'); l9_prop <- sum(l9$Actual)/length(l9$Actual)
l10 <- pred_actual %>% filter(bins=='L10'); l10_prop <- sum(l10$Actual)/length(l10$Actual)

props <- c(l1_prop, l2_prop, l3_prop, l4_prop, l5_prop,
           l6_prop, l7_prop, l8_prop, l9_prop, l10_prop)
plot(seq(0.05, 0.95, 0.1), props, xlim=c(0,1), ylim=c(0,1),
     xlab='Average Estimated Probability',
     ylab='Actual Proportion of UK RASFFs', pch=19)
lines(seq(0.05, 0.95, 0.1), seq(0.05, 0.95, 0.1), lwd=2, lty=1, col='grey50')
hist(l10$Predicted)

#saveRDS(bn_tan_fit, 'bn_tan_fit_210819.rds')
