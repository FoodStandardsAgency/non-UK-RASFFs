library(bnlearn)
library(bnviewer)
library(caret)
source('helpers.r')
source('etl.r')
# Load the data.
df_features <- etl()
alpha <- 30  ## Approximately what the previous estimates were.
## Create train and test sets.
train_index <- base::sample(
  x=1:dim(df_features)[1], 
  size=floor(dim(df_features)[1] * 0.7)
  )
df_train <- df_features[train_index,]
df_train_1s <- df_train[df_train$uk_rasff_soon=='1',]
df_train_0s <- df_train[df_train$uk_rasff_soon=='0',]
zero_indices <- base::sample(
  x=1:dim(df_train_0s)[1], size=dim(df_train_1s) * 1
  )
df_train_usample <- base::rbind(df_train_1s, df_train_0s[zero_indices,])
df_test <- df_features[-train_index,]
df_test_1s <- df_test[df_test$uk_rasff_soon=='1',]
df_test_0s <- df_test[df_test$uk_rasff_soon=='0',]
zero_inds <- base::sample(
  x=1:dim(df_test_0s)[1], size=dim(df_test_1s) * 1
  )
df_test_usample= rbind(df_test_1s, df_test_0s[zero_inds,])
train_data <- df_train_usample
test_data <- df_test_usample
# Manual structure based on expert knowledge.
g = bnlearn::empty.graph(nodes=names(df_features))
g = bnlearn::set.arc(x=g, from='month', to='origin_country')
g = bnlearn::set.arc(x=g, from='origin_country', to='product')
g = bnlearn::set.arc(x=g, from='product_category', to='product')
g = bnlearn::set.arc(x=g, from='product', to='hazard_type')
g = bnlearn::set.arc(x=g, from='hazard_type', to='hazard')
g = bnlearn::set.arc(x=g, from='product', to='dist_uk')
g = bnlearn::set.arc(x=g, from='dist_uk', to='notifying_country')
g = bnlearn::set.arc(x=g, from='notifying_country', to='uk_rasff_soon')
#g = bnlearn::set.arc(x=g, from='product', to='uk_rasff_soon')
g = bnlearn::set.arc(x=g, from='hazard', to='uk_rasff_soon')
bnviewer::viewer(bayesianNetwork=g)
alpha_g <- bnlearn::alpha.star(x=g, data=train_data)
g_fitted = bnlearn::bn.fit(
  x=g,
  data=train_data,
  method='bayes',
  iss=alpha_g
  )
g_pred <- stats::predict(
  g_fitted,
  node='uk_rasff_soon',
  data=test_data
  )
caret::confusionMatrix(data=g_pred, reference=test_data$uk_rasff_soon)
g_ps<- estimate_probabilities(
 data_frame=test_data, bayesian_network=g_fitted, predict_column=6
 )
g_comparison <- estimates_vs_reality(
 estimates=g_ps, reality=test_data$uk_rasff_soon
 )
g_comparison_plot <- plot_estimates_vs_reality(data_frame=g_comparison)
g_comparison_plot
# Hierarchical clustering
hc_dag <- bnlearn::hc(x=train_data)
bnviewer::viewer(bayesianNetwork=hc_dag)
alpha_hcluster <- bnlearn::alpha.star(x=hc_dag, data=train_data)
hcluster_fitted <- bnlearn::bn.fit(
  x=hc_dag,
  data=train_data,
  method='bayes',
  iss=alpha_hcluster
  )
hcluster_pred <- stats::predict(
  hcluster_fitted,
  node='uk_rasff_soon',
  data=test_data
  )
caret::confusionMatrix(data=hcluster_pred, reference=test_data$uk_rasff_soon)
hcluster_ps<- estimate_probabilities(
  data_frame=test_data, bayesian_network=hcluster_fitted, predict_column=6
  )
hcluster_comparison <- estimates_vs_reality(
  estimates=hcluster_ps, reality=test_data$uk_rasff_soon
  )
hcluster_comparison_plot <- plot_estimates_vs_reality(
  data_frame=hcluster_comparison
  )
hcluster_comparison_plot
# Naive Bayes
nb_dag <- bnlearn::naive.bayes(x=train_data, training='uk_rasff_soon')
bnviewer::viewer(bayesianNetwork=nb_dag)
nb_fitted <- bnlearn::bn.fit(
  x=nb_dag,
  data=train_data,
  method='bayes',
  iss=alpha
  )
nb_pred <- stats::predict(nb_fitted, data=test_data)
caret::confusionMatrix(data=nb_pred, reference=test_data$uk_rasff_soon)
nb_ps<- estimate_probabilities(
  data_frame=test_data, bayesian_network=nb_fitted, predict_column=6
  )
nb_comparison <- estimates_vs_reality(
  estimates=nb_ps, reality=test_data$uk_rasff_soon
  )
nb_comparison_plot <- plot_estimates_vs_reality(data_frame=nb_comparison)
nb_comparison_plot
# Tree-augmented naive Bayes
tan_dag <- bnlearn::tree.bayes(
  x=train_data, 
  training='uk_rasff_soon'
  )
bnviewer::viewer(bayesianNetwork=tan_dag)
tan_fitted <- bnlearn::bn.fit(
  x=tan_dag, 
  data=train_data,
  method='bayes',
  iss=alpha
  )
tan_pred <- stats::predict(tan_fitted, data=test_data)
caret::confusionMatrix(data=tan_pred, reference=test_data$uk_rasff_soon)
tan_ps<- estimate_probabilities(
  data_frame=test_data, bayesian_network=tan_fitted, predict_column=6
  )
tan_comparison <- estimates_vs_reality(
  estimates=tan_ps, reality=test_data$uk_rasff_soon
  )
tan_comparison_plot <- plot_estimates_vs_reality(data_frame=tan_comparison)
tan_comparison_plot
## Exploration and comparison
#bnlearn::BF(num=g, den=tan, data=train_data, log=FALSE)
#bnlearn::BF(num=hcluster, den=g, data=train_data, log=FALSE)
#tan_bf <- bnlearn::bf.strength(x=tan, data=train_data)
#bnlearn::strength.plot(x=tan, strength=tan_bf, threshold=0.99)
#g_bf <- bnlearn::bf.strength(x=g, data=train_data)
#bnlearn::strength.plot(x=g, strength=g_bf, threshold=0.99)
#bnlearn::BF(num=hc_dag, den=tan_dag, data=train_data, log=FALSE)
