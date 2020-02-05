library(bnlearn)
library(bnviewer)
library(caret)
source('helpers.r')
source('etl.r')

df_features <- etl()
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
## Manual structure based on expert knowledge.
# g = bnlearn::empty.graph(nodes=names(df_features))
# g = bnlearn::set.arc(x=g, from='month', to='origin_country')
# g = bnlearn::set.arc(x=g, from='origin_country', to='product')
# g = bnlearn::set.arc(x=g, from='product_category', to='product')
# g = bnlearn::set.arc(x=g, from='product', to='hazard_type')
# g = bnlearn::set.arc(x=g, from='hazard_type', to='hazard')
# g = bnlearn::set.arc(x=g, from='product', to='dist_uk')
# g = bnlearn::set.arc(x=g, from='dist_uk', to='notifying_country')
# g = bnlearn::set.arc(x=g, from='notifying_country', to='uk_rasff_soon')
# #g = bnlearn::set.arc(x=g, from='product', to='uk_rasff_soon')
# g = bnlearn::set.arc(x=g, from='hazard', to='uk_rasff_soon')
# bnviewer::viewer(bayesianNetwork=g)
# # Exploring differnet iss values.
# alpha <- bnlearn::alpha.star(x=g, data=df_train)
# g_fitted = bnlearn::bn.fit(
#   x=g, 
#   data=df_train,
#   method='bayes',
#   iss=alpha
#   )
# g_pred <- stats::predict(
#   g_fitted,
#   node='uk_rasff_soon',
#   data=df_test
#   )
# caret::confusionMatrix(data=g_pred, reference=df_test$uk_rasff_soon)
#g_ps<- estimate_probabilities(
#  data_frame=df_test, bayesian_network=g_fitted, predict_column=6
#  )
#g_comparison <- estimates_vs_reality(
#  estimates=g_ps, reality=df_test$uk_rasff_soon
#  )
#g_comparison_plot <- plot_estimates_vs_reality(data_frame=g_comparison)
#g_comparison_plot
# Tree-augmented naive Bayes
train_data <- df_train_usample
test_data <- df_test_usample
bn_tan <- bnlearn::tree.bayes(
  x=train_data, 
  training='uk_rasff_soon'
  )
bnviewer::viewer(bayesianNetwork=bn_tan)
alpha <- 25
tan_fitted <- bnlearn::bn.fit(
  x=bn_tan, 
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
#tan_bf <- bnlearn::bf.strength(x=bn_tan, data=train_data)
#bnlearn::strength.plot(x=bn_tan, strength=tan_bf, threshold=0.99)
