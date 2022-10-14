# Class Five in Class Work


library(rsample)
library(recipes)
library(ggplot2)
library(dplyr)
library(themis)
library(parsnip)
library(workflows)
library(yardstick)
library(dials)
library(tune)
library(coefplot)

credit <- tibble::as_tibble(credit_data)

data(credit_data, package = 'modeldata')
set.seed(2715)
credit_split <- initial_split(
    credit, prop = .85, strata = 'Status'
)
credit_split
train <- training(credit_split)
test <- testing(credit_split)

set.seed(2816)
cv_splits <- vfold_cv(train, v=5, strata='Status')

#Feature Engineering/Preprocessing ####
ggplot(credit, aes(x=Status)) + geom_bar()

credit |> count(Status)
#When we have uneven numbers of our outcome variable we can either weight the 
# lower variable more or we can upsample or downsample the data to get an even number of each. 


#Penalized regression: all variables need to be on the same scale. 
rec1 <- recipe(Status ~ ., data=train) |> 
    themis::step_downsample(Status) |> 
    step_impute_knn(all_numeric_predictors()) |> 
    #step_impute_mode(all_nominal_predictors()) |> 
    step_unknown(all_nominal_predictors(), new_level = 'missing') |> 
    step_nzv(all_predictors()) |> 
    step_normalize(all_numeric_predictors()) |> 
    step_intercept() |> 
    #The step other function collapses infrequent categories to misc
    step_other(all_nominal_predictors(), other='misc') |> 
    #Step novel tells the model what to do if it comes across a category it hasn't seen before
    step_novel(all_nominal_predictors()) |> 
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
    #We don't need to worry about dummy variable trap here

rec1 |> prep() |> bake(new_data=NULL)

#Define our model ####
linear_reg()
boost_tree()
logistic_reg() |> set_engine('glmnet')

logistic_reg(penalty = 7) |> set_engine('glmnet')

#Setting penalty tune allows the model to find the best penalty value
spec_glm <- logistic_reg(penalty = tune()) |> set_engine('glmnet')
spec_glm

#Combining the recipe and the model spec
flow_glm_one <- workflow(rec1, spec_glm)
flow_glm_one

fit1 <- fit(flow_glm_one, data=train)
metrics1 <- metric_set(roc_auc, mn_log_loss)
metrics1

# Set parameter values
flow_glm_one |> workflows::extract_parameter_dials('penalty')

#Tune our model ####
grid_glm_one <- dials::grid_random(flow_glm_one, size = 100)
grid_glm_one

tune_glm_1 <- tune_grid(
    flow_glm_one,
    resamples = cv_splits,
    grid=grid_glm_one,
    metrics=metrics1
)
tune_glm_1    
tune_glm_1$.metrics[[1]]

tune_glm_1 |> collect_metrics()
tune_glm_1 |> autoplot(metric='roc_auc')

tune_glm_1 |> select_best(metric='roc_auc')
tune_glm_1 |> select_by_one_std_err(metric='roc_auc', -penalty)
#Above gives the simplist model that is within one std error of the best model.

mod_glm_1 <- flow_glm_one |> 
    finalize_workflow(parameters = tune_glm_1 |> select_by_one_std_err(metric='roc_auc', -penalty)
    )
mod_glm_1

fit_glm_1 <- fit(mod_glm_1, data=train)
fit_glm_1 |> extract_fit_engine() |> coefplot(lambda=.0082, sort='magnitude')mod_glm_1

fit_glm_1 |> 
    extract_fit_engine() |> 
    coefpath()


predict(fit_glm_1, new_data = test, type='prob')

last_fit(mod_glm_1, split=credit_split) |> collect_metrics()

# Xgboost ####
# Two ways to deal with overfitting in tree models:
# random forests and booted trees


#xgboost doesn't care about missing data
rec2 <- recipe(Status ~ ., data=train) |> 
    themis::step_downsample(Status) |> 
    step_nzv(all_numeric_predictors()) |> 
    step_other(all_nominal_predictors(), other='misc') |> 
    step_novel(all_nominal_predictors()) |> 
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

spec_xg_1 <- boost_tree(mode='classification',
                        trees=tune(), 
                        tree_depth = tune()) |> 
    set_engine('xgboost')
spec_xg_1

flow_xg_1 <- workflow(preprocessor = rec2, spec_xg_1)
flow_xg_1


grid_xg_1 <- grid_random(flow_xg_1, size=30)
grid_xg_1

tune_xg_1 <- tune_grid(
    flow_xg_1,
    resamples = cv_splits,
    grid=grid_xg_1,
    metrics=metrics1
)

tune_xg_1 |> collect_metrics()
tune_xg_1 |> show_best(n=5)
