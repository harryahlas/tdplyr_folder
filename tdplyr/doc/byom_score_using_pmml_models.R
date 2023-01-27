## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 6L, tibble.print_max = 6L, digits = 3)
library(knitr)

## ----load_lib_create_connection, echo=FALSE, include=FALSE--------------------
library(dplyr)
library(DBI)
library(tdplyr)

# Reading Vantage cluster details from td_config.cfg
config_file <- system.file("extdata", "td_config.cfg", package = "tdplyr")
config_df <- read.csv(config_file, sep = ":", stringsAsFactors = FALSE, header = TRUE)
config_list <- unlist(lapply(config_df$value, function(x) {trimws(x)}))

con <- td_create_context(host = config_list[1], uid = config_list[2], pwd = config_list[3], dType = "native")

## ----create_tbls, message=FALSE, warning=FALSE, results="hide"----------------
loadExampleData("pmmlpredict_example", "boston")
loadExampleData("pmmlpredict_example", "iris_train", "iris_test")

# Create tbl_teradata objects.
boston <- tbl(con, "boston")
boston_train <- boston %>% filter(id < 200)
iris_train <- tbl(con, "iris_train")
iris_test <- tbl(con, "iris_test")

## ----prepare_data-------------------------------------------------------------
library(pmml)
library(MASS)
library(randomForest)
dataBox <- xform_wrap(boston)
# Normalize all numeric variables of the loaded boston dataset to lie
# between 0 and 1.
dataBox <- xform_min_max(dataBox)
# Include only derived fields for modeling.
# Last element derived_medv index.
last <- length(names(dataBox$data))
# Create dataset without original predictors 1-13 and last derived_medv
# to train the model.
dataset <- dataBox$data[-c(1:13, eval(last))]
names(dataset)

## ----rf_pmml_generation-------------------------------------------------------
fit <- randomForest(medv ~ ., data=na.omit(dataset), ntree=50)
tpmml <- pmml(fit, transforms = dataBox)
# pmml file will be saved in the tdplyr installation directory.
save_pmml(tpmml,"boston_trans_normcont_model.pmml")

## ----rf_load_pmml-------------------------------------------------------------
# Create following table on vantage. 
# crt_tbl <- "CREATE SET TABLE pmml_models(model_id VARCHAR(40), model BLOB) 
#             PRIMARY INDEX (model_id);"
# DBI::dbExecute(con, sql(crt_tbl))

# Create a file load_pmml_model.txt that has a model_id and a model file name 
# entry such as:
# boston_rf_model|boston_trans_normcont_model.pmml
# This file and the pmml models to be loaded should be in the same directory. 
#  
# Loading model with BTEQ. 
# .import vartext file load_pmml_model.txt
# .repeat *
# USING (c1 VARCHAR(40), c2 BLOB AS DEFERRED BY NAME) INSERT INTO pmml_models(:c1, :c2);

# Loading model with TDStudio. The '?' will open a GUI to browse the PMML file 
# that needs to be updated.

# insert into pmml_models values ('boston_rf_model', ?);

## ----rf_scoring---------------------------------------------------------------
# Create test data to score. 
boston_test <- td_sample(df=boston, n=0.3)           

modeldata <- tbl(con, "pmml_models") %>% filter(model_id=='boston_rf_model')

result <- td_pmml_predict(modeldata = modeldata, 
                          newdata = boston_test, 
                          accumulate = c("id"))
result

## ----glm_pmml_generation------------------------------------------------------
library(pmml)
library(stats)

iris_glm <- glm(sepal_length ~ ., data=iris_train)
iris_glm_pmml <- pmml(iris_glm)
# pmml file will be saved in the tdplyr installation directory.
save_pmml(iris_glm_pmml,"iris_glm.pmml")

## ----glm_load_pmml------------------------------------------------------------
# Create following table on vantage. 
#crt_tbl <- "CREATE SET TABLE pmml_models(model_id VARCHAR(40), model BLOB) 
#            PRIMARY INDEX (model_id);"
#DBI::dbExecute(con, sql(crt_tbl))

# Create a file load_pmml_model.txt that has a model_id and a model file name 
# entry such as:
# iris_glm|iris_glm.pmml
# This file and the pmml models to be loaded should be in the same directory.  

# Loading model with BTEQ. 
# .import vartext file load_pmml_model.txt
# .repeat *
# USING (c1 VARCHAR(40), c2 BLOB AS DEFERRED BY NAME) INSERT INTO pmml_models(:c1, :c2);

# Loading model with TDStudio. The '?' will open a GUI to browse the PMML file 
# that needs to be updated.

# insert into pmml_models values ('iris_glm', ?);

## ----glm_scoring--------------------------------------------------------------
modeldata <- tbl(con, "pmml_models") %>% filter(model_id=='iris_glm')
result <- td_pmml_predict(modeldata = modeldata, 
                          newdata = iris_test, 
                          accumulate = c("id"))
result

## ----NaiveBayes_pmml_generation-----------------------------------------------
library(pmml)
library(e1071)
# naiveBayes function works with "data.frame".
# Convert tbl_teradata to "data.frame".
iris_train_df <- as.data.frame(iris_train)
nb_iris <- naiveBayes(species ~ ., data=iris_train_df)
nb_iris

# Convert the generated model to "PMML" format.
nb_iris_pmml <- pmml(nb_iris, 
                     dataset=iris_train_df, 
                     predicted_field = "species")
# pmml file will be saved in the tdplyr installation directory.
save_pmml(nb_iris_pmml, "nb_iris_tdplyr.pmml")

## ----NaiveBayes_load_pmml-----------------------------------------------------
# Create following table on vantage. 
# crt_tbl <- "CREATE SET TABLE pmml_models(model_id VARCHAR(40), model BLOB) 
#             PRIMARY INDEX (model_id);"
# DBI::dbExecute(con, sql(crt_tbl))
#
# Create a file load_pmml_model.txt that has a model_id and a model file name 
# entry such as:
# nb_iris_tdplyr|nb_iris_tdplyr.pmml
#
# This file and the pmml models to be loaded should be in the same directory.  
# Loading model with BTEQ. 
# .import vartext file load_pmml_model.txt
# .repeat *
# USING (c1 VARCHAR(40), c2 BLOB AS DEFERRED BY NAME) INSERT INTO pmml_models(:c1, :c2);
#
# Loading model with TDStudio. The '?' will open a GUI to browse the PMML file 
# that needs to be updated.
# insert into pmml_models values ('nb_iris_tdplyr', ?);

## ----NaiveBayes_scoring-------------------------------------------------------
modeldata <- tbl(con, "pmml_models") %>% filter(model_id=='nb_iris_tdplyr')
nb_result <- td_pmml_predict(modeldata = modeldata, 
                             newdata = iris_test, 
                             accumulate = c("id"))
nb_result

## ----xgboost_pmml_generation--------------------------------------------------
library(pmml)
library(xgboost)
# xgboost() function works with "matrix" only.
# Convert tbl_teradata to "data.frame".
iris_train_df <- as.data.frame(iris_train)

# Multinomial model using iris data.
xgb_model <- xgboost(data = as.matrix(iris_train_df[, 2:5]),
                     label = as.numeric(iris_train_df[, 6])-1,
                     max_depth = 2, 
                     eta = 1, 
                     nthread = 2, 
                     nrounds = 2,
                     objective = "multi:softprob", 
                     num_class = 3
                     )
xgb_model 

# Convert the generated model to "PMML" format.
# Save the tree information in an external file.
xgb.dump(xgb_model, "xgb_model.dumped.trees")

# Convert to PMML.
train_data_colnames <- colnames(as.matrix(iris_train_df[, 2:5]))
xgb_model_pmml <- pmml(xgb_model,
                       input_feature_names = train_data_colnames,
                       output_label_name = "species",
                       output_categories = c(1, 2, 3), 
                       xgb_dump_file = "xgb_model.dumped.trees"
                       )

# pmml file will be saved in the tdplyr installation directory.
save_pmml(xgb_model_pmml, "xgb_model_tdplyr.pmml")


## ----xgboost_load_pmml--------------------------------------------------------
# Create following table on vantage. 
# crt_tbl <- "CREATE SET TABLE pmml_models(model_id VARCHAR(40), model BLOB) 
#             PRIMARY INDEX (model_id);"
# DBI::dbExecute(con, sql(crt_tbl))
#
# Create a file load_pmml_model.txt that has a model_id and a model file name 
# entry such as:
# xgb_model_tdplyr|xgb_model_tdplyr.pmml
#
# This file and the pmml models to be loaded should be in the same directory.  
# Loading model with BTEQ. 
# .import vartext file load_pmml_model.txt
# .repeat *
# USING (c1 VARCHAR(40), c2 BLOB AS DEFERRED BY NAME) INSERT INTO pmml_models(:c1, :c2);
#
# Loading model with TDStudio. The '?' will open a GUI to browse the PMML file 
# that needs to be updated.
# insert into pmml_models values ('xgb_model_tdplyr', ?);

## ----xgboost_scoring----------------------------------------------------------
modeldata <- tbl(con, "pmml_models") %>% filter(model_id=='xgb_model_tdplyr')
xgb_result <- td_pmml_predict(modeldata = modeldata, 
                              newdata = iris_test, 
                              accumulate = c("id"))
xgb_result

## ----decision_tree_pmml_generation--------------------------------------------
library(pmml)
library(rpart)

# Use 'rpart()' function to generate decision tree model.
dt_iris <- rpart(species ~ ., data=iris_train)
dt_iris

# Convert the generated model to "PMML" format.
dt_iris_tdplyr <- pmml(dt_iris, 
                       predicted_field = "species")

# pmml file will be saved in the tdplyr installation directory.
save_pmml(dt_iris_tdplyr, "dt_iris_tdplyr.pmml")

## ----decision_tree_load_pmml--------------------------------------------------
# Create following tables on vantage. 
# crt_tbl <- "CREATE SET TABLE pmml_models(model_id VARCHAR(40), model BLOB) 
#             PRIMARY INDEX (model_id);"
# DBI::dbExecute(con, sql(crt_tbl))
#
# Create a file load_pmml_model.txt that has a model_id and a model file name 
# entry such as:
# dt_iris_tdplyr|dt_iris_tdplyr.pmml
#
# This file and the pmml models to be loaded should be in the same directory.  
# Loading model with BTEQ. 
# .import vartext file load_pmml_model.txt
# .repeat *
# USING (c1 VARCHAR(40), c2 BLOB AS DEFERRED BY NAME) INSERT INTO pmml_models(:c1, :c2);
#
# Loading model with TDStudio. The '?' will open a GUI to browse the PMML file 
# that needs to be updated.
# insert into pmml_models values ('dt_iris_tdplyr', ?);

## ----decision_tree_scoring----------------------------------------------------
modeldata <- tbl(con, "pmml_models") %>% filter(model_id=='dt_iris_tdplyr')
dt_result <- td_pmml_predict(modeldata = modeldata, 
                             newdata = iris_test, 
                             accumulate = c("id"))
dt_result

