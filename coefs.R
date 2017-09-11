library(config)
library(glmnet)

source("utils.R")

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")

set.seed(conf$seed)

load(file=conf$full_models_file)

cat("Baseline Coefs\n")
print(baseline)

cat("Proposed Coefs\n")
print(logistic.2.eq)
