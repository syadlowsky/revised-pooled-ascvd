library(caret)
library(config)
library(ggsci)

library(ggplot2)

# Maybe don't need all of these...
library(survival)
library(pROC)
library(PredictABEL)

source("utils.R")

source("data_loaders.R")

source("nhanes_analysis.R")
source("calculate_stats.R")
source("reclassification_report.R")
source("validation_results.R")

source("plots.R")

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")

load(file=conf$full_models_file)
load("all_results.rda")

data <- load.training.data(paste(conf$working_dir, conf$train_file, sep="/"))
nofram.data <- data[data$study != "FRAM", ]

pce.results <- nofram.data
for (group in 1:4) {
    in.group <- pce.results$grp == group
    pce.results[in.group, "risk"] <- original.model(pce.results[in.group,],
                                                    group)
    pce.results[in.group, "link"] <- original.link(pce.results[in.group,],
                                                   group)
}

baseline.results <- baseline.results[row.names(pce.results),]
logistic.2.eq.results <- logistic.2.eq.results[row.names(pce.results),]
logistic.4.eq.results <- logistic.4.eq.results[row.names(pce.results),]

pce.stats <- validation.statistics(
    pce.results, training.data=pce.results, auc.function=cv_auc)

baseline.stats <- validation.statistics(
    baseline.results, pce.results,
    training.data=baseline.results, auc.function=cv_auc)

logistic.4.stats <- validation.statistics(
    logistic.4.eq.results, pce.results, training.data=logistic.4.eq.results,
    auc.function=cv_auc)

logistic.2.stats <- validation.statistics(
    logistic.2.eq.results, pce.results, logistic.4.eq.results,
    training.data=logistic.2.eq.results,
    auc.function=cv_auc)

print.noquote("Table 1")
print.validation.report(
    concat.validation.report(
        report.from.validation(
            pce.stats,
            name="Original PCEs"),
        report.from.validation(
            baseline.stats,
            name="Cox model 4 equations"),
        report.from.validation(
            logistic.4.stats,
            name="Logistic model 4 equations"),
        report.from.validation(
            logistic.2.stats,
            name="Logistic model 2 equations")))

print.noquote("Table 2")
print.reclassification.report(
    baseline=reclassification.from.cross.validation(
        pce.results, name="Original PCEs"),
    reclassification.from.cross.validation(
        baseline.results, pce.results, name="Cox model 4 equations"),
    reclassification.from.cross.validation(
        logistic.4.eq.results, pce.results, "Logistic model 4 equations"),
    reclassification.from.cross.validation(
        logistic.2.eq.results, pce.results, "Logistic model 2 equations")) 
