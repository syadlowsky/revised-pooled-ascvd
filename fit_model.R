library(caret)
library(config)
library(glmnet)
library(ggsci)

library(grid)
library(ggplot2)
library(gridExtra)

# Maybe don't need all of these...
library(parallel)
library(doParallel)
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

set.seed(conf$seed)

if (conf$run_parallel) {
    registerDoParallel(cores=((conf$cores-1)%/%2) + 1)
}

cross.validate.model.on.dataset <- function(data, subgroup_system,
                                            model_for_subgroup,
                                            formula_in_subgroup) {
    # TODO: should be smart about saving based on the model name or something
    if (conf$fit_model) {
        run.on.fold <- function(fold, data) {
            print(paste("Fold", fold))
            training.data <- data[data$fold != fold,]
            test.data <-  data[data$fold == fold,]
            model <- subgroup_system(formula_in_subgroup, model_for_subgroup, training.data)
            fold.info <- list()
            fold.info$model <- model
            fold.info$training.data <- training.data
            fold.info$test.data <- test.data
            return(fold.info)
        }
        models.by.fold <- mclapply(unique(data$fold), run.on.fold, data=data,
                                   mc.cores=conf$cores)
        save(models.by.fold, file=paste(conf$working_dir, conf$models_file, sep="/"))
    } else {
        load(file=paste(conf$working_dir, conf$models_file, sep="/"))
    }

    risks <- list()
    links <- list()
    test.data.collection <- data.frame()

    for (fold.info in models.by.fold) {
        if (is.null(fold.info)) {
            next
        }
        training.data <- fold.info$training.data
        test.data <- fold.info$test.data
        model <- fold.info$model
        
        link <- link(model, test.data)
        risk <- predict(model, test.data)

        links <- append(links, link)
        risks <- append(risks, risk)
        test.data.collection <- rbind(test.data.collection, test.data)
    }

    risks <- do.call("rbind", risks)
    links <- do.call("rbind", links)
    test.data.collection$risk <- risks
    test.data.collection$link <- links
    return(test.data.collection)
}

default.feature.equation <-
    outcome ~ age + I(sysbp ^ 2) + sysbp * rxbp + dm +
        age * sysbp + cursmoke + cholratio + 0 

feature.equation.age.interactions <-
    outcome ~ I(log(age)^2) + log(age) * I((1-rxbp) * log(sysbp)) + log(age) * dm +
        log(age) * cursmoke + log(age) * log(totchol) +
        log(age) * log(hdl) + log(age) * I(rxbp * log(sysbp)) + 0

default.feature.equation.with.race <-
    outcome ~ age * race + I(sysbp ^ 2) + sysbp * rxbp * race +
        sysbp * age * race + dm * race + cursmoke * race +
        cholratio * race + 0

data <- load.training.data(paste(conf$working_dir, conf$train_file, sep="/"))
nofram.data <- data[data$study != "FRAM", ]
nhanes.data <- load.nhanes(paste(conf$working_dir, conf$nhanes_csv, sep="/"))

if (conf$fit_model) {
    baseline <- race_and_sex_subgroup_model(feature.equation.age.interactions,
                                            fit.baseline.model, nofram.data)
    logistic.2.eq <- sex_subgroup_model(default.feature.equation.with.race,
                                        fit.logistic.model, nofram.data)
    logistic.4.eq <- race_and_sex_subgroup_model(default.feature.equation,
                                                 fit.logistic.model, nofram.data)
    save(baseline, logistic.2.eq, logistic.4.eq,
         file=conf$full_models_file)
} else {
    load(file=conf$full_models_file)
}

cat("Baseline Coefs\n")
print(baseline)

cat("Logistic 4 Eq Coefs\n")
print(logistic.4.eq)

cat("Proposed Coefs\n")
print(logistic.2.eq)

svg(paste(conf$working_dir, "nhanes_analysis_original.svg", sep="/"), width=4, height=4)
nhanes.ratio.original(nhanes.data,
                      "Original AHA/ACC equations")
dev.off()

svg(paste(conf$working_dir, "nhanes_analysis_baseline.svg", sep="/"), width=4, height=4)
nhanes.ratio(nhanes.data,
             baseline,
             "Cox model 4 equations")
dev.off()

svg(paste(conf$working_dir, "nhanes_analysis_proposed.svg", sep="/"), width=4, height=4)
nhanes.ratio(nhanes.data,
             logistic.2.eq,
             "Logistic model 2 equations")
dev.off()

svg(paste(conf$working_dir, "nhanes_analysis_logistic_4_eq.svg", sep="/"), width=4, height=4)
nhanes.ratio(nhanes.data,
             logistic.4.eq,
             "Logistic model 4 equations")
dev.off()

if (conf$load_cv_results) {
    load("all_results.rda")
} else {
    baseline.results <- cross.validate.model.on.dataset(
        nofram.data, race_and_sex_subgroup_model, fit.baseline.model, feature.equation.age.interactions)
    logistic.4.eq.results <- cross.validate.model.on.dataset(
        nofram.data, race_and_sex_subgroup_model, fit.logistic.model, default.feature.equation)
    logistic.2.eq.results <- cross.validate.model.on.dataset(
        nofram.data, sex_subgroup_model, fit.logistic.model, default.feature.equation.with.race)

    save(baseline.results, logistic.2.eq.results, logistic.4.eq.results,
         file="all_results.rda")
}

logistic.2.eq.results <- logistic.2.eq.results[row.names(baseline.results),]
logistic.4.eq.results <- logistic.4.eq.results[row.names(baseline.results),]

baseline.stats <- validation.statistics(
    baseline.results, training.data=baseline.results, auc.function=cv_auc)

logistic.4.stats <- validation.statistics(
    logistic.4.eq.results, baseline.results, training.data=logistic.4.eq.results,
    auc.function=cv_auc)

logistic.2.stats <- validation.statistics(
    logistic.2.eq.results, baseline.results, logistic.4.eq.results,
    training.data=logistic.2.eq.results,
    auc.function=cv_auc)

print.noquote("Table 1")
print.validation.report(
    concat.validation.report(
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
        baseline.results, name="Cox model 4 equations"),
    reclassification.from.cross.validation(
        logistic.4.eq.results, baseline.results, "Logistic model 4 equations"),
    reclassification.from.cross.validation(
        logistic.2.eq.results, baseline.results, "Logistic model 2 equations")) 

for (group in 1:4) {
    svg(paste("cv_calibration_group", group, ".svg", sep=""), width=4, height=4)
    group.baseline.stats <- baseline.stats[[group]]
    group.logistic.2.stats <- logistic.2.stats[[group]]
    group.logistic.4.stats <- logistic.4.stats[[group]]
    p <- ggplot(group.baseline.stats$calibration, name="1 - Cox model 4 equations")
    p <- ggplot(group.logistic.4.stats$calibration, name="2 - Logistic model 4 equations", guide=F, plot.obj=p)
    p <- ggplot(group.logistic.2.stats$calibration, name="3 - Logistic model 2 equations", guide=F, plot.obj=p) +
        labs(title=paste("Calibration curves for risk models among\n",
                         group_to_description(group),
                         sep="")) +
        scale_color_jama(name="Model") +
        scale_shape_discrete(name="Model") +
        theme(plot.title = element_text(hjust = 0.5, size=conf$title_size),
              axis.title.y = element_text(size=conf$title_size),
              axis.title.x = element_text(size=conf$title_size),
              legend.title = element_text(size=conf$inset_title_size),
              legend.text = element_text(size=conf$inset_title_size * 0.95),
              legend.margin=margin(0.1, 0.1, 0.1, 0.1, unit='cm'),
              legend.key.size = unit(1.0, "lines"),
              legend.justification=c(1,0), legend.position=c(0.955,0.045))
    p_inset <- ggplot(group.baseline.stats$calibration, "1 - Cox model 4 equations",
                      xlim=0.15, ylim=0.15)
    p_inset <- ggplot(group.logistic.4.stats$calibration, "2 - Logistic model 4 equations",
                      xlim=0.15, ylim=0.15, guide=F, plot.obj=p_inset)
    p_inset <- ggplot(group.logistic.2.stats$calibration, "3 - Logistic model 2 equations",
                      xlim=0.15, ylim=0.15, guide=F, plot.obj=p_inset) +
        labs(title="Range [0, 0.15] for clarity") +
        scale_color_jama(name="Model") +
        scale_shape_discrete(name="Model") +
        theme(
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            text = element_text(size=conf$inset_title_size),
            plot.title = element_text(size=conf$inset_title_size,
                                      hjust=0.9),
            legend.position="none"
        )
    p <- p + annotation_custom(grob=ggplotGrob(p_inset),
                               xmin=0.5,
                               xmax=1.0,
                               ymin=0.5,
                               ymax=1.0)
    print(p)
    dev.off()
}
