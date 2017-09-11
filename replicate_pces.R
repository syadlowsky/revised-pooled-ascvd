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

source("plots.R")

source("nhanes_analysis.R")
source("calculate_stats.R")
source("validation_results.R")

source("subgroup_model.R")
source("baseline_model.R")
source("pce_model.R")

set.seed(conf$seed)

# PCE's use all data from CARDIA, CHS, ARIC, FRAM, and FRAMOFF
# and test on JHS and MESA, so we need to divide up the data differently.
training.data <- load.training.data(paste(conf$working_dir, conf$train_file, sep="/"))
training.data$fold <- NULL
test.data <- load.test.data(paste(conf$working_dir, conf$test_file, sep="/"))

data <- rbind(training.data, test.data)

training.data <- subset(data, study == "CHS" |
                              study == "FRAM" |
                              study == "CARD" |
                              study == "FRAM" |
                              study == "FRAMOFF" |
                              study == "ARIC")
test.data <- subset(data, study == "MESA" |
                          study == "JHS")

nhanes.data <- load.nhanes(paste(conf$working_dir, conf$nhanes_csv, sep="/"))

feature.equation.age.interactions <-
    outcome ~ I(log(age)^2) + log(age) * I((1-rxbp) * log(sysbp)) + log(age) * dm +
        log(age) * cursmoke + log(age) * log(totchol) +
        log(age) * log(hdl) + log(age) * I(rxbp * log(sysbp)) + 0

if (conf$fit_model) {
    pce_pre_selected_model <- pce_model(training.data)
    pce_model_selection <- race_and_sex_subgroup_model(
        feature.equation.age.interactions, fit.baseline.model, training.data)

    save(pce_pre_selected_model, pce_model_selection,
         file=conf$replication_model)
} else {
    load(file=conf$replication_model)
}

cat("PCE Pre-selected Model Coefs\n")
print(pce_pre_selected_model)

cat("PCE Methodology applied to pooled cohorts Coefs\n")
print(pce_model_selection)

svg(paste(conf$working_dir, "nhanes_pce_pre_selected_model.svg", sep="/"), width=4, height=4)
nhanes.ratio(nhanes.data,
             pce_pre_selected_model,
             "Using model selected by AHA/ACC",
             F)
dev.off()

svg(paste(conf$working_dir, "nhanes_pce_model_selection.svg", sep="/"), width=4, height=4)
nhanes.ratio(nhanes.data,
             pce_model_selection,
             "Using model selection methodology of AHA/ACC",
             F)
dev.off()

pce_pre_selected_model.results <- test.data
pce_pre_selected_model.results$risk <- predict(pce_pre_selected_model, pce_pre_selected_model.results)
pce_pre_selected_model.results$link <- link(pce_pre_selected_model, pce_pre_selected_model.results)

pce_model_selection.results <- test.data
pce_model_selection.results$risk <- predict(pce_model_selection, pce_model_selection.results)
pce_model_selection.results$link <- link(pce_model_selection, pce_model_selection.results)

pce_pre_selected_model.stats <- validation.statistics(pce_pre_selected_model.results,
                                                      training.data=training.data)

pce_model_selection.stats <- validation.statistics(
    pce_model_selection.results, pce_pre_selected_model.results,
    training.data=training.data)

print.validation.report(
    concat.validation.report(
        report.from.validation(
            pce_pre_selected_model.stats, name="Using model selected by AHA/ACC"),
        report.from.validation(
            pce_model_selection.stats, name="Using model selection methodology of AHA/ACC")))

for (group in 1:4) {
    svg(paste("replication_calibration",group,".svg", sep=""), width=4, height=4)
    group.pce_pre_selected_model.stats <- pce_pre_selected_model.stats[[group]]
    group.pce_model_selection.stats <- pce_model_selection.stats[[group]]

    p <- ggplot(group.pce_pre_selected_model.stats$calibration,
                name="Pre selected model")
    p <- ggplot(group.pce_model_selection.stats$calibration,
                name="AHA/ACC selection methodolgy", guide=F, plot.obj=p) +
        ggtitle(paste("Calibration curves for risk models among ",
                      group_to_description(group))) +
        scale_color_jama(name="Model") +
        scale_shape_discrete(name="Model") +
        theme(plot.title = element_text(hjust = 0.5, size=10),
              axis.title.y = element_text(size=10),
              axis.title.x = element_text(size=10),
              legend.justification=c(1,0), legend.position=c(0.91,0.112))

    p_inset <- ggplot(group.pce_pre_selected_model.stats$calibration,
                      name="Pre selected model", ylim=0.15, xlim=0.15)
    p_inset <- ggplot(group.pce_model_selection.stats$calibration,
                      name="AHA/ACC selection methodolgy", guide=F, plot.obj=p_inset,
                      ylim=0.15, xlim=0.15) +
        ggtitle("Range [0, 0.15] for clarity") +
        scale_color_jama(name="Model") +
        scale_shape_discrete(name="Model") +
        theme(
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            text = element_text(size=7),
            plot.title = element_text(size=7),
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
