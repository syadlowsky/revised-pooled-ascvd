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

source("data_loaders.R")

source("utils.R")
source("plots.R")

source("nhanes_analysis.R")
source("calculate_stats.R")
source("validation_results.R")
source("reclassification_report.R")

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")

train.data <- load.training.data(paste(conf$working_dir, conf$train_file, sep="/"))

data <- load.test.data(paste(conf$working_dir, conf$test_file, sep="/"))
nofram.data <- data[data$study != "FRAM", ]

load(file=conf$full_models_file)

baseline.results <- nofram.data
baseline.results$risk <- predict(baseline, baseline.results)
baseline.results$link <- link(baseline, baseline.results)

logistic.2.eq.results <- nofram.data
logistic.2.eq.results$risk <- predict(logistic.2.eq, logistic.2.eq.results)
logistic.2.eq.results$link <- link(logistic.2.eq, logistic.2.eq.results)

logistic.4.eq.results <- nofram.data
logistic.4.eq.results$risk <- predict(logistic.4.eq, logistic.4.eq.results)
logistic.4.eq.results$link <- link(logistic.4.eq, logistic.4.eq.results)

logistic.2.eq.results <- logistic.2.eq.results[row.names(baseline.results),]
logistic.4.eq.results <- logistic.4.eq.results[row.names(baseline.results),]

baseline.stats <- validation.statistics(baseline.results, training.data=train.data)

logistic.4.stats <- validation.statistics(
    logistic.4.eq.results, baseline.results, training.data=train.data)

logistic.2.stats <- validation.statistics(
    logistic.2.eq.results, baseline.results, logistic.4.eq.results,
    training.data=train.data)

print.validation.report(
    concat.validation.report(
        report.from.validation(
            baseline.stats, name="Model set 1: Cox model 4 equations"),
        report.from.validation(
            logistic.4.stats, name="Model set 2: Logistic model 4 equations"),
        report.from.validation(
            logistic.2.stats, name="Model set 3: Logistic model 2 equations")))

cat("\nReclassification report\n")
print.reclassification.report(
    baseline=reclassification.from.cross.validation(
        baseline.results, name="Cox model 4 equations"),
    reclassification.from.cross.validation(
        logistic.4.eq.results, baseline.results, "Logistic model 4 equations"),
    reclassification.from.cross.validation(
        logistic.2.eq.results, baseline.results, "Logistic model 2 equations")) 

for (group in 1:4) {
    svg(paste("validation_calibration",group,".svg", sep=""), width=4, height=4)
    group.baseline.stats <- baseline.stats[[group]]
    group.logistic.2.stats <- logistic.2.stats[[group]]
    group.logistic.4.stats <- logistic.4.stats[[group]]
    p <- ggplot(group.baseline.stats$calibration,
                name="1 - Cox model 4 equations")
    p <- ggplot(group.logistic.4.stats$calibration,
                name="2 - Logistic model 4 equations", guide=F, plot.obj=p)
    p <- ggplot(group.logistic.2.stats$calibration,
                name="3 - Logistic model 2 equations", guide=F, plot.obj=p) +
        ggtitle(paste("Calibration curves for risk models among ",
                      group_to_description(group))) +
        scale_color_jama(name="Model") +
        scale_shape_discrete(name="Model") +
        theme(plot.title = element_text(hjust = 0.5, size=10),
              axis.title.y = element_text(size=10),
              axis.title.x = element_text(size=10),
              legend.position="bottom")
              #legend.justification=c(1,0), legend.position=c(0.91,0.112))
    p_inset <- ggplot(group.baseline.stats$calibration,
                      "1 - Cox model 4 equations", xlim=0.15, ylim=0.15)
    p_inset <- ggplot(group.logistic.4.stats$calibration,
                      "2 - Logistic model 4 equations",
                      xlim=0.15, ylim=0.15, guide=F, plot.obj=p_inset)
    p_inset <- ggplot(group.logistic.2.stats$calibration,
                      "3 - Logistic model 2 equations",
                      xlim=0.15, ylim=0.15, guide=F, plot.obj=p_inset) +
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
