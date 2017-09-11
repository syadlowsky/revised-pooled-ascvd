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
source("expected_over_observed_ratio.R")

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")
source("original_model.R")

train.data <- load.training.data(paste(conf$working_dir, conf$train_file, sep="/"))

data <- load.test.data(paste(conf$working_dir,conf$test_file, sep="/"))
nofram.data <- data[data$study != "FRAM", ]

pce.results <- nofram.data
for (group in 1:4) {
    in.group <- pce.results$grp == group
    pce.results[in.group, "risk"] <- original.model(pce.results[in.group,],
                                                    group)
    pce.results[in.group, "link"] <- original.link(pce.results[in.group,],
                                                   group)
}

load(file=conf$full_models_file)

baseline.results <- nofram.data
baseline.results$risk <- predict(baseline, baseline.results, recalibrate=F)
baseline.results$link <- link(baseline, baseline.results)

logistic.2.eq.results <- nofram.data
logistic.2.eq.results$risk <- predict(logistic.2.eq, logistic.2.eq.results, recalibrate=F)
logistic.2.eq.results$link <- link(logistic.2.eq, logistic.2.eq.results)

logistic.4.eq.results <- nofram.data
logistic.4.eq.results$risk <- predict(logistic.4.eq, logistic.4.eq.results)
logistic.4.eq.results$link <- link(logistic.4.eq, logistic.4.eq.results)

baseline.results <- baseline.results[row.names(pce.results),]
logistic.2.eq.results <- logistic.2.eq.results[row.names(pce.results),]
logistic.4.eq.results <- logistic.4.eq.results[row.names(pce.results),]

cat("pce survival per group\n")
print.survival_table(survival.per.group(pce.results))
cat("model 1 survival per group\n")
print.survival_table(survival.per.group(
        baseline.results, pce.results))
cat("model 2 survival per group\n")
print.survival_table(survival.per.group(
        logistic.2.eq.results, pce.results))
exit()

cat("\nTable 2\n")
print(concat.expected_over_observed_table(
    expected_over_observed_table(
        pce.results, model.name="Original PCEs"),
    expected_over_observed_table(
        baseline.results, pce.results, model.name="Model set 1: Cox model 4 equations"),
    expected_over_observed_table(
        logistic.4.eq.results, pce.results, model.name="Model set 2: Logistic model 4 equations"),
    expected_over_observed_table(
        logistic.2.eq.results, pce.results, model.name="Model set 3: Logistic model 2 equations")))

cat("\nTable 5\n")
print.two.by.two.report(
    two.by.two.from.cross.validation(
        pce.results, name="Original PCEs"),
    two.by.two.from.cross.validation(
        baseline.results, name="Cox model 4 equations"),
    two.by.two.from.cross.validation(
        logistic.4.eq.results, name="Logistic model 4 equations"),
    two.by.two.from.cross.validation(
        logistic.2.eq.results, name="Logistic model 2 equations")) 


pce.stats <- validation.statistics(pce.results, training.data=pce.results, auc.function=auc_pce)

baseline.stats <- validation.statistics(
    baseline.results, pce.results, training.data=train.data)

logistic.4.stats <- validation.statistics(
    logistic.4.eq.results, pce.results, training.data=train.data)

logistic.2.stats <- validation.statistics(
    logistic.2.eq.results, pce.results, logistic.4.eq.results,
    training.data=train.data)

cat("\nStats table\n")
print.validation.report(
    concat.validation.report(
        report.from.validation(
            pce.stats, name="Original PCEs"),
        report.from.validation(
            baseline.stats, name="Model set 1: Cox model 4 equations"),
        report.from.validation(
            logistic.4.stats, name="Model set 2: Logistic model 4 equations"),
        report.from.validation(
            logistic.2.stats, name="Model set 3: Logistic model 2 equations")))

cat("\nReclassification report\n")
print.reclassification.report(
    baseline=reclassification.from.cross.validation(
        pce.results, name="Original PCEs"),
    reclassification.from.cross.validation(
        baseline.results, pce.results, name="Cox model 4 equations"),
    reclassification.from.cross.validation(
        logistic.4.eq.results, pce.results, "Logistic model 4 equations"),
    reclassification.from.cross.validation(
        logistic.2.eq.results, pce.results, "Logistic model 2 equations")) 

for (group in 1:4) {
    group.pce.stats <- pce.stats[[group]]
    group.baseline.stats <- baseline.stats[[group]]
    group.logistic.2.stats <- logistic.2.stats[[group]]
    group.logistic.4.stats <- logistic.4.stats[[group]]
    p <- ggplot(group.pce.stats$calibration,
                name="Original PCEs")
    p <- ggplot(group.baseline.stats$calibration,
                name="1 - Cox model 4 equations", guide=F, plot.obj=p)
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
              #legend.position="bottom")
              legend.justification=c(1,0), legend.position=c(0.91,0.112))
    p_inset <- ggplot(group.pce.stats$calibration,
                      "Original PCEs", xlim=0.15, ylim=0.15)
    p_inset <- ggplot(group.baseline.stats$calibration,
                      "1 - Cox model 4 equations", xlim=0.15, ylim=0.15,
                      guide=F, plot.obj=p_inset)
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

    ggsave(plot=p, filename=paste("validation_calibration",group,".svg", sep=""), width=4, height=4)
}


# What happens before / after 2000?
stat.table <- function(pce.results, baseline.results, logistic.2.eq.results, train.data) {
pce.stats <- validation.statistics(pce.results, training.data=pce.results, auc.function=auc_pce)

baseline.stats <- validation.statistics(
    baseline.results, pce.results, training.data=train.data)

logistic.2.stats <- validation.statistics(
    logistic.2.eq.results, pce.results,
    training.data=train.data)

print(table(baseline.results$grp, baseline.results$ascvd & baseline.results$studytime <= 10))

cat("\nStats table\n")
print.validation.report(
    concat.validation.report(
        report.from.validation(
            pce.stats, name="Original PCEs"),
        report.from.validation(
            baseline.stats, name="Model set 1 - new data"),
        report.from.validation(
            logistic.2.stats, name="Model set 2 - new data + method")))
}

cat("Before 2000\n")
stat.table(
    subset(pce.results, study %in% c("FRAMOFF", "ARIC", "CHS", "CARD")),
    subset(baseline.results, study %in% c("FRAMOFF", "ARIC", "CHS", "CARD")),
    subset(logistic.2.eq.results, study %in% c("FRAMOFF", "ARIC", "CHS", "CARD")),
    train.data)

cat("After 2000\n")
stat.table(
    subset(pce.results, study %in% c("MESA", "JHS")),
    subset(baseline.results, study %in% c("MESA", "JHS")),
    subset(logistic.2.eq.results, study %in% c("MESA", "JHS")),
    train.data)
