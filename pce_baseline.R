library(caret)
library(config)
library(ggsci)
library(doParallel)
library(foreach)

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
source("expected_over_observed_ratio.R")

source("plots.R")

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")
source("original_model.R")

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

pce.stats <- validation.statistics(
    pce.results, training.data=pce.results, auc.function=auc_pce)

baseline.stats <- validation.statistics(
    baseline.results, pce.results,
    training.data=baseline.results, auc.function=cv_auc)

logistic.2.stats <- validation.statistics(
    logistic.2.eq.results, pce.results,
    training.data=logistic.2.eq.results,
    auc.function=cv_auc)

name.of.model <- function(model, prefix="Model Set ") {
  if (model == "pce") {
    return("Original PCEs")
  }
  if (model == "baseline") {
    return(paste(prefix, "1 - new data", sep=""))
  }
  if (model == "logistic.2.eq") {
    return(paste(prefix, "2 - new data + method", sep=""))
  }
  return("unknown model")
}

cat("\nTable 1\n")
print(concat.expected_over_observed_table(
    expected_over_observed_table(
        pce.results, model.name=name.of.model("pce")),
    expected_over_observed_table(
        baseline.results, pce.results, model.name=name.of.model("baseline")),
    expected_over_observed_table(
        logistic.2.eq.results, pce.results, model.name=name.of.model("logistic.2.eq"))))

cat("pce survival per group\n")
print.survival_table(survival.per.group(pce.results))
cat("model 1 survival per group\n")
print.survival_table(survival.per.group(
        baseline.results, pce.results))
cat("model 2 survival per group\n")
print.survival_table(survival.per.group(
        logistic.2.eq.results, pce.results))
exit()

cat("\nTable 4\n")
print.two.by.two.report(
    two.by.two.from.cross.validation(
        pce.results, name=name.of.model("pce")),
    two.by.two.from.cross.validation(
        baseline.results, name=name.of.model("baseline")),
    two.by.two.from.cross.validation(
        logistic.2.eq.results, name=name.of.model("logistic.2.eq"))) 

print.noquote("Reclassification table")
print.reclassification.report(
    baseline=reclassification.from.cross.validation(
        pce.results, name=name.of.model("pce")),
    reclassification.from.cross.validation(
        baseline.results, pce.results, name=name.of.model("baseline")),
    reclassification.from.cross.validation(
        logistic.2.eq.results, pce.results, name=name.of.model("logistic.2.eq"))) 

print.noquote("Stats table")
print.validation.report(
    concat.validation.report(
        report.from.validation(
            pce.stats,
            name=name.of.model("pce")),
        report.from.validation(
            baseline.stats,
            name=name.of.model("baseline")),
        report.from.validation(
            logistic.2.stats,
            name=name.of.model("logistic.2.eq"))))

for (group in 1:4) {
    group.pce.stats <- pce.stats[[group]]
    group.baseline.stats <- baseline.stats[[group]]
    group.logistic.2.stats <- logistic.2.stats[[group]]
    p <- ggplot(group.pce.stats$calibration,
                name=name.of.model("pce"))
    p <- ggplot(group.baseline.stats$calibration,
                name=name.of.model("baseline", prefix=""), guide=F, plot.obj=p)
    p <- ggplot(group.logistic.2.stats$calibration,
                name=name.of.model("logistic.2.eq", prefix=""), guide=F, plot.obj=p) +
        labs(title=paste("Calibration curves for risk models among\n",
                         group_to_description(group),
                         sep="")) +
        scale_color_jama(name="Model Set") +
        scale_shape_discrete(name="Model Set") +
        theme(plot.title = element_text(hjust = 0.5, size=conf$title_size),
              axis.title.y = element_text(size=conf$title_size),
              axis.title.x = element_text(size=conf$title_size),
              legend.title = element_text(size=conf$inset_title_size),
              legend.text = element_text(size=conf$inset_title_size * 0.95),
              legend.margin=margin(0.1, 0.1, 0.1, 0.1, unit='cm'),
              legend.key.size = unit(1.0, "lines"),
              legend.justification=c(1,0), legend.position=c(0.955,0.045))
    p_inset <- ggplot(group.pce.stats$calibration,
                      name.of.model("pce"), xlim=0.15, ylim=0.15)
    p_inset <- ggplot(group.baseline.stats$calibration,
                      name.of.model("baseline", prefix=""), xlim=0.15, ylim=0.15,
                      guide=F, plot.obj=p_inset)
    p_inset <- ggplot(group.logistic.2.stats$calibration,
                      name.of.model("logistic.2.eq", prefix=""),
                      xlim=0.15, ylim=0.15, guide=F, plot.obj=p_inset) +
        labs(title="Range [0, 0.15] for clarity") +
        scale_color_jama(name="Model Set") +
        scale_shape_discrete(name="Model Set") +
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

    ggsave(plot=p, filename=paste("cv_calibration",group,".svg", sep=""), width=4, height=4)
}
