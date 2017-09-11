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

source("calculate_stats.R")

validation.statistics <- function(test.results, baseline.results=NULL,
                                  model.set.2.results=NULL,
                                  training.data=NULL,
                                  auc.function=calc.auc) {
    test.data.collection <- test.results

    if (is.null(training.data)) {
        error("Need training data now")
    }

    risks <- test.results$risk
    links <- test.results$link

    if (!is.null(baseline.results)) {
        baseline.risks <- baseline.results$risk
        baseline.links <- baseline.results$link
    }
    if (!is.null(model.set.2.results)) {
        model.set.2.risks <- model.set.2.results$risk
        model.set.2.links <- model.set.2.results$link
    }
    
    stats <- list()
    for (grp in 1:4) {
        grp.stats <- list()
        in.subgroup <- grp == test.data.collection$grp
        subgroup.test.data <- test.data.collection[in.subgroup, ]

        subgroup.training.data <- training.data[training.data$grp == grp, ]

        subgroup.risks <- risks[in.subgroup]
        subgroup.links <- links[in.subgroup]

        grp.stats$gnd <- calculate.gnd(subgroup.risks,
                                       subgroup.test.data, 10, nbins=15)

        grp.stats$auc <- auc.function(subgroup.training.data,
                                      subgroup.test.data,
                                      subgroup.links)

        if (!is.null(baseline.results)) {
            subgroup.baseline.risks <- baseline.risks[in.subgroup]
            subgroup.baseline.links <- baseline.links[in.subgroup]

            outcome <- data.frame(outcome=subgroup.test.data$ascvd & (subgroup.test.data$studytime <= 10))
            keep <- as.logical(subgroup.test.data$ascvd | (subgroup.test.data$studytime >= 10))
            grp.stats$nri <- nri.at.level(outcome[keep,,drop=F],
                                          new.risk=subgroup.risks[keep],
                                          original.risk=subgroup.baseline.risks[keep],
                                          0.075, "logit")
        } else {
            grp.stats$nri <- NULL
        }

        if (!is.null(model.set.2.results)) {
            subgroup.model.set.2.risks <- model.set.2.risks[in.subgroup]
            subgroup.model.set.2.links <- model.set.2.links[in.subgroup]

            outcome <- data.frame(outcome=subgroup.test.data$ascvd & (subgroup.test.data$studytime <= 10))
            keep <- as.logical(subgroup.test.data$ascvd | (subgroup.test.data$studytime >= 10))
            grp.stats$model.set.2.nri <- nri.at.level(outcome[keep,,drop=F],
                                          new.risk=subgroup.risks[keep],
                                          original.risk=subgroup.model.set.2.risks[keep],
                                          0.075, "logit")
        } else {
            grp.stats$model.set.2.nri <- NULL
        }

        grp.stats$calibration <- calculate.calib.slope(outcome=subgroup.test.data$ascvd,
                                                       subgroup.test.data$studytime,
                                                       risk=subgroup.risks,
                                                       link=subgroup.links,
                                                       bins=10, t=10,
                                                       plt=F)

        stats[[grp]] <- grp.stats
    }
    return(stats)
}

report.from.validation <- function(stats, name="Logistic") {
    report <- list()
    for (grp in 1:4) {
        subgroup.stats <- stats[[grp]]

        if (!is.null(subgroup.stats$nri)) {
            event.nri.lower <- subgroup.stats$nri$event.nri -
                qnorm(0.975) * subgroup.stats$nri$event.se
            event.nri.upper <- subgroup.stats$nri$event.nri +
                qnorm(0.975) * subgroup.stats$nri$event.se

            nonevent.nri.lower <- subgroup.stats$nri$nonevent.nri -
                qnorm(0.975) * subgroup.stats$nri$nonevent.se
            nonevent.nri.upper <- subgroup.stats$nri$nonevent.nri +
                qnorm(0.975) * subgroup.stats$nri$nonevent.se

            event.nri <- paste(
                specify.decimal(subgroup.stats$nri$event.nri, nsmall=3),
                " (95% CI ",
                specify.decimal(event.nri.lower, nsmall=3),
                ", ",
                specify.decimal(event.nri.upper, nsmall=3),
                ")",
                sep="")
            nonevent.nri <- paste(
                specify.decimal(subgroup.stats$nri$nonevent.nri, nsmall=3),
                " (95% CI ",
                specify.decimal(nonevent.nri.lower, nsmall=3),
                ", ",
                specify.decimal(nonevent.nri.upper, nsmall=3),
                ")",
                sep="")
        } else {
            event.nri <- "N/A"
            nonevent.nri <- "N/A"
        }

        if (!is.null(subgroup.stats$model.set.2.nri)) {
            event.nri.lower <- subgroup.stats$model.set.2.nri$event.nri -
                qnorm(0.975) * subgroup.stats$model.set.2.nri$event.se
            event.nri.upper <- subgroup.stats$model.set.2.nri$event.nri +
                qnorm(0.975) * subgroup.stats$model.set.2.nri$event.se

            nonevent.nri.lower <- subgroup.stats$model.set.2.nri$nonevent.nri -
                qnorm(0.975) * subgroup.stats$model.set.2.nri$nonevent.se
            nonevent.nri.upper <- subgroup.stats$model.set.2.nri$nonevent.nri +
                qnorm(0.975) * subgroup.stats$model.set.2.nri$nonevent.se

            event.nri.ms.2 <- paste(
                specify.decimal(subgroup.stats$model.set.2.nri$event.nri, nsmall=3),
                " (95% CI ",
                specify.decimal(event.nri.lower, nsmall=3),
                ", ",
                specify.decimal(event.nri.upper, nsmall=3),
                ")",
                sep="")
            nonevent.nri.ms.2 <- paste(
                specify.decimal(subgroup.stats$model.set.2.nri$nonevent.nri, nsmall=3),
                " (95% CI ",
                specify.decimal(nonevent.nri.lower, nsmall=3),
                ", ",
                specify.decimal(nonevent.nri.upper, nsmall=3),
                ")",
                sep="")
        } else {
            event.nri.ms.2 <- "N/A"
            nonevent.nri.ms.2 <- "N/A"
        }

        subgroup.report <- data.frame(
            auc=specify.decimal(subgroup.stats$auc, nsmall=3),
            event.nri=event.nri,
            nonevent.nri=nonevent.nri,
            event.nri=event.nri.ms.2,
            nonevent.nri=nonevent.nri.ms.2,
            gnd.chisq=specify.decimal(subgroup.stats$gnd$chi2gw, nsmall=1),
            gnd.df=subgroup.stats$gnd$df,
            gnd.p.value=specify.decimal(subgroup.stats$gnd$pvalgw, nsmall=3),
            slope=specify.decimal(subgroup.stats$calibration$slope, nsmall=2),
            intercept=specify.decimal(subgroup.stats$calibration$intercept, nsmall=2))

        rownames(subgroup.report) <- c(name)
        report[[grp]] <- subgroup.report
    }
    return(report)
}

concat.validation.report <- function(...) {
    reports <- list(...)
    report <- list()
    for (grp in 1:4) {
        grp.reports <- lapply(reports, function(rep, grp) { return(rep[[grp]]) }, grp)
        report[[grp]] <- do.call("rbind", grp.reports)
    }
    return(report)
}

print.validation.report <- function(report) {
    for (grp in 1:4) {
        print(group_to_description(grp))
        rep <- t(report[[grp]])
        rownames(rep) <- c("C-statistic",
                           "Event NRI at 7.5% risk vs Model Set 1",
                           "Nonevent NRI at 7.5% risk vs Model Set 1",
                           "Event NRI at 7.5% risk vs Model Set 2",
                           "Nonevent NRI at 7.5% risk vs Model Set 2",
                           "GND Chi squared statistic",
                           "GND Chi squared degrees of freedom",
                           "GND P-value",
                           "Calibration slope", "Calibration intercept")
        write.table(rep, col.names=T, row.names=T, sep="\t", quote=F)
    }
}
