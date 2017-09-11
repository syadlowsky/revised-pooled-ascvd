source("compute_nri.R")
source("calculate_stats.R")
source("utils.R")

threshl = 0.1

two.by.two.from.cross.validation.on.group <- function(test.results, name="Logistic") {
    test.results <- subset(test.results, test.results$studytime <= 10 & !test.results$ascvd)
    counts <- classification.counts(test.results$ascvd & (test.results$studytime <= 10),
                                       #test.results$studytime,
                                       test.results$risk, threshl)
    counts$name <- name
    return(counts)
}

two.by.two.from.cross.validation <- function(test.results, name="Logistic") {
    two.by.two.by.group <- list()
    for (group in 1:4) {
        two.by.two.by.group[[group]] <-
             two.by.two.from.cross.validation.on.group(
                 subset(test.results, grp==group),
                 name)
    }
    two.by.two.by.group[[5]] <-
         two.by.two.from.cross.validation.on.group(
             test.results,
             name)
    return(two.by.two.by.group)
}

print.two.by.two.report <- function(...) {
    other.models.grouped <- list(...)
    for (group in 1:5) {
        cat(group_to_description(group))
        cat("\n")
        other.models <- lapply(other.models.grouped, function(om, group) { om[[group]] }, group)
        formatted <- data.frame(
            desc=unlist(c("Model",
                   " ",
                   lapply(other.models, function(t) {t$name}))),
             evt.down=unlist(c(
                 paste("People who experienced ASCVD events in 10 years (N = ",
                       specify.decimal(other.models[[1]]$events, 1), ")", sep=""),
                 "Classified as low-risk (<7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                   specify.decimal(m$events.not.recc, 1)
                 }))),
             evt.up=unlist(c(
                 " ",
                 "Classified as high-risk (>7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                   specify.decimal(m$events.recc, 1)
                 }))),
             n.evt.down=unlist(c(
                 paste("People who did not experience ASCVD events in 10 years (N = ",
                       specify.decimal(other.models[[1]]$nonevents, 1), ")", sep=""),
                 "Classified as low-risk (<7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                   specify.decimal(m$nonevents.not.recc, 1)
                 }))),
             n.evt.up=unlist(c(
                 " ",
                 "Classified as high-risk (>7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                   specify.decimal(m$nonevents.recc, 1)
                 }))))
        write.table(formatted, sep="\t", quote=F, eol="\n", row.names=F, col.names=F)
        cat("\n")
    }
}

reclassification.from.cross.validation.on.group <- function(test.results, baseline.results=NULL,
                                                            name="Logistic") {
    test.data.collection <- test.results

    risks <- test.results$risk

    if (is.null(baseline.results)) {
        outcome <- test.data.collection$ascvd & (test.data.collection$studytime <= 10)
        keep <- as.logical(test.data.collection$ascvd | (test.data.collection$studytime >= 10))

        counts <- classification.counts(outcome[keep], risks[keep], threshl)
    } else {
        baseline.risks <- baseline.results$risk
        outcome <- test.data.collection$ascvd & (test.data.collection$studytime <= 10)
        keep <- as.logical(test.data.collection$ascvd | (test.data.collection$studytime >= 10))

        counts <- compute.nri.counts(
            outcome[keep],
            original.risk=baseline.risks[keep],
            new.risk=risks[keep],
            level=threshl)
    }
    counts$name <- name
    return(counts)
}

reclassification.from.cross.validation <- function(test.results, baseline.results=NULL, name="Logistic") {
    reclassification.by.group <- list()
    for (group in 1:4) {
        if (is.null(baseline.results)) {
            reclassification.by.group[[group]] <-
                 reclassification.from.cross.validation.on.group(
                     subset(test.results, grp==group),
                     NULL,
                     name)
        } else {
            reclassification.by.group[[group]] <-
                 reclassification.from.cross.validation.on.group(
                     subset(test.results, grp==group),
                     subset(baseline.results, grp==group),
                     name)
        }
    }
    if (is.null(baseline.results)) {
        reclassification.by.group[[5]] <-
             reclassification.from.cross.validation.on.group(
                 test.results,
                 NULL,
                 name)
    } else {
        reclassification.by.group[[5]] <-
             reclassification.from.cross.validation.on.group(
                 test.results,
                 baseline.results,
                 name)
    }
    return(reclassification.by.group)
}

print.reclassification.report <- function(baseline, ...) {
    baseline.grouped <- baseline
    other.models.grouped <- list(...)
    for (group in 1:5) {
        cat(group_to_description(group))
        cat("\n")
        baseline <- baseline.grouped[[group]]
        other.models <- lapply(other.models.grouped, function(om, group) { om[[group]] }, group)
        formatted <- data.frame(
            desc=unlist(c("Model comparison",
                   " ",
                   "Baseline",
                   " ",
                   lapply(other.models, function(t) {t$name}))),
             evt.up=unlist(c(
                 paste("People who experienced ASCVD events in 10 years (N = ",
                       baseline$events, ")", sep=""),
                 "Classified as low-risk (<7.5% 10-yr risk)",
                 baseline$events.not.recc,
                 "Reclassified as high-risk (>7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                     if (m$n.up.events > baseline$events.not.recc) {
                         print(paste("events up", m$n.up.events, "not recc", baseline$events.not.recc))
                     }
                     paste(m$n.up.events, " (", specify.decimal(100 * m$n.up.events / baseline$events.not.recc, 1),
                           "% correctly reclassified)", sep="")
                 }))),
             evt.down=unlist(c(
                 " ",
                 "Classified as high-risk (>7.5% 10-yr risk)",
                 baseline$events.recc,
                 "Reclassified as low-risk (<7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                     paste(m$n.down.events, " (", specify.decimal(100 * m$n.down.events / baseline$events.recc, 1),
                           "% wrongly reclassified)", sep="")
                 }))),
             n.evt.up=unlist(c(
                 paste("People who did not experience ASCVD events in 10 years (N = ",
                       baseline$nonevents, ")", sep=""),
                 "Classified as low-risk (<7.5% 10-yr risk)",
                 baseline$nonevents.not.recc,
                 "Reclassified as high-risk (>7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                     paste(m$n.up.nonevents, " (", specify.decimal(100 * m$n.up.nonevents / baseline$nonevents.not.recc, 1),
                           "% wrongly reclassified)", sep="")
                 }))),
             n.evt.down=unlist(c(
                 " ",
                 "Classified as high-risk (>7.5% 10-yr risk)",
                 baseline$nonevents.recc,
                 "Reclassified as low-risk (<7.5% 10-yr risk)",
                 lapply(other.models, function(m) {
                     paste(m$n.down.nonevents, " (", specify.decimal(100 * m$n.down.nonevents / baseline$nonevents.recc, 1),
                           "% correctly reclassified)", sep="")
                 }))))
        write.table(formatted, sep="\t", quote=F, eol="\n", row.names=F, col.names=F)
        cat("\n")
    }
}
