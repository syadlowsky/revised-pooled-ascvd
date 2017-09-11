source("compute_nri.R")
source("calculate_stats.R")
source("utils.R")

reclassification.from.cross.validation.on.group <- function(test.results, baseline.results=NULL,
                                                            name="Logistic") {
    test.data.collection <- test.results

    risks <- test.results$risk

    if (is.null(baseline.results)) {
        outcome <- test.data.collection$ascvd & (test.data.collection$studytime <= 10)
        keep <- as.logical(test.data.collection$ascvd | (test.data.collection$studytime >= 10))

        counts <- classification.counts(outcome[keep], risks[keep], 0.075)
    } else {
        baseline.risks <- baseline.results$risk
        outcome <- test.data.collection$ascvd & (test.data.collection$studytime <= 10)
        keep <- as.logical(test.data.collection$ascvd | (test.data.collection$studytime >= 10))

        counts <- compute.nri.counts(
            outcome[keep],
            original.risk=baseline.risks[keep],
            new.risk=risks[keep],
            level=0.075)
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
    return(reclassification.by.group)
}

print.reclassification.report <- function(baseline, ...) {
    baseline.grouped <- baseline
    other.models.grouped <- list(...)
    for (group in 1:4) {
        cat(group_to_description(group))
        cat("\n")
        baseline <- baseline.grouped[[group]]
        other.models <- lapply(other.models.grouped, function(om, group) { om[[group]] }, group)
        formatted <- data.frame(
            desc=unlist(c("Model comparison",
                   " ",
                   "Baseline",
                   "Number of people in each column reclassified by each alternative model:",
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
                     m$n.up.events
                 }))),
             evt.down=unlist(c(
                 " ",
                 "Classified as high-risk (>7.5% 10-yr risk)",
                 baseline$events.recc,
                 "Reclassified as low-risk (>7.5% 10-yr risk)",
                 lapply(other.models, function(m) {m$n.down.events}))),
             n.evt.up=unlist(c(
                 paste("People who did not experience ASCVD events in 10 years (N = ",
                       baseline$nonevents, ")", sep=""),
                 "Classified as low-risk (<7.5% 10-yr risk)",
                 baseline$nonevents.not.recc,
                 "Reclassified as high-risk (>7.5% 10-yr risk)",
                 lapply(other.models, function(m) {m$n.up.nonevents}))),
             n.evt.down=unlist(c(
                 " ",
                 "Classified as high-risk (>7.5% 10-yr risk)",
                 baseline$nonevents.recc,
                 "Reclassified as low-risk (<7.5% 10-yr risk)",
                 lapply(other.models, function(m) {m$n.down.nonevents}))),
            nri=unlist(c("Net reclassification index (NRI) (positive numbers indicate better performance by newer model than original ASCVD)",
                  " ",
                  " ",
                  " ",
                  lapply(other.models, function(m) {
            	  nri.val <- (m$n.up.events - m$n.down.events) / m$n.events +
            	  (m$n.down.nonevents - m$n.up.nonevents) / m$n.nonevents
            	  event.nri.var <- with(m,
            			  (n.up.events + n.down.events) / (n.events^2) -
            			  (n.up.events - n.down.events)^2 / (n.events^3))

            	  nonevent.nri.var <- with(m,
            			  (n.up.nonevents + n.down.nonevents) / (n.nonevents^2) -
            			  (n.up.nonevents - n.down.nonevents)^2 / (n.nonevents^3))
            	  nri.se <- sqrt(event.nri.var + nonevent.nri.var)
            	  paste(specify.decimal(nri.val, nsmall=3),
            			  " (95% CI ",
            			  specify.decimal(nri.val - qnorm(0.975)*nri.se, nsmall=3, zero=F),
            			  ", ",
            			  specify.decimal(nri.val + qnorm(0.975)*nri.se, nsmall=3, zero=F),
            			  ")", sep="")
                  }))))
        write.table(formatted, sep="\t", quote=F, eol="\n", row.names=F, col.names=F)
        cat("\n")
    }
}
