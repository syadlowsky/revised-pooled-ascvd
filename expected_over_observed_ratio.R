library(survival)
library(plyr)

km.survival <- function(data,t) {
    sf <- survfit(Surv(studytime, ascvd) ~ 1, data=data)
    survival <- 1 - min(sf$surv[sf$time<=t])
    return(survival)
}

survival.per.group <- function(data, baseline=NULL, t=10) {
    if (is.null(baseline)) {
        risk <- data$risk
    } else {
        risk <- baseline$risk
    }
    data$risk_bin <- cut(risk, breaks=c(0, 0.05, 0.075, 0.1, 1),
                         labels=c("< 5%", "5% to < 7.5%", "7.5% to < 10%", "\u2265 10%"))
    survival.table <- ddply(
        data, ~risk_bin,
        function(group.data) {
            cbind(observed=km.survival(group.data, t),
                  expected=mean(group.data$risk),
                  n_participants=nrow(group.data))
        }
    )
    return(survival.table)
}

print.survival_table <- function(table) {
    cat("\t")
    write.table(table, sep="\t", quote=F, eol="\n", row.names=T, col.names=T)
    cat("\n")
}

expected_over_observed_table <- function(data, baseline=NULL, t=10, model.name=NULL) {
    survival.table <- survival.per.group(data, baseline, t)
    survival.table$expected_over_observed_ratio <- survival.table$expected / survival.table$observed
    eoo.table <- t(data.frame(apply(survival.table, 1, function(row) {
        paste(specify.decimal(as.numeric(row['expected_over_observed_ratio']), 2),
              " (n = ",
              as.integer(row['n_participants']),
              ")",
              sep="")
    })))
    colnames(eoo.table) <- survival.table$risk_bin
    if (!is.null(model.name)) {
        rownames(eoo.table) <- c(model.name)
    } else {
        rownames(eoo.table) <- c()
    }
    class(eoo.table) <- "expected_over_observed_table"
    return(eoo.table)
}

concat.expected_over_observed_table <- function(...) {
    eoo.table <- rbind(...)
    class(eoo.table) <- "expected_over_observed_table"
    return(eoo.table)
}

print.expected_over_observed_table <- function(table) {
    cat("\t")
    write.table(table, sep="\t", quote=F, eol="\n", row.names=T, col.names=T)
    cat("\n")
}
