library(config)
library(survival)
library(MASS) # for stepAIC
library(Hmisc) # for improveProb

conf <- config::get()

link <- function(x, ...) UseMethod("link")

breslow.estimator <- function(time, event, features, cox.model) {
    tab <- data.frame(table(time[event==1]))
    y <- as.numeric(levels(tab[, 1]))[tab[, 1]] #ordered distinct event times
    d <- tab[, 2]                               #number of events
    links <- predict(cox.model, features, type="lp")
    links <- links - mean(links)

    h0 <- rep(NA, length(y))
    for(l in 1:length(y))
    {
        h0[l] <- d[l] / sum(exp(links[time >= y[l]]))
    }
    H0 <- cumsum(h0)
    S0 <- exp(-H0)
    return(data.frame(time=y, hazard=H0, surv=S0))
}

fit.baseline.model <- function(data, feature.equation, time=10) {
    study.time = data$studytime
    outcome <- Surv(time=pmin(study.time, time+2), event=data$ascvd)
    data$outcome <- outcome
    e <- new.env()
    parent.env(e) = environment(feature.equation)
    e$outcome = outcome
    e$data = data
    attr(feature.equation, ".Environment") <- e
    # Using Breslow to be like SAS, which was used for original model
    cvfit <- coxph(feature.equation, data=data, ties="breslow")
    hazard <- survfit(cvfit, type="breslow")
    ten.year.avg.risk = min(hazard$surv[hazard$time<=time])
    # Perform variable selection

    pvals <- coef(summary(cvfit))[,"Pr(>|z|)"]
    term.order <- attr(terms(cvfit), "order")
    term.labels <- attr(terms(cvfit), "term.labels")
    for (tt in term.labels[order(term.order, decreasing=T)]) {
        if (!(grepl("age", tt) & (grepl(":", tt) | grepl("2", tt)))) {
            # We only throw things out if they're an age interaction
            next
        }
        if (pvals[tt] > 0.05) {
            alternative <- update(cvfit, as.formula(paste("~ . -", tt)))
            tfac <- attr(terms(alternative), "factors")
            if (all(tfac <= 1)) {
                # there's no higher order interaction terms
                cvfit <- alternative
                hazard <- survfit(cvfit, type="breslow")
                ten.year.avg.risk = min(hazard$surv[hazard$time<=time])
            }
        } else if (pvals[tt] > 0.01) {
            alternative <- update(cvfit, as.formula(paste("~ . -", tt)))
            tfac <- attr(terms(alternative), "factors")
            if (any(tfac > 1)) {
                # there's higher order interaction terms
                next
            }
            alternative.hazard <- survfit(alternative, type="breslow")
            alt.ten.year.avg.risk = min(hazard$surv[hazard$time<=time])

            risk.without.var <- 1 - alt.ten.year.avg.risk ^ predict(alternative, type="risk")
            risk.with.var <- 1 - ten.year.avg.risk ^ predict(cvfit, type="risk")

            nri <- nri.at.level(data.frame(outcome=data$ascvd & (data$studytime <= 10)),
                                risk.without.var,
                                risk.with.var,
                                0.075)
            idi <- improveProb(
                risk.without.var, risk.with.var, data$ascvd & (data$studytime <= 10))

            if (nri$nri < 0.15 & idi$idi < qnorm(0.975) * idi$se.idi) {
                # doesn't help much so drop
                cvfit <- alternative
                hazard <- alternative.hazard
                ten.year.avg.risk = alt.ten.year.avg.risk
            }
        }
    }

    hazard <- survfit(cvfit, type="breslow")
    ten.year.avg.risk = min(hazard$surv[hazard$time<=time])

    aric.mesa <- (data$study == "ARIC") | (data$study == "MESA")
    aric.mesa.hazard <- breslow.estimator(study.time[aric.mesa], data$ascvd[aric.mesa],
                                data[aric.mesa, ], cvfit)
    ten.year.aric.mesa.risk = min(aric.mesa.hazard$surv[aric.mesa.hazard$time<=time])

    baseline.obj <- list(model=cvfit, ten.year.avg.risk=ten.year.avg.risk,
                         ten.year.aric.mesa.risk=ten.year.aric.mesa.risk)

    class(baseline.obj) <- "baseline.model"

    return(baseline.obj)
}

link.baseline.model <- function(model, newdata) {
    link <- predict(model$model, newdata, type="lp")
    return(link)
}

predict.baseline.model <- function(model, newdata, recalibrate=F, ...) {
    risk <- predict(model$model, newdata, type="risk")
    if (recalibrate) {
        risk <- 1 - model$ten.year.aric.mesa.risk ^ risk
    } else {
        risk <- 1 - model$ten.year.avg.risk ^ risk
    }
    return(risk)
}

print.baseline.model <- function(model) {
    print(t(t(coef(model$model))))
    tmp.mdl <- model$model
    print(tmp.mdl$coefficients %*% tmp.mdl$means)
    print(model$ten.year.aric.mesa.risk)
}
