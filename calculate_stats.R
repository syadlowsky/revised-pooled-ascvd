library(survival)
library(survAUC)
library(plyr)

source("compute_nri.R")
source("gnd_test.R")

extract.outcome <- function(df) {
    return(Surv(df$studytime, df$ascvd))
}

calc.auc <- function(train.frame, test.frame, link) {
    time = 10
    auc = AUC.uno(extract.outcome(train.frame),
                  extract.outcome(test.frame),
                  link, times=c(time))$auc
    return(auc)
}

calculate.gnd <- function(risk, data, time, nbins=NULL, plt=FALSE) {
                                        #split into deciles
    bin.size = 1.0/nbins
    probs = seq(0, 1, by=bin.size)
    deciles=as.numeric(cut2(risk, quantile(risk, probs=probs)))

                                        #calculate the GND test
    censored = data$studytime < time & !data$ascvd
    GND.result=GND.calib(pred=risk, tvar=data$studytime, out=data$ascvd,
                         cens.t=censored, groups=deciles, adm.cens=time, plt=plt)
}

cv_auc <- function(training.data, test.data, links) {
    data <- test.data
    aucsum <- 0
    for (fld in unique(data$fold)) {
        training.data <- subset(data, fold!=fld)
        test.data <-  subset(data, fold==fld)
        link <- links[data$fold==fld]
        fold.auc <- calc.auc(training.data,
                             test.data,
                             link)
        aucsum <- aucsum + fold.auc
    }
    return(aucsum / length(unique(data$fold)))
}

calculate.calib.slope <- function(outcome, time, risk, link, bins, t=10, plt=F) {
    risk <- unlist(risk)
    groups <- cut(risk,
                  breaks = quantile(risk, probs = seq(0, 1, 1.0/bins)),
                  include.lowest = TRUE, labels = 1:bins)
    expected <- aggregate(list(risk=risk), list(groups), mean)
    observed <- plyr::ddply(data.frame(event=outcome,time=time, groups=groups), ~groups, function(f) {
        sf <- survfit(Surv(event=f$event, time=f$time)~1)
        if (all(sf$time>t)) {
            return(cbind(risk=0.0))
        }
        return(cbind(risk=1-min(sf$surv[sf$time<=t])))
    })
    calib.model <- lm(observed$risk ~ expected$risk)

    calibration.object <- list(
        intercept=calib.model$coef["(Intercept)"],
        expected.risk=expected$risk,
        observed.risk=observed$risk,
        slope=calib.model$coef["expected$risk"])

    class(calibration.object) <- "calibration_curve"

    return(calibration.object)
}
