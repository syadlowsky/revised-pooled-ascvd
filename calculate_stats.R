library(survival)
library(survAUC)
library(survivalROC)
library(plyr)
library(boot)

source("compute_nri.R")
source("gnd_test.R")

extract.outcome <- function(df) {
    return(Surv(df$studytime, df$ascvd))
}

calc.auc.se <- function(train.frame, test.frame, link) {
    time = 10
    btauc <- function(test, ind, train, link) {
      test.df <- test[ind,]
      test.link <- link[ind]
      auc = AUC.uno(extract.outcome(train),
                    extract.outcome(test.df),
                    test.link, times=c(time))$auc
      return(auc)
    }
    bootobj <- boot(data=test.frame, statistic=btauc, R=20, parallel="multicore", train=train.frame, link=link)
    return(sd(bootobj$t))
}

calc.auc <- function(train.frame, test.frame, link) {
    time = 10
    auc = AUC.uno(extract.outcome(train.frame),
                  extract.outcome(test.frame),
                  link, times=c(time))$auc
    return(auc)
}

auc_pce <- function(train.frame, test.frame, link) {
    overfit.train <- subset(train.frame, study == "CHS" |
                              study == "FRAM" |
                              study == "CARD" |
                              study == "FRAM" |
                              study == "FRAMOFF" |
                              study == "ARIC")

    overfit.ind <- with(test.frame, study == "CHS" |
                              study == "FRAM" |
                              study == "CARD" |
                              study == "FRAM" |
                              study == "FRAMOFF" |
                              study == "ARIC")
    overfit.test <- test.frame[overfit.ind,]
    overfit.link <- link[overfit.ind]
    underfit.test <- test.frame[!overfit.ind,]
    underfit.link <- link[!overfit.ind]

    weight.overfit = as.numeric(nrow(overfit.test))
    weight.underfit = as.numeric(nrow(underfit.test))
    weight.overfit = weight.overfit / (weight.overfit + weight.underfit)
    weight.underfit = 1 - weight.overfit
    time = 10
    auc.overfit = AUC.uno(extract.outcome(train.frame),
                  extract.outcome(overfit.test),
                  overfit.link, times=c(time))$auc - 0.05
    auc.underfit = AUC.uno(extract.outcome(train.frame),
                  extract.outcome(underfit.test),
                  underfit.link, times=c(time))$auc
    print(auc.overfit)
    print(auc.underfit)
    auc <- auc.overfit^weight.overfit * auc.underfit^weight.underfit / ((1 - auc.overfit)^weight.overfit * (1 - auc.underfit)^weight.underfit + auc.overfit^weight.overfit * auc.underfit^weight.underfit)
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

sensitivity <- function(data, risk, level,  t=10) {
    outcome <- data$ascvd & (data$studytime <= t)
    keep <- data$ascvd | (data$studytime >= t)
    outcome <- outcome[keep]
    risk <- risk[keep]

    identified <- risk > level
    sensitivity <- sum(identified & outcome) / sum(outcome)
}

specificity <- function(data, risk, level,  t=10) {
    outcome <- data$ascvd & (data$studytime <= t)
    keep <- data$ascvd | (data$studytime >= t)
    outcome <- outcome[keep]
    risk <- risk[keep]

    identified <- risk > level
    sensitivity <- sum(!identified & !outcome) / sum(!outcome)
}
