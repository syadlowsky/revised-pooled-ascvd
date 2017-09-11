library(caret)
library(config)
library(glmnet)

# Maybe don't need all of these...
library(parallel)
library(doParallel)
library(survival)
library(config)
library(MASS)
library(pROC)
library(PredictABEL)
library(survAUC)

source("gnd_test.R")
source("compute_nri.R")

conf <- config::get()
#setwd(conf$working_dir)

basehaz.coxnet <- function(model, outcome, features, s="lambda.min") {
    tab <- data.frame(table(outcome[outcome[,"status"] == 1, "time"])) 
    y <- as.numeric(levels(tab[, 1]))[tab[, 1]] #ordered distinct event times
    d <- tab[, 2]                               #number of events
    links <- predict(model, newx=as.matrix(features), type="link", s=s)
    links <- links - mean(links)

    h0 <- rep(NA, length(y))
    for(l in 1:length(y))
    {
        h0[l] <- d[l] / sum(exp(links[outcome[,"time"] >= y[l]]))
    }
    s0 <- cumsum(h0)
    return(data.frame(time=y, hazard=s0))
}

censorship.rate <- function(data, t.cens=10, var=1) {
    outcome.times <- data$studytime
    censoring.var <- 1-data$ascvd

    # If var is 1, then uses KM curve. If it's a variable, it either partitions
    # and uses KM (or something equivalent) or if the var is not a factor, it
    # uses a Cox model.
    km <- survfit(Surv(outcome.times, censoring.var)~1)
    survival <- data.frame(time=c(-1, km$time), survival=c(1.0, km$surv))
    outcome.weights <- vapply(
        pmin(outcome.times, t.cens),
        function(t) {return(1/min(survival$survival[survival$time<=t]))},
        1.0)
    return(outcome.weights)
}

censorship.rate.by.study <- function(data, t.cens=10, var=1) {
  outcome.weights <- rep(1, nrow(data))
  outcome.weights <- censorship.rate(data, t.cens, var)
  # TODO: change config var
  if (conf$elastic_net$logistic$censorship_per_study) {
      for (study in unique(data$study)) {
          outcome.weights[data$study==study] <- 
              censorship.rate(data[data$study==study,], t.cens, var)
      }
  }
  return(outcome.weights)
}

get.features <- function(data) {
    age = data$age
    totchol = data$totchol
    hdl = data$hdl
    sysbp = data$sysbp
    rxbp = data$rxbp
    dm = data$dm
    statin = data$cholmed
    cursmoke = data$cursmoke
    bmi = data$bmi

    race = data$grp %% 2

    age.sq = age^2
    totchol.sq = totchol^2
    age.totchol = age*totchol
    hdl.sq = hdl^2
    cholratio = totchol / hdl
    cholratio.sq = cholratio^2
    age.hdl = age*hdl
    sysbp.sq = sysbp^2
    rx.sbp = sysbp*rxbp
    age.rxs.bp = age*rx.sbp
    age.dm = age*dm
    age.cursmoke = age * cursmoke
    age.bmi = age * bmi
    dm.bmi = dm * bmi
    bmi.sq = bmi^2
    age.statin = age * statin
    cholratio.statin = cholratio * statin
    hdl.statin = hdl * statin
    hdl.dm = hdl * dm

    race.age = race * age
    race.sysbp = race * sysbp
    race.dm = race * dm
    race.rxbp = race * rxbp
    race.cursmoke = race * cursmoke
    race.cholratio = race * cholratio

    fulldata = data.frame(age,
                          race,
                          race.age,
                          #age.sq,
                          #totchol,
                          #totchol.sq,
                          #hdl,
                          #hdl.sq,
                          sysbp,
                          race.sysbp,
                          #sysbp.sq,
                          #bmi,
                          dm, rxbp, cursmoke,
                          race.dm,
                          race.rxbp,
                          race.cursmoke,
                          #age.totchol, age.hdl,
                          cholratio,
                          race.cholratio,
                          #cholratio.sq,
                          rx.sbp) #,
                          #age.rxs.bp,
                          #age.dm,
                          #age.cursmoke,
                          #age.bmi,
                          #hdl.dm)
    return(fulldata)
}

fit.logistic.model <- function(data, time=10) {
    outcome = data$ascvd & (data$studytime<=time)
    keep = data$ascvd | (data$studytime >= time)
    features = get.features(data)

    blah = 2*data$cursmoke + data$dm
    weights <- censorship.rate.by.study(data, t.cens=time, factor(blah))
    features = features[keep,]
    outcome = outcome[keep]
    weights = weights[keep]
    study = data[keep, "study"]
    x <-  data.matrix(features)
    mu <- colMeans(x)
    x <- x - rep(mu, rep.int(nrow(x), ncol(x)))
    outcome <- as.integer(outcome)

    alpha <- conf$elastic_net$logistic$alpha
    nfolds <- conf$elastic_net$logistic$folds
    foldid <- sample(1:nfolds,size=length(outcome),replace=TRUE)
    alphas = seq(0, 1, 0.2)
    gof = rep(-1, length(alphas))
    models = list()
    for (i in 1:length(alphas)) {
        alpha = alphas[i]
        cvfit = cv.glmnet(x, outcome, family = "binomial", alpha=alpha,
                                        # TODO: change config var
                          weights=weights, foldid=foldid,
                          standardize=T,
                                        # TODO: change config var
                          parallel=conf$run_parallel)
        best.fit <- cvfit$lambda == cvfit$lambda.min
        gof[i] <- cvfit$cvm[best.fit]
        models[[i]] = cvfit
    }
    best.index <- which.min(gof)
    cvfit <- models[[best.index]]

    baseline.prob <- mean(outcome)
    baseline.or <- baseline.prob / (1 - baseline.prob)

    representative.prob <- mean(outcome[study == "MESA" | study == "ARIC"])
    representative.or <- representative.prob / (1 - representative.prob)

    calibration <- representative.or / baseline.or

    # TODO: what should be returned?
    return(list(model=cvfit, mu=mu, calibration=calibration))
}

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

fit.stepwise.cox.model <- function(data, time=10) {
    outcome = data$ascvd
    study.time = data$studytime
    features = get.features(data)

    survival.outcome <- Surv(time=pmin(study.time, time+2), event=outcome)
    # Using Breslow to be like SAS, which was used for original model
    cvfit <- coxph(survival.outcome ~ ., data=features, ties="breslow")
    # Perform variable selection
    back <- stepAIC(cvfit, ~., direction="backward", trace=0)
    
    hazard <- survfit(back, type="breslow")
    ten.year.avg.risk = min(hazard$surv[hazard$time<=time])
    
    aric.mesa <- (data$study == "ARIC") | (data$study == "MESA")
    aric.mesa.hazard <- breslow.estimator(study.time[aric.mesa], outcome[aric.mesa],
                                features[aric.mesa, ], back)
    ten.year.aric.mesa.risk = min(aric.mesa.hazard$surv[aric.mesa.hazard$time<=time])

    return(list(model=back, ten.year.avg.risk=ten.year.avg.risk,
                ten.year.aric.mesa.risk=ten.year.aric.mesa.risk))
}

link.logistic.model <- function(model, newdata) {
    newx <- data.matrix(get.features(newdata))
    newx <- newx - rep(model$mu, rep.int(nrow(newx), ncol(newx)))
    link <- predict(model$model, newx, s=conf$elastic_net$logistic$s, type="link")
    return(link)
}

predict.logistic.model <- function(model, newdata, recalibrate=F, ...) {
    newx <- data.matrix(get.features(newdata))
    newx <- newx - rep(model$mu, rep.int(nrow(newx), ncol(newx)))
    response <- predict(model$model, newx, s=conf$elastic_net$logistic$s, type="response")
    if (recalibrate) {
        response <- response * model$calibration / (1 - (1 - model$calibration) * response)
    }
    return(response)
}

link.stepwise.cox.model <- function(model, newdata, time=10) {
    link <- predict(model$model, get.features(newdata), type="lp")
    return(link)
}

predict.stepwise.cox.model <- function(model, newdata, time=10, recalibrate=F, ...) {
    risk <- predict(model$model, get.features(newdata), type="risk")
    if (recalibrate) {
        risk <- 1 - model$ten.year.aric.mesa.risk ^ risk
    } else {
        risk <- 1 - model$ten.year.avg.risk ^ risk
    }
    return(risk)
}

specify.decimal <- function(t, nsmall=3) {
    return(format(round(t, nsmall), nsmall=nsmall))
}

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

if (conf$run_parallel) {
    registerDoParallel(cores=conf$cores)
}

to.model.group <- function(grp) {
 return((as.integer(grp - 1) %/% 2) + 1)
} 

data <- read.csv(file=paste(conf$working_dir, conf$train_file, sep="/"))
data$mdlgrp <- to.model.group(data$grp)

load(file=paste(conf$working_dir, conf$models_file, sep="/"))

cohort.by.risk.decile <- function(risk, data, t.time, nbins=NULL) {
      bin.size = 1.0/nbins
    probs = seq(0, 1, by=bin.size)
    deciles=as.numeric(cut2(risk, quantile(risk, probs=probs)))
    foo <- plyr::ddply(data.frame(event=data$ascvd, time=data$studytime,
			    decile=deciles, study=data$study),
		    ~ decile + study,
		    function(s.d.f){
		    km <- survfit(Surv(time, event)~1, data=s.d.f)
		    return(data.frame(ascvd=1-pmin(min(km$surv[km$time <= t.time]), 1)))
		    })
      print(round(xtabs(ascvd~decile+study, foo), 3))
}

for (grp in 1:4) {
    print(paste("Group", grp))
    models.by.fold <- models.by.group[[to.model.group(grp)]]
    baseline.aucsum <- 0
    logistic.aucsum <- 0
    logistic.risks <- list()
    logistic.links <- list()
    baseline.risks <- list()
    baseline.links <- list()
    test.data.collection <- data.frame()

    for (model in models.by.fold) {
        if (is.null(model)) {
            next
        }
        training.data <- model$training.data
        test.data <- model$test.data[model$test.data$grp == grp, ]
        baseline <- model$baseline
        logistic <- model$logistic
        
        link <- link.stepwise.cox.model(baseline, test.data)
        risk <- predict.stepwise.cox.model(baseline, test.data)
        baseline.links <- append(baseline.links, link)
        baseline.risks <- append(baseline.risks, risk)

        link <- link.logistic.model(logistic, test.data)
        risk <- predict.logistic.model(logistic, test.data)
        logistic.links <- append(logistic.links, link)
        logistic.risks <- append(logistic.risks, risk)
        test.data.collection <- rbind(test.data.collection, test.data)
    }
    baseline.risks <- do.call("rbind", baseline.risks)
    baseline.links <- do.call("rbind", baseline.links)
    logistic.risks <- do.call("rbind", logistic.risks)
    logistic.links <- do.call("rbind", logistic.links)
    #cohort.by.risk.decile(baseline.risks, test.data.collection, 10, nbins=15)
    test.data.all <- test.data.collection
    test.data.all$study <- "ALL"
    cohort.by.risk.decile(rbind(logistic.risks, logistic.risks),
                          rbind(test.data.collection, test.data.all),
                          10, nbins=15)
}

