library(config)
library(glmnet)
library(survival)

library(caret)
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

conf <- config::get()

source("utils.R")

link <- function(x, ...) UseMethod("link")

breslow.estimate <- function(time, event, links) {
    tab <- data.frame(table(time[event==1]))
    y <- as.numeric(levels(tab[, 1]))[tab[, 1]] #ordered distinct event times
    d <- tab[, 2]                               #number of events
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

censorship.rate <- function(data, X, t=10) {
    outcome.times <- pmin(data$studytime, t+2)
    censoring.var <- 1 - data$ascvd
    #vars <- data.frame(X)

    #cm <- coxph(Surv(outcome.times, censoring.var) ~ ., data=vars)
    #links <- predict(cm, vars, type="lp")
    cm <- cv.glmnet(X, Surv(outcome.times, censoring.var), family="cox")
    links <- predict(cm, newx=X, s="lambda.min", type="link")
    sf <- breslow.estimate(outcome.times, censoring.var, links)
    sf <- data.frame(time=c(-1, sf$time), survival=c(1.0, sf$surv))
    hazard.ratio <- predict(cm, newx=X, type="response", s="lambda.min")
    mlhr = exp(mean(log(hazard.ratio)))
    hazard.ratio <- hazard.ratio / mlhr
    survival <- vapply(pmin(outcome.times, t),
                       function(u) {min(sf$surv[sf$time<=u])},
                       1)
    survival <- survival ^ hazard.ratio
    outcome.weights <- survival
    return(outcome.weights)
}

censorship.rate.by.study <- function(data, features, t.cens=10) {
  outcome.weights <- rep(1, nrow(data))
  if (conf$elastic_net$logistic$censorship_per_study) {
      for (study in unique(data$study)) {
          outcome.weights[data$study==study] <- 
              censorship.rate(data[data$study==study,],
                              features[data$study==study,],
                              t.cens)
      }
  } else {
      outcome.weights <- censorship.rate(data, features, t.cens)
  }
  return(outcome.weights)
}

fit.logistic.model <- function(data, feature.equation, time=10) {
    outcome = data$ascvd & (data$studytime<=time)
    keep = data$ascvd | (data$studytime >= time)
    data$outcome <- outcome

    # This seems unnecessary...
    e <- new.env()
    parent.env(e) = environment(feature.equation)
    e$outcome = outcome
    e$data = data
    attr(feature.equation, ".Environment") <- e
    features = model.matrix(feature.equation, data=data)

    weights <- 1.0 / censorship.rate.by.study(data, features, t.cens=time)
    features = features[keep,]
    outcome = outcome[keep]
    weights = weights[keep]
    study = data[keep, "study"]
    race = data[keep, "race"]
    x <-  features
    mu <- colMeans(x)
    x <- x - rep(mu, rep.int(nrow(x), ncol(x)))
    outcome <- as.integer(outcome)

    alpha <- conf$elastic_net$logistic$alpha
    nfolds <- conf$elastic_net$logistic$folds

    alphas = seq(0, 1, 0.2)

    foldid <- 1:nrow(x)

    rseed <- .Random.seed
    set.seed(conf$seed)
    folds <- createFolds(factor(paste(outcome, study, race)),
                         k = nfolds)
    .Random.seed <- rseed
    i <- 1
    for (fold in folds) {
        foldid[fold] <- i
        i <- i + 1
    }

    gof = rep(-1, length(alphas))
    models = list()
    for (i in 1:length(alphas)) {
        alpha = alphas[i]
        cvfit = cv.glmnet(x, outcome, family = "binomial", alpha=alpha,
                          weights=weights, foldid=foldid,
                          standardize=T,
                          parallel=F)
        best.fit <- cvfit$lambda == "[["(cvfit, conf$elastic_net$logistic$s)
        gof[i] <- cvfit$cvm[best.fit]
        models[[i]] = cvfit
    }
    best.index <- which.min(gof)

    cvfit <- models[[best.index]]

    intercept <- predict(cvfit, t(-mu), s=conf$elastic_net$logistic$s, type="link")

    calibration <- list()

    for (race.value in unique(race)) {
        baseline.prob <- weighted.mean(outcome[race == race.value], weights[race == race.value])
        baseline.or <- baseline.prob / (1 - baseline.prob)

        representative.selector <- (study == "MESA" | study == "ARIC") & race==race.value
        representative.prob <- weighted.mean(outcome[representative.selector],
                                             weights[representative.selector])
        representative.or <- representative.prob / (1 - representative.prob)

        calibration[[race.value+1]] <- representative.or / baseline.or
    }

    logistic.obj <- list(model=cvfit, mu=mu, intercept=intercept,
                         calibration=calibration, eq=feature.equation)

    class(logistic.obj) <- "logistic.model"

    return(logistic.obj)
}

link.logistic.model <- function(model, newdata) {
    newx <- model.matrix(delete.response(terms(model$eq)), data=newdata)
    newx <- newx - rep(model$mu, rep.int(nrow(newx), ncol(newx)))
    link <- predict(model$model, newx, s=conf$elastic_net$logistic$s, type="link")
    return(link)
}

predict.logistic.model <- function(model, newdata, recalibrate=F, ...) {
    newx <- model.matrix(delete.response(terms(model$eq)), data=newdata)
    newx <- newx - rep(model$mu, rep.int(nrow(newx), ncol(newx)))
    response <- predict(model$model, newx, s=conf$elastic_net$logistic$s, type="response")
    if (recalibrate) {
        lor <- log(response / (1 - response)) + log(unlist("["(model$calibration, newdata$race + 1)))
        response <- 1 / (1 + exp(-lor))
    }
    return(response)
}

print.logistic.model <- function(model) {
     print(coef(model$model, s=conf$elastic_net$logistic$s))
     print(model$intercept)
     print(lapply(model$calibration, log.if.not.null))
     print(lapply(lapply(model$calibration, log.if.not.null),
                  `+`,
                  model$intercept))
}
