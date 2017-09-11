library(survival)
library(MASS)

source("utils.R")

link <- function(x, ...) UseMethod("link")

get.features.for.group <- function(data, group) {
    age = data$age
    totchol = data$totchol
    hdl = data$hdl
    sysbp = data$sysbp
    dm = data$dm
    rxbp = data$rxbp
    cursmoke = data$cursmoke

    features = data.frame(log(age),
                          log(age)^2,
                          log(totchol),
                          log(age)*log(totchol),
                          log(hdl),
                          log(age)*log(hdl),
                          rxbp*log(sysbp),
                          rxbp*log(age)*log(sysbp),
                          (1-rxbp)*log(sysbp),
                          (1-rxbp)*log(age)*log(sysbp),
                          cursmoke,
                          log(age)*cursmoke,
                          dm)

    feature.cols <- colnames(features)
    
    if (group == 1) {
        x = features[,feature.cols[c(1, 3, 5, 6, 7, 8, 9, 10, 11, 13)]]
    } else if (group == 2) {
        x = features[,feature.cols[c(1, 2, 3, 4, 5, 6, 7, 9, 11, 12, 13)]]
    } else if (group == 3) {
        x = features[,feature.cols[c(1, 3, 5, 7, 9, 11, 13)]]
    } else if (group == 4) {
        x = features[,feature.cols[c(1, 3, 4, 5, 6, 7, 9, 11, 12, 13)]]
    } else {
        error("Need a group for unreg")
    }
}

extract.survival <- function(data) {
    survival.outcome <- Surv(time=data$studytime, event=data$ascvd)
}

pce_model <- function(data) {
    model.by.group <- list()
    for (subgroup in unique(data$grp)) {
        subgroup.data <- subset(data, grp == subgroup)
        x <- get.features.for.group(subgroup.data, subgroup)

        survival.outcome <- extract.survival(subgroup.data)
        cvfit <- coxph(survival.outcome ~ ., data=x, iter.max=300, ties="breslow")
        hazard <- survfit(cvfit, type="breslow")
        ten.year.avg.risk = min(hazard$surv[hazard$time<=10])
        model.by.group[[subgroup]] <- list(model=cvfit, ten.year.avg.risk=ten.year.avg.risk)
    }
    model.by.group$groups <- unique(data$grp)
    class(model.by.group) <- "pce_model"
    return(model.by.group)
}

predict.pce_model <- function(object, newdata, recalibrate=F) {
    if (recalibrate) {
        error("Can't recalibrate this model")
    }

    predictions <- rep(0.0, nrow(newdata))
    for (subgroup in unique(newdata$grp)) {
        in.subgroup <- newdata$grp == subgroup
        subgroup.data <- subset(newdata, grp==subgroup)
        features <- get.features.for.group(subgroup.data, subgroup)
        model.for.subgroup <- object[[subgroup]]

        risk <- predict(
            model.for.subgroup$model, features, type="risk")
        predictions[in.subgroup] = pmin(
            pmax(1 - model.for.subgroup$ten.year.avg.risk ^ risk, 0), 1)
    }
    return(predictions)
}

link.pce_model <- function(object, newdata) {
    links <- rep(0.0, nrow(newdata))
    for (subgroup in unique(newdata$grp)) {
        in.subgroup <- newdata$grp == subgroup
        subgroup.data <- subset(newdata, grp==subgroup)
        features <- get.features.for.group(subgroup.data, subgroup)
        model.for.subgroup <- object[[subgroup]]

        links[in.subgroup] <- predict(
            model.for.subgroup$model, features, type="lp")
    }
    return(links)
}

print.pce_model <- function(model.obj) {
    for (group in model.obj$groups) {
        cat(paste(group_to_description(group), "\n", sep=""))
        write.csv(cbind("Point estimate"=coef(model.obj[[group]]$model), confint(model.obj[[group]]$model)))
    }
}
