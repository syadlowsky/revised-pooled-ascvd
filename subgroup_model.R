link <- function(x, ...) UseMethod("link")

race_and_sex_subgroup_model <- function(formula.no.race.terms, subgroup_model, data, time=10, ...) {
    model.by.group <- list()
    for (subgroup in unique(data$grp)) {
        subgroup.data <- subset(data, grp == subgroup)
        model.by.group[[subgroup]] <- subgroup_model(
            formula.no.race.terms, data=subgroup.data,
            time=time, ...)
    }
    model.by.group$groups <- unique(data$grp)
    class(model.by.group) <- "race_and_sex_subgroup_model"
    return(model.by.group)
}

link.race_and_sex_subgroup_model <- function(object, newdata, ...) {
    links <- rep(0.0, nrow(newdata))
    for (subgroup in unique(newdata$grp)) {
        in.subgroup <- newdata$grp == subgroup
        subgroup.data <- subset(newdata, in.subgroup)
        model.for.subgroup <- object[[subgroup]]
        links[in.subgroup] <- link(
            model.for.subgroup, subgroup.data, ...)
    }
    return(links)
}

predict.race_and_sex_subgroup_model <- function(object, newdata, ...) {
    predictions <- rep(0.0, nrow(newdata))
    for (subgroup in unique(newdata$grp)) {
        in.subgroup <- newdata$grp == subgroup
        subgroup.data <- subset(newdata, grp==subgroup)
        model.for.subgroup <- object[[subgroup]]
        predictions[in.subgroup] <- predict(
            model.for.subgroup, subgroup.data, ...)
    }
    return(predictions)
}

print.race_and_sex_subgroup_model <- function(model.obj) {
    for (group in model.obj$groups) {
        cat(paste(group, "\n", sep=""))
        print(model.obj[[group]])
    }
}

to.model.group <- function(grp) {
 return((as.integer(grp - 1) %/% 2) + 1)
} 

sex_subgroup_model <- function(formula.with.race.terms, subgroup_model, data, time=10, ...) {
    data$mdlgrp <- to.model.group(data$grp)
    model.by.group <- list()
    for (subgroup in unique(data$mdlgrp)) {
        subgroup.data <- subset(data, mdlgrp == subgroup)
        model.by.group[[subgroup]] <- subgroup_model(
            formula.with.race.terms, data=subgroup.data,
            time=time, ...)
    }
    model.by.group$groups <- unique(data$mdlgrp)
    class(model.by.group) <- "sex_subgroup_model"
    return(model.by.group)
}

predict.sex_subgroup_model <- function(model.obj, newdata, type="risk", ...) {
    newdata$mdlgrp <- to.model.group(newdata$grp)
    prediction <- rep(0.0, nrow(newdata))
    for (subgroup in unique(newdata$mdlgrp)) {
        in.subgroup <- newdata$mdlgrp == subgroup
        subgroup.data <- subset(newdata, in.subgroup)
        model.for.subgroup <- model.obj[[subgroup]]
        if (type=="risk") {
            prediction[in.subgroup] <- predict(
                model.for.subgroup, subgroup.data, ...)
        } else {
            prediction[in.subgroup] <- link(
                model.for.subgroup, subgroup.data, ...)
        }
    }
    return(prediction)
}

link.sex_subgroup_model <- function(model.obj, newdata, ...) {
    newdata$mdlgrp <- to.model.group(newdata$grp)
    prediction <- rep(0.0, nrow(newdata))
    for (subgroup in unique(newdata$mdlgrp)) {
        in.subgroup <- newdata$mdlgrp == subgroup
        subgroup.data <- subset(newdata, in.subgroup)
        model.for.subgroup <- model.obj[[subgroup]]
        prediction[in.subgroup] <- link(
            model.for.subgroup, subgroup.data, ...)
    }
    return(prediction)
}

print.sex_subgroup_model <- function(model.obj) {
    for (group in model.obj$groups) {
        cat(paste(group, "\n", sep=""))
        print(model.obj[[group]])
    }
}
