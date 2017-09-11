library(caret)
library(config)
library(plyr)
library(foreign)
library(pROC)
library(PredictABEL)
library(survival)
library(survAUC)
library(Hmisc)
library(config)

conf <- config::get()

specify.decimal <- function(t, nsmall=3) {
    return(format(round(t, nsmall), nsmall=nsmall))
}

print.table.many.ways <- function(t) {
    print(addmargins(t))
    print(prop.table(t, margin=1))
}

load.mesa <- function(filename){
    cohdata = read.table(filename, head=TRUE, sep=",", row.names="idno")

    excluded.for.race <- is.na(cohdata$grp)
    excluded.for.age <- (cohdata$age < 40 | cohdata$age > 79) & !excluded.for.race
    excluded.for.prevalent <- ((cohdata$studytime <= 0) | cohdata$prevcond) &
        !(excluded.for.race | excluded.for.age)
    print(paste("MESA excluded",
                sum(excluded.for.age, na.rm=T),
                "individuals for age outside [40, 79],",
                sum(excluded.for.race, na.rm=T),
                "individuals that were neither Black nor White, and",
                sum(excluded.for.prevalent, na.rm=T),
                "individuals for prevalent conditions at baseline"))


    cohdata <- cohdata[!(excluded.for.age | excluded.for.prevalent | excluded.for.race), ]
    fulldata = data.frame(ascvd=cohdata$ascvd,
                          studytime=cohdata$studytime,
                          age=cohdata$age1c,
                          totchol=cohdata$totchol,
                          hdl=cohdata$hdlc,
                          sysbp=cohdata$sysbp,
                          rxbp=cohdata$hyptmdsr,
                          bmi=cohdata$bmi,
                          dm=cohdata$diabt126,
                          cursmoke=cohdata$cursmoke,
                          cholmed=cohdata$cholmed1,
                          grp=cohdata$grp,
                          study=cohdata$study)

    print(paste("MESA excluded", nrow(fulldata) - sum(complete.cases(fulldata)),
                "with missing covariates"))
    nonadata = fulldata[complete.cases(fulldata),]
    print(paste("MESA had N =", nrow(nonadata), "cases total"))

    return(nonadata)
}

load.jhs <- function(filename){
    cohdata = read.table(filename, head=TRUE, sep=",", row.names="idno")
    cohdata$study = "JHS"

    excluded.for.race <- is.na(cohdata$grp)
    excluded.for.age <- (cohdata$age < 40 | cohdata$age > 79) & !excluded.for.race
    excluded.for.prevalent <- ((cohdata$studytime <= 0) | cohdata$prevcond) &
        !(excluded.for.race | excluded.for.age)
    print(paste("JHS excluded",
                sum(excluded.for.age, na.rm=T),
                "individuals for age outside [40, 79],",
                sum(excluded.for.race, na.rm=T),
                "individuals that were neither Black nor White, and",
                sum(excluded.for.prevalent, na.rm=T),
                "individuals for prevalent conditions at baseline"))
    cohdata <- cohdata[!(excluded.for.age | excluded.for.race | excluded.for.prevalent),]

    fulldata = data.frame(ascvd=cohdata$ascvd,
                          studytime=cohdata$studytime,
                          age=cohdata$age,
                          totchol=cohdata$totchol,
                          hdl=cohdata$hdlc,
                          sysbp=cohdata$sysbp,
                          rxbp=cohdata$hyptmdsr,
                          bmi=cohdata$bmi,
                          dm=cohdata$diabt126,
                          cursmoke=cohdata$cursmoke,
                          cholmed=cohdata$cholmed1,
                          grp=cohdata$grp,
                          study=cohdata$study)

    print(paste("JHS excluded", nrow(fulldata) - sum(complete.cases(fulldata)),
                "with missing covariates"))
    nonadata = fulldata[complete.cases(fulldata),]
    print(paste("JHS had N =", nrow(nonadata), "cases total"))

    return(nonadata)
}

load.cohdata <- function(filename) {
    cohdata = read.table(filename, head=TRUE, sep=",", row.names="nid")

    excluded.for.age <- cohdata$age < 40 | cohdata$age > 79
    excluded.for.prevalent <- cohdata$prevcond | (cohdata$studytime <= 0)
    print(paste("Pooled cohorts excluded", sum(excluded.for.age, na.rm=T),
                "individuals for age outside [40, 79], and",
                sum(excluded.for.prevalent, na.rm=T),
                "individuals for prevalent conditions at baseline"))
    cat("By study...\n")
    cat("...for age\n")
    print.table.many.ways(table(cohdata$study, excluded.for.age, dnn=c("Study", "Excluded for age")))
    cat("...for prevalent condition at baseline\n")
    print.table.many.ways(table(cohdata$study, excluded.for.prevalent, dnn=c("Study", "Excluded for prevalence")))
    cohdata <- cohdata[!(excluded.for.age | excluded.for.prevalent),]

    fulldata = data.frame(ascvd=cohdata$ascvd,
                          studytime=cohdata$studytime,
                          age=cohdata$age,
                          totchol=cohdata$totchol,
                          hdl=cohdata$hdlc,
                          sysbp=cohdata$sysbp,
                          rxbp=cohdata$hyptmdsr,
                          bmi=cohdata$bmi,
                          dm=cohdata$diabt126,
                          cursmoke=cohdata$cursmoke,
                          cholmed=cohdata$cholmed1,
                          grp=cohdata$grp,
                          study=cohdata$study)

    print(paste("Pooled cohorts excluded",
                nrow(fulldata) - sum(complete.cases(fulldata)),
                "with missing covariates"))
    cat("By study...\n")
    print.table.many.ways(table(fulldata$study, complete.cases(fulldata), dnn=c("Study", "Has all covariates")))
    nonadata = fulldata[complete.cases(fulldata),]
    print(paste("Pooled cohorts had N =", nrow(nonadata), "cases total"))

    return(nonadata)
}

event.rate.per.1000.pyears <- function(data){
  event.rate <- with(
    data,
    pyears(Surv(time=studytime, event=ascvd)~1,
           scale=1))
  return(1000*event.rate$event / event.rate$pyears)
}

person.years <- function(data) {
  event.rate <- with(
    data,
    pyears(Surv(time=studytime, event=ascvd)~1,
           scale=1))
  return(event.rate$pyears)
}


kmrate <- function(df, plt=F, lin=F) {
    study.time <- pmin(df$studytime, 12)
    sf=survfit(Surv(time=study.time, event=df$ascvd)~1)
    if (plt) {
        plot(sf)
    } else if (lin) {
        lines(sf)
    }
    print.noquote(paste("At 6 years we have", (1-min(sf$surv[sf$time<=6]))))
    if(any(sf$time<=10)) {
        return(1-min(sf$surv[sf$time<=10]))
    } else {
        return(0)
    }
}

splitdf <- function(dataframe, seed=NULL, fraction=0.8) {
    if (!is.null(seed)) set.seed(seed)
    folds <- createFolds(factor(paste(dataframe$ascvd, trunc(dataframe$studytime/2), dataframe$grp)), k = 10)
    testindex <- c(folds[[1]], folds[[2]])
    trainset <- dataframe[-testindex, ]
    testset <- dataframe[testindex, ]
    list(trainset=trainset,testset=testset)
}

all.data <- rbind(
    load.cohdata(conf$pooled_cohort_csv),
    load.jhs(conf$jhs_csv),
    load.mesa(conf$mesa_csv))

try.on.study <- function(study) {
    if (!is.na(study)) {
        study.data <- all.data[all.data$study==study,]
        print(study)
    } else {
        study.data <- all.data
    }

print.noquote(
  paste("Participants in the development cohorts were followed for a mean of ",
        format(mean(study.data$studytime),
               digits=3),
        " years, leading to ",
        format(
            person.years(study.data),
            digits=6),
        " with a 10 year KM rate of first hard ASCVD event of ",
        specify.decimal(
            100*kmrate(
                    study.data, plt=T),
          nsmall=3),
        " per 1,000 person-years (ranging from ",
        specify.decimal(
            100*kmrate(
                    study.data[study.data$grp==1,]),
          nsmall=3),
        " to ",
        specify.decimal(
            100*kmrate(
                    study.data[study.data$grp==3,]),
          nsmall=3),
        " among blacks and ",
        specify.decimal(
            100*kmrate(
            study.data[study.data$grp==2,]),
          nsmall=3),
        " to ",
        specify.decimal(
            100*kmrate(
            study.data[study.data$grp==4,]),
          nsmall=3),
        " among whites).",
        sep=""))
}
try.on.study(NA)

print(paste("Age", mean(all.data$age)))
print(paste("Women", mean(all.data$grp <= 2)))
print(paste("Black", mean((all.data$grp %% 2) == 1)))
print(paste("Women", sum(all.data$grp <= 2)))
print(paste("Black", sum((all.data$grp %% 2) == 1)))

print(table(all.data$grp, all.data$ascvd))

train.and.test.splits <- splitdf(
    dataframe=all.data,
    seed=conf$seed,
    fraction=0.8)

train <- train.and.test.splits$trainset
print(nrow(train))
folds <- createFolds(factor(paste(train$ascvd, train$study, train$grp)), k = conf$folds)
i <- 1
for (fold in folds) {
    train[fold, "fold"] <- i
    i <- i + 1
}
test <- train.and.test.splits$testset
print(nrow(test))

write.csv(train, file = conf$train_file, row.names = FALSE)
write.csv(test, file = conf$test_file, row.names = FALSE)
