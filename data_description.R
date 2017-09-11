library(plyr)
library(foreign)
library(pROC)
library(PredictABEL)
library(survival)
library(survAUC)
library(Hmisc)
library(config)
library(ggplot2)
library(ggsci)

source("data_loaders.R")

conf <- config::get()
model.dir <- conf$model_dir
set.seed(101)

setwd(conf$working_dir)
options(digits=3)

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

survival.at.time <- function(sf, time) {
    return(min(sf$surv[sf$time<=time]))
}

plot.km <- function(df, plot.obj=NULL, name=NULL) {
    study.time <- pmin(df$studytime, 12.1)
    sf=survfit(Surv(time=study.time, event=df$ascvd)~1)

    ymin = 0.86

    surv.data <- data.frame(time=sf$time, surv=sf$surv)
    if (!is.null(name)) {
       surv.data$name <- name
    }

    if (is.null(plot.obj)) {
        plot.obj <- ggplot()
    }

    if (!is.null(name)) {
        plot.obj <- plot.obj + geom_line(
            data = surv.data,
            mapping=aes(x=time, y=surv, colour=name)
        )
    } else {
        plot.obj <- plot.obj + geom_line(
            data = surv.data,
            mapping=aes(x=time, y=surv)
        )
    }

    incident.labels <- data.frame(time=c(6, 10),
                                  surv=c(survival.at.time(sf, 6),
    				      survival.at.time(sf, 10)))

    plot.obj <- plot.obj +
        geom_line(data=data.frame(x=c(6, 6), y=c(ymin, 1)),
                  linetype=2,
                  mapping=aes(x, y),
                  color="gray") +
        geom_line(data=data.frame(x=c(10, 10), y=c(ymin, 1)),
                  linetype=2,
                  mapping=aes(x, y),
                  color="gray") +
        geom_text(data=incident.labels,
                  linetype=2,
                  mapping=aes(x=time, y=surv, label=signif(surv, 3)),
                  hjust=0,
                  vjust=0,
                  nudge_x=0.1)

    plot.obj <- plot.obj +
        xlim(0, 12) + ylim(ymin, 1) +
        labs(title="Survival curve for incidence of ASCVD",
             x="Time (years)",
             y="ASCVD incidence survival probability"
        ) +
        scale_color_jama(name="Population") +
        scale_shape_discrete(name="Population") +
        theme(plot.title = element_text(hjust = 0.5, size=conf$title_size),
              axis.title.y = element_text(size=conf$title_size),
              axis.title.x = element_text(size=conf$title_size),
              legend.title = element_text(size=conf$inset_title_size),
              legend.text = element_text(size=conf$inset_title_size * 0.95),
              legend.margin=margin(0.2, 0.2, 0.2, 0.2, unit='cm'),
              legend.key.size = unit(1.0, "lines"),
              legend.justification=c(0, 0), legend.position=c(0.045, 0.045))
    return(plot.obj)
}

kmrate <- function(df, plt=F, plot.obj=NULL, name=NULL) {
    study.time <- pmin(df$studytime, 12)
    sf=survfit(Surv(time=study.time, event=df$ascvd)~1)

    print.noquote(paste("At 6 years we have", (1-survival.at.time(sf, 6))))
    if(any(sf$time<=10)) {
        return(1-min(sf$surv[sf$time<=10]))
    } else {
        return(0)
    }
}

training.data <- load.training.data(paste(conf$working_dir, conf$train_file, sep="/"))
training.nofram <- training.data[training.data$study!="FRAM",]
test.data <- load.test.data(paste(conf$working_dir, conf$test_file, sep="/"))
test.nofram <- test.data[test.data$study!="FRAM",]

all.data <- rbind(training.data[,names(training.data) %in% c("fold")], test.data)
all.data.original.pooled <- subset(
    all.data, (study=="CHS" | study=="FRAM" | study=="FRAMOFF" | study=="ARIC" | study=="CARD"))
all.data.mesa <- subset(
    all.data, study=="MESA")
pooled.km.plot <- plot.km(all.data.original.pooled, name="AHA/ACC pooled cohorts")
comparison.km.plot <- plot.km(all.data.mesa, pooled.km.plot, name="MESA")
svg("compare_pooled_to_mesa.svg", height=4, width=4)
print(comparison.km.plot)
dev.off()

cat(paste("The final study sample size for the development cohorts was N=",nrow(training.data),"\n",sep=""))

population.stats <- function(data) {
    age.range <- list(min=min(data$age), max=max(data$age))
    age <- list(mean=mean(data$age), sd=sd(data$age))
    totchol <- list(mean=mean(data$totchol), sd=sd(data$totchol))
    hdlc <- list(mean=mean(data$hdl), sd=sd(data$hdl))
    untreated <- data[data$rxbp!=1, "sysbp"]
    treated <- data[data$rxbp==1, "sysbp"]
    untreated.sbp <- list(mean=mean(untreated), sd=sd(untreated))
    treated.sbp <- list(mean=mean(treated), sd=sd(treated))
    frac.on.bp.rx <- mean(data$rxbp)
    frac.with.diab <- mean(data$dm)
    frac.cursmoke <- mean(data$cursmoke)
    km.rate.10.yr <- kmrate(data)
    return(list(count=nrow(data),
                age.range=age.range,
                age=age, 
                totchol=totchol,
                hdlc=hdlc,
                untreated.sbp=untreated.sbp,
                treated.sbp=treated.sbp,
                frac.on.bp.rx=frac.on.bp.rx,
                frac.with.diab=frac.with.diab,
                frac.cursmoke=frac.cursmoke,
                km.rate.10.yr=km.rate.10.yr))
}

format.age.range <- function(range) {
    return(paste("(",round(range$min), ", ", round(range$max), ")", sep=""))
}

specify.decimal <- function(t, nsmall=3) {
    return(format(round(t, nsmall), nsmall=nsmall))
}

format.stat <- function(val) {
    return(specify.decimal(val, 1))
}

format.frac <- function(val) {
    return(specify.decimal(100*val, 2))
}

format.count <- function(val) {
    return(paste("n =", val))
}

study.grp.summary.stats <- function(data) {
    summary <- population.stats(data)
    df <- data.frame(mean=c(format.count(summary$count),
                            format.age.range(summary$age.range),
                            format.stat(summary$age$mean),
                            format.stat(summary$totchol$mean),
                            format.stat(summary$hdlc$mean),
                            format.stat(summary$untreated.sbp$mean),
                            format.stat(summary$treated.sbp$mean),
                            format.frac(summary$frac.on.bp.rx),
                            format.frac(summary$frac.cursmoke),
                            format.frac(summary$frac.with.diab),
                            format.frac(summary$km.rate.10.yr)),
                     sd=c("",
                          "",
                          format.stat(summary$age$sd),
                          format.stat(summary$totchol$sd),
                          format.stat(summary$hdlc$sd),
                          format.stat(summary$untreated.sbp$sd),
                          format.stat(summary$treated.sbp$sd),
                          "",
                          "",
                          "",
                          ""),
                     stringsAsFactors=F)
    return(df)
}

pooled.cohort.group.summary.stats <- function(train.data, test.data, grp) {
    train.data <- train.data[train.data$grp==grp,]
    test.data <- test.data[test.data$grp==grp,]
    summary <- cbind(
        study.grp.summary.stats(train.data),
        study.grp.summary.stats(test.data))
    summary <- rbind(c("Development", "", "Validation", ""),
		     c("Mean", "Std. dev.", "Mean", "Std. dev."),
		     summary)
}

add.summary.labels <- function(tbl) {
    cbind(c("", "", "", "Women", "Age Range", "Age (yrs)", "Total Cholesterol (mg/dl)",
      "HDL Cholesterol (mg/dL)", "Untreated SBP (mmHg)", "Treated SBP (mmHg)",
      "BP Meds (%)", "Current Smoker (%)", "Diabetes (%)",
      "10yr ASCVD incidence per 1,000 person-yrs", "", "", "Men", "Age Range",
      "Age (yrs)", "Total Cholesterol (mg/dl)", "HDL Cholesterol (mg/dL)",
      "Untreated SBP (mmHg)", "Treated SBP (mmHg)", "BP Meds (%)", "Current Smoker (%)",
      "Diabetes (%)", "10yr ASCVD incidence per 1,000 person-yrs"),
      tbl)
}

pooled.cohort.summary.stats <- function(training.data, test.data) {
    add.summary.labels(
    rbind(c("Black", "", "", "", "White", "", "", ""),
          cbind(pooled.cohort.group.summary.stats(training.data, test.data, 1),
                pooled.cohort.group.summary.stats(training.data, test.data, 2)),
          cbind(pooled.cohort.group.summary.stats(training.data, test.data, 3),
                pooled.cohort.group.summary.stats(training.data, test.data, 4))))
}

write.table(pooled.cohort.summary.stats(training.data, test.data),
#            conf$output$data_description$pooled_cohort,
            sep="\t", quote=F, row.names=F, na="", col.names=F)
cat("sans FRAM\n")
write.table(pooled.cohort.summary.stats(training.nofram, test.nofram),
#            conf$output$data_description$pooled_cohort,
            sep="\t", quote=F, row.names=F, na="", col.names=F)


table(training.data$grp)
table(test.data$grp)
