library(config)

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")

displayable <- read.csv("data/results_on_examples.csv", header=F, skip=1, col.names=c("age", "sex", "sysbp", "rxbp", "totchol", "hdlc", "cursmoke", "dm", "pce.white.risk", "pce.black.risk", "pce.risk.ratio"))

examples <- displayable

examples$cholratio <- examples$totchol / examples$hdlc
examples$sex <- ifelse(examples$sex == "Male", 1, 0)
examples$rxbp <- ifelse(examples$rxbp == "Yes", 1, 0)
examples$dm <- ifelse(examples$dm == "Yes", 1, 0)
examples$cursmoke <- ifelse(examples$cursmoke == "Yes", 1, 0)

load(file=conf$full_models_file)

race <- examples$race
examples$race <- 0
examples$grp <- 2 * examples$sex + 2
examples[,"model.set.3.white.risk"] <- predict(logistic.2.eq, examples, recalibrate=T)
examples$race <- 1
examples$grp <- 2 * examples$sex + 1
examples[,"model.set.3.black.risk"] <- predict(logistic.2.eq, examples, recalibrate=T)
displayable[,"model.set.3.white.risk"] <- paste(
    specify.decimal(100 * examples[,"model.set.3.white.risk"], nsmall=1),
    "%", sep="")
displayable[,"model.set.3.black.risk"] <-  paste(
    specify.decimal(100 * examples[,"model.set.3.black.risk"], nsmall=1),
    "%", sep="")
displayable[,"model.set.3.ratio"] <- specify.decimal(
    examples[,"model.set.3.black.risk"] / examples[,"model.set.3.white.risk"], nsmall=2)

write.table(displayable, row.names=F, quote=F, sep="\t")
