library(foreign)
library(survival)
library(rms)

options(width=140)
set.seed(101)

source("data_utils.R")
source("data_loaders.R")

setwd("~/phd/ASCVD/ASCVD")

nonadata <- load.training.data("data/train.csv")

extract.outcome <- function(datagrp) {
    survtime <- datagrp$studytime
    survtime[survtime==0] <- 1/12
    return(cbind(time=survtime,status=datagrp$ascvd))
}

extract.survival <- function(data) {
    outcome <- extract.outcome(data)
    survival.outcome <- Surv(time=outcome[,"time"], event=outcome[,"status"])
}


group_to_description <- function(group) {
    if (group == 1) {
        return("Black women")
    } else if (group == 2) {
        return("White women")
    } else if (group == 3) {
        return("Black men")
    } else if (group == 4) {
        return("White men")
    } else {
        return("undefined group")
    }
}

#km.by.smoking <- npsurv(extract.survival(nonadata) ~ nonadata$cursmoke)
#svg("km_by_smoking.svg", 5, 5)
#plot(km.by.diabetes, fun="cloglog", ylabel="log(-log(S(t)))", xlabel="t"## Full-fledged grammer
# survplot(fit  = km.by.smoking,
#          conf = "bands",
#          xlab = "log(time)",
#          ylab = "log(-log(survival))",
#          #label.curves = TRUE,                     # label curves directly
#          label.curves = list(keys = "lines"),  # legend instead of direct label
#          levels.only  = FALSE,                    # show only levels, no label
#          abbrev.label = FALSE,                    # if label used, abbreviate
#          ## fun = function(x) {1 - x},            # Cumulative probability plot         
#          loglog   = TRUE,                        # log(-log Survival) plot
#          logt     = TRUE,                        # log time
#          dots     = TRUE,                         # dot grid
#          n.risk   = F,                         # show number at risk
#          ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
#          ## y.n.risk = 0, cex.n.risk = 0.6
#          )
# 
# svg("prop_hazards.svg", 8, 8)
# par(mfrow=c(2,2))

pvals <- c(0, 0, 0, 0)

for (grp in 1:4) {
    grpdata <- nonadata[nonadata$grp==grp,]
    outcome <- extract.survival(grpdata)
    cat(paste(grp, "\n", sep=""))
    cox.all <- coxph(
    outcome ~ I(log(age)^2) + log(age) * I((1-rxbp) * log(sysbp)) + log(age) * dm +
        log(age) * cursmoke + log(age) * log(totchol) +
        log(age) * log(hdl) + log(age) * I(rxbp * log(sysbp)) + 0, data=grpdata)
    zph.test = cox.zph(cox.all, transform="log")
    pvals[grp] <- signif(zph.test$table["GLOBAL", "p"], digits=2)

    svg(paste("results/20170804/prop_hazards_", grp, ".svg", sep=""), height=8, width=8)
    plot(zph.test)
    title(paste("Test for proportionality of hazards among",
		group_to_description(grp)))
}
write.table(data.frame(Subgroup=c("Black women", "White women", "Black men", "White men"),
		       "ZPH P-value for global test"=pvals), row.names=F, col.names=T, sep="\t", quote=F)
