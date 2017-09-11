load.training.data <- function(filename) {
    data <- read.csv(file=filename)
    data$race <- data$grp %% 2
    data$cholratio <- data$totchol / data$hdl
    data$ldl <- data$totchol - data$hdl
    return(data)
}

load.test.data <- function(filename) {
    data <- read.csv(file=filename)
    data$race <- data$grp %% 2
    data$cholratio <- data$totchol / data$hdl
    data$ldl <- data$totchol - data$hdl
    return(data)
}

load.nhanes <- function(filename) {
    nhanesdata = read.table(filename, head=TRUE, sep=",")
    nhanesdata$sysbp <- nhanesdata$sbp
    nhanesdata$bmi <- nhanesdata$bmxbmi
    nhanesdata$cursmoke <- nhanesdata$tob
    nhanesdata$cholmed <- nhanesdata$statin
    nhanesdata$study <- "NHANES"
    nhanesdata$grp <- nhanesdata$cohort
    nhanesdata$race <- nhanesdata$grp %% 2
    nhanesdata$ldl <- nhanesdata$totchol - nhanesdata$hdl

    nhanesdata$cholratio <- nhanesdata$totchol / nhanesdata$hdl

    nhanesdata$weightvar = nhanesdata$weightvar
    nhanesdata = nhanesdata[complete.cases(nhanesdata),]
    nhanesdata <- subset(nhanesdata, 20 < hdl & hdl < 100 &
                                 130 < totchol & totchol < 320 &
                                 90 < sysbp & sysbp < 200 &
                                 39 < age & age <= 79)

    return(nhanesdata)
}
