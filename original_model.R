original.link <- function(x, group) {
    age = x$age
    totchol = x$totchol
    hdl = x$hdl
    sysbp = x$sysbp
    dm = x$dm
    rxbp = x$rxbp
    cursmoke = x$cursmoke

    vars = matrix(c(log(age),
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
                    dm), ncol=13)

    coefs = list(
        c(17.114, 0, 0.94, 0, -18.920, 4.475, 29.291, -6.432, 27.820, -6.087, 0.691, 0, 0.874),
        c(-29.799, 4.884, 13.54, -3.114, -13.578, 3.149, 2.019, 0, 1.957, 0, 7.574, -1.665, 0.661),
        c(2.469, 0, 0.302, 0, -0.307, 0, 1.916, 0, 1.809, 0, 0.549, 0, 0.645),
        c(12.344, 0, 11.853, -2.664, -7.990, 1.769, 1.797, 0, 1.7864, 0, 7.837, -1.795, 0.658))
    mean.risk = c(86.61, -29.18, 19.54, 61.18)
    individual.risk = unlist(vars %*% coefs[[group]])
    link = individual.risk - mean.risk[group]
    return(link)
}

original.model <- function(x, group, time=10) {
    link = original.link(x, group)
    relative.risk = exp(link)

    if (time != 10) {
        stop("Original model only works at 10 years")
    }

    baseline.survival = c(0.9533, 0.9665, 0.8954, 0.9144)
    risk = 1 - baseline.survival[group] ^ relative.risk
    return(risk)
}

framingham.link <- function(x, group) {
    # group is really only sex in this model
    sex = as.integer(group >= 3) + 1

    age = x$age
    totchol = x$totchol
    hdl = x$hdl
    sysbp = x$sysbp
    dm = x$dm
    rxbp = x$rxbp
    cursmoke = x$cursmoke

    vars = matrix(c(log(age),
                    log(totchol),
                    log(hdl),
                    (1-rxbp)*log(sysbp),
                    rxbp*log(sysbp),
                    cursmoke,
                    dm), ncol=7)

    coefs = list(
        c(2.32888, 1.20904, -0.70833, 2.76157, 2.82263, 0.52873, 0.69154),
        c(3.06117, 1.12370, -0.93263, 1.93303, 1.99881, 0.65451, 0.57367))
    coefs <- coefs[[sex]]
    mean.risk = c(26.1931, 23.9802)
    individual.risk = unlist(vars %*% coefs)
    link = individual.risk - mean.risk[sex]
    return(link)
}

framingham.model <- function(x, group, time=10) {
    link = framingham.link(x, group)
    relative.risk = exp(link)

    if (time != 10) {
        stop("Original model only works at 10 years")
    }

    sex = as.integer(group >= 3) + 1
    baseline.survival = c(0.95012, 0.88936)
    risk = 1 - baseline.survival[sex] ^ relative.risk
    return(risk)
}
