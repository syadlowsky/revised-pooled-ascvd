specify.decimal <- function(t, nsmall=3, zero=T) {
    if (round(t, nsmall) == 0 & !zero) {
        return(t)
    }
    return(format(round(t, nsmall), nsmall=nsmall))
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

log.if.not.null <- function(x) {
  if (is.null(x)) {
    return(x)
  } else {
    return(log(x))
  }
}
