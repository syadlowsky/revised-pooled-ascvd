compute.nri.counts <- function(outcome, original.risk, new.risk, level) {
    original.recc <- original.risk >= level
    new.recc <- new.risk >= level

    reclass.up <- new.recc & (!original.recc)
    reclass.down <- original.recc & (!new.recc)

    n.events <- sum(outcome)
    n.up.events <- sum(reclass.up & outcome)
    n.down.events <- sum(reclass.down & outcome)

    n.nonevents <- sum(!outcome)
    n.up.nonevents <- sum(reclass.up & !outcome)
    n.down.nonevents <- sum(reclass.down & !outcome)

    events.recc <- sum(new.recc & outcome)
    events.not.recc <- sum(!new.recc & outcome)
    nonevents.recc <- sum(new.recc & !outcome)
    nonevents.not.recc <- sum(!new.recc & !outcome)

    return(list(n.events=n.events, n.up.events=n.up.events,
                events.recc=events.recc,
                events.not.recc=events.not.recc,
                n.down.events=n.down.events,
                n.nonevents=n.nonevents, n.up.nonevents=n.up.nonevents,
                n.down.nonevents=n.down.nonevents,
                nonevents.recc=nonevents.recc,
                nonevents.not.recc=nonevents.not.recc))
}

classification.counts <- function(outcome, risk, level) {
    recc <- risk >= level

    events.recc <- sum(recc & outcome)
    events.not.recc <- sum(!recc & outcome)
    nonevents.recc <- sum(recc & !outcome)
    nonevents.not.recc <- sum(!recc & !outcome)
    return(list(events=sum(outcome),
                events.recc=events.recc,
                events.not.recc=events.not.recc,
                nonevents=sum(!outcome),
                nonevents.recc=nonevents.recc,
                nonevents.not.recc=nonevents.not.recc))
}

survival.prob <- function(outcome, time, t=10) {
  km <- survfit(Surv(time, outcome) ~ 1)
  return(min(km$surv[km$time <= t]))
}

classification.counts.km <- function(outcome, time, risk, level, t=10) {
    recc <- risk >= level
    n <- length(outcome)

    events.recc <- sum(recc) * (1 - survival.prob(outcome[recc], time[recc], t))
    events.not.recc <- (n - sum(recc)) * (1 - survival.prob(outcome[!recc], time[!recc], t))
    nonevents.recc <- sum(recc) - events.recc
    nonevents.not.recc <- (n - sum(recc)) - events.not.recc
    return(list(events=events.recc + events.not.recc,
                events.recc=events.recc,
                events.not.recc=events.not.recc,
                nonevents=nonevents.recc + nonevents.not.recc,
                nonevents.recc=nonevents.recc,
                nonevents.not.recc=nonevents.not.recc))
}

nri.at.level <- function(outcome, new.risk, original.risk=NULL, level=0.075, name="untitled") {
    nri.counts <- compute.nri.counts(outcome$outcome, original.risk, new.risk, level)

    event.nri <- with(nri.counts, (n.up.events - n.down.events) / n.events)
    event.nri.var <- with(nri.counts,
                          (n.up.events + n.down.events) / (n.events^2) -
                          (n.up.events - n.down.events)^2 / (n.events^3))

    nonevent.nri <- with(nri.counts,
                         (n.down.nonevents - n.up.nonevents) / n.nonevents)
    nonevent.nri.var <- with(nri.counts,
                             (n.up.nonevents + n.down.nonevents) / (n.nonevents^2) -
                             (n.up.nonevents - n.down.nonevents)^2 / (n.nonevents^3))

    return(list(nri=event.nri + nonevent.nri,
                event.nri=event.nri,
                nonevent.nri=nonevent.nri,
                event.se=sqrt(event.nri.var),
                nonevent.se=sqrt(nonevent.nri.var),
                se=sqrt(event.nri.var + nonevent.nri.var)))
}
