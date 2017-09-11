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

    return(list(n.events=n.events, n.up.events=n.up.events,
                n.down.events=n.down.events,
                n.nonevents=n.nonevents, n.up.nonevents=n.up.nonevents,
                n.down.nonevents=n.down.nonevents))
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
