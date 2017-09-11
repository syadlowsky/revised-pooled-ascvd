library(ggsci)

source("original_model.R")

nhanes.stats.for.gender <- function(white.risk, black.risk, data) {
    weights <- data$weightvar
    total.weight <- sum(weights)

    average.ratio <- weighted.mean(black.risk / white.risk, weights)
    log.odds.var <- wtd.var(black.risk / white.risk, weights)

    min.ratio <- min(black.risk / white.risk)
    max.ratio <- max(black.risk / white.risk)

    diff.treatment = ((black.risk >= 0.075) & (white.risk < 0.075)) |
        ((black.risk < 0.075) & (white.risk >= 0.075))
    very.diff.treatment <- diff.treatment & (((black.risk / white.risk) > 2) |
                                             ((black.risk / white.risk) < 0.5))

    prob.diff.treatment <- weighted.mean(diff.treatment, weights)
    prob.very.diff.treatment <- weighted.mean(very.diff.treatment, weights)

    return(list(total.weight=total.weight,
                average.ratio.of.odds=average.ratio,
                log.odds.var=log.odds.var,
                min.ratio=min.ratio,
                max.ratio=max.ratio,
                prob.diff.treatment=prob.diff.treatment,
                prob.very.diff.treatment=prob.very.diff.treatment))
}

nhanes.report.from.risk <- function(white.f.risk, black.f.risk, white.m.risk,
                                    black.m.risk, description, females.data,
                                    males.data, title) {
    female.nhanes.stats <- nhanes.stats.for.gender(white.f.risk, black.f.risk,
                                                   females.data)
    male.nhanes.stats <- nhanes.stats.for.gender(white.m.risk, black.m.risk,
                                                   males.data)


    nhanes.report = data.frame(male=double(), female=double())

    print.noquote(description)

    male.risk.data <- data.frame(x=black.m.risk, y=white.m.risk, Sex="Men")
    female.risk.data <- data.frame(x=black.f.risk, y=white.f.risk, Sex="Women")
    risk.data <- rbind(male.risk.data, female.risk.data)

    p <- ggplot() + geom_point(data=risk.data,
                          mapping=aes(x=x, y=y, colour=Sex, shape=Sex),
                          size=conf$point_size) +
            geom_line(data=data.frame(x=c(0, 1), y=c(0, 0.5)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
            geom_line(data=data.frame(x=c(0, 0.5), y=c(0, 1)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
            geom_line(data=data.frame(x=c(0, 1), y=c(0.075, 0.075)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
            geom_line(data=data.frame(y=c(0, 1), x=c(0.075, 0.075)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
	    scale_color_jama() +
	    scale_shape_discrete() +
            ylim(0, 1) + xlim(0, 1) +
            xlab("Risk under black model") +
            ylab("Risk under white model") +
            ggtitle(paste("Effect of race on risk predictions for", title, sep="\n")) +
            theme(plot.title = element_text(hjust = 0.5, size=conf$title_size),
                  axis.title.y = element_text(size=conf$title_size),
                  axis.title.x = element_text(size=conf$title_size),
              legend.justification=c(1,0), legend.position=c(0.955,0.045))

        p_inset <- ggplot() + geom_point(data=risk.data,
                          mapping=aes(x=x, y=y, colour=Sex, shape=Sex),
                          size=conf$point_size) +
            geom_line(data=data.frame(x=c(0, 0.15), y=c(0, 0.15*0.5)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
            geom_line(data=data.frame(x=c(0, 0.15/2), y=c(0, 0.15)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
            geom_line(data=data.frame(x=c(0, 0.15), y=c(0.075, 0.075)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
            geom_line(data=data.frame(y=c(0, 0.15), x=c(0.075, 0.075)),
                      linetype=2,
                      mapping=aes(x, y),
                      color="grey28") +
	    scale_color_jama() +
	    scale_shape_discrete() +
            ylim(0, 0.15) + xlim(0, 0.15) +
            ggtitle("Range [0, 0.15] for clarity") +
            theme(
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                text = element_text(size=conf$inset_title_size),
                plot.title = element_text(size=conf$inset_title_size,
                                          hjust=0.9),
                legend.position="none"
            )

    p <- p + annotation_custom(grob=ggplotGrob(p_inset),
                               xmin=0.5,
                               xmax=1.0,
                               ymin=0.5,
                               ymax=1.0)

    print(p)

    nhanes.report["average",] = c(male.nhanes.stats$average.ratio.of.odds,
                                  female.nhanes.stats$average.ratio.of.odds)
    nhanes.report["min",] = c(male.nhanes.stats$min.ratio,
                                  female.nhanes.stats$min.ratio)
    nhanes.report["max",] = c(male.nhanes.stats$max.ratio,
                                  female.nhanes.stats$max.ratio)
    nhanes.report["max",] = c(male.nhanes.stats$max.ratio,
                                  female.nhanes.stats$max.ratio)

    nhanes.report["p(diff recc)",] = c(
        male.nhanes.stats$prob.diff.treatment,
        female.nhanes.stats$prob.diff.treatment)
    nhanes.report["p(very diff recc)",] = c(
        male.nhanes.stats$prob.very.diff.treatment,
        female.nhanes.stats$prob.very.diff.treatment)

    print(nhanes.report)

    return(nhanes.report)
}

nhanes.ratio <- function(dataframe, model, description) {
    title <- description

    females.data = dataframe[dataframe$grp<3,]
    males.data = dataframe[dataframe$grp>2,]

    females.data$grp = 2
    males.data$grp = 4
    females.data$race = 0
    males.data$race = 0
    white.f.risk = predict(model, females.data, recalibrate=T)
    white.m.risk = predict(model, males.data, recalibrate=T)

    females.data$grp = 1
    males.data$grp = 3
    females.data$race = 1
    males.data$race = 1
    black.f.risk = predict(model, females.data, recalibrate=T)
    black.m.risk = predict(model, males.data, recalibrate=T)

    nhanes.report.from.risk(white.f.risk, black.f.risk, white.m.risk, black.m.risk,
                            description, females.data, males.data, title=title)
}

nhanes.ratio.original <- function(dataframe, description) {
    title <- description

    females.data = dataframe[dataframe$grp<3,]
    males.data = dataframe[dataframe$grp>2,]
    white.f.risk = original.model(females.data, 2)
    white.m.risk = original.model(males.data, 4)
    black.f.risk = original.model(females.data, 1)
    black.m.risk = original.model(males.data, 3)

    nhanes.report.from.risk(white.f.risk, black.f.risk, white.m.risk, black.m.risk,
                            description, females.data, males.data, title=title)
}
