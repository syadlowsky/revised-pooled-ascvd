library(ggplot2)
library(config)
library(ggsci)

ggplot.calibration_curve <- function(calibration.object, name="", col="black", xlim=1, ylim=1, guide=T,
                                     plot.obj=NULL) {
    if (!is.null(plot.obj)) {
        p <- plot.obj
    } else {
        p <- ggplot()
    }
    if (guide) {
        p <- p +
            geom_line(data=data.frame(x=c(0, pmin(xlim, ylim)), y=c(0, pmin(xlim, ylim))),
                      linetype=2,
                      mapping=aes(x, y),
                      color="gray")
    }
    plot.data <- data.frame(x=calibration.object$expected.risk,
                            y=calibration.object$observed.risk, name=name)
    p <- p +
        geom_point(data=plot.data,
                   mapping=aes(x=x, y=y, colour=name, shape=name),
                   size=conf$point_size) +
        ylim(0, ylim) + xlim(0, xlim) +
        ylab("Observed KM 10-yr ASCVD incidence rate") +
        xlab("Expected 10-yr ASCVD incidence rate")
    return(p)
}
