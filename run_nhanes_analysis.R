library(caret)
library(config)
library(glmnet)
library(ggsci)

library(svglite)

library(grid)
library(ggplot2)
library(gridExtra)

# Maybe don't need all of these...
library(parallel)
library(doParallel)
library(survival)
library(pROC)
library(PredictABEL)

source("utils.R")

source("data_loaders.R")

source("nhanes_analysis.R")
source("calculate_stats.R")
source("reclassification_report.R")
source("validation_results.R")
source("expected_over_observed_ratio.R")

source("plots.R")

source("subgroup_model.R")
source("baseline_model.R")
source("logistic_model.R")
source("cox_model.R")

nhanes.data <- load.nhanes(paste(conf$working_dir, conf$nhanes_csv, sep="/"))

load(file=conf$full_models_file)

print(logistic.2.eq)

user <- data.frame(age=67, race=1, sysbp=134, dm=0, cursmoke=0, totchol=165, hdl=58, cholratio=165/58, rxbp=1, grp=1)
p <- predict(logistic.2.eq, user, recalibrate=T)
predict(logistic.2.eq, user, recalibrate=T)

p <- ggplot()

p <- nhanes.ratio.original(nhanes.data,
                      "Original AHA/ACC equations",
file=paste(conf$working_dir, "nhanes_analysis_original.svg", sep="/"),
kde_plot=p)

p <- nhanes.ratio(nhanes.data,
             baseline,
             "1 - new data",
file=paste(conf$working_dir, "nhanes_analysis_baseline.svg", sep="/"),
kde_plot=p)

p <- nhanes.ratio(nhanes.data,
             logistic.2.eq,
             "2 - new data + method",
file=paste(conf$working_dir, "nhanes_analysis_proposed.svg", sep="/"),
kde_plot=p
)

p <- p + 
   geom_vline(xintercept=0.7, linetype = "longdash") +
   geom_vline(xintercept=2.5, linetype = "longdash") +
   annotate("text", x=0.65, y=1.5, size=conf$inset_title_size * 0.8 / ggplot2:::.pt, label = "0.7", angle=90) +
   annotate("text", x=0.928571*2.5, y=1.5, size=conf$inset_title_size * 0.8 / ggplot2:::.pt, label = "2.5", angle=90) +
   labs(title="Effect of Black race on 10-year estimated risk", x="Risk if Black / Risk if White", y="Density") +
        scale_color_jama(name="Model Set") +
        scale_fill_jama(name="Model Set") +
        guides(colour = guide_legend(reverse=T), fill=guide_legend(reverse=T)) +
        scale_x_log10(breaks=c(0.25, 0.5, 1, 2, 4), labels=c("0.25", "0.5", "1", "2", "4")) +
        #scale_shape_discrete(name="Model Set") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size=conf$title_size),
              axis.title.y = element_text(size=conf$title_size),
              axis.title.x = element_text(size=conf$title_size),
              legend.title = element_text(size=conf$inset_title_size),
              legend.position = c(0.29, 0.81),
              legend.text = element_text(size=conf$inset_title_size * 0.8))

ggsave(filename=paste(conf$working_dir, "nhanes_kde.svg", sep="/"), plot=p, height=4, width=4)

nhanes.ratio.black.original(nhanes.data,
                      "Original AHA/ACC equations")

nhanes.ratio.black(nhanes.data,
             baseline,
             "1 - new data")

nhanes.ratio.black(nhanes.data,
                   logistic.2.eq,
                   "2 - new data + method",
		   file=paste(conf$working_dir, "nhanes_analysis_black.svg", sep="/"))

for (grp in 1:4) {
    nhanes.data[nhanes.data$grp==grp, "risk.pce"] <- as.vector(unlist(original.model(nhanes.data[nhanes.data$grp==grp, ], grp)))
}
nhanes.data[,"risk.proposed"] <- unlist(predict(logistic.2.eq, nhanes.data, recalibrate=T))
nhanes.data <- as.data.frame(nhanes.data)

library(dplyr)
race.crosses <- nhanes.data %>% dplyr::mutate(cross.threshold.down = as.numeric(risk.pce >= 0.075 & risk.proposed < 0.075),
                                             cross.threshold.up = as.numeric(risk.pce < 0.075 & risk.proposed >= 0.075))
print(head(race.crosses))
race.crosses <- race.crosses %>%
                     dplyr::group_by(race) %>% dplyr::summarize(cross.threshold.up = sum(weightvar * cross.threshold.up) / 1e6,
                                 cross.threshold.down = sum(weightvar * cross.threshold.down) / 1e6)
print(race.crosses)
c <- race.crosses %>% dplyr::ungroup() %>% dplyr::summarize(cross.threshold.up = sum(cross.threshold.up),
                                               cross.threshold.down = sum(cross.threshold.down))
print(c)

cat("\n10%\n")
race.crosses <- nhanes.data %>% dplyr::mutate(cross.threshold.down = as.numeric(risk.pce >= 0.1 & risk.proposed < 0.1),
                                             cross.threshold.up = as.numeric(risk.pce < 0.1 & risk.proposed >= 0.1)) %>%
                     dplyr::group_by(race) %>% dplyr::summarize(cross.threshold.up = sum(weightvar * cross.threshold.up) / 1e6,
                                 cross.threshold.down = sum(weightvar * cross.threshold.down) / 1e6)
print(race.crosses)
c <- race.crosses %>% dplyr::ungroup() %>% dplyr::summarize(cross.threshold.up = sum(cross.threshold.up),
                                               cross.threshold.down = sum(cross.threshold.down))
print(c)
