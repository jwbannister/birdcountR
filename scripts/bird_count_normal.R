library(readxl)
library(reshape)
library(dplyr)
library(ggplot2)

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(paste0("../output/", filename, ".png"), width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}

dwb_df <- data.frame(read_excel("../data/DWB data.xlsx", skip=1))
names(dwb_df) <- tolower(names(dwb_df))
names(dwb_df)[3:9] <- paste0(substring(names(dwb_df)[3:9], 1, 3), ".2012")
names(dwb_df)[10:16] <- paste0(substring(names(dwb_df)[10:16], 1, 3), ".2013")
names(dwb_df)[17:23] <- paste0(substring(names(dwb_df)[17:23], 1, 3), ".2014")
names(dwb_df)[24:30] <- paste0(substring(names(dwb_df)[24:30], 1, 3), ".ave")
names(dwb_df)[31:37] <- paste0(substring(names(dwb_df)[31:37], 1, 3), ".max")

# only consider normal operation DCAs fort this analysis 
# normal operation defined by report provided by J. Nordin (LADWP)
normal_op <- c("T13-1", "T13-2", "T16", "T2-1", "T2-1 Addition", "T2-2", "T24", "T25N",
               "T26", "T27 Addition", "T27N", "T27S", "T29-1", "T29-3", "T30-3", "T36-1E",
               "T36-1W", "T36-2W", "T36-3 Addition", "T36-3E", "T36-3W", "T4-4", 
               "T5-1", "T5-2", "T8W")

dwb_df <- filter(dwb_df, dca %in% normal_op)
dwb_df <- select(dwb_df, -(24:37))

dwb_melt <- melt(dwb_df, id=c("dca", "type"))
dwb_melt$year <- substring(dwb_melt$variable, 5)
dwb_melt$variable <- substring(dwb_melt$variable, 1, 3)
names(dwb_melt)[3] <- "month"
names(dwb_melt)[4] <- "count"

filter(dwb_melt, month=="oct") %>%
  ggplot(aes(x=count)) +
  geom_density()

filter(dwb_melt, month=="oct") %>%
  ggplot(aes(x=dca, y=count)) +
  geom_boxplot()

max(filter(dwb_melt, month=="oct")$count)
min(filter(dwb_melt, month=="oct")$count)

normal_curves <- data.frame(dca=character(0), sample.mean=numeric(0), 
                            sample.max=numeric(0), sample.sd=numeric(0),
                            ci.lower=numeric(0), ci.upper=numeric(0),
                            sample.max.quantile=numeric(0),
                            upper.10=numeric(0))
                          
for (i in unique(dwb_melt$dca)){
  dca <- i
  mean <- mean(filter(dwb_melt, dca==i, month=="oct")$count)
  max <- max(filter(dwb_melt, dca==i, month=="oct")$count)
  sd <- sd(filter(dwb_melt, dca==i, month=="oct")$count)
  ci_lower <- mean + qt(.05, 2) * (sd / sqrt(3))
  ci_upper <- mean + qt(.95, 2) * (sd / sqrt(3))
  quant <- ecdf(rnorm(1000, mean=mean, sd=sd))(max)
  upper <- round(quantile(rnorm(1000, mean=mean, sd=sd), probs=0.90), 0)
  row <- data.frame(dca=dca, sample.mean=mean, sample.max=max, sample.sd=sd, 
                    ci.lower=ci_lower, ci.upper=ci_upper, sample.max.quantile=quant,
                    upper.10=upper)
  normal_curves <- rbind(normal_curves, row)
}
normal_curves <- filter(normal_curves, sample.mean>0)
normal_curves$prcnt.increase <- (normal_curves$upper.10 - normal_curves$sample.max) / normal_curves$sample.max

set.seed(1)
plotDCA <- function(area){
  df <- data.frame(x=rnorm(10000, mean=filter(normal_curves, dca==area)$sample.mean,
                         sd=filter(normal_curves, dca==area)$sample.sd))
  quant <- ecdf(df$x)(filter(normal_curves, dca==area)$sample.max)
  ggplot(df, aes(x=x)) +
    geom_density() +
    geom_vline(xintercept=filter(normal_curves, dca==area)$sample.max, color="red") +
    ggtitle(paste0(area, " (sample max at ", as.character(round(quant*100, 0)), "%-tile)")) +
    xlab("bird count")
}
plotDCA("T16")

df <- data.frame(x=rnorm(10000, mean=1000, sd=500))
mx <- quantile(df$x, probs=c(min(normal_curves$sample.max.quantile), max(normal_curves$sample.max.quantile)))
ggplot(df, aes(x=x)) +
  geom_density() +
  geom_vline(xintercept=mx[1], color="red") +
  geom_vline(xintercept=mx[2], color="red") +
  ggtitle(paste0("Normal Operations DCAs \n (sample max counts between ", names(mx[1]), " and ", names(mx)[2], "-tiles)")) +
  theme(axis.text.x=element_blank()) + xlab("bird count")

mean(normal_curves$sample.max.quantile)

plotDCAUpper <- function(area){
  df <- data.frame(x=rnorm(10000, mean=filter(normal_curves, dca==area)$sample.mean,
                           sd=filter(normal_curves, dca==area)$sample.sd))
  max <- filter(normal_curves, dca==area)$sample.max
  quant <- ecdf(df$x)(max)
  upper <- quantile(df$x, probs=0.9)
  ggplot(df, aes(x=x)) +
    geom_density() +
    geom_vline(xintercept=filter(normal_curves, dca==area)$sample.max, color="red") +
    geom_vline(xintercept=upper, color="blue") +
    ggtitle(paste0(area, " (sample max ", as.character(round(max, 0)), " at ", 
                   as.character(round(quant*100, 0)), "%-tile) \n 10% chance of count > ",
                   as.character(round(upper, 0)))) +
    xlab("bird count")
}
plotDCAUpper("T16")

year_summary <- group_by(sample_data, year) %>% filter(year!="ave" & year!="max") %>% 
  summarize(count=sum(count))
year_mean <- mean(year_summary$count)
year_sd <- sd(year_summary$count)
year_norm <- data.frame(x=rnorm(1000, mean=year_mean, sd=year_sd))
max_quant <- ecdf(year_norm$x)(max(year_summary$count))
ggplot(year_norm, aes(x=x)) +
  geom_density() +
  geom_vline(xintercept=max(year_summary$count), color="red") +
  ggtitle(paste0("Normal Operation DCAs Yearly Sum of Counts \n (max observed (Y2012 = 5302) at ",
          as.character(max_quant*100), "%-tile)")) +
  xlab("total bird count")



set.seed(1)
sample_data <- filter(dwb_melt, month=="oct")
pois_data <- data.frame(x=rpois(75, 4.4))

ggplot(sample_data, aes(sample=log(count))) +
  stat_qq(distribution=qpois, dparams=list(p=c(1:10), lambda=3)) +
  geom_abline(intercept=0, slope=1)

max_summary <- group_by(sample_data, dca) %>% filter(count>0) %>%
  summarize(max.count=max(count), log.max.count=round(log(max(count)), 1))
dca_summary <- group_by(sample_data, dca) %>% summarize(avg.count=mean(count))
bad_habitat <- filter(dca_summary, avg.count<3)$dca
sample_data <- filter(sample_data, !(dca %in% bad_habitat))

quantile(dpois(seq(0, 8, 1), 4.4), probs=seq(0, 1, .01))

ggplot(sample_data, aes(x=log(count))) +
  geom_density() + 
  geom_density(data=pois_data, aes(x=x), color="blue") +
  xlim(0, 10) +
  geom_point(data=max_summary, aes(x=log.max.count, y=0, color="log(max count)"), size=5) +
  scale_color_manual(values="black")

ggplot(sample_data, aes(x=log(count))) +
  stat_ecdf() + 
  stat_ecdf(data=pois_data, aes(x=x), color="blue") +
  xlim(0, 10) +
  geom_point(data=max_summary, aes(x=log.max.count, y=0, color="log(max count)"), size=5) +
  scale_color_manual(values="black") + 
  theme(legend.title=element_blank(), legend.position=c(0.85, 0.25))

# CI for poisson man for single observations
for (i in 1:nrow(max_summary)){
  x <- max_summary$log.max.count[i]
  lower <- 0.5 * qchisq(0.5, 2 * x)
  upper <- 0.5 * qchisq(0.95, 2 * (x + 1))
  max_summary$ci.lower[i] <- lower
  max_summary$ci.upper[i] <- upper
}