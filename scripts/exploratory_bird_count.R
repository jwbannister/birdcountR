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

# areas that have a 0 average for bird counts in October can be considered to have
# no viable bird habitat. Remove these areas from the analysis.
dwb_df <- filter(dwb_df, oct.ave>0)

dwb_df <- select(dwb_df, -(24:37))

dwb_df <- melt(dwb_df, id=c("dca", "type"))
dwb_df$year <- substring(dwb_df$variable, 5)
dwb_df$variable <- substring(dwb_df$variable, 1, 3)
names(dwb_df)[3] <- "month"
names(dwb_df)[4] <- "count"

nrow(filter(dwb_df, month=="oct"))

filter(dwb_df, month=="oct") %>%
  ggplot(aes(x=count)) +
  geom_density()

filter(dwb_df, month=="oct") %>%
  ggplot(aes(x=month, y=count)) +
  geom_boxplot()

max(filter(dwb_df, month=="oct")$count)
min(filter(dwb_df, month=="oct")$count)

dens_plot <- filter(dwb_df, month=="oct", count<1500) %>%
  ggplot(aes(x=count)) +
  geom_density() +
  ylab("prob. density") + xlab("bird count") +
  ggtitle("Probability Density of October Bird Counts")
# makePNG(dens_plot, "oct_dense_plot")

hist_plot <- filter(dwb_df, month=="oct", count<1500) %>%
  ggplot(aes(x=count)) +
  geom_histogram(aes(fill=type)) +
  ylab("occurences") + xlab("bird count") +
  ggtitle("Histogram of October Bird Counts")
# makePNG(hist_plot, "oct_hist_plot")

dens2_plot <- filter(dwb_df, month=="oct", count<1500) %>%
  ggplot(aes(x=count)) +
  geom_density() +
  ylab("prob. density") + xlab("bird count") +
  ggtitle("Probability Density of October Bird Counts") +
  geom_point(aes(x=count, y=0, color="observation")) +
  theme(legend.title=element_blank())
# makePNG(dens2_plot, "oct_dense_plot_with_obs")

lines <- data.frame(a = c("mean", "25% quantile", "median", "75% quantile"), b = c(mean(filter(dwb_df, month=="oct")$count),
                    quantile(filter(dwb_df, month=="oct")$count, prob=0.25),
                    quantile(filter(dwb_df, month=="oct")$count, prob=0.5),
                    quantile(filter(dwb_df, month=="oct")$count, prob=0.75)))

dens3_plot <- filter(dwb_df, month=="oct", count<1500) %>%
  ggplot(aes(x=count)) +
  geom_density() +
  ylab("prob. density") + xlab("birds observed") +
  ggtitle("Probability Density of October Bird Counts") +
  geom_vline(data=lines, mapping=aes(xintercept=b, color=a), show_guide=TRUE) + 
  scale_x_discrete(breaks = c(100, 500, 1000, 1400), labels = c("low count", "", "", "high count")) +
  theme(legend.title=element_blank())
# makePNG(dens3_plot, "oct_dense_plot_with_quantiles", ht=4)

ecdf(filter(dwb_df, month=="oct")$count)(mean(filter(dwb_df, month=="oct")$count))

dens <- density(filter(dwb_df, month=="oct")$count, kernel="gaussian", n=10000)
dens <- data.frame(x = dens$x, y= dens$y)
dens <- filter(dens, x<1500 & x > 0)

dens4_plot <- ggplot(dens, aes(x=x, y=y)) +
  geom_line() +
  ylab("prob. density") + xlab("birds observed") +
  ggtitle("Probability Density of October Bird Counts") +
  geom_vline(data=filter(lines, a=="mean"), mapping=aes(xintercept=b, color=a), show_guide=TRUE) + 
  scale_x_discrete(breaks = c(100, 500, 1000, 1400), labels = c("low count", "", "", "high count")) +
  theme(legend.title=element_blank()) +
  geom_ribbon(data=filter(dens, x<filter(lines, a=="mean")$b), aes(x=x, ymax=y), ymin=0, fill="blue") +
  annotate("text", x=900, y=.002, label="~80% of individual sample results will fall below", size=3) +
  annotate("text", x=900, y=.00175, label="the arithmetic mean of the sample distribution", size=3)
# makePNG(dens4_plot, "oct_dense_plot_below_mean", ht=4)

filter(dwb_df, month=="oct", count<1500) %>%
  ggplot(aes(x=count)) +
  stat_ecdf()

set.seed(1)
gam_data <- data.frame(x=rgamma(200, shape=.37, rate=1)*400)
pois_data <- data.frame(x=rpois(200, 5))

filter(dwb_df, month=="oct") %>%
ggplot(aes(x=count)) +
  geom_density() +
  geom_density(data=gam_data, aes(x=x), color="red") +
  geom_density(data=pois_data, aes(x=exp(x)), color="blue") +
  xlim(0,1500)

filter(dwb_df, month=="oct", count<1500) %>%
  ggplot(aes(x=log(count))) +
  geom_density() +
  geom_density(data=pois_data, aes(x=x), color="red") +
  geom_density(data=gam_data, aes(x=log(x)), color="blue")

