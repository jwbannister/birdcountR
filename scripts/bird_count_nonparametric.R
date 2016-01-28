library(readxl)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)

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

max(filter(dwb_melt, month=="oct")$count)
min(filter(dwb_melt, month=="oct")$count)

bad_habitats <- group_by(dwb_melt, dca) %>% filter(month=="oct") %>% 
  summarize(mean=mean(count)) %>% filter(mean==0)                      
sample_data <- filter(dwb_melt, !(dca %in% bad_habitats$dca), month=="oct")
max_summary <- group_by(sample_data, dca) %>% 
  summarize(max.count=max(count), max.nonpar.quantile = ecdf(sample_data$count)(max(count)))

ggplot(sample_data, aes(x=count)) +
  geom_density() +
  xlim(0, 1000)

ggplot(sample_data, aes(x=dca, y=count)) +
  geom_boxplot()

median_max_percentile <- median(max_summary$max.nonpar.quantile)
median_max <- median(max_summary$max.count)
quantile(sample_data$count, probs=mean_max_percentile)

dens <- density(sample_data$count, kernel="gaussian", n=10000)
dens <- data.frame(x=dens$x, y=dens$y)
dens <- filter(dens, x>0 & x<1000)

plot_points2 <- filter(dens, x>103 & x<105)
exampleDCAplot <- ggplot(dens, aes(x=x, y=y)) +
  geom_path() +
  geom_ribbon(data=filter(dens, x>=plot_points2$x), aes(x=x, ymax=y), ymin=0, fill="blue", alpha=0.5) +
  geom_segment(aes(x=plot_points2$x, xend=plot_points2$x, y=0, yend=plot_points2$y), color="red") +
  ylab("probability density") + xlab("bird count") +
  ggtitle("Nonparametric Bird Count Distribution \n Single DCA Example") +
  annotate(geom="text", x=250, y=0.0035, label="maximum number of birds observed = \n    most birds observed on DCA on sampled days  \n    in October (2012, 2013, 2014)", hjust=0, color="red") +
  annotate(geom="text", x=250, y=0.0020, label="area under density curve to the right = \n    probability of more birds than max. observed in \n    DCA on random (unobserved) day in October", hjust=0, color="blue") +
  annotate(geom="text", x=250, y=0.005, label="kernel density estimate based on all available bird \ncounts for all (similar) DCAs", hjust=0) + 
  scale_x_discrete(breaks = c(50, 250, 500, 750, 950), labels = c("low count", "", "", "", "high count")) +
  geom_segment(aes(x=250, y=.0035, xend=plot_points2$x+10, yend=plot_points2$y+.0001), arrow=arrow(angle=15, length=unit(.25, "inches")), color="red", size=.3) +
  geom_segment(aes(x=250, y=.0020, xend=200, yend=.001), arrow=arrow(angle=15, length=unit(.25, "inches")), color="blue", size=.3) +
  geom_segment(aes(x=230, y=.005, xend=70, yend=.0045), arrow=arrow(angle=15, length=unit(.25, "inches")), size=.3)
makePNG(exampleDCAplot, "example", wt=9)

plot_points <- filter(dens, x>median_max-1 & x<median_max+1)
medianDCAplot <- ggplot(dens, aes(x=x, y=y)) +
  geom_path() +
  geom_ribbon(data=filter(dens, x>=median_max), aes(x=x, ymax=y), ymin=0, fill="blue", alpha=0.5) +
  geom_segment(aes(x=plot_points$x, xend=plot_points$x, y=0, yend=plot_points$y), color="red") +
  ylab("probability density") + xlab("bird count") +
  ggtitle("Shallow Flood, Normal Operation, Bird-Habitat DCAs \n Median Results") +
  annotate(geom="text", x=270, y=0.0035, label="median maximum bird count observed = 89", hjust=0, color="red") +
  annotate(geom="text", x=270, y=0.002, label="median probability of more birds \nthan maximum count = 34%", hjust=0, color="blue")  +
  geom_segment(aes(x=250, y=.0035, xend=plot_points$x+10, yend=plot_points$y+.0001), arrow=arrow(angle=15, length=unit(.25, "inches")), color="red", size=.3) +
  geom_segment(aes(x=250, y=.0020, xend=200, yend=.001), arrow=arrow(angle=15, length=unit(.25, "inches")), color="blue", size=.3)
makePNG(medianDCAplot, "median", wt=9)

plotDCA <- function(area){
  quant <- ecdf(sample_data$count)(filter(max_summary, dca==area)$max.count)
  max <- filter(max_summary, dca==area)$max.count
  points <- filter(dens, x>max-1 & x<max+1)
  xp <- mean(points$x)
  yp <- mean(points$y)
  
  ggplot(dens, aes(x=x, y=y)) +
    geom_path() +
    geom_ribbon(data=filter(dens, x>=max), aes(x=x, ymax=y), ymin=0, fill="blue", alpha=0.5) +
    geom_segment(aes(x=xp, xend=xp, y=0, yend=yp), color="red") +
    ylab("probability density") + xlab("bird count") +
    ggtitle(paste0("DCA ", area)) +
    annotate(geom="text", x=270, y=0.0035, label=paste0("maximum bird count observed = ", as.character(round(max, 0))), hjust=0, color="red") +
    annotate(geom="text", x=270, y=0.002, label=paste0("probability of more birds \nthan maximum count = ", as.character(round((1-quant)*100, 0)), "%"), hjust=0, color="blue")
}
plotDCA("T2-2")

for (i in c(1, 2, 4:20)){
area <- max_summary$dca[i]
quant <- ecdf(sample_data$count)(filter(max_summary, dca==area)$max.count)
max <- filter(max_summary, dca==area)$max.count
points <- filter(dens, x>max-1 & x<max+1)
xp <- mean(points$x)
yp <- mean(points$y)

plt <- ggplot(dens, aes(x=x, y=y)) +
  geom_path() +
  geom_ribbon(data=filter(dens, x>=max), aes(x=x, ymax=y), ymin=0, fill="blue", alpha=0.5) +
  geom_segment(aes(x=xp, xend=xp, y=0, yend=yp), color="red") +
  ylab("probability density") + xlab("bird count") +
  ggtitle(paste0("DCA ", area)) +
  annotate(geom="text", x=375, y=0.004, label=paste0("maximum bird count observed = ", as.character(round(max, 0))), hjust=0, color="red") +
  annotate(geom="text", x=375, y=0.0035, label=paste0("probability of more birds than maximum count \non random day in October = ", as.character(round((1-quant)*100, 0)), "%"), hjust=0, color="blue")
makePNG(plt, area, wt=9)
}

# special for T16
area <- "T16"
dens <- density(sample_data$count, kernel="gaussian", n=10000)
dens <- data.frame(x=dens$x, y=dens$y)
dens <- filter(dens, x>0 & x<3000)
quant <- ecdf(sample_data$count)(filter(max_summary, dca==area)$max.count)
max <- filter(max_summary, dca==area)$max.count
points <- filter(dens, x>max-1 & x<max+1)
xp <- mean(points$x)
yp <- mean(points$y)

plt <- ggplot(dens, aes(x=x, y=y)) +
  geom_path() +
  geom_ribbon(data=filter(dens, x>=max), aes(x=x, ymax=y), ymin=0, fill="blue", alpha=0.5) +
  geom_segment(aes(x=xp, xend=xp, y=0, yend=yp), color="red") +
  ylab("probability density") + xlab("bird count") +
  ggtitle(paste0("DCA ", area)) +
  annotate(geom="text", x=375, y=0.004, label=paste0("maximum bird count observed = ", as.character(round(max, 0))), hjust=0, color="red") +
  annotate(geom="text", x=375, y=0.0035, label=paste0("probability of more birds than maximum count \non random day in October = ", as.character(round((1-quant)*100, 0)), "%"), hjust=0, color="blue")
makePNG(plt, area, wt=9)
