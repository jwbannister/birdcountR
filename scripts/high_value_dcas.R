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

high_value_dcas <- c("T13-2", "T13-1", "T17-2", "T16", "T18N", "T1A-2", "T23SW",
                     "T23SE", "T25N", "T25S", "T30-2", "T5-3 Addition", "T18S")
for (i in 1:nrow(dwb_df)){
  if (dwb_df$dca[i] %in% high_value_dcas){
    dwb_df$high.value[i] <- TRUE
  } else{
    dwb_df$high.value[i] <- FALSE
  }
}

dwb_df <- filter(dwb_df, oct.ave>0)
dwb_df <- select(dwb_df, -(24:37))

dwb_melt <- melt(dwb_df, id=c("dca", "type", "high.value"))
dwb_melt$year <- substring(dwb_melt$variable, 5)
dwb_melt$variable <- substring(dwb_melt$variable, 1, 3)
names(dwb_melt)[4] <- "month"
names(dwb_melt)[5] <- "count"

dwb_melt <- filter(dwb_melt, month=="oct")

ggplot(dwb_melt, aes(x=dca, y=count)) +
  geom_boxplot(aes(fill=high.value))

ggplot(dwb_melt, aes(x=count)) +
  geom_density(aes(color=high.value)) +
  xlim(0, 2000)

shape <- 2
scale <- 140
gam_dens <- data.frame(x=c(0:6000), y=dgamma(0:6000, shape=shape, scale=scale))

mean <- 200
sd <- 150
norm_dens <- data.frame(x=c(0:6000), y=dnorm(0:6000, mean=mean, sd=sd))

mean(filter(dwb_melt, year==2012, count>0)$count)
prod(filter(dwb_melt, year==2012, count>0)$count)^(1/length(filter(dwb_melt, year==2012)$count))

filter(dwb_melt, high.value==TRUE) %>%
  ggplot(aes(x=count)) +
  geom_density() + 
  xlim(0, 6000) +
  ggtitle("Bird Count Distribution for High Habitat Value DCAs")

filter(dwb_melt, high.value==TRUE) %>%
  ggplot(aes(x=count)) +
  stat_ecdf() + 
  ggtitle("Bird Count Cumulative Distribution for High Habitat Value DCAs")

low <- quantile(filter(dwb_melt, high.value==TRUE)$count, prob=.05)
up <- quantile(filter(dwb_melt, high.value==TRUE)$count, prob=.95)
up2 <- quantile(filter(dwb_melt, high.value==TRUE)$count, prob=.75)

dens <- density(filter(dwb_melt, high.value==TRUE)$count, kernel="gaussian", n=10000)
dens <- data.frame(x = dens$x, y= dens$y)

ggplot(dens, aes(x=x, y=y)) +
  geom_line() +
  ylab("prob. density") + xlab("birds observed") +
  ggtitle("High Habitat Value DCAs") +
  geom_ribbon(data=filter(dens, x>low & x<up), aes(x=x, ymax=y), ymin=0, fill="blue") +
  annotate("text", x=1800, y=.0006, label="90% of observations", size=8, hjust=0) +
  annotate("text", x=1800, y=.0005, label="between 13 and 3448", size=8, hjust=0) +
  xlim(0, 6000)

ggplot(dens, aes(x=x, y=y)) +
  geom_line() +
  ylab("prob. density") + xlab("birds observed") +
  ggtitle("High Habitat Value DCAs") +
  geom_ribbon(data=filter(dens, x>low & x<up2), aes(x=x, ymax=y), ymin=0, fill="blue") +
  annotate("text", x=2000, y=.0006, label="70% of observations", size=8, hjust=0) +
  annotate("text", x=2000, y=.0005, label="between 13 and 887", size=8, hjust=0) +
  xlim(0, 6000)

ggplot(dens, aes(x=x, y=y)) +
  geom_line() +
  ylab("prob. density") + xlab("birds observed") +
  ggtitle("High Habitat Value DCAs") +
  geom_ribbon(data=filter(dens, x>up2 & x<up), aes(x=x, ymax=y), ymin=0, fill="blue") +
  annotate("text", x=2000, y=.0006, label="20% of observations", size=8, hjust=0) +
  annotate("text", x=2000, y=.0005, label="between 887 and 3448", size=8, hjust=0) +
  xlim(0, 6000)


ggplot(gam_dens, aes(x=log(x), y=y)) +
  geom_path()

dist_mean <- shape * scale
dist_sd <- sqrt(shape * scale^2)

filter(dwb_melt, high.value==TRUE) %>%
  ggplot(aes(x=dca, y=count)) +
  geom_point(aes(color=year))

ggplot(dwb_melt, aes(x=count)) +
  stat_ecdf(aes(color=high.value))

total_summary <- group_by(dwb_melt, year) %>% 
  summarize(n=length(year), mean=mean(count), sum=sum(count))

dca_summary <- group_by(dwb_melt, dca) %>% 
  summarize(mean=mean(count), sd=sd(count), max=max(count), high.value=all(high.value)) %>%
  arrange(desc(max)) %>% mutate(ci.95.lower = mean - 1.96 * sd, ci.95.upper = mean + 1.96 * sd)

# # add in additional areas to "high value"
# add_high_value <- c("T2-4", "T13-3", "T23-5", "T4-4")
# for (i in 1:nrow(dwb_melt)){
#   if (dwb_melt$dca[i] %in% add_high_value){
#     dwb_melt$high.value[i] <- TRUE
#   }
# }

high_summary <- filter(dwb_melt, high.value==TRUE) %>% group_by(year) %>% 
  summarize(n=length(year), mean=mean(count), sum=sum(count))

high_mean <- mean(high_summary$sum)
high_sd <- sd(high_summary$sum)
high_ci_low <- high_mean - 1.96 * high_sd
high_ci_high <- high_mean + 1.96 * high_sd

high_dca_summary <- filter(dwb_melt, high.value==TRUE) %>% group_by(dca) %>%
  summarize(max.count=max(count), mean.count=mean(count))

sum(high_dca_summary$max.count)

set.seed(11)
lambda <- 5
pois_data <- data.frame(dca=rep(NA, 200), type=rep(NA, 200), high.value=rep(TRUE, 200), 
                        month=rep("oct", 200), count=rpois(200, lambda), year=rep("model", 200))
dwb_melt_log <- dwb_melt
dwb_melt_log$count <- log(dwb_melt_log$count)
model_melt <- rbind(dwb_melt_log, pois_data)
names(model_melt)[5] <- "log.count"
model_melt <- filter(model_melt, log.count!=-Inf)

mean_df <- data.frame(label = c("2012", "2013", "2014", "model"), means = 
                        c(mean(filter(model_melt, year==2012)$log.count), 
                          mean(filter(model_melt, year==2013)$log.count),
                          mean(filter(model_melt, year==2014)$log.count),
                          mean(filter(model_melt, year=="model")$log.count)))

filter(model_melt, high.value==TRUE) %>%
ggplot(aes(x=log.count)) +
  geom_density(aes(color=year)) +
  geom_vline(data=mean_df, aes(xintercept=means, color=label))

ks.test(filter(model_melt, year=="model")$count, filter(model_melt, year==2012)$count)
ks.test(filter(model_melt, year=="model")$count, filter(model_melt, year==2013)$count)
ks.test(filter(model_melt, year=="model")$count, filter(model_melt, year==2014)$count)

est_log.count <- mean(filter(model_melt, year==2012, high.value==TRUE)$log.count)
term1 <- (est_log.count * (exp(-lambda)/((1 - exp(-lambda))^2)))^2
term2 <- ((est_log.count * (1 - exp(-lambda) - lambda * exp(lambda)))/(lambda * (1 - exp(-lambda))^2))^-1
term3 <- est_log.count * (exp(-lambda)/((1 - exp(-lambda))^2))
var_log.count_est <- term1 * term2 + term3
sd <- sqrt(var_log.count_est)

lower <- exp(est_log.count - 1.96*sd)
upper <- exp(est_log.count + 1.96*sd)
