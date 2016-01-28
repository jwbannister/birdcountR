dwb_df <- data.frame(read_excel("../data/DWB data.xlsx", skip=1))
names(dwb_df) <- tolower(names(dwb_df))
names(dwb_df)[3:9] <- paste0(substring(names(dwb_df)[3:9], 1, 3), ".2012")
names(dwb_df)[10:16] <- paste0(substring(names(dwb_df)[10:16], 1, 3), ".2013")
names(dwb_df)[17:23] <- paste0(substring(names(dwb_df)[17:23], 1, 3), ".2014")
names(dwb_df)[24:30] <- paste0(substring(names(dwb_df)[24:30], 1, 3), ".ave")
names(dwb_df)[31:37] <- paste0(substring(names(dwb_df)[31:37], 1, 3), ".max")

dwb_df <- select(dwb_df, -(24:37))

dwb_melt <- melt(dwb_df, id=c("dca", "type"))
dwb_melt$year <- substring(dwb_melt$variable, 5)
dwb_melt$variable <- substring(dwb_melt$variable, 1, 3)
names(dwb_melt)[3] <- "month"
names(dwb_melt)[4] <- "count"

normal_op <- c("T13-1", "T13-2", "T16", "T2-1", "T2-1 Addition", "T2-2", "T24", "T25N",
               "T26", "T27 Addition", "T27N", "T27S", "T29-1", "T29-3", "T30-3", "T36-1E",
               "T36-1W", "T36-2W", "T36-3 Addition", "T36-3E", "T36-3W", "T4-4", 
               "T5-1", "T5-2", "T8W")
for (i in 1:nrow(dwb_melt)){
  if (dwb_melt$dca[i] %in% normal_op){
    dwb_melt$type[i] <- "normal"
  } else{
    dwb_melt$type[i] <-"non-normal"
  }
}
dwb_melt <- filter(dwb_melt, month=="oct")

normal_summary <- group_by(dwb_melt, dca) %>% filter(type=="normal") %>%
  summarize(avg.count=mean(count))
bad_habitats <- filter(normal_summary, avg.count<3)$dca
sample_data <- filter(dwb_melt, !(dca %in% bad_habitats), type=="normal")
sample_summary <- group_by(sample_data, dca) %>% filter(type=="normal") %>%
  summarize(avg.count=mean(count), max.count=max(count), min.count=min(count))

ggplot(sample_data, aes(x=dca, y=count)) +
  geom_point()

model <- glm(count~dca, data=sample_data, family=poisson)
model_results <- data.frame(dca=rownames(summary(model)$coefficients), 
                            beta=summary(model)$coefficients[, 1],
                            std.error=summary(model)$coefficients[, 2])
rownames(model_results) <- seq(1, nrow(model_results), 1)
model_results$dca <- as.character(model_results$dca)
model_results$dca[2:nrow(model_results)] <- substring(model_results$dca[2:nrow(model_results)], 4)
base_row <- data.frame(dca="T13-1", beta=0, std.error=mean(model_results$std.error))
model_results <- rbind(model_results, base_row)

# confidence bounds calculated to 90%, +- 1.645 SE
model_results$beta.lower <- model_results$beta - 1.645 * model_results$std.error
model_results$beta.upper <- model_results$beta + 1.645 * model_results$std.error
model_results$count.lower <- exp(filter(model_results, dca=="(Intercept)")$beta.lower + model_results$beta.lower)
model_results$count.upper <- exp(filter(model_results, dca=="(Intercept)")$beta.upper + model_results$beta.upper)

summary(model)$coefficients

  
