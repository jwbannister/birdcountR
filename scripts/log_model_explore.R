
library(ggplot2)
library(dplyr)

df1 <- readxl::read_excel("~/Desktop/DWB_JBannister.xlsx")
names(df1) <- c("dca", "season", "shw", "hva", "count")

p_linear <- ggplot(df1, aes(x=hva, y=count)) +
  geom_point() +
  geom_smooth(method="lm")
p_log <- ggplot(df1, aes(x=log(hva+1), y=log(count+1))) +
  geom_point() +
  geom_smooth(method="lm")
linear_mod <- lm(count~hva, df1)
log_mod <- lm(log(count+1)~log(hva+1), df1)
pred <- exp(predict.lm(log_mod, newdata=select(df1, hva), interval="prediction"))
conf <- exp(predict.lm(log_mod, newdata=select(df1, hva), interval="confidence"))
df2 <- data.frame(hva=df1$hva, count=conf[, 'fit'], ci.lwr=conf[, 'lwr'], 
                  ci.upr=conf[, 'upr'], pi.lwr=pred[, 'lwr'], 
                  pi.upr=pred[, 'upr'])

p1 <- df2 %>% arrange(hva) %>%
  ggplot(aes(x=hva, y=count)) +
  geom_point() +
  geom_smooth(aes(ymin=ci.lwr, ymax=ci.upr), stat="identity", fill='red') +
  geom_smooth(aes(ymin=pi.lwr, ymax=pi.upr), stat="identity") 
