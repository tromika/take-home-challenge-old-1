setwd("../emarsys")

source("R/init.R")

# Base stats ------

summary(import)

rapply(import,function(x) length(unique(x)))

import %>% summarise(min(purchase_date), 
                     max(purchase_date))

# Check some aspects

import %>% 
  group_by(contact_id) %>%
  summarise(cnt=n()) %>%
  arrange(desc(cnt))

hist(import$sales_amount, breaks = 100)
hist(import[sales_amount<quantile(import$sales_amount)[4]]$sales_amount)

View(import %>% 
  group_by(quantity) %>%
  summarise(cnt=n()) %>%
  arrange(desc(quantity)))


# Negative and zero purchases could be an error. You find more explaination in the Questions.md

chisq.out.test(import$sales_amount,variance = var(import$sales_amount),opposite = FALSE)

# as a result of chisq.test highest value 10997.5 is an outlier but visually make sense to set it to 3000. 
# There are more artificial solutions for this but it makes sense.

cleaned <- import %>% filter(quantity>0 & sales_amount < 3000 )

ggplot(cleaned, aes(x=purchase_date, y=sales_amount)) + 
  geom_point(aes(colour = sales_amount))

ggplot(cleaned, aes(x=purchase_date, y=sales_amount)) + 
  geom_point(aes(colour = product_id))


ggplot(cleaned %>% group_by(purchase_date) %>% summarise(sales_amount=sum(sales_amount, na.rm = T)), aes(x=purchase_date, y=sales_amount)) + 
  geom_line()

ggplot(cleaned %>% group_by(purchase_date) %>% summarise(quantity=sum(quantity, na.rm = T)), aes(x=purchase_date, y=quantity)) + 
  geom_line()


ts.q <- msts(cleaned[,sum(quantity), by="purchase_date"]$V1, seasonal.periods=c(7,365.25), ts.frequency=7, start=c(2,3))
plot(decompose(ts.q))

ts.s <- msts(cleaned[,sum(sales_amount), by="purchase_date"]$V1, seasonal.periods=c(7,365.25), ts.frequency=7, start=c(2,3))
plot(decompose(ts.s))

fit <- tbats(ts.q)

fc <- forecast(fit, h = 365)

stage <- cleaned
stage[,quantity:=sum(quantity,na.rm = T), by="purchase_date"]

ts.fc <- data.frame(purchase_date = seq(max(cleaned$purchase_date)+days(1) , length.out=length(fc$mean),by = 'day'))
ts.fc$purchase_date <- ymd(ts.fc$purchase_date)
ts.fc$pred <- fc$mean
ts.fc$low95 <- fc$upper[,2]
ts.fc$low80 <- fc$upper[,1]
ts.fc$high95 <- fc$lower[,2]
ts.fc$high80 <- fc$lower[,1]
ts.fc.out <- bind_rows(stage, ts.fc)
ts.fc.out$quantity <- ifelse(is.na(ts.fc.out$quantity),ts.fc.out$pred,ts.fc.out$quantity)
ts.fc.out$pred <- NULL
stage <- NULL

ggplot(melt(select(ts.fc.out, purchase_date, quantity, low80, low95, high80, high95), id.vars= c('purchase_date'), variable.name='forecast',value.name = "quantity" ), aes( purchase_date, quantity,group=forecast)) +   
  geom_line(aes(color=forecast)) +
  ggtitle("Forecasted quantity") +
  labs(x="date") +
  scale_colour_manual(name="Forecasts", values = c("quantity" = "#74BD56", "high95" = "#FF5600","low95" = "#FF5600", "high80" = "#123A4D", "low80" = "#123A4D"))


cleaned %>% 
  group_by(contact_id) %>%
  summarise(sumSpend=sum(sales_amount, na.rm = T)) %>%
  ungroup() %>%
  summarise(meanAmount=mean(sumSpend))


