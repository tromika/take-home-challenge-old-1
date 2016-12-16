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


# Negative and zero purchases could be an error
# The negative sales amount could be return or something else
# These are outliers so make sence to take it out
# TODO - raise Q 
cleaned <- import %>% filter(quantity>0)

ggplot(cleand, aes(x=purchase_date, y=sales_amount)) + 
  geom_point(aes(colour = sales_amount))

ggplot(cleaned, aes(x=purchase_date, y=sales_amount)) + 
  geom_point(aes(colour = product_id))


ggplot(cleaned %>% group_by(purchase_date) %>% summarise(sales_amount=sum(sales_amount, na.rm = T)), aes(x=purchase_date, y=sales_amount)) + 
  geom_line()

ggplot(cleaned %>% group_by(purchase_date) %>% summarise(quantity=sum(quantity, na.rm = T)), aes(x=purchase_date, y=quantity)) + 
  geom_line()


ts <- msts(cleaned[,sum(quantity), by="purchase_date"]$V1, seasonal.periods=c(7,365.25), ts.frequency=7, start=c(2,3))
plot(decompose(ts))

ts <- msts(cleaned[,sum(sales_amount), by="purchase_date"]$V1, seasonal.periods=c(7,365.25), ts.frequency=7, start=c(2,3))
plot(decompose(ts))

cleaned %>% 
  group_by(contact_id) %>%
  summarise(sumSpend=sum(sales_amount, na.rm = T)) %>%
  ungroup() %>%
  summarise(CLV=mean(sumSpend))
