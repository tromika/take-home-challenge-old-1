setwd("../emarsys")

source("R/init.R")

# Base stats ------

summary(import)

rapply(import,function(x) length(unique(x)))

import %>% summarise(min(purchase_date), 
                     max(purchase_date))

quantile(import$sales_amount, c(.85, .90, .99995)) 

quantile(import$quantity, c(.85, .90, .9999)) 

# Check some aspects -------

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

chisq.out.test(import$quantity,variance = var(import$quantity),opposite = FALSE)

# as a result of chisq.test highest value 10997.5 is an outlier but visually make sense to set it to 3000. 
# There are more artificial solutions for this but it makes sense.

cleaned <- import %>% filter(quantity>0 & quantity <= 50  & sales_amount < 3000 )

ggplot(cleaned, aes(x=purchase_date, y=sales_amount)) + 
  geom_point(aes(colour = sales_amount))

ggplot(cleaned, aes(x=purchase_date, y=sales_amount)) + 
  geom_point(aes(colour = product_id))


ggplot(cleaned %>% group_by(purchase_date) %>% summarise(sales_amount=sum(sales_amount, na.rm = T)), aes(x=purchase_date, y=sales_amount)) + 
  geom_line()

ggplot(cleaned %>% group_by(purchase_date) %>% summarise(quantity=sum(quantity, na.rm = T)), aes(x=purchase_date, y=quantity)) + 
  geom_line()




ts.t <- msts(cleaned[,uniqueN(order_id), by="purchase_date"]$V1, seasonal.periods=c(7,365.25), ts.frequency=7, start=c(2,3))
plot(decompose(ts.q))

ts.s <- msts(cleaned[,sum(sales_amount), by="purchase_date"]$V1, seasonal.periods=c(7,365.25), ts.frequency=7, start=c(2,3))
plot(decompose(ts.s))

fit <- tbats(ts.t)

fc <- forecast(fit, h = 365)

stage <- cleaned
stage[,transaction:=uniqueN(order_id), by="purchase_date"]

ts.fc <- data.frame(purchase_date = seq(max(cleaned$purchase_date)+days(1) , length.out=length(fc$mean),by = 'day'))
ts.fc$purchase_date <- ymd(ts.fc$purchase_date)
ts.fc$pred <- fc$mean
ts.fc$low95 <- fc$upper[,2]
ts.fc$low80 <- fc$upper[,1]
ts.fc$high95 <- fc$lower[,2]
ts.fc$high80 <- fc$lower[,1]
ts.fc.out <- bind_rows(stage, ts.fc)
ts.fc.out$transaction <- ifelse(is.na(ts.fc.out$transaction),ts.fc.out$pred,ts.fc.out$transaction)
ts.fc.out$pred <- NULL
stage <- NULL

ggplot(melt(select(ts.fc.out, purchase_date, transaction, low80, low95, high80, high95), id.vars= c('purchase_date'), variable.name='forecast',value.name = "transaction" ), aes( purchase_date, transaction,group=forecast)) +   
  geom_line(aes(color=forecast)) +
  ggtitle("Forecasted quantity") +
  labs(x="date") +
  scale_colour_manual(name="Forecasts", values = c("transaction" = "#74BD56", "high95" = "#FF5600","low95" = "#FF5600", "high80" = "#123A4D", "low80" = "#123A4D"))

ts.t <- NULL
ts.s <- NULL

# User metrics ------

cleaned %>% 
  group_by(contact_id) %>%
  summarise(sumSpend=sum(sales_amount, na.rm = T)) %>%
  ungroup() %>%
  summarise(meanAmount=mean(sumSpend))

cleaned %>% 
  group_by(contact_id, year(purchase_date)) %>%
  summarise(sumSpend=sum(sales_amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(`year(purchase_date)`) %>%
  summarise(meanAmount=mean(sumSpend))

cleaned %>% 
  group_by(contact_id) %>%
  summarise(sumQuantity=sum(quantity, na.rm = T)) %>%
  ungroup() %>%
  summarise(meanQuantitiy=mean(sumQuantity))





# Cohorts --------

firstVisits <- cleaned %>% 
                  group_by(contact_id) %>%
                  summarise(firstVisit=min(purchase_date))

cohorts<- cleaned %>% 
  left_join(firstVisits) %>%
  mutate(isReturning = ifelse(purchase_date>firstVisit, T, F))

cohorts[, cohort:=floor_date(firstVisit, unit = 'month')]
cohorts[, seq:= interval(cohort,floor_date(purchase_date, unit = 'month')) %/% months(1)]

cohorts %>%
  group_by(cohort,seq) %>%
  summarise(cnt = n_distinct(contact_id)) %>%
  arrange(cohort, seq) %>%
  dcast(cohort ~ seq, value.var = 'cnt')

cohorts %>%
  group_by(cohort,seq) %>%
  summarise(cnt = n_distinct(order_id)) %>%
  arrange(cohort, seq) %>%
  dcast(cohort ~ seq, value.var = 'cnt')


new <-cohorts %>% 
  filter(isReturning == FALSE) %>%
  group_by(cohort) %>%
  summarise(new = n_distinct(contact_id)) %>%
  arrange(cohort)

arpu <- cohorts %>%
  group_by(cohort,seq) %>%
  summarise(cnt = sum(sales_amount,na.rm = T)) %>%
  left_join(new) %>%
  mutate(arpu=cnt/new) %>%
  arrange(cohort, seq) %>%
  dcast(cohort ~ seq, value.var = 'arpu')

View(clv)
View(new)

plot(new)

cohorts %>% 
  filter(isReturning == T) %>%
  group_by(order_id, contact_id) %>%
  summarise(sales_amount=sum(sales_amount, na.rm = T)) %>%
  ungroup() %>%
  group_by(contact_id) %>%
  summarise(sum(sales_amount, na.rm = T)/n(), n())

cohorts %>% filter(isReturning=T) %>%
  group_by(purchase_date) %>%
  summarise(newVisitors=n()) %>%
  ungroup() %>%
  ggplot(., aes(purchase_date,newVisitors)) + geom_line()



##########
#training set
#########

train <- cohorts 
train.0 <- train %>% group_by(contact_id) %>%
  summarise(freq=n(), sumQuantity=sum(quantity), sumSalesAmount=sum(sales_amount))

hist((train.0 %>% filter(freq < 50))$freq, breaks = 50)
hist(train.0$sumSalesAmount)
