library(scales)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(lubridate)


### 001_A read-in raw file ####
raw_camp = read_delim('./Dataset/01-campaign-data.csv',
         col_names = TRUE, delim = ';')
View(raw_camp)

### 001_B check for data quality ####
any(is.na(raw_camp))
any(duplicated(raw_camp))
raw_camp = raw_camp[-which(duplicated(raw_camp)),]
names(raw_camp) = str_to_lower(names(raw_camp))

### 002 all_volumn ####

# is.numeric(raw_camp$revenue) -> ??????return TRUE
# mutate -> ???????????????
group_var = 'week'
all = raw_camp %>% group_by_(.dots = group_var) %>% 
      summarise_if(.predicate = is.numeric, .funs = sum, na.rm=TRUE) %>% 
      mutate(campaign = 'All')

all_volumn = bind_rows(raw_camp, all)

all_volumn$week_prefix = sprintf("week %02d",all_volumn$week)
all_volumn$week = as.character(all_volumn$week)
all_volumn$revenue = round(all_volumn$revenue, digits = 2)
all_volumn$cost = round(all_volumn$cost, digits = 2)
all_volumn$campaign = factor(all_volumn$campaign)
all_volumn$campaign = reorder(all_volumn$campaign, all_volumn$visits, FUN=mean, na.rm=TRUE)
all_volumn$campaign = factor(all_volumn$campaign, levels = rev(levels(all_volumn$campaign)))
View(all_volumn)
str(all_volumn)
# calculate the avg growth rate
all_volumn2 = all_volumn %>% group_by(campaign) %>% arrange(campaign, week_prefix) %>% 
      mutate(net_profit = round(revenue - cost, digits=2),
             net_profit_grow = round((net_profit - lag(net_profit))/lag(abs(net_profit)),digits=4),
             visits_grow = round((visits - lag(visits))/lag(visits),digits=4),
             revenue_grow = round((revenue - lag(revenue))/lag(revenue),digits=4),
             cost_grow = round((cost - lag(cost))/lag(cost),digits=4))
# calculate the ratio of visits, revenue, cost 
all_volumn2 = all_volumn2 %>% group_by(campaign) %>% 
      mutate(visits_cost_ratio = (visits/cost),
             revenue_cost_ratio = (revenue/cost),
             revenue_visits_ratio = (revenue/visits))
View(all_volumn2)


### 003_A all_volumn3 ####
all_volumn3 = all_volumn2[all_volumn2$campaign != 'All',] %>% group_by(week) %>% 
      mutate(revenue_sum = sum(revenue, na.rm=TRUE),
             revenue_share = round(revenue/revenue_sum, digits=4))
View(all_volumn3)


### 003_B all_volumn_pivot, which calculates the derived columns ####
all_volumn_pivot = all_volumn2 %>% group_by(campaign) %>% 
      summarise_if(.predicate= is.numeric, .funs = c(min=min,mean=mean,median=median,max=max,sum=sum), na.rm=TRUE)
View(all_volumn_pivot)


### 004 plot01 ####
plot01 = ggplot(all_volumn2, aes(x=week_prefix, y=visits, group=campaign, col=campaign)) +
      geom_line() + geom_point() + geom_text_repel(aes(label=visits)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_color_manual(values=c("#67001F","#4D4D4D","#6E8E84","#CAC27E")) +
      xlab("week number") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # theme(axis.title.x=element_blank(),
      #       axis.text.x=element_blank(),
      #       axis.ticks.x=element_blank())

plot01

### notes: does the market grow or shrink or keep steady?
# even though the all visits number is growing, but it's driving force mainly from single campaign,
# it is hard to tell.
# but as we can see, campaign aldebaran keeps attracting traffic to our website.

### 005 plot02 ####
plot02 = ggplot(all_volumn2, aes(x=week_prefix, y=revenue, group=campaign, col=campaign)) +
      geom_line() + geom_point() + geom_text_repel(aes(label=revenue)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_color_manual(values=c("#67001F","#4D4D4D","#6E8E84","#CAC27E")) +
      xlab("week number") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank())

plot02


### notes: which campgin brings in the most gross revenue?
# but from the gross revenue perspective, aldebaran brings in the least revenue
# while cottington now brings in the most gross revenue, it's performance remain steady
# but bartledan and aldebaran's gross revenue keeps growing over the course

plot03_D = ggplot(all_volumn2[all_volumn2$campaign != 'All',], aes(x = campaign, y = (visits/cost), group=campaign, col=campaign)) +
      geom_boxplot() + scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      scale_y_continuous(breaks = pretty_breaks(n = 10))
# geom_smooth(method=lm) +
# theme_minimal()
plot03_D

plot03_E = ggplot(all_volumn2[all_volumn2$campaign != 'All',], aes(x = cost, y = revenue, group=campaign, col=campaign)) +
      geom_point() + scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      geom_smooth(method=lm) +
      theme_minimal()
plot03_E


### 006 plot03_A ####
plot03_A = ggplot(all_volumn2[all_volumn2$campaign != 'All',], aes(x = campaign, y = (revenue/cost), group=campaign, col=campaign)) +
      geom_boxplot() + scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      ylab("revenue/cost")
# geom_smooth(method=lm) +
# theme_minimal()
plot03_A

### 007 plot03_B ####
plot03_B = ggplot(all_volumn2[all_volumn2$campaign != 'All',], 
                  aes(x=week_prefix, y=round(revenue/cost, digits=2), group=campaign, col=campaign)) +
      geom_line() + geom_point() + geom_text_repel(aes(label=round(revenue/cost, digits=2))) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      xlab("week number") +
      ylab("revenue/cost") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot03_B


### 008 plot03_C ####
plot03_C = ggplot(all_volumn2[all_volumn2$campaign != 'All',], aes(x = cost, y = visits, group=campaign, col=campaign)) +
      geom_point() + scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      geom_smooth(method=lm) +
      theme_minimal()
plot03_C


### notes: which campaign is most efficient and effecitve in traffic and revenue generating?
### on the cost efficieny perspective
# obviously, aldebaran is definitely the most effecitve traffic generating campaign.
# while notice that no matter how much money cottington spends, it seems no effect on traffic generating.

# but observing the trend, we can tell that aldebaran keeps improving on its revenue/cost ratio,
# while cottington's ratio keeps deteriating.
# and aldebaran has been outperforming others for quite a few weekdays.
# it seems a steady trend.


### 008 plot04_A ####
plot04_A = ggplot(all_volumn2[all_volumn2$campaign != 'All',], aes(x = campaign, y = (revenue/visits), group=campaign, col=campaign)) +
      geom_boxplot() + scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      ylab("revenue/visits")
plot04_A

### 009 plot04_B ####
plot04_B = ggplot(all_volumn2[all_volumn2$campaign != 'All',],
                  aes(x = week_prefix, y = cost, group = campaign, col = campaign)) +
      geom_line() +
      geom_text_repel(aes(label=cost)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      xlab("week number") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot04_B

### 010 plot04_C ####
plot04_C = ggplot(all_volumn3, aes(x=week_prefix,y=revenue_share, fill=campaign)) + 
      geom_bar(stat = "identity", position = 'stack') +
      scale_fill_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      scale_y_continuous(breaks = seq(0.1,1,by=0.1), labels = sprintf("%s%%",seq(10,100,by=10))) +
      ylab("revenue share") +
      xlab("week number") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot04_C

### 011 plot04_D ####
plot04_D = ggplot(all_volumn3, aes(x=week_prefix,y=round(revenue-cost,digits=2), fill=campaign)) +
      geom_bar(stat="identity", position = "dodge") + 
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_fill_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      ylab("net profit") +
      xlab("week number") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot04_D
      
# use gridExtra package to arrange two charts together
layout = rbind(c(1,1),
               c(2,2))
plot_mix = grid.arrange(grobs=list(plot04_C,plot04_D), layout_matirx = layout)
plot_mix



plot04_E = ggplot(all_volumn[all_volumn$campaign != 'All',], 
                  aes(x=week_prefix, y=round(revenue/visits, digits=2), group=campaign, col=campaign)) +
      geom_line() + geom_point() + geom_text_repel(aes(label=round(revenue/visits, digits=2))) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_color_manual(values=c("#4D4D4D","#6E8E84","#CAC27E")) +
      xlab("week number") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot04_E



### notes: on the traffic quality perspective, what campaign's traffic is most valuable?
# we can tell that even though cottington's traffic is very valuable,
# but it seems the campaign already reaches all its potential customer based on the certain cost.
# more spend will not increase its traffic and its revenue, but only to decrease the cost efficiency.


### 012 fit linear model ####
fit.visits.lm = lm(data=all_volumn3[all_volumn3$campaign == 'Campaign_A',],
                   formula=visits ~ cost)
summary(fit.visits.lm)
names(summary(fit.visits.lm))
summary(fit.visits.lm)$adj.r.squared
names(fit.visits.lm)
fit.visits.lm$coefficients
predict(fit.visits.lm, newdata = data.frame(cost = c(250)), interval = "confidence")

fit.revenue.lm = lm(data=all_volumn3[all_volumn3$campaign == 'Aldebaran',],
                   formula=revenue ~ cost)
summary(fit.revenue.lm)
names(summary(fit.revenue.lm))
summary(fit.revenue.lm)$adj.r.squared
names(fit.revenue.lm)
fit.revenue.lm$coefficients
predict(fit.revenue.lm, newdata = data.frame(cost = c(250)), interval = "confidence")

# Although there are still some assumptions needed to be assessed, but considering the high
# R squared score and for the purpose of breity. There will not be mentioned here.


### 013 read-in raw file for session dataset ####
raw_sess = read_delim('./Dataset/02-session-data.csv',
                      col_names = TRUE, delim = ';',
                      col_types = cols(
                            session = col_character(),
                            session_start_text = col_time(format = "%H:%M:%S"),
                            session_end_text = col_time(format = "%H:%M:%S"),
                            clickouts = col_integer(),
                            booking = col_integer()
                      ))

View(raw_sess)

### 014 check for data quality ####
any(is.na(raw_sess))
any(duplicated(raw_sess))

if (length(which(duplicated(raw_sess)))) {
      raw_sess = raw_sess[-which(duplicated(raw_sess)),]
}

names(raw_sess) = str_to_lower(names(raw_sess))
names(raw_sess)[which(names(raw_sess) == "session_start_text")] = "session_start_time"
names(raw_sess)[which(names(raw_sess) == "session_end_text")] = "session_end_time"

### 015 sess_derived ####
sess_derived = raw_sess %>% 
      mutate(hour = sprintf("%02d",hour(session_start_time)),
             booking = factor(booking),
             stay_time = as.numeric(session_end_time - session_start_time)) %>% 
      ungroup()

# remove negative stay_time

if (length(which(sess_derived$stay_time < 0))) {
      sess_derived = sess_derived[-which(sess_derived$stay_time < 0),]
}
View(sess_derived)
str(sess_derived)

# some observations are omited because they have negtive duration time on websites.
# around 13 observations are removed.


### 016 sess_derived_h ####
sess_derived_h = sess_derived %>% group_by(hour,booking) %>% 
      summarise(hour_count = n(),
                stay_time_mean=mean(stay_time,na.rm=TRUE),
                stay_time_median=median(stay_time, na.rm=TRUE),
                clickouts_mean=mean(clickouts,na.rm=TRUE),
                clickouts_median=median(clickouts,na.rm=TRUE)) %>% 
      ungroup() %>% 
      group_by(booking) %>% 
      mutate(hour_count_median = median(hour_count,na.rm=TRUE),
             hour_count_above_median = factor(ifelse(hour_count >= hour_count_median,1,0)))

View(sess_derived_h)


### 017 sess_derived2 ####
# which is appended with the flag for 'hour_count_above_median' 
sess_derived2 = sess_derived %>% 
      left_join(sess_derived_h[,c("booking","hour","hour_count_above_median")], by = c("hour"="hour","booking"="booking"))

View(sess_derived2)

### 016 descriptive statistics ####


# for now we will not dive into the web surfers who did not book.
# for they always represents the major population.
# in order to avoid the noise brought up by these web surfers.
# and discover some insights
# we will focus on the customers who actually booked.


# we will dig into two aspects in this analysis.
# One is the best timing for marketer to communicate with potential customers.
# The other is seperating two catogories of customers, one is the fast decision maker.
# the second is the long time decision maker. They represents different personal traits,
# hence implys different marketing strategy.

### 018 The Distribution of Appearing For Both Type of Visitors ####
plot05_A = ggplot(sess_derived2, aes(x=as.numeric(hour), col=booking)) + 
      geom_density() +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = seq(0,23,by=1)) +
      scale_color_manual(values = c("#1A476F","#90353B")) +
      labs(fill="booking or not") +
      ylab("appearing distribution") +
      xlab("hour") +
      theme_minimal()
plot05_A

### 019 the distribution of session duration time for both type of visitors ####
plot05_B = ggplot(sess_derived2, aes(x=booking,y=stay_time,fill=booking)) +
      geom_boxplot() +
      scale_y_continuous(breaks = pretty_breaks(n = 20)) +
      scale_fill_manual(values = c("#1A476F","#90353B")) +
      labs(fill="booking or not") +
      ylab("session duration time") +
      theme_minimal()
plot05_B


### 020 the appearing time of the day for booking visitors ####
plot06_A = ggplot(sess_derived_h[sess_derived_h$booking==1,], aes(x=hour, y=hour_count, fill=hour_count_above_median)) + 
      geom_bar(stat='identity') +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_fill_manual(values = c("#D9D9D9", "#FB6A4A")) +
      geom_text(aes(y = hour_count, label = hour_count), vjust = 1.5) +
      labs(fill="above median booking number") +
      ylab("booking number") +
      theme_minimal()
plot06_A


### 021 the distribution of session duration time over 24 hours range ####
plot06_B = ggplot(sess_derived2[sess_derived2$booking==1,], aes(x=hour,y=stay_time,fill=hour_count_above_median)) +
      geom_boxplot() +
      scale_y_continuous(breaks = pretty_breaks(n = 20)) +
      scale_fill_manual(values = c("#D9D9D9", "#FB6A4A")) +
      labs(fill="above median booking number") +
      ylab("session duration time") +
      theme_minimal()
plot06_B


plot06_C = ggplot(sess_derived_h[sess_derived_h$booking==1,], aes(x=hour,y=stay_time_median, fill=hour_count_above_median)) +
      geom_bar(stat='identity') +
      scale_y_continuous(breaks = pretty_breaks(n = 20)) +
      scale_fill_manual(values = c("#D9D9D9", "#FB6A4A")) +
      geom_text(aes(y = stay_time_median, label = stay_time_median), vjust = 1.5) +
      labs(fill="above median hour booking") +
      ylab("median staying time") +
      theme_minimal()
plot06_C


# it seems like there is not connection between customer's staying time and their timing of presence.
# if we compare the distribution of staying time between the cusomter who booked and the ones didn't.
# the same conclusion prevails.



# we can tell actually the staying time of customer does not affect the propensity of booking.



### 022 ANOVA test ####
View(sess_derived2)
fit.session.anova = aov(formula = stay_time ~ factor(hour), 
                        data = sess_derived2[sess_derived2$booking==1,])
summary(fit.session.anova)

fit.session.high_booking_hour.anova = aov(formula = stay_time ~ factor(hour_count_above_median), 
                             data = sess_derived2[sess_derived2$booking==1,])
summary(fit.session.high_booking_hour.anova)



# The anova F test shows that it is not significant (with p-value 0.09)

# let's put it this way, (session_duration_time ~ above_median_hour_booking)
# and another one (session_duration_time ~ hour)
# both shows that there is no relationship between session_duration_time and the other variables.

# we should have more information in order to predict vistors' behavior precisely.