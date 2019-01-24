Online Campaign Analysis Case
================
Tom Lin
2017

Campaign Analysis
-----------------

-   Quality assessment:
    1.  The campaign data doesn't contain missing values.
    2.  The campaign data has one duplicated row, which is now removed from the analyzing dataset.
-   Analysis Goal:
    1.  Is the tourism market booming or falling now?
    2.  On the perspective of cost effectiveness, which campaign does perform the best?
    3.  On the perspective of traffic quality, which campaign does perform the best?
    4.  What is the recommended campaign to be extended and its predicted outcome?
    5.  Appendix: the descriptive statistics of campaign performance.(in email attachment)

### Is the tourism market booming or falling now?

**Plot\_01:** Gross Visits Over The Weeks ![](02-online-campaign-report_files/figure-markdown_github/create%20plot01-1.png)

> The overall visits keep growing but the main driving force comes from Campaign\_A.

As we can tell, the only significant growth of traffic comes from `Campaign_A`, hence **it's hard to say we are in the tourist booming season**. But we can definitely say Campaign\_A performs very well on attracting potential customers and build up the brand awareness.

Next, let's look at the revenue performance.

**Plot\_02:** Gross Revenue Over The Weeks ![](02-online-campaign-report_files/figure-markdown_github/create%20plot02-1.png)

> While all campaign's gross revenue are growing or remain steady, Campaign\_A contributes the least revenue among all.

On the other hand, `Campaign_C` instead brings in **the most revenue over this period**. In order to better assess each campaign, let's look at the cost performance on each campaign then.

### Regarding cost effectiveness, which one performs the best?

**Plot\_03\_A:** The Distribution of Revenue Generated per Cost ![](02-online-campaign-report_files/figure-markdown_github/create%20plot03_A-1.png)

In the chart above, **the horizontal line** in each box represents the median of revenue versus cost for the campaign and **the length of the box** represents the major distribution of the revenue versus cost over the period.

We can tell that **Campaign\_C** has the highest ratio on revenue generated per cost `median 1.05`, whereas the medians for **Campaign\_A** and **Campaign\_B** are `median (0.92,0.90)`

> But if we look at the trend, it tells another story.

**Plot\_03\_B:** The Trend of Revenue Generated per Cost ![](02-online-campaign-report_files/figure-markdown_github/create%20plot03_B-1.png)

> The revenue versus cost ratio of Campaign\_A keeps rising but the Campaign\_C's is decreasing.

**The ratio for Campaign\_A surpasses others at around week 21** and the growth has sustained for a couple of weeks, which implies it's now already a steady trend.

Also, it's worth mentioning that by the `week 21 and following weeks`, only Campaign\_A's revenue can cover it's investment, **the rest are in deficit**.

So how could this happened? Let's look at the `visits` versus `cost` then.

**Plot\_03\_C:** The Gross Visits versus Gross Cost ![](02-online-campaign-report_files/figure-markdown_github/create%20plot03_C-1.png)

> As the cost increases, only visits of Campaign\_A follow up, but the other's visits almost remain the same.

**Yes, now we see the power of traffic**, the greatest thing of `Campaign_A` lays in its power on attracting traffic. However, from the chart we can tell **maybe the other two campaigns already reach their full potential customers**. No matter how much the money we spend on them, there is almost no growth on visits.

If it is the case, then what's the merit of Conttington? **Should we just close it right away?**

Maybe let's set back and look the other way around. Let's dicuss the quality of traffic.

### Regarding traffic quality, which one performs the best?

**Plot\_04\_A:** The Distribution of Revenue Generated per Visit ![](02-online-campaign-report_files/figure-markdown_github/create%20plot04_A-1.png)

> The visit of Conttington brings in the most revenue.

Though we can't increase the visits of Conttington, **but the customers directed from Conttington seem to be quite loyal and generate large revenues**. Besides, the reason why revenue versus cost of Conttington keeps falling is because we spend `useless money` on expanding the campaign. **As it is evident from the chart below. The cost keeps growing while visits don't.**

**Plot\_04\_B:** The Trend of Cost Over the weeks ![](02-online-campaign-report_files/figure-markdown_github/create%20plot04_B-1.png)

Furthermore, it is also suppported by `the changes of revenue share` among all campaigns and `the net profit` of each campaign over the weeks.

Over the campaign course, Campaign\_A's revenue share keeping growing and the Campaign\_C one is shrinking. In addition, **the net profits of Campaign\_A keeps improving over the period whereas the Conttington one is deteriorating.**

**Plot\_04\_C:** The Change of Revenue Share and Net Profit Over The Weeks ![](02-online-campaign-report_files/figure-markdown_github/create%20plot04_C%20and%20plot04_D-1.png)![](02-online-campaign-report_files/figure-markdown_github/create%20plot04_C%20and%20plot04_D-2.png)

> So with all these reasonings, we are confident to say it's best to focus on Campaign\_A, and increase budgets on it.

**But the question raised...**

### What is its predicted outcome?

Since now we've decided to extend the period of Campaign\_A campaign, in order to predict `the effects of extra 250 euros spending`, we can apply **simple linear regresion** to answer the question.

> The followings are the respective regression for the effect of cost on visits and revenue:

-   **Visits vs Cost**
    *V**i**s**i**t**s* = 52.7 + 2.72 \* *C**o**s**t*         *R*<sup>2</sup> = 0.92

-   **Revenue vs Cost**
    *R**e**v**e**n**u**e* = −16.8 + 1.18 \* *C**o**s**t*         *R*<sup>2</sup> = 0.98

Hence, we can expect that the extra visits would be **732.7** with confidence interval between `(685.41, 782.69)` under 95% confidence level.

And the revenue would be **278.2** with confidence interval between `(268.03, 288.73)` under 95% confidence level.

### Appendix: transformed analysis dataset and descriptive statistics

> The infomation is inclosed in the email attachment.

Session Analysis
----------------

-   Quality assessment:
    1.  The session data doesn't contain missing values.
    2.  The session data has 13 observations which have negative session duration time. They are removed from the analysis dataset.
-   Analysis Goal:
    1.  What is the appearing hour of the day/session duration time for booking/non-booking visitors? Are they different from each other?
    2.  When is the hottest booking hour for visitors?
    3.  How to identify long session time visitors from the short session time visitors?
    4.  ANOVA test for the relationship between appearing time, hottest booking hours and session duration time.
    5.  Final words.
    6.  Appendix: the transformed analysis dataset and descriptive statistics. (in the email attachment)

> As marketing professionals, we often ask, what is the best timing to reach our potential customers? So, let's see if we can discover something interesting in the following analysis.

### The appearing hour of day for booking/non-booking visitors?

**The chart below shows up the appearing distribution in the hour of day for each group. It is a quantile scale now.**

`Zero` represensts non-booking visitors, while `one` represents booking visitors.

**Plot\_05\_A:** The Distribution of Appearing For Both Type of Visitors ![](02-online-campaign-report_files/figure-markdown_github/create%20plot05-1.png)

> It seems like there doesn't exit significant difference of appearing distribution between the two group.

**In addition, we can take a look at the distribution of session duration time, see if they are different from each other.**

**Plot\_05\_B:** The Distribution of Session Duration Time For Both Type of Visitors ![](02-online-campaign-report_files/figure-markdown_github/create%20plot05_B-1.png)

> Again, their distribution seem quite smilar. No significant difference of session duration time, either.

Therefore, it is hard to dig out insights from such results.

However, `in most marketing cases, we don't pay much attention to the non-booking visitors.` For booking visitors always account only a pretty small proportion of the whole population. Excluding the non-booking visitors will **reduce the data noise** brought up by them, and only by then `the key features` of the booking visitors will be more `evident`.

While on the subject, allow me mention that session duration time in both groups looks like following normal distribution.

**So now, let's focus on the booking visitors in the following analysis.**

### The hottest booking hour of day

In the chart below, `the dark-shaded bar` represents that its booking number is above the median of all hours. On the contrary, the `light-shaded bar` means its number below the median.

**Plot\_06\_A:** The Appearing Hour of Day For Booking Visitors ![](02-online-campaign-report_files/figure-markdown_github/create%20plot06-1.png)

> Though not obviously, it still reveals mild deviation of show-up time for the booking visitors.

Therefore, we can say that the hottest booking hour of the day can break into the following time frame.

-   mid-night (01~02)
-   the early morning (06~07), probably the commuting hour
-   at noon break (12~13)
-   the evening (18~20), this is the longest continuous time for the booking visitors to show up.

**Hence, It is suggested that we should increase the communication intensity in the evening**, in order to maximize the performance.

### How to identify long session time visitors from the short session time visitors

**Usually visitors with long session duration time have rather different personal character to the ones with short session duration time.** Therefore, it requires different marketing strategy to appeal to them. The question we have now is that could we identify long/short session duration time visitors just based on their appearing hour of day?

**plot\_06\_B:** The Distribution of Session Duration Time Over 24 hours Range ![](02-online-campaign-report_files/figure-markdown_github/create%20plot06_B-1.png)

> It seems like both long session time visitors and short session time visitors are distributed evenly in every hour.

As we can tell that the distribution of session duration time is somehow `symmetric` in every hour.

Furthermore, if we focus on the hottest booking hour when visitors mostly likely to book `(in the dark-shaded box of the chart above)`, the distribution of session duration time in these hours still shows no significant skewness or deviation. **It implies that we can't identify long session time visitor from short session time visitor just based on their appearing hour nor the hottest booking hours.**

If we want to further confirm with our judgement. We can apply `ANOVA test`, to see if there has statistical significance of difference on `hours versus session duration time`, and `the hottest booking hours versus session duration time`.

### ANOVA test for statistic proof

-   **ANOVA (session duration time ~ hour)**: used to test if different hour has impact on session duration time distribution.

<!-- -->

    ##               Df  Sum Sq Mean Sq F value Pr(>F)  
    ## factor(hour)  23  167647    7289   1.419 0.0911 .
    ## Residuals    943 4842258    5135                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The test result is `p-value 0.091` which is not significant. So the distribution among each hours are the same.

-   **ANOVA (session duration time ~ hottest booking hour)**: used to test if hottest booking hours have impact on session duration time distribution.

<!-- -->

    ##                                  Df  Sum Sq Mean Sq F value Pr(>F)
    ## factor(hour_count_above_median)   1    9935    9935   1.917  0.166
    ## Residuals                       965 4999971    5181

The test result is `p-value 0.16` which is not significant. So the session duration time distribution between hottest booking hours and low booking hours are the same.

> Both ANOVA test confirm that session duraiton time is ditributed evenly among hours and has no relation to hottest booking hour either.

### Final words

In a glimpse, we can tell booking and non-booking visitors seem to have the same characteristics on appearing hour and session duration time. That yields no fruitful results.

Following the analysis above, we can conclude that **the best hours to communicate with visitors lay in the evening (18~20).** It is also a rather longer continuous period for booking visitors to show up.

Because of no relation between session duration time with the appearing hours, given the current info, **we cannot tell nor identify who will stay longer and who won't and thus communicate in different strategy.** We should have more information in order to predict vistors' behavior precisely, such as the demography of visitors.

### Appendix: transformed analysis dataset and descriptive statistics

> The infomation is inclosed in the email attachment.
