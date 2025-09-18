install.packages("summarytools")
library(summarytools)
dfSummary(murders) # provides a detailed summary of your data frame
view(dfSummary(murders)) # see the results in RStudio’s Viewer; stview(dfSummary(murders))
descr(murders) # get descriptive statistics for your numeric variables
freq(murders) # generates frequency tables for categorical variables

stats <- freq(murders$region) #生成 murders 數據集中 region 變量的頻率表，並將結果存儲在 stats 變量中。

print(stats,
      report.nas     = FALSE, # NA那行
      totals         = FALSE, # Total那行
      display.type   = F, # Type那行
      Variable.label = "Region")

ctable(x = murders$abb, 
       y = murders$region, 
       prop = "r")   # Show row proportions

with(murders, 
     print(ctable(x = abb, 
                  y = region,
                  prop     = 'n')))

# https://www.r-bloggers.com/2024/09/how-to-analyze-your-data-faster-with-r-using-summarytools/?fbclid=IwY2xjawFY-CBleHRuA2FlbQIxMQABHYNnTvR8x7JV5QgaFwwnwno0KoZl2J1FamhHHHwf0rwQChGQDhBKaZBRJg_aem__Wuzq6Y0J235ikqC0xnXNQ