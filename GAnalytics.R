library(googleAnalyticsR)
library(ggplot2)
ga_auth()
my_accounts <- ga_account_list()
View(my_accounts)
my_id <- "115311684"
start_date <- "2020-03-01"
end_date <- "yesterday"

df1 <- dim_filter("pagePath",operator = "REGEXP",expressions = "^/moodle/course/view.php")
df2 <- dim_filter("pagePath",operator = "REGEXP",expressions = "[&]",not=T)
df3 <- dim_filter("pagePath",operator = "REGEXP",expressions = ".*=122511")


# Now, put that filter object into a filter clause. If you had multiple filters, there
# are two aspects of this that wouldn't seem so weird. With one... they do seem weird:
# 1. The "filters" argument takes a list. So, you have to put your filter object in a
#    list. It's a one-element list.
# 2. The "operator" argument is moot -- it can be AND or OR...but you have to have
#    it be something, even though it doesn't do anything.
my_dim_filter_clause <- filter_clause_ga4(list(df1,df2,df3),operator = "AND")

# Pull the data. See ?google_analytics_4() for additional parameters. Depending on what
# you're expecting back, you probably would want to use an "order" argument to get the
# results in descending order. But, we're keeping this example simple.
ga_data <- google_analytics(viewId = my_id,
                            date_range = c(start_date, end_date),
                            metrics = "Users",
                            dimensions = c("date","dateHourMinute","latitude","longitude",
                                           "pagePath"),
                            dim_filters = my_dim_filter_clause,
                            rows_per_call = 40000,
                            samplingLevel="LARGE",
                            slow_fetch=T,
                            anti_sample=T)
save(ga_data,file="GA_19_141227.Rdata")
ga_data$pagePath<-NULL
#ga_data$Users <- NULL

