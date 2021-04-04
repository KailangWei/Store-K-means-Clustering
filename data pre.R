rm(list=ls(all=TRUE))
library(data.table)
library(lubridate)

# customer analysis------------------------------------------------------------------------------------------
# load in the data sets-------------------------------------------------------------------------------------

transaction = fread("transaction_table.csv",header=TRUE)
product = fread("product_table.csv",header=TRUE)
data = merge(transaction, product, by='prod_id', all=TRUE)
rm(product)
rm(transaction)
data<-na.omit(data)

# analysis on the customer level------------------------------------------------------------------------------

# convert the tran_dt column into the date format
data$tran_dt = as.Date(data$tran_dt)
# create a new column for transaction id
data$new_id<-paste(as.character(data$tran_dt),as.character(data$cust_id),data$store_id,sep='-')
# drop the old transaction id column
data$tran_id <- NULL
# create columns for discount percentage, weekdays, and month
data[,discount_rate:= abs(tran_prod_discount_amt)/tran_prod_sale_amt][,weekdays := weekdays(data$tran_dt)][,months := month(data$tran_dt)]

# create columns for monthly spending, monthly frequency, weekday spending, discount rate, types of products on discount, 
# discount frequency, product types, total spending, total frequency per customer
data[,month_spending := sum(tran_prod_paid_amt), by=.(months,cust_id)]
data[,month_freq := uniqueN(tran_dt), by=.(months,cust_id)]
data[,weekday_spending := sum(tran_prod_paid_amt), by=.(weekdays,cust_id)]
data[,discount_rate := sum(abs(tran_prod_discount_amt))/sum(tran_prod_sale_amt),by=cust_id]
data[,discount_count := sum(tran_prod_discount_amt!= 0), by=cust_id]
data[,discount_freq := discount_count/.N, by=cust_id]
data[,product_types := uniqueN(prod_id), by=cust_id]
data[,total_spending := sum(tran_prod_paid_amt),by=cust_id]
data[,total_freq:= uniqueN(tran_dt)/(365*2),by=cust_id]

# customer segmentation data preparation
customers = as.data.table(unique(data$cust_id))
names(customers) = "cust_id"
# monthly spending & visiting frequency
customers[,months:=1]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jan_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jan_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=2]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          feb_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          feb_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=3]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          mar_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          mar_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=4]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          apr_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          apr_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=5]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          may_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          may_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=6]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jun_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jun_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=7]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jul_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jul_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=8]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          aug_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          aug_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=9]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          sep_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          sep_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=10]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          oct_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          oct_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=11]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          nov_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          nov_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=12]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          dec_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          dec_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

# weekday spending
customers[,weekdays:="Monday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Monday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Tuesday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Tuesday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Wednesday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Wednesday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Thursday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Thursday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Friday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Friday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Saturday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Saturday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Sunday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Sunday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

# create column for overall discount rate per customer
customers[data[customers, discount_rate, on=.(cust_id=cust_id),by= .EACHI],
          discount_rate := discount_rate , on=.(cust_id=cust_id)]
# create column for overall discount frequency per customer
customers[data[customers, discount_freq, on=.(cust_id=cust_id),by= .EACHI],
          discount_freq := discount_freq , on=.(cust_id=cust_id)]
# create column for product types per customer
customers[data[customers, product_types, on=.(cust_id=cust_id),by= .EACHI],
          product_types := product_types , on=.(cust_id=cust_id)]
# create column for total spendings per customer
customers[data[customers, total_spending, on=.(cust_id=cust_id),by= .EACHI],
          total_spending := total_spending , on=.(cust_id=cust_id)]
# create column for total frequency per customer
customers[data[customers, total_freq, on=.(cust_id=cust_id),by= .EACHI],
          total_freq := total_freq , on=.(cust_id=cust_id)]

# write out the customer files----------------------------------------------------------------------------------

write.csv(customers, file = "customer_data.csv",row.names=FALSE)

rm(list=ls(all=TRUE))


# product analysis------------------------------------------------------------------------------------------
# load in the data sets-------------------------------------------------------------------------------------

transaction = fread("transaction_table.csv",header=TRUE)
product = fread("product_table.csv",header=TRUE)
data = merge(transaction, product, by='prod_id', all=TRUE)
rm(product)
rm(transaction)
data<-na.omit(data)

# analysis on the product level------------------------------------------------------------------------------

# subset the product related columns
pdata <- data[, c(1,2,3,4,5,7,8,10,11,13,15,17,18)]
# convert the tran_dt column into the date format
pdata$tran_dt = as.Date(pdata$tran_dt)

rm(data)

# create columns for volumes per product, revenue per product
pdata[,pvol:=sum(tran_prod_sale_qty),by='prod_id'][,prev:=sum(tran_prod_sale_amt),by='prod_id']
# create column for profits per product
pdata[,pprofit:=sum(tran_prod_paid_amt),by='prod_id']
# create column for number of transactions per product
pdata$tran_new_id<-paste(as.character(pdata$tran_dt),as.character(pdata$cust_id),pdata$store_id,sep='-')
pdata[,ptran:=uniqueN(tran_new_id), by='prod_id']
# create columns for number of customers per product
pdata[,pcustomer:=length(unique(cust_id)),by='prod_id']
# create column for promotion days per product
pdata[, saledate:=ifelse(tran_prod_offer_cts!=0,tran_dt,NA)][,pfreq:=length(unique(saledate))-1,by='prod_id']

# data set of volumes, revenue, profits, transactions, customers, promotion days on the product level
pdata_1 <- unique(pdata[,c('prod_id','category_id','pvol','prev','pprofit','ptran','pcustomer','pfreq')])

# create columns for volumes per product category, revenue per product category
pdata[,gvol:=sum(tran_prod_sale_qty),by='category_id'][,grev:=sum(tran_prod_sale_amt),by='category_id']
# create columns for profits per product category
pdata[,gprofit:=sum(tran_prod_paid_amt),by='category_id']
# create column for number of transactions per category
pdata[,gtran:=uniqueN(tran_new_id), by='category_id']
# create columns for number of customers per product category
pdata[,gcustomer:=length(unique(cust_id)),by='category_id']
# create column for promotion days per product category
pdata[,gfreq:=length(unique(saledate))-1,by='category_id']

# data set of volumes, revenue, profits, transactions, customers, promotion days on the product category level
pdata_2 <- unique(pdata[,c('category_id','category_desc_eng','gvol','grev','gprofit','gtran','gcustomer','gfreq')])

# write out both files----------------------------------------------------------------------------------

write.csv(pdata_1, file = "product_data.csv",row.names=FALSE)
write.csv(pdata_2, file = "productcategory_data.csv",row.names=FALSE)

rm(list=ls(all=TRUE))


# store analysis----------------------------------------------------------------------------------------
# load in the data sets---------------------------------------------------------------------------------
transaction = fread("transaction_table.csv",header=TRUE)
product = fread("product_table.csv",header=TRUE)
data = merge(transaction, product, by='prod_id', all=TRUE)
rm(product)
rm(transaction)
data<-na.omit(data)

# analysis on the store level---------------------------------------------------------------------------

# create columns for volumes per store, revenue per store
data[,volume := sum(tran_prod_sale_qty), by=store_id][,revenue := sum(tran_prod_sale_amt), by=store_id]
# create column for profits per store
data[,profit := sum(tran_prod_paid_amt), by=store_id]

# create column for number of transactions per store
data$tran_new_id<-paste(as.character(data$tran_dt),as.character(data$cust_id),data$store_id,sep='-')
data[,transaction := uniqueN(tran_new_id), by=store_id]
# profits per transaction
data[,profit_trans:=profit/transaction]
# revenue per transaction
data[,revenue_trans:=revenue/transaction]

# create column for number of customers per store
data[,customer := uniqueN(cust_id), by=store_id]
# profits per unique customer
data[,profit_cust := profit/customer]
# revenue per unique customer
data[,revenue_cust := revenue/customer]

# create columns for promotions per store
# discount rate by stores
data[,discount_rate := sum(abs(tran_prod_discount_amt))/sum(tran_prod_sale_amt),by=store_id]
# discount freq by stores
data[,discount_freq := sum(tran_prod_discount_amt!= 0)/.N, by=store_id]

# create column for number of products per store
data[,product_types := uniqueN(prod_id), by=store_id]

# look at transaction frequency per store
# time setting up
data$tran_dt = as.Date(data$tran_dt)
data[,weekdays := weekdays(tran_dt)]
data[,weekends := ifelse(weekdays =="Saturday" | weekdays == "Sunday",1,0)]
data[,weekday := ifelse(weekends==0,1,0)]
# weekend transactions frequency by stores
data[,weekend_freq := sum(weekends)/(sum(weekends)+sum(weekday)), by=store_id]

# data set of volumes, revenue, profits, transactions, profit per transaction, revenue per transation, customers, profits per customer,
# revenue per customer, discount rate, discount frequency, weekend transaction frequency, product types on the store level
sdata <- unique(data[,c('store_id','volume','revenue','profit','transaction','profit_trans','revenue_trans',
                         'customer','profit_cust','revenue_cust','discount_rate','discount_freq',
                         'weekend_freq','product_types')])

# write out the store files----------------------------------------------------------------------------------

write.csv(sdata, file = "stores_data.csv",row.names=FALSE)

rm(list=ls(all=TRUE))

#------------------------------------------------------------------------------------------------------------























