# # dataset contains 817741 transactions belonging to 32266 customers and 23812 products
# 
# getwd()
# dir("/Users/dk186033/Downloads/D11-02/")
# 
# library(data.table)
# library(bit64)
# 
# test1 <- fread("/Users/dk186033/Downloads/D11-02/D01")
# test2 <- fread("/Users/dk186033/Downloads/D11-02/D02")
# test3 <- fread("/Users/dk186033/Downloads/D11-02/D11")
# test4 <- fread("/Users/dk186033/Downloads/D11-02/D12")
# 
# dt <- rbind(test1,test2,test3,test4)
# rm(test1,test2,test3,test4)
# colnames(dt) <- c("timestamp","customerID","age","residence","productSubclass","productID","quantity","asset","salesPrice")
# dt <- as.data.table(dt)
# library(lubridate)
# dt[, date := ymd(substr(timestamp,1,10)),]
# 
# # weekday
# dt[, weekday := wday(date),]
# dt[, weekend := weekday %in% c(1,7),] #1: Sunday, 7: Saturday
# 
# dt[, residence := as.factor(residence),]
# dt[, age := as.factor(age),]
# 
# # Sales Item
# dt[, salesItem := asset > salesPrice,]
# 
# ### Feature Engineering --------------------------------------------------------
# 
# # see: http://www.slideshare.net/jonsedar/customer-clustering-for-marketing
# 
# # calculate totals
# perCustomer <- dt[, list(totalBaskets = length(unique(timestamp)),
#                          totalItems = sum(quantity),
#                          totalMoney = sum(salesPrice),
#                          avgItemPrice = sum(salesPrice)/sum(quantity),
#                          prcSalesItems = sum(quantity * salesItem)/sum(quantity)),
#                   by = customerID]
# 
# perShopping <- dt[, list(nItems = sum(quantity),
#                          nMoney = sum(salesPrice),
#                          weekend = mean(weekend)),
#                   by = .(customerID,date)]
# 
# perShopping <- perShopping[, list(maxItems = max(nItems),
#                                   maxMoney = max(nMoney),
#                                   avgItems = round(sum(nItems)/.N,1),
#                                   avgMoney = round(sum(nMoney)/.N,1),
#                                   prcWeekend = mean(weekend)) ,
#                            by = customerID]
# 
# # duration between shopping visits
# 
# duration <- copy(dt)
# duration <- duration[order(customerID, date),1, by = .(customerID,date)]
# duration <- duration[, lag.date:=c(NA, date[-.N]),
#                   by = .(customerID)]
# duration <- duration[, diff := as.numeric(date - lag.date),]
# duration <- duration[, list(meanDuration = mean(diff, na.rm = TRUE)), by = customerID]
# 
# 
# #test <- dt[customerID == 1376753]
# 
# # customerID
# # (done) total visits/ num baskets
# # (done) total money spent / total profit
# # (done) max items
# # (done) total items
# # (done) items per basket
# # (done) mean item cost
# # (done) profit per basket
# # (done) sales items (%)
# # (canceled) profit margin
# # budget items (%)
# # Premium items (%)
# # (done) weekend baskets (%)
# # (done) max baskets cost
# # (done) mean duration between visits
# 
# 
# # Description -------------------------------------------------------------
# 
# # 1: Transaction date
# # 2: Customer ID
# # 3: Age: 10 possible values
# # 4: Residence Area
# # 5: Product subclass
# # 6: Product ID
# # 7: Amount
# # 8: Asset
# # 9: Sales Price
# 
# # - age
# # A: < 25
# # B: 25-29
# # C: 30-34
# # D: 35-39
# # E: 40-44
# # F: 45-49
# # G: 50-54
# # H: 55-59
# # I: 60-64
# # J: > 65
# 
# # Residence Area: 8 possible values, A-F: zipcode area: 105,106,110,114,115,221,G: others, H: Unknown Distance to store, from the closest: 115,221,114,105,106,110
# 
# 
# # Combine data sets -------------------------------------------------------
# 
# dt.merged <- merge(perCustomer, perShopping, by = "customerID")
# dt.merged <- merge(dt.merged, duration, by = "customerID")
# 
# # delete all customers with only one visit
# 
# taFeng <- dt.merged[!is.na(meanDuration),,]
# 
# # Export data set ---------------------------------------------------------
# 
# #write.csv(dt.subset, file = "taFengGrocery.csv", row.names = FALSE)
# save(taFeng, file = "data/taFengGrocery.Rda")
# 
