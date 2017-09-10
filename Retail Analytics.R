#Final Project R Code:


#load rawdata.rdata to see initial data set
#OR Dunn_Final.rdata to jump directly to univariate analysis (Line No 105) and skip data merging, aggregation, and cleaning

#Importing CSV files:
transaction<-read.csv(file.choose(),header=T)
campaign_desc<-read.csv(file.choose(),header=T)
campaign_table<-read.csv(file.choose(),header=T)
coupon<-read.csv(file.choose(),header=T)
coupon_redempt<-read.csv(file.choose(),header=T)
hh_demographic<-read.csv(file.choose(),header=T)
product<-read.csv(file.choose(),header=T)
DAY_MAPPING<-read.csv(file.choose(), header = T)

View(transaction)
View(product)
View(hh_demographic)
View(coupon)
View(coupon_redempt)
View(DAY_MAPPING)
#Merging datasets based on common columns as seen in ER Diagram

#Merging transaction's DAY column with DAY MAPPING's DAY column
transaction<-merge(transaction, DAY_MAPPING, by="DAY")
#Merging transaction and product CSVs using PRODUCT_ID:
transaction_product<-merge(transaction, product, by=c("PRODUCT_ID","PRODUCT_ID"))
#Merging transaction_product with household dataset using household_key:
transaction_product_household<-merge(hh_demographic,transaction_product,by="household_key")
#Merging coupon and coupon redempt using COUPON_UPC
coupon1<-merge(coupon, coupon_redempt, by=c("COUPON_UPC","COUPON_UPC"))
#merging coupon and transaction_product_household gives memory related error, so we will perform this merging at a later stage
#At this point, we are not considering campaign related CSVs as our research question mainly focusses on transaction, product, household, and coupon related attributes

#Adding weekday information:
transaction_product$DATE<-as.Date(transaction_product$DATE,format='%m/%d/%Y')
transaction_product$Day_of_Week<-weekdays(transaction_product$DATE)
#Creating weekend flag
transaction_product$weekend_flag <- ifelse(transaction_product$Day_of_Week %in% c("Saturday","Sunday"),1,0)
#Creating Flag for National Products Purchased
transaction_product$National_Flag <- ifelse(transaction_product$BRAND %in% "National",1,0)

# calculate the Recency(days) with respect to end Date, the smaller days value means more recent
transaction_product$endDate<-max(transaction_product$DATE)
transaction_product$Recency<-as.numeric(difftime(transaction_product$endDate,transaction_product$DATE,units="days"))

#aggregating data at household level:
install.packages('data.table')
library(data.table)
library(dplyr)
#We intend to obtain count of unique values of some of the columns and that's where we want to use uniqueN function from data.table package
#Aggregating at household level:
Dunn1 <- transaction_product  %>% group_by(household_key) %>% summarize(total_sales = sum(SALES_VALUE), products_purchased=sum(QUANTITY), COUPON_DISCOUNT=sum(COUPON_DISC), no_of_stores=uniqueN(STORE_ID), no_of_orders=uniqueN(BASKET_ID), total_visits=uniqueN(DAY), no_of_national_products=sum(National_Flag), recency=min(Recency)+1)
#Aggregating coupon data at household level
Dunn_coupon <- coupon1 %>% group_by(household_key) %>% summarize(No_of_coupon_redemptions=uniqueN(COUPON_UPC))
#Merging coupon information with Dunn1 data:
Dunn2<-merge(Dunn1, Dunn_coupon, by="household_key", all.x = TRUE)
#Calculating weekend visits for each household:
Dunn3 <- transaction_product %>% group_by(household_key, DATE) %>% summarize(weekend_visits_count=sum(weekend_flag))
Dunn3$weekend_visits <- ifelse(Dunn3$weekend_visits_count!=0,1,0)
Dunn4 <- Dunn3 %>% group_by(household_key) %>% summarize(weekend_visits=sum(weekend_visits))
#Merging weekend_visits info with Dunn2
Dunn_Final <- merge(Dunn2, Dunn4, by="household_key")

#Checking data types of all columns
str(Dunn_Final)
#Converting household key to factor:
Dunn_Final$household_key<-as.factor(Dunn_Final$household_key)
#Adding percentages for national brand products and weekend_visits purchased for better exploration:
Dunn_Final$BrandNational <- (Dunn_Final$no_of_national_products/Dunn_Final$products_purchased)*100
Dunn_Final$PercentWeekendVisits <- (Dunn_Final$weekend_visits/Dunn_Final$total_visits)*100
#Calculating Average Basket Size for each household:
Dunn_Final$Average_Basket_Size<-Dunn_Final$products_purchased/Dunn_Final$no_of_orders

#Data Cleaning:

#Checking for NAs:
sum(is.na(Dunn_Final))
#We expect NAs in No_of_coupon_redemptions as all households may not have redeemed their coupons, checking for that
sum(is.na(Dunn_Final$No_of_coupon_redemptions))
#All of them in expected column
#Replacing them with 0s
Dunn_Final[is.na(Dunn_Final)]<-0

#Univariate Analysis:
summary(Dunn_Final)
#We see some unexpected results in summary output especially for Average_Basket_Size, it looks right skewed
#Average_Basket_Size shows number of products purchased by a given household averagred for all visits
#We suspect there might be few products that are purchased in very large quantities and this might be the reason for this skewness
#To delve further, we aim to calculate percentage of products purchased and sales produced by those products:
x<- transaction_product %>% group_by(SUB_COMMODITY_DESC) %>% summarize(quantity_sold=sum(QUANTITY), sales_produced=sum(SALES_VALUE)) %>% arrange(-quantity_sold)
x$average_product_price<-x$sales_produced/x$quantity_sold
head(x)
#We see product like Gasoline-Reg-Unleaded have very low product price, nearly 0.2 cents and are purchased in very high quantities
#We suspect this could be a major cause of skewness and hence we remove all instances of product- Gasoline-Reg-Unleaded from our consideration

transaction_product1<-transaction_product[which(transaction_product$SUB_COMMODITY_DESC!="GASOLINE-REG UNLEADED"),]

#Repeating above code to recreate Dunn_Final
transaction_product<-transaction_product1
#Re run code from line 49 to 76
summary(Dunn_Final)

#Univariate Analysis:
Dunn_Final1<-Dunn_Final #creating a backup
plot(density(Dunn_Final$total_sales)) #right skewed
#trying out transformations:
plot(density(1/Dunn_Final$total_sales^0.5)) #slightly better
plot(density(1/Dunn_Final$total_sales^2)) # does not help
plot(density(1/Dunn_Final$total_sales)) # little more better
plot(density(log(Dunn_Final$total_sales))) # best plot
#removing outliers
b<-boxplot(log(Dunn_Final$total_sales))
c<-b$out
max(c)
#less than max(c) must be removed
Dunn_Final<-Dunn_Final[which(log(Dunn_Final$total_sales)>max(c)),]
boxplot(log(Dunn_Final$total_sales))
plot(density(log(Dunn_Final$total_sales)), main = "Distribution of total_sales") # data is now normally distributed

#COUPON_DISCOUNT has negative values which make sense in business context, we transform it to positive scale to see if higher discounts influenced sales in our further analysis:
Dunn_Final$COUPON_DISCOUNT<-Dunn_Final$COUPON_DISCOUNT*(-1)
str(Dunn_Final)
#We see no_of_national_products and weekend_visits are represented as percentages and hence we drop these columns:
Dunn_Final$weekend_visits<-NULL
Dunn_Final$no_of_national_products<-NULL
sum(is.na(Dunn_Final))


#Data is now free of any NAs and our dependent variable has a normal distribution, thus the data is clean for further analysis
#Note: Everywhere we would be using log(total_sales) as this transformation gave us normal distribution

#Bivariate Analysis:
library(corrplot)
library(psych)
str(Dunn_Final)
round(cor(Dunn_Final[,-c(1,12)]),3)
pairs.panels(Dunn_Final[,-c(1,12)])
corrplot(cor(Dunn_Final[,-c(1,12)]), method="circle", addCoef.col="grey", type="upper")


#Total Sales seem to be highly positively correlated with total visits,no of orders and weekend visits.Products purchased also seem to be slighly correlated.

library(car) 
scatterplot(total_sales ~ products_purchased, data=Dunn_Final, 
            xlab="Products Purchased", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final)) # indicates a linear positive trend, with the increase in product  purchased total sales increases

scatterplot(total_sales ~ COUPON_DISCOUNT, data=Dunn_Final, 
            xlab="Coupon Discount", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final))  # Higher the coupon Discount lower the total sales value

scatterplot(total_sales ~ no_of_stores, data=Dunn_Final, 
            xlab="# of stores", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final)) # Number of stores does not seem to affect the total sales

scatterplot(total_sales ~ no_of_orders, data=Dunn_Final, 
            xlab="# of orders", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final)) # shows a positive proportional relation i.e with the increase in the the no of orders total sales increases

scatterplot(total_sales ~ total_visits, data=Dunn_Final, 
            xlab="Total Visits", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final))# strong postive relation i.e Total sales increases with the incrase in the number of total visits to the store

scatterplot(total_sales ~ recency, data=Dunn_Final, 
            xlab="Recent Visit", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final)) #High Total Sales value generated from people who visited more recently and total sales value steadily decreasing for people no longer visiting the stores

scatterplot(total_sales ~ No_of_coupon_redemptions, data=Dunn_Final, 
            xlab="# of Coupons redemptions ", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final)) # higher sales with Higher coupon redemptions

scatterplot(total_sales ~ weekend_visits, data=Dunn_Final, 
            xlab="# of Weekend visits ", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final)) #Higher number of visits on the weekends generate higher sales

scatterplot(total_sales ~ BrandNational, data=Dunn_Final, 
            xlab="% of National Brand Products ", ylab="Total Sales", 
            main="Enhanced Scatter Plot", 
            labels=row.names(Dunn_Final))# Does not seem to have any relation

#Principle Component Analysis:

Dunn_s <- data.frame(scale(Dunn_Final[,-1]))
head(Dunn_s)
describe(Dunn_s)[c(3:4)] #data is scaled
corrplot(cor(Dunn_s), method="circle", addCoef.col="grey", type="upper")  #we seem some extremely strong correlation amongst no_of_orders and total_visits
Dunn_s$household_key <- Dunn_Final$household_key
str(Dunn_s)
Dunn_pca1 <- prcomp(Dunn_s[,-12])
summary(Dunn_pca1)
#4 PCAs explain 70% variance
plot(Dunn_pca1)
#simulate # of pcas
fa.parallel(Dunn_s[,-12], fa="pc", n.iter=100)
#Parallel Analysis suggests 3 components, so we will choose 3 components to obatin weights
biplot(Dunn_pca1, cex=0.6, col=c("lightgray", "black"), main="PCA of Customer Spend Attributes, using first two PCs") 
#We see strong correlation between 2 independent variables- total_visits and no_of_orders
#obtain weights:
Dunn_pca2 <- principal(Dunn_s[-12], missing=F, nfactors=3, rotate="varimax") # varimax is default 
round((Dunn_pca2$weights),3)
#At this step, we have dropped no_of_orders from Dunn_Final data set as it is highly correlated with total_visits
Dunn_Final$no_of_orders<-NULL

#Regression Analysis:

#Linear Regression:
mod1<-lm(log(total_sales)~., data = Dunn_Final[,-1]) #not considering household key and using log of dependent variable
summary(mod1)

#removing insignificant columns:
mod2<-lm(log(total_sales)~products_purchased+no_of_stores+total_visits+recency+BrandNational+Average_Basket_Size, data = Dunn_Final)
summary(mod2)

#Adding interactions:
mod3<-lm(log(total_sales)~Average_Basket_Size+no_of_stores+recency+BrandNational+products_purchased:total_visits, data = Dunn_Final)
summary(mod3) #Significant interaction but adjusted R square decreases
mod4<-lm(log(total_sales)~no_of_stores+BrandNational+recency+total_visits+products_purchased:Average_Basket_Size, data = Dunn_Final)
summary(mod4) #Significant interaction but adjusted R square decreases
mod5<-lm(log(total_sales)~Average_Basket_Size+BrandNational+total_visits+recency+products_purchased:no_of_stores, data = Dunn_Final)
summary(mod5) #Significant interaction and ajusted R square is also good
mod6<-lm(log(total_sales)~Average_Basket_Size+BrandNational+total_visits+recency+products_purchased:COUPON_DISCOUNT, data = Dunn_Final)
summary(mod6) #Not a significant interaction
mod7<-lm(log(total_sales)~Average_Basket_Size+BrandNational+total_visits+products_purchased+recency+COUPON_DISCOUNT:No_of_coupon_redemptions, data = Dunn_Final)
summary(mod7) #Slightly significant interaction and adjusted R square is also good
mod8<-lm(log(total_sales)~Average_Basket_Size+BrandNational+total_visits+recency+products_purchased:PercentWeekendVisits, data = Dunn_Final)
summary(mod8) #Significant Interaction and adjusted R square is also good
mod9<-lm(log(total_sales)~Average_Basket_Size+BrandNational+total_visits+recency+No_of_coupon_redemptions+products_purchased:No_of_coupon_redemptions, data = Dunn_Final)
summary(mod9) #Significant Interaction and adjusted R square is also good
mod10<-lm(log(total_sales)~Average_Basket_Size+no_of_stores+recency+BrandNational+products_purchased:BrandNational, data = Dunn_Final)
summary(mod10) #Significant interaction but adjusted R square decreases
#Evaluating mod2, mod5, mod7, mod8, mod9:

#Evaluating mod2:
head(log(Dunn_Final$total_sales))-head(predict(mod2))
psych::describe(residuals(mod2))
plot(residuals(mod2))
plot(density(residuals(mod2))) #residuals are slightly left skewed
summary(mod2)$adj.r.squared  #Nearly 80%

#Evaluating mod5:
head(log(Dunn_Final$total_sales))-head(predict(mod5))
psych::describe(residuals(mod5))
plot(residuals(mod5))
plot(density(residuals(mod5))) #residuals are slightly left skewed
summary(mod5)$adj.r.squared  #Nearly 78%

#Evaluating mod7:
head(log(Dunn_Final$total_sales))-head(predict(mod7))
psych::describe(residuals(mod7))
plot(residuals(mod7))
plot(density(residuals(mod7))) #residuals are slightly left skewed
summary(mod7)$adj.r.squared  #Nearly 79%

#Evaluating mod8:
head(log(Dunn_Final$total_sales))-head(predict(mod8))
psych::describe(residuals(mod8))
plot(residuals(mod8))
plot(density(residuals(mod8))) #residuals are left skewed
summary(mod8)$adj.r.squared  #Nearly 79%

#Evaluating mod9:
head(log(Dunn_Final$total_sales))-head(predict(mod9))
psych::describe(residuals(mod9))
plot(residuals(mod9))
plot(density(residuals(mod9))) #residuals are left skewed
summary(mod9)$adj.r.squared  #Nearly 77%

#We see that addition of interaction explain more about the data and we good numbers for adjusted R square too. 
#Based on the performance parameters seen above, we choose Model 8 for predicting dependent variable as adjusted R square is also good and all variables and interactions are significant in explaining about total_sales
plot(mod8) #Residuals are almost normally distributed
plot(predict (mod8), residuals (mod8))
plot(hatvalues(mod8)) 
tail(sort(hatvalues(mod8)), n=10)
identify(hatvalues(mod8), col="steelblue")
outliers <- c(100,  267,  352, 1036, 1094, 1352, 1410, 1434, 1550, 1867, 2326)
Dunn_Final_new <- Dunn_Final[-outliers,]
nrow(Dunn_Final_new)-nrow(Dunn_Final) # We removed 11
mod8_out <- lm(log(total_sales)~Average_Basket_Size+BrandNational+total_visits+recency+products_purchased:PercentWeekendVisits, data = Dunn_Final)
summary(mod8_out) #not much improvement in adjusted R square
anova(mod8,mod8_out, test="Chisq")
#almost no improvement

#Logistic Regression:
#With this model, we aim to identify variables that influence log of odds of high value of total_sales
#For this we create a factor column called customer_spend as:
Dunn_Final$customer_spend[Dunn_Final$total_sales<=mean(Dunn_Final$total_sales, trim=0.1)]<-"Low"
Dunn_Final$customer_spend[Dunn_Final$total_sales>mean(Dunn_Final$total_sales, trim=0.1)]<-"High"
Dunn_Final$customer_spend<-as.factor(Dunn_Final$customer_spend)
mod11<-glm(customer_spend~., data=Dunn_Final[,-c(1,2)], family=binomial)
summary(mod11)
mod12<-glm(customer_spend~products_purchased+BrandNational, data = Dunn_Final[,-c(1,2)], family=binomial)
summary(mod12) #slightly improved AIC
#Adding Interactions
mod13<-glm(customer_spend~BrandNational+products_purchased:PercentWeekendVisits, data = Dunn_Final[,-c(1,2)], family=binomial)
summary(mod13) #model did not improve, evaluating mod12 further:
library(rms)
library(car)
vif(mod12)
sqrt(vif(mod12))>2 #no multi collinearity
durbinWatsonTest(mod12) #residuals are not correlated
#Computing pseudo R square:
mod12_fit <- lrm (customer_spend~products_purchased+BrandNational, data = Dunn_Final[,-c(1,2)])
mod12_fit$stats["R2"] #90% -- not bad!


#Creating regression model with household information:
Dunn_Household <- merge(Dunn_Final, hh_demographic, by="household_key")
str(Dunn_Household)
#Performing chisquare test to check correlation between customer_spend and demographic parameters:
chisq.test(Dunn_Household$customer_spend, Dunn_Household$AGE_DESC) #no correlation
chisq.test(Dunn_Household$customer_spend, Dunn_Household$MARITAL_STATUS_CODE) #no correlation
chisq.test(Dunn_Household$total_sales, Dunn_Household$INCOME_DESC) #no correlation
chisq.test(Dunn_Household$customer_spend, Dunn_Household$HOMEOWNER_DESC) #no correlation
chisq.test(Dunn_Household$customer_spend, Dunn_Household$HH_COMP_DESC) #no correlation
chisq.test(Dunn_Household$customer_spend, Dunn_Household$HOUSEHOLD_SIZE_DESC) #no correlation
chisq.test(Dunn_Household$customer_spend, Dunn_Household$KID_CATEGORY_DESC) #no correlation

mod14 <- glm(total_sales~., data = Dunn_Household[,-c(1:11)], family=binomial)
summary(mod14)

#No demographic variable is significant in explaining about customer spend, hence we conclude that sales are not influenced by any of the demographic parameters

#Cluster Analysis:

#Please reload Dunn_Final.rdata before executing below lines of code:

library(factoextra) # plots 
library(gmodels) # for crosstabs 
library(NbClust)# determine number of clusters 
library(cluster)

#We selected the significant variables from regression model to use for clustering
#products_purchased
#no of stores 
#brand national
#total visits
#Average Basket Size
#Recency
#And Sales along with these variables

CData<-Dunn_Final[,c(2,3,4,6,8,11,12)]
#applying min max transformation
CData[1:7] <- lapply(CData[1:7],function(x) { return ((x - min(x)) / (max(x) - min(x))) })
norm_data<-data.frame(CData)
#run cluster analysis using normalized data
fviz_nbclust(norm_data, kmeans, method = "wss")  # knee around 4 to 6
set.seed(1024)

#using k=4, nstart=100
mod1 <- kmeans(norm_data, 4, nstart = 100) 
norm_data$kmeans1 <- mod1$cluster
clusplot(norm_data, mod1$cluster, color=T, shade=T, labels=4, lines=0, cex=.6)
#74.33% variability explained
#Distinct clusters are being formed
mod1$tot.withinss
#97.5

#try changing seeds to see whether the results vary
set.seed(557)
#using k=4, nstart=100
mod2 <- kmeans(norm_data[,-c(8)], 4, nstart = 100) 
norm_data$kmeans2 <- mod2$cluster
clusplot(norm_data[,-c(8)], mod2$cluster, color=T, shade=T, labels=4, lines=0, cex=.6)
mod2$tot.withinss
#97.5

#tot within sum of squares remains constant although variability decreases a bit



#Let's increase nstart to 500
mod3 <- kmeans(norm_data[,-c(8,9)], 4, nstart = 500) 
norm_data$kmeans3 <- mod3$cluster
clusplot(norm_data, mod3$cluster, color=T, shade=T, labels=4, lines=0, cex=.6)
# 72% of variability explained 
# But clusters are more distinct
# Better grouping with nstart=500 as compared to nstart=100




#Let's increase nstart=1000
mod4 <- kmeans(norm_data[,-c(8,9,10)], 4, nstart = 1000) 
norm_data$kmeans4 <- mod4$cluster
clusplot(norm_data[,-c(8,9,10)], mod4$cluster, color=T, shade=T, labels=4, lines=0, cex=.6)
#71% of variability explained with increase to 1000
#Less Distinct Clusters 
#Sticking to nstart=500 and k=4 (so far the best model)





#Now k=5 nstart=500
mod5 <- kmeans(norm_data[,-c(8,9,10,11)], 5, nstart = 1000) 
norm_data$kmeans5<- mod5$cluster
clusplot(norm_data[,-c(8,9,10,11)], mod5$cluster, color=T, shade=T, labels=5, lines=0, cex=.6)
#70%,variability decreased  hence sticking to 4 
#clusters are also overlapping
#More clusters means more complexity
#Motive is to get insights with meaningful and simple groupings
#4 Clusters give better results and are simpler to interpret for business

#dropping the unnecessary kmeans
#Keeping only kmeans model 3 results
norm_data<-norm_data[,-c(8,9,11,12)]
#Let's try 3 cluster
#using k=3, nstart=100
mod6 <- kmeans(norm_data[,-c(8)], 3, nstart = 100) 
norm_data$kmeans6 <- mod6$cluster
clusplot(norm_data, mod6$cluster, color=T, shade=T, labels=3, lines=0, cex=.6)
#Lot of overlap between two clusters
#Better to stick with 4
mod6$tot.withinss 
#114
#Higher total withinss as compared to 4
#dropping latest cluster
norm_data<-norm_data[,-c(9)]
#Choosing 4 clusters with n start=500 explaining 72% of variabilty

#Can we do better with PAM or different transformation method?

#Let's see whether using scale function instead of min max transformation can improve clustering



norm_data1 <- scale(CData)
norm_data1<-data.frame(norm_data1)
#run cluster analysis using normalized data
fviz_nbclust(norm_data1, kmeans, method = "wss")  # knee around 4 to 6
set.seed(1024)

#Let's start with our best model,k=4
#using k=4, nstart=100
mod1 <- kmeans(norm_data1, 4, nstart = 100) 
norm_data1$kmeans1 <- mod1$cluster
clusplot(norm_data1, mod1$cluster, color=T, shade=T, labels=4, lines=0, cex=.6)
#72% variability explained
mod1$tot.withinss
#7167
#High total within sum of squares
#That definetly indicates that min max transformation is yielding better results than scale function


#What about PAM method instead of Kmeans?
#Let's try PAM
# Find optimal number of clusters in the data 

pam_data<-data.frame(norm_data[,-c(8)]) #using min max transformed data
fviz_nbclust(pam_data, pam, method = "silhouette")+theme_classic() # suggests k=2 
dev.off()

#Will try 2,3,4,5
#k=2
mod_pam1 <- pam(pam_data, 2)
pam_data$pam <- mod_pam1$cluster
mod_pam1$silinfo$avg.width 
#0.57
#k=3
pam_data<-data.frame(norm_data[,-c(8)]) #using min max transformed data
mod_pam1 <- pam(pam_data, 3)
pam_data$pam <- mod_pam1$cluster
mod_pam1$silinfo$avg.width 
#0.48 , value decreases

#k=4

pam_data<-data.frame(norm_data[,-c(8)]) #using min max transformed data

mod_pam1 <- pam(pam_data, 4)
pam_data$pam <- mod_pam1$cluster
mod_pam1$silinfo$avg.width 
#0.40

#value decreases further

#The best model is 2 as suggested by Scree plot

#Let's plot k=2
#k=2
pam_data<-data.frame(norm_data[,-c(8)]) #using min max transformed data
mod_pam1 <- pam(pam_data, 2)
pam_data$pam <- mod_pam1$cluster
clusplot(pam_data, mod_pam1$cluster, color=T, shade=T, labels=2, lines=0, cex=.6)
## Wohooo !! 75% variability explained with just 2 clusters
## Both the clusters are distinct and no overlap observed
## Going ahead with PAM model cluster with k=2 as our final clustering model



fviz_cluster(mod_pam1,
             palette = c("red", "blue"), # color palette
             ellipse.type = "t", # concentration ellipse
             repel = F, # label overplotting, TRUE is very slow
             ggtheme = theme_classic()
)




#Once we have done our clustering, we try to look at the clusters individually
#Try to identify the distinct characteristics of each clusters to share insights about customers

View(pam_data)
pam_data$ID <- seq.int(nrow(pam_data))
Dunn_Final$ID<-seq.int(nrow(Dunn_Final))
clustered_data<-pam_data[,c(8,9)]
a<-merge(Dunn_Final,clustered_data,by="ID")
table(a$pam)
#1      2    
#1022  1444   

#Equal proportion of members in both clusters

#Let's take a close look at sales, products, recency, coupons, visits, type of products purchased for these clusters
View(Dunn_Final)
#Aggregating data at household level
library(dplyr)
str(a)
a$pam<-as.factor(a$pam)
str(a)
analysis_C <- a %>% group_by(pam) %>% summarize(avg_sales = mean(total_sales), avg_prods=mean(products_purchased), avg_disc=mean(COUPON_DISCOUNT), avg_visits=mean(total_visits),avg_perc_national=mean(BrandNational), avg_recency=mean(recency),tot_members=n(),redemption=mean(No_of_coupon_redemptions))
View(analysis_C)
#Cluster 1- lower sales, less products, less recent, more % of national products, less discounts
#This cluster has potential to grow and can be targeted so that customers move from cluster 1 to cluster 2

#Cluster 2- Cream members in these clusters, high values for KPIs such as sales, products, visits, redemptions, discounts
# They have low % of national products
# We have earlier found national products drive sales but this cluster is buying more of private products
# Business can decide to target this cluster with more offers for national products since their redemption is much better than cluster 1


# Time Series Forecasting

library(dplyr)
library(lubridate)
library(xts)
library(zoo)
library(forecast)
library(tseries)

#load TS.rdata

View(ts1)
#convert date to class date
ts1$DATE<-as.Date(ts1$DATE,format='%m/%d/%Y')
View(ts1)
#aggregating sales by date
ts_daily <- ts1  %>% group_by(DATE) %>%summarize(daily_sales = sum(SALES_VALUE))
#converting into year month
ts_daily$yearmon<-as.yearmon(ts_daily$DATE)
#aggregating  sales at year month level
ts_monthly <- ts_daily%>% group_by(yearmon) %>%summarize(tot_sales = mean(daily_sales))
View(ts_monthly)

# Convert to time series data 
dunn_ts <- ts(ts_monthly[,2], start=c(2014,1), end=c(2015,12), freq=12)
#Dunn_monthly_ts <- ts(Dunn_monthly_obs[,2], start=c(2015,1), end=c(2017,2), freq=12)

# Plot individual time components 
par(mfrow=c(1,2))
monthplot(dunn_ts) # by month 
seasonplot(dunn_ts, year.labels=T, col=seq(1:11)) # by season 

dev.off()

dec_dunn <- decompose(dunn_ts, type="additive") 
plot(dec_dunn) # seasonality is visible, trend looks constant

# Convert vector to time series object
ts_V3 <- as.ts(ts_daily[,2], start=c(2014,1), frequency=12)

# Plot with regression line
RegLine <- lm(ts_V3 ~ as.numeric(1:length(ts_V3)))
plot.ts(ts_V3, xlab="Day", ylab = "Sales",
        main = "Sales vs. Day")
abline(RegLine, col = "red")

#the regression line fits into the data for most the data points





#Removing first 90 days because of anomaly in data
ts<-ts(ts_daily[91:711,2])
View(ts)

############Forecasting Daily Sales for Next 30 Days##############

#Naive Forecasting
naive_sales <- meanf(ts, h=30) # mean forecast 
names(naive_sales) 
# we get 80% and 95% CI 
naive_sales$lower
naive_sales$upper 
head(naive_sales$fitted)
plot(naive_sales$residuals)
accuracy(naive_sales)

#naive model
naive_sales <- naive(ts, h=30) # mean forecast 
names(naive_sales) 
naive_sales$lower
naive_sales$upper 
head(naive_sales$fitted)
head(ts)
head(naive_sales$residuals)
plot(naive_sales) 
accuracy(naive_sales)



#snaive model
naive_sales <- snaive(ts, h=30) 
names(naive_sales) 
naive_sales$lower
naive_sales$upper 
head(naive_sales$fitted)
head(ts)
head(naive_sales$residuals)
plot(naive_sales) 
accuracy(naive_sales)


# Tripple 
# using ets: Error, Trend, Seasonality
ets_ts <- ets(ts) 

shapiro.test(ets_ts$residuals)#p value is less than 5%
qqnorm(ets_ts$residuals)# residuals normally distributed
qqline(ets_ts$residuals) 
accuracy(ets_ts) 
plot(ets_ts)
forecast(ets_ts, 30)

# Differencing 
# compute the differences between consecutive data points to get stationarity 
# First, check for trend 
ndiffs(ts) # 1
adf.test(ts) # opposite result 
#stationary
View(ts)
str(ts)
ts_arima <- auto.arima(ts) # may take a while 
ts_arima #ARIMA (5,1,1) 
shapiro.test(ts_arima$residuals) #passes the test
qqnorm(ts_arima$residuals)
qqline(ts_arima$residuals) # excellent for the most part but there is outlier 
accuracy(ts_arima) #RMSE also reduces
plot(forecast(ts_arima, 30))


##Use differencing based on ndiff results
#transforming data

transformed_ts<-diff(ts,1)
ndiffs(transformed_ts)# the data is tranformed

ts_arima1 <- auto.arima(transformed_ts)  
ts_arima1 

#values are getting converted into negative sales value
# did not go ahead with forecasting of sales using transformed data
shapiro.test(ts_arima1$residuals) 
qqnorm(ts_arima1$residuals)
qqline(ts_arima1$residuals) 
accuracy(ts_arima1) 
plot(forecast(ts_arima1, 30))

# THANK YOU !



