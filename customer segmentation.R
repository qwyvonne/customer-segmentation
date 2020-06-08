#Clustering 
seg = read.csv("C:/Users/qw508/OneDrive/Documents/Mindshare Case Competition/cleaned.csv")
str(seg)
#Cleaning Data

#Online_shopping
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.completely
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.somewhat = 2*seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.somewhat
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.somewhat = 3*seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.somewhat
seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.completely = 4*seg$I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.completely
seg$online_shopping = seg %>%
  select(`I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.completely`,
         `I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Disagree.somewhat`,
         `I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.somewhat`,
         `I.m.willing.to.use.the.Internet.to.shop.for.fashion.products...Agree.completely`) %>%
  rowSums()
seg$online_shopping

#loyalty
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.completely
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.somewhat = 2*seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.somewhat
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.somewhat = 3*seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.somewhat
seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.completely = 4*seg$I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.completely
seg$loyalty = seg %>% 
  select(`I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.completely`,
         `I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Disagree.somewhat`,
         `I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.somewhat`,
         `I.am.loyal.to.only.a.few.fashion.brands.and.stick.with.them..Agree.completely`) %>%
  rowSums()

seg$loyalty
#price_sensitive
seg$I.only.spend.what.I.budget.on.fashion.items...Disagree.completely
seg$I.only.spend.what.I.budget.on.fashion.items...Disagree.somewhat = 2*seg$I.only.spend.what.I.budget.on.fashion.items...Disagree.somewhat
seg$I.only.spend.what.I.budget.on.fashion.items...Agree.somewhat = 3*seg$I.only.spend.what.I.budget.on.fashion.items...Agree.somewhat
seg$I.only.spend.what.I.budget.on.fashion.items...Agree.completely = 4*seg$I.only.spend.what.I.budget.on.fashion.items...Agree.completely
seg$price_sensitive = seg %>%
  select(`I.only.spend.what.I.budget.on.fashion.items...Disagree.completely`,
         `I.only.spend.what.I.budget.on.fashion.items...Disagree.somewhat`,
         `I.only.spend.what.I.budget.on.fashion.items...Agree.somewhat`,
         `I.only.spend.what.I.budget.on.fashion.items...Agree.completely`) %>%
  rowSums()
seg$price_sensitive

#seasonality
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.completely
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.somewhat = 2*seg$I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.somewhat
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.somewhat = 3*seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.somewhat
seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.completely = 4*seg$I.buy.new.clothes.at.the.beginning.of.each.season...Agree.completely
seg$seasonality = seg %>%
  select(`I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.completely`,
         `I.buy.new.clothes.at.the.beginning.of.each.season...Disagree.somewhat`,
         `I.buy.new.clothes.at.the.beginning.of.each.season...Agree.somewhat`,
         `I.buy.new.clothes.at.the.beginning.of.each.season...Agree.completely`) %>%
  rowSums()
seg$seasonality

#look(is important than brand)
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.completely
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.somewhat = 2*seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.somewhat
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.somewhat = 3*seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.somewhat
seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.completely = 4*seg$When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.completely
seg$look = seg %>%
  select(`When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.completely`,
         `When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Disagree.somewhat`,
         `When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.somewhat`,
         `When.buying.fashion.products_.the.overall.look.is.more.important.than.the.brand...Agree.completely`) %>%
  rowSums()
seg$look

#Hierarchical clustering 
data_cluster = seg[,c("online_shopping","loyalty","price_sensitive","seasonality","look")]
data_cluster = scale(data_cluster)
head(data_cluster)
d = dist(data_cluster, method = "euclidean")
clusters = hclust(d = d, method = "ward.D2")
plot(clusters)
cor(cophenetic(clusters),d) #0.4631542 indicates moderate fit
plot(cut(as.dendrogram(clusters),h=5)$upper)
#two clusters solution
plot(clusters)
rect.hclust(tree = clusters, k = 2, border = "tomato")
#three cluster solution 
plot(clusters)
rect.hclust(tree = clusters, k = 3, border = "tomato")
#four cluster solution 
plot(clusters)
rect.hclust(tree = clusters, k = 4, border = "tomato")
#five cluster solution 
plot(clusters)
rect.hclust(tree = clusters, k = 5, border = "tomato")

#Visualization 
h_segments = cutree(tree = clusters, k = 3)
table(h_segments) 

#We know we will end with three clusters(3 products), choose k = 3
library(psych)
library(ggplot2)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = "varimax")$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp, aes(x=factor1, y = factor2, col = cluster))+geom_point()

#Similarly 
library(cluster)
clusplot(data_cluster,
         h_segments,
         color = T,shade= T,labels = 4, lines = 0, main = "Hierarchical Cluster Plot")

# K-means clustering 
set.seed(617)
km = kmeans(x = data_cluster,centers = 3,iter.max = 10000, nstart = 25)
table(km$cluster)

within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#Silhouette Plot indicates a cluster of 4 to be the optimal
set.seed(617)
km = kmeans(x = data_cluster,centers = 4,iter.max=10000,nstart=25)
k_segments = km$cluster
table(k_segments)

library(psych)
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

library(cluster)
clusplot(data_cluster,
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')
#Model based clustering
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)
clusters_mclust_3 = Mclust(data_cluster,G=3)
summary(clusters_mclust_3)
clusters_mclust_3$bic
mclust_bic = -sapply(1:10,FUN = function(x) Mclust(data_cluster,G=x)$bic)
mclust_bic
ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#the plot indicates 2 clusters （3 clusters also has a low bic)
m_clusters = Mclust(data = data_cluster,G = 2)
m_segments = m_clusters$classification
table(m_segments)
library(psych)
temp = data.frame(cluster = factor(m_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

library(cluster)
clusplot(data_cluster,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='mclust Cluster Plot')

table(h_segments)
table(k_segments)
table(m_segments)

#Profile clusters
data_cluster2 = cbind.data.frame(data_cluster,h_segments, k_segments,m_segments)
#?????出现了一个error导致dyplr无法用但是把cbind换成cbind.data.frame就好了
#http://www.milanor.net/blog/reshape-data-r-tidyr-vs-reshape2/
str(data_cluster2)
library(dplyr); library(ggplot2); library(tidyr)
data_cluster2 %>%
  select(buzz_score:consideration_score,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,buzz_score:consideration_score)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
data_cluster2 %>%
  select(buzz_score:consideration_score,k_segments) %>%
  group_by(k_segments) %>% 
  filter(impression_score<0)
#data_cluster standardize之后出现了很多负数。。。是可能的

prop.table(table(data_cluster2$k_segments,data_cluster2[,6]),1)

seg$Men = factor(seg$Men, labels = c("woman", "man"))
seg$Women = factor(seg$Women, labels = c("man","woman"))
seg$Educ..attended.college = factor(seg$Educ..attended.college, labels = c("other","attend college"))
seg$Educ..graduated.college.plus = factor(seg$Educ..graduated.college.plus, labels = c("other","graduate college plus"))
seg$Educ..no.college = factor(seg$Educ..no.college, labels = c("other","no college"))
seg$Educ..post.graduate = factor(seg$Educ..post.graduate, labels = c("others","post graduate"))
seg$Educ..did.not.graduate.HS = factor(seg$Educ..did.not.graduate.HS, labels = c("other","did not graduate high school"))
seg$Educ..graduated.high.school = factor(seg$Educ..graduated.high.school, labels = c("other","high school"))
seg$Age.18.24 = factor(seg$Age.18.24, labels = c("other","18 - 24"))
seg$Age.25.34 = factor(seg$Age.25.34, labels = c("other", "25 - 34"))
seg$Age.35.44 = factor(seg$Age.35.44, labels = c("other","35 - 44"))
seg$Age.45.54 = factor(seg$Age.45.54, labels = c("other","45 - 54"))
seg$Age.55.64 = factor(seg$Age.55.64, labels = c("other","55 - 64"))
seg$Age.65. = factor(seg$Age.65., labels = c("other","65 and above"))




#Competitive Spending 
spending = read.csv("/Users/qingyuanwang/Documents/Mindshare Case Competition/case_competition_competitive_spend.csv")
#converting wide data to tall data using gather()
library(tidyr)
spending_tall = 
  spending %>% 
  gather("Date","Competitive Spending",2:105)
spending_tall
summary(spending_tall$Date)
class(spending_tall$Date)
library(stringr)
spending_tall$Date = str_remove(spending_tall$Date,"X") 
#https://www.statmethods.net/input/dates.html %Y four digits year
spending_tall$Date = as.Date(spending_tall$Date, '%m.%d.%Y')
#Filtering out CityDweller and plot the spending 
library(dplyr)
citydweller_spending = 
  spending_tall %>% 
  filter(Competitors.for == "CityDweller") %>%
  group_by(Competitors.for) 
citydweller_spending
library(ggplot2)
ggplot(citydweller_spending, aes(x = Date, y = `Competitive Spending`)) +
  geom_point() +
  ggtitle("CityDweller Competitive Spending")

#Filtering out WaveMaker and plot the spending 
wavemaker_spending = 
  spending_tall %>% 
  filter(Competitors.for == "WaveMaker") %>%
  group_by(Competitors.for) 
wavemaker_spending
ggplot(wavemaker_spending, aes(x = Date, y = `Competitive Spending`)) + 
  geom_point() + 
  ggtitle("WaveMaker Competitive Spending")

#Filtering out TimePeace and plot the spending 
timepeace_spending = 
  spending_tall %>% 
  filter(Competitors.for == "TimePeace") %>%
  group_by(Competitors.for) 
timepeace_spending
library(ggplot2)
ggplot(timepeace_spending, aes(x = Date, y = `Competitive Spending`)) +
  geom_point() + 
  ggtitle("TimePeace Competitive Spending")

library(readxl)
setwd("/Users/qingyuanwang/Documents/Mindshare Case Competition/")
search_interest = read_xlsx("Product Search Interest Index.xlsx",col_names = TRUE,skip = 2)
search_interest[search_interest == ""] <- NA
search_interest = na.omit(search_interest)

#Search Interest Distribution 
ggplot(search_interest, aes(x=search_interest$CityDweller)) + 
  geom_histogram(color="black", fill="pink",stat="count") +
  xlab("CityDweller Search Interest") +
  ylab("Number of Search")

ggplot(search_interest, aes(x=search_interest$WaveMaker)) + 
  geom_histogram(color="black", fill="blue",stat="count") +
  xlab(" WaveMaker Search Interest") +
  ylab("Number of Search")

ggplot(search_interest, aes(x=search_interest$TimePeace)) + 
  geom_histogram(color="black", fill="yellow",stat="count") +
  xlab("TimePeace Search Interest") +
  ylab("Number of Search")

# Interest of each product through the year 
ggplot(search_interest,aes(x = Week, y = CityDweller)) + 
  geom_point(position = "jitter") +
  ggtitle("CityDweller Search Interest")

ggplot(search_interest,aes(x = Week, y = WaveMaker)) + 
  geom_point(position = "jitter") +
  ggtitle("WaveMaker Search Interest")

ggplot(search_interest,aes(x = Week, y = TimePeace)) + 
  geom_point(position = "jitter") +
  ggtitle("TimePeace Search Interest")

#感觉trend看得更清楚
plot(search_interest$CityDweller) # seasonal
plot(search_interest$WaveMaker) # increasing trend
plot(search_interest$TimePeace) #flat(0), then a peak 
