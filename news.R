library(party)
database<-OnlineNewsPopularity
shares<-database[61] #no of shares
shares <- na.omit(shares)

#shares <- scale(shares)
clusters<-kmeans(shares,5) #clustering to get the five classes from no of shares
aggregate(shares,by=list(clusters$cluster),FUN=mean)
shares<- data.frame(shares, clusters$cluster)
database[61]<-shares[2]

data<-database[c(1:30000),] #training data
test<-database[c(30001:39644),] #testing data

#write.csv(shares[2],"clusters.csv")
#write.csv(data,"data.csv")
data1<-NULL
for(i in c(1:5))
{
  newdata<-subset(data,shares==i) #taking samples from each clusters
if(nrow(newdata)<=3000)
{
  data1<-rbind(data1,newdata)
}else
{
  data1<-rbind(data1,newdata[sample(nrow(data), 3000), ])
}
}

#data1<-data[sample(nrow(data), 3000), ] 
data1 <- na.omit(data1) #Omitting NA's getting added during sampling
a<-ctree_control(mincriterion = 0.5,minbucket = 5,minsplit = 5)
tr<-ctree(shares~timedelta+ n_tokens_title+ n_tokens_content+ n_unique_tokens+ n_non_stop_words+ n_non_stop_unique_tokens+ num_hrefs+ num_self_hrefs+ num_imgs+ num_videos+ average_token_length+ num_keywords+ data_channel_is_lifestyle+ data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_socmed+ data_channel_is_tech+ data_channel_is_world+ kw_min_min+ kw_max_min+ kw_avg_min+ kw_min_max+ kw_max_max+ kw_avg_max+ kw_min_avg+ kw_max_avg+ kw_avg_avg+ self_reference_min_shares+ self_reference_max_shares+ self_reference_avg_sharess+ weekday_is_monday+ weekday_is_tuesday+ weekday_is_wednesday+ weekday_is_thursday+ weekday_is_friday+ weekday_is_saturday+ weekday_is_sunday+ is_weekend+ LDA_00+ LDA_01+ LDA_02+ LDA_03+ LDA_04+ global_subjectivity+ global_sentiment_polarity+ global_rate_positive_words+ global_rate_negative_words+ rate_positive_words+ rate_negative_words+ avg_positive_polarity+ min_positive_polarity+ max_positive_polarity+ avg_negative_polarity+ min_negative_polarity+ max_negative_polarity+ title_subjectivity+ title_sentiment_polarity+ abs_title_subjectivity+ abs_title_sentiment_polarity,data=data1,controls = a)

testresult=predict(tr,test)
results=testresult-test[,61]<0.5
View(results)