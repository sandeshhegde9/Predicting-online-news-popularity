#Converting 1's and 0's to TRUE and FAlSE
library(rpart)
data<-OnlineNewsPopularity
data$data_channel_is_lifestyle<-data$data_channel_is_lifestyle==1
data$data_channel_is_entertainment<-data$data_channel_is_entertainment==1
data$data_channel_is_bus<-data$data_channel_is_bus==1
data$data_channel_is_socmed<-data$data_channel_is_socmed==1
data$data_channel_is_tech<-data$data_channel_is_tech==1
data$data_channel_is_world<-data$data_channel_is_world==1
data$weekday_is_monday<-data$weekday_is_monday==1
data$weekday_is_tuesday<-data$weekday_is_tuesday==1
data$weekday_is_wednesday<-data$weekday_is_wednesday==1
data$weekday_is_thursday<-data$weekday_is_thursday==1
data$weekday_is_friday<-data$weekday_is_friday==1
data$weekday_is_saturday<-data$weekday_is_saturday==1
data$weekday_is_sunday<-data$weekday_is_sunday==1
data$is_weekend<-data$is_weekend==1

#Getiing shares/time taken and clustering them into 5 different popularity level.
shares1<-data[,57]/data$timedelta
data<-data.frame(data,shares1)
clusters<-kmeans(shares1,c(0.003,0.005,1,100,1000))
data<-data.frame(data,clusters$cluster)

#PCA of the data.
#a<-c(3:11,18:27,37:56)
#log.ir<-log(data[,a])

#Generation of classification tree
tree<-rpart(data=data[1:8000,],formula=clusters.cluster~n_tokens_title+ n_tokens_content+ n_non_stop_words+ num_hrefs+ num_self_hrefs+ num_imgs+ num_videos+ average_token_length+ num_keywords+ data_channel_is_lifestyle+ data_channel_is_entertainment+ data_channel_is_bus+ data_channel_is_socmed+ data_channel_is_tech+ data_channel_is_world+ kw_min_min+ kw_max_min+ kw_min_max+ kw_max_max+ kw_avg_max+ kw_min_avg+ kw_max_avg+ kw_avg_avg+ self_reference_min_shares+ self_reference_max_shares+ weekday_is_monday+ weekday_is_tuesday+ weekday_is_wednesday+ weekday_is_thursday+ weekday_is_friday+ weekday_is_saturday+ weekday_is_sunday+ is_weekend+ LDA_00+ LDA_01+ LDA_02+ LDA_03+ LDA_04+ global_subjectivity+ global_sentiment_polarity+ global_rate_positive_words+ global_rate_negative_words+ rate_positive_words+ rate_negative_words+ avg_positive_polarity+ min_positive_polarity+ max_positive_polarity+ avg_negative_polarity+ min_negative_polarity+ max_negative_polarity+ title_subjectivity+ title_sentiment_polarity+ abs_title_subjectivity+ abs_title_sentiment_polarity,method = "class",minbucket=1,minsplit=1)

#predicting of the test data
res<-predict(tree,data[8001:39644,],type=c("class"))
results<-data.frame(res)
#plotting the tree
plot(tree, compress = TRUE)
text(tree, use.n = TRUE)

clu<-data$clusters.cluster
final_res<-clu[8001:39644]==res
final_res<-data.frame(final_res)
View(final_res)