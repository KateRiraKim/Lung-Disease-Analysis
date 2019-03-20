#package download
install.packages("ggplot2")
install.packages("tm")

library(tm)
library(ggplot2)
library(arulesSequences)
library(arules)
library(dplyr)
library(tidyr)
library(reshape)
library(splitstackshape)


# setting the directory 
getwd()
setwd("/Users/rira/R")

data = read.csv("Data_Entry_2017 copy.csv", header = TRUE)


data$Image.Index <- NULL
data$X <- NULL
data$View.Position <- NULL
data$Height. <- NULL
data$OriginalImage.Width <-NULL
data$y. <- NULL
data$OriginalImagePixelSpacing.x <-NULL
data$Patient.Age <-NULL
data$Patient.Gender <-NULL


#remove unusable column
data<-data [!(data$Finding.Labels=="No Finding"),]


# write.csv(data, "dataset.csv")
# data_a <- data_a [!(data_a$Finding.Labels == pathology_list)]


df <- data

## below one will leave the data as I wanted.
df <- df[grepl('Atelectasis|Pneumonia', df$Finding.Labels),]


# check the dataset.
head(df)
summary(df)


# transform data to transactions 
list_df = split(df$Finding.Labels, df$Follow.up.., df$Patient.ID)
list_df
trans_df = as(list_df, "transactions")
inspect(trans_df)
summary(trans_df)





# check itemsets in sparse matrix
inspect(trans_df[1:10])

# support per item: itemFrequency()
itemFrequency(trans_df[,1:10])

# graph.
# frist graph
itemFrequencyPlot(trans_df, support = 0.3, main = "disease frequency plot above support 30%")

# second graph
itemFrequencyPlot(trans_df, topN = 10, main = "support top 10 disease")




# make the association rule.
# by adjusting support rate, list can be limited.
rules1 <- apriori(trans_df, parameter = list(support = 0.3, confidence = 0.7))
summary(rules1)
inspect(rules1)


# save as a data frame. ### trying....
df_rule = data.frame(
  lhs = labels(lhs(rules1)),
  rhs = labels(rhs(rules1)), 
  rules1@quality)

df_rule


# 
results <- df_rule[(df_rule$lhs == "{Pneumonia}") | 
                 (df_rule$lhs =="{Atelectasis}"), ]
results



write.csv(results, "results.csv")





#######################################################################################
#######################################################################################
#######################################################################################



#cf.  subsetting.
rule_interest <- subset(rules1, items %in% c("Atelectasis", "Pneumonia"))
rule_interest
inspect(rule_interest)


# save the file as a dataframe.  // doesn't work.
rules_list_df <- as.data.frame(inspect(rule_interest))

rules_list_df <- rules_list_df[order(rules_list_df$confidence, decreasing=TRUE), ]
a <- rules_list_df[(rules_list_df$lhs == "{Pneumonia}") | 
                             (rules_list_df$lhs =="{Atelectasis}"), ]






## 
# save the file as a dataframe.
rules_list_df <- as.data.frame(head(inspect(rules_df), 50))
rules_list_df <- rules_df[order(rules_list_df$confidence, decreasing=TRUE), ]
rules_list_df

# resultssss? (no working.)
a <- rules_list_df[(rules_list_df$lhs == "{Pneumonia}") | 
                     (rules_list_df$lhs =="{Atelectasis}"), ]

a


########################################################################################
########################################################################################
########################################################################################
##시도##

## dataframe
as(trans_df, "data.frame")
main_disease <- as_dataframe[(as_dataframe$items =="{Atelectasis}") | 
                               (as_dataframe$items =="{Pneumonia}") |
                               (as_dataframe$items =="{Effusion}") |
                               (as_dataframe$items =="{Infiltration}"),]



#####################################################################################
## randomly pick rows. ##

random2=sample(x=1:nrow(df), size=nrow(df))
length(random2)
#View(random2)

data_random <- df
for (i in 1:length(random2)){
  data_random[i,]=df[random2[i],]
} 
View(data_random)

df3 <- head(data_random, 850)

#trans_data = read.transactions("seq_analysis.csv", format = "basket", sep=",");
#####################################################################################

# leave only the disease that I explore into.
df <- data %>% filter(Finding.Labels == "Atelectasis" |
                        Finding.Labels == "Pneumonia")
df2 <- subset(data, Finding.Labels %in% c("Atelectasis", "Pneumonia"))
## above two remove everythin beside those word.

