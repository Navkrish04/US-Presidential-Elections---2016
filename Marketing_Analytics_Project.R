library(dplyr)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(DT)
library(GGally)
library(randomForest)

primary <- read.csv("C:/MSBA/MarketingAnalytics/primary_results.csv", stringsAsFactors = FALSE)
demographics <- read.csv("C:/MSBA/MarketingAnalytics/county_facts.csv", stringsAsFactors = FALSE)
head(demographics)
votes <- primary %>%   #get the winners and the fraction of votes the won
  filter(party == "Democrat") %>% 
  group_by(state_abbreviation, county) %>% 
  summarize(winner = candidate[which.max(fraction_votes)],
            Vote = max(fraction_votes),
            votes = max(votes))

demographics %<>%
  filter(state_abbreviation %in% c("AR", "VA", "OH","CO","OK","MI","UT","MO","MA","TN")) %>% 
  select(state_abbreviation = state_abbreviation, county = area_name, 
         income = INC110213, hispanic = RHI725214,
         white= RHI825214, college = EDU685213, density = POP060210,
         Population = PST045214,FemalePop = SEX255214,Age18 = AGE295214,Age65 = AGE775214) %>% 
  # This particular mutate function is finding all the Counties name from the county_facts data, removing the 'County' term from every counties name and replacing it with no space and finally saving it in the column name County. For example, We had the county's name in the data as 'Adair County'. So the function found out the ' County", replaced it with "" and then saved it back in the coulmn name 'county' to be used in the present analysis. 
  mutate(county = gsub(" County", "", county))
head(votes)
head(demographics)

demographics$hispanic = round((demographics$hispanic*demographics$Population)/1000)
demographics$white = round((demographics$white*demographics$Population)/1000)
demographics$college = round((demographics$college*demographics$Population)/1000)
demographics$VoteAge_Pop = round(((100 - demographics$Age65-demographics$Age18)*demographics$Population)/1000)
demographics$FemalePop = round((demographics$FemalePop*demographics$Population/1000))

demographics <- demographics[,c(-8,-10,-11)]

head(demographics)

votes <- inner_join(votes, demographics, by = c("state_abbreviation","county"))
datatable(votes, class = 'compact')

votes %>% 
  group_by(winner) %>% 
  summarize(round(mean(income)), round(mean(white)), 
            round(mean(college),1),round(mean(density)),round(mean(hispanic),1),
            round(mean(FemalePop),1),round(mean(VoteAge_Pop),1))%>%      
  datatable( colnames = c(" ",  "Winner", "Income", "White (non-Hispanic)", "Colege", "Density (pop/sq m)", "Hispanic","FemalePop","VotingAge"), class = 'compact', caption = "Average County Demographics by Winner")

ggplotly(qplot(x =  white, y = college, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Winner, Race(White) and Educational Attainment"))

head(votes)

p1 = ggplot(data = votes, aes(x = winner, y = white, fill = winner))
  geom_boxplot() + coord_flip()

p2 = ggplot(data = votes, aes(x = winner, y = hispanic, fill = winner)) +
  geom_boxplot() + coord_flip()+method("lm")

grid.arrange(p1, p2, ncol  = 1)

head(votes)
library(sqldf)
library(grid)
library(gridExtra)

Hillary <- sqldf('select A.*,case when VoteAge_Pop between 18 and 35 then "Young"
                                  when VoteAge_Pop between 36 and 51 then "MiddleAge"
                                  when VoteAge_Pop between 52 and 65 then "Old" end as Age_Bin
                  from votes A where winner = "Hillary Clinton" ')
Bernie <- sqldf('select A.*,case when VoteAge_POP between 18 and 35 then "Young"
                                  when VoteAge_Pop between 36 and 51 then "MiddleAge"
                when VoteAge_Pop between 52 and 65 then "Old" end as Age_Bin
                from votes A where winner = "Bernie Sanders" ')

Hillary$Vote = Hillary$Vote*100
Bernie$Vote = Bernie$Vote*100

g1 <- qplot(x = income, y = Vote, data = Hillary, ylab = "Fraction Votes") +
  geom_smooth(method='lm')
g2 <- qplot(x = college, y = Vote, data = Hillary, ylab = "Fraction Votes") +
  geom_smooth(method='lm')
g3 <- qplot(x = hispanic, y = Vote, data = Hillary, ylab = "Fraction Votes") +
  geom_smooth(method='lm')
g4 <- qplot(x = density, y = Vote, data = Hillary, ylab = "Fraction Votes") +
  geom_smooth(method='lm')

grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)


g5 <- qplot(x = income, y = Vote, data = Bernie, ylab = "Fraction Votes") +
  geom_smooth(method='lm')
g6 <- qplot(x = college, y = Vote, data = Bernie, ylab = "Fraction Votes") +
  geom_smooth(method='lm')
g7 <- qplot(x = hispanic, y = Vote, data = Bernie, ylab = "Fraction Votes") +
  geom_smooth(method='lm')
g8 <- qplot(x = density, y = Vote, data = Bernie, ylab = "Fraction Votes") +
  geom_smooth(method='lm')

grid.arrange(g5,g6,g7,g8,nrow=2,ncol=2)

ggplotly(qplot(x =  white, y = hispanic, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Winner, Percentage of Whites and Non-Hispanic Whites"))

ggplotly(qplot(x =  income, y = college, data = votes, 
               color = winner, size = Vote) +
           ggtitle("Counties by Income, Educational Attainment colored by Winner"))

votes$winner <- as.factor(votes$winner)
ggplot(data = votes, aes(x = winner, y = density, fill = winner)) +
  geom_boxplot() + coord_flip()

ggplotly(qplot(x =  income, y = college, data = votes, 
               color = winner, size = votes) +
           ggtitle("Counties by Income, Educational Attainment colored by Winner"))

##### Segmentation Analysis###############
head(votes)
seg.votes <- votes[,c(-1,-2,-4,-5)]
head(seg.votes)
seg.votes$winner <- ifelse(seg.votes$winner=="Hillary Clinton",1,0)
set.seed(100)
seg.k = kmeans(seg.votes,centers = 3)

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

boxplot(seg.votes$income ~ seg.k$cluster,ylab="Income",xlab="cluster")
boxplot(seg.votes$hispanic ~ seg.k$cluster,ylab="Hispanic",xlab="cluster")
boxplot(seg.votes$FemalePop ~ seg.k$cluster,ylab="FemalePop",xlab="cluster")

library(cluster)
clusplot(seg.votes, seg.k$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="K-means cluster plot")

ct.km = table(seg.votes$winner,seg.k$cluster)
ct.km
seg.summ(seg.votes,seg.k$cluster)

seg.votes$cluster = seg.k$cluster
head(seg.votes)

################### Model Building ###############
head(votes)
votes.class <- votes[,c(-1,-2,-4,-5)]
train_num = sample(1:nrow(votes.class),nrow(votes.class)*0.80)
test_num = -train_num
crosstrain = votes.class[train_num,]
crosstest = votes.class[test_num,]

str(crosstrain)

#Model1 - Decision Tree
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(partykit)
library(party)
set.seed(100)

Votes.tree <- rpart(winner~.,data=crosstrain,method="class")
fancyRpartPlot(Votes.tree,palettes=c("Greys", "Oranges"))

summary(Votes.tree)
printcp(Votes.tree)

plot(Votes.tree,uniform=TRUE,main="Classification Tree")
text(Votes.tree,use.n = TRUE,all = TRUE,cex=0.8)

#Prediction
pred.tree = predict(Votes.tree,crosstest,type = "class")
crosstab = table(crosstest$winner,pred.tree)

#Accuracy
(crosstab[1,1]+crosstab[2,2])/dim(crosstest)[1]  # 64.33% Accuracy 

### Model2. Random Forest ###

library(randomForest)
library(MASS)
set.seed(100)
crosstrain$winner = as.factor(crosstrain$winner)
votes.rf = randomForest(winner~.,data=crosstrain,ntree=750,importance=TRUE)
library(cluster)

#Prediction
votes.rf.class = predict(votes.rf,crosstest,predict.all = TRUE)
n = dim(crosstest)[1]

pred.rf = rep(0,n)
for(i in 1:n)
{
  tmp = table(votes.rf.class$individual[i,])
  pred.rf[i]=names(tmp)[which.max(tmp)]
}

crosstab1 = table(crosstest$winner,pred.rf)
crosstab1

#Accuracy
dim(crosstest)
(crosstab1[1,1]+crosstab1[2,2])/dim(crosstest)[1] # 70% Accuracy

importance(votes.rf)

varImpPlot(votes.rf,main="Variable Importance")

