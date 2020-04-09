load("googleplaystore_cleaned.RData")
str(googleplaystore_cleaned)
googleplaystore_cleaned$Size=as.numeric(googleplaystore_cleaned$Size)
str(googleplaystore_cleaned)
reviews<-googleplaystore_cleaned$Reviews
size<-googleplaystore_cleaned$Size
price<-googleplaystore_cleaned$Price
rating<-googleplaystore_cleaned$Rating
#checkin the summary
summary(googleplaystore_cleaned)

# checking for relationships between the variables
installs<-googleplaystore_cleaned$Installs
plot(rating,installs, xlab="Rating", ylab="Installs")
round(cor(rating,installs),3)
plot(reviews,installs, xlab="Reviews", ylab="Installs")
round(cor(reviews,installs),3)
plot(size,installs, xlab="Size", ylab="Installs")
round(cor(size,installs),3)
price<-googleplaystore_cleaned$Price
plot(price,installs, xlab="Price", ylab="Installs")
round(cor(price,installs),3)
#the most popular app
library(dplyr)
library(readr)
category<-googleplaystore_cleaned$Category
catoverview<-table(category)
print(catoverview)


n_installs<-googleplaystore_cleaned%>%group_by(Category)%>%summarise(installs=mean(Installs,na.rm = TRUE))%>%ungroup()%>%top_n(10)
print(n_installs)
p<-ggplot(n_installs,aes(x=reorder(Category,installs),
                         y=installs,fill=Category))+geom_bar(stat = "identity",width =0.5)+coord_flip()+labs(x="No of installs in each Category",y="Category",title="No of installs for each category")
p+theme(legend.position = "None")


#Setting the factor: 
installs<-googleplaystore_cleaned$Installs
installsf <- factor(installs)

#splitting the data
set.seed(1234)
sp <- sample(2, nrow(googleplaystore_cleaned), replace=TRUE, prob=c(0.70,0.30))
googleplaystore_cleanedtrain<-googleplaystore_cleaned[sp==1,]
googleplaystore_cleanedtest<-googleplaystore_cleaned[sp==2,]


#regression model with reviews, size, price
model1<-lm(installs~reviews+size+price+rating, data=googleplaystore_cleanedtrain)
summary(model1)

#remove price and rating
model1<-lm(installs~reviews+size, data=googleplaystore_cleanedtrain)
summary(model1)
coef(summary(model1))
summary(model1)$coefficient
confint(model1)
anova(model1)

residmodel1<-resid(model1)
fitmodel1<- fitted(model1)
plot(fitmodel1, residmodel1, xlab=" ", ylab=" ", ylim=c(-1,1))
abline(h=0, lty=2)

predict(model1, data.frame(reviews=10000, size=300),
        se.fit = TRUE, interval="confidence")
sigma(model1)/mean(installs)

#checking test data
model1<-lm(installs~reviews+size, data=googleplaystore_cleanedtest)
summary(model1)
predict(model1, data.frame(reviews=10000, size=300),
        se.fit = TRUE, interval="confidence")
sigma(model1)/mean(installs)

# adding category
model2<-lm(installs~category+reviews+size, data=googleplaystore_cleanedtrain)
summary(model2)
coef(summary(model2))


# making prediction: number of installs vs category, reviews, size

predict(model2, data.frame(category="COMMUNICATION",reviews=10000, size=300),
        se.fit = TRUE, interval="confidence")
sigma(model2)/mean(installs)

#plot the effect                        
plot(allEffects(model2))
data.frame(Effect("category", model2))
data.frame(Effect("reviews", model2))
data.frame(Effect("size", model2))

#checking test data
model2<-lm(installs~category+reviews+size, data=googleplaystore_cleanedtest)
summary(model2)
predict(model2, data.frame(category="COMMUNICATION",reviews=10000, size=300),
        se.fit = TRUE, interval="confidence")
sigma(model2)/mean(installs)

# tree

tree_installs <- ctree(installsf~category+reviews+size, data = googleplaystore_cleanedtrain, 
                       controls = ctree_control(mincriterion=0.95,
                                                minsplit=500))
plot(tree_installs)
rt_installs <- predict(tree_installs,googleplaystore_cleanedtrain)
cfmtrn_installs <- table(rt_installs, installs)
print(cfmtrn_installs)



