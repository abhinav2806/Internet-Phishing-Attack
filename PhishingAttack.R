library("rpart")
library("ggplot2")
library("tidyverse")
library("psych")
library("corrplot")
library("RColorBrewer")

#Load Dataset

q5=read.csv("/Users/abhinavram/Documents/IDS572 Data Mining/Assignment 1/Q5.csv")


#Summary of dataset

summary(q5)

#Starting elements of dataset

head(q5)

#Checking for missing values

sapply(q5, function(x) sum(is.na(x)))

#Split data into training and test

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(q5), replace=TRUE, prob=c(0.7,0.3))
q5_train  <- q5[sample, ]
q5_test   <- q5[!sample, ]

q5_train = select(q5_train, -id)
q5_test = select(q5_test, -id)
typeof(q5_test)
#Correlation Plot

cor_graph_phishing <- cor(q5_train)
corrplot(cor_graph_phishing, type="upper",order= "hclust", tl.cex = 0.7,col=brewer.pal(n=8, name="RdYlBu"))

# Box Plot

boxplot(q5_train)
#No outliers in this data

#Decision Tree

library(rpart.plot)
fit_phishing=rpart(Result~.,data=q5_train, parms = list(split="information"), method = 'class')
rpart.plot(fit_phishing, extra = 106)

q5_train$Result <- as.numeric(q5_train$Result)
cor_graph_phishing <- cor(q5_train)
corrplot(cor_graph_phishing, type="upper",order= "hclust",tl.cex = 0.7,col=brewer.pal(n=8, name="RdYlBu"))

#Confusion Matrix for combined

t_pred_phishing=predict(fit_phishing, q5_train, type='class')
table(q5_train$Result, t_pred_phishing)
confusion_mat_phishing_dt = table(q5_train$Result, t_pred_phishing)
acc_phishing_dt = sum(diag(confusion_mat_phishing_dt))/sum(confusion_mat_phishing_dt)

#ggplot

df_phishing=data.frame(imp=fit$variable.importance)
df2_phishing=df_phishing %>%
  tibble::rownames_to_column()%>%
  dplyr::rename("variable"= rowname)%>%
  dplyr::arrange(imp)%>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) + 
  geom_col(aes(x=variable, y=imp), col="black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

#Confusion Matrix for test

t_pred_phishing_test=predict(fit_phishing, q5_test, type='class')

table(q5_test$Result, t_pred_phishing_test)
confusion_mat_test_phishing_dt = table(q5_test$Result, t_pred_phishing_test)
accTest_phishing_dt = sum(diag(confusion_mat_test_phishing_dt))/sum(confusion_mat_test_phishing_dt)

#Random Forest
library(party)
library(randomForest)

# Create the forest.
#output.forest <- randomForest(Result~., data = q5_train)

fit_rf=randomForest(Result~ SSLfinal_State + URL_of_Anchor + web_traffic , data=q5_train, ntree=500,
                    importance=TRUE, proximity=TRUE)
# View the forest results.
t_pred_rf=predict(fit_rf, q5_test, type='class')

table(q5_test$Result, t_pred_rf)
confusion_mat_test_phishing_rf = table(q5_test$Result, t_pred_rf)
print(confusion_mat_test_phishing_rf)
plot(fit_rf)


# Importance of each predictor.
out.importance <- round(importance(fit_rf), 2)
print(out.importance )

#Graph of RF Model
varImpPlot(fit_rf)


