library(ggplot2)
library(dplyr)
library(stringr)
library(cluster)
library(fpc)

LA_KPI_DATA = read.csv("Los_Angeles_KPIs.csv")

#Feature Scaling and normalization before doing cluster Analysis....
LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_No_Educated_High_School_2010 = ((No_Educated_High_School_2010 - mean(No_Educated_High_School_2010))/(max(No_Educated_High_School_2010) - min(No_Educated_High_School_2010))))

LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_Perc_Unemployment_2010 = ((Perc_Unemployment_2010 - mean(Perc_Unemployment_2010))/(max(Perc_Unemployment_2010) - min(Perc_Unemployment_2010))))

LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_Per_Capita_Household_Income_2010 = ((Per_Capita_Household_Income_2010 - mean(Per_Capita_Household_Income_2010))/(max(Per_Capita_Household_Income_2010) - min(Per_Capita_Household_Income_2010))))

LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_No_Educated_High_School_2015 = ((No_Educated_High_School_2015 - mean(No_Educated_High_School_2015))/(max(No_Educated_High_School_2015) - min(No_Educated_High_School_2015))))

LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_Per_Capita_Household_Income_2015 = ((Per_Capita_Household_Income_2015 - mean(Per_Capita_Household_Income_2015))/(max(Per_Capita_Household_Income_2015) - min(Per_Capita_Household_Income_2015))))

LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_Perc_Unemployment_2014  = ((Perc_Unemployment_2014 - mean(Perc_Unemployment_2014))/(max(Perc_Unemployment_2014) - min(Perc_Unemployment_2014))))

LA_KPI_DATA = LA_KPI_DATA %>%
  mutate(Norm_No_of_Art_Projects = ((No_of_Art_Projects - mean(No_of_Art_Projects))/(max(No_of_Art_Projects) - min(No_of_Art_Projects))))


#Cluster Analysis 2010
set.seed(1)
art_matrix_cluster_2010 = LA_KPI_DATA[,c(56,57,58,62)]

# Checking optimal number of clusters...
wss = (nrow(art_matrix_cluster_2010)-1)*sum(apply(art_matrix_cluster_2010,2,var))
for (i in 2:15) wss[i] = sum(kmeans(art_matrix_cluster_2010,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Cluster Analysis for 3 cluster
kmeans.fit = kmeans(art_matrix_cluster_2010,6,nstart=25)
kmeans.fit
plotcluster(art_matrix_cluster_2010,kmeans.fit$cluster)
clusplot(art_matrix_cluster_2010, kmeans.fit$cluster, color=TRUE, shade=TRUE, labels=6, lines=0)
plot(art_matrix_cluster_2010, col =(kmeans.fit$cluster +1) , main="K-Means Clustering Results (2010)", pch =20, cex =6)


#Cluster Analysis 2015
set.seed(1)
art_matrix_cluster_2015 = LA_KPI_DATA[,c(59,60,61,62)]

# Checking optimal number of clusters...
wss = (nrow(art_matrix_cluster_2015)-1)*sum(apply(art_matrix_cluster_2015,2,var))
for (i in 2:15) wss[i] = sum(kmeans(art_matrix_cluster_2015,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Cluster Analysis for 6 cluster
kmeans.fit.2015 = kmeans(art_matrix_cluster_2015,6,nstart=25)
kmeans.fit.2015
plotcluster(art_matrix_cluster_2015,kmeans.fit.2015$cluster)
clusplot(art_matrix_cluster_2015, kmeans.fit.2015$cluster, color=TRUE, shade=TRUE, labels=6, lines=0)
plot(art_matrix_cluster_2015, col =(kmeans.fit.2015$cluster +1) , main="K-Means Clustering Results (2015)", pch =20, cex =6)

#Without Normalization
#Cluster Analysis 2010
set.seed(1)
art_matrix_cluster_2010 = LA_KPI_DATA[,c(3:6)]

# Checking optimal number of clusters...
wss = (nrow(art_matrix_cluster_2010)-1)*sum(apply(art_matrix_cluster_2010,2,var))
for (i in 2:15) wss[i] = sum(kmeans(art_matrix_cluster_2010,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Cluster Analysis for 6 cluster
kmeans.fit = kmeans(art_matrix_cluster_2010,3,nstart=25)
kmeans.fit
plotcluster(art_matrix_cluster_2010,kmeans.fit$cluster)
clusplot(art_matrix_cluster_2010, kmeans.fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0)
plot(art_matrix_cluster_2010, col =(kmeans.fit$cluster +1) , main="K-Means Clustering Results (2010)", pch =20, cex =3)

#Without Normalization
#Cluster Analysis 2015
set.seed(1)
art_matrix_cluster_2015 = LA_KPI_DATA[,c(3,6,7,8)]

# Checking optimal number of clusters...
wss = (nrow(art_matrix_cluster_2015)-1)*sum(apply(art_matrix_cluster_2015,2,var))
for (i in 2:15) wss[i] = sum(kmeans(art_matrix_cluster_2015,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Cluster Analysis for 3 cluster
kmeans.fit.2015 = kmeans(art_matrix_cluster_2015,3,nstart=25)
kmeans.fit.2015
plotcluster(art_matrix_cluster_2015,kmeans.fit.2015$cluster)
clusplot(art_matrix_cluster_2015, kmeans.fit.2015$cluster, color=TRUE, shade=TRUE, labels=3, lines=0)
plot(art_matrix_cluster_2015, col =(kmeans.fit.2015$cluster +1) , main="K-Means Clustering Results (2015)", pch =20, cex =3)

#Linear Regression, Scatter Plot and Box Plot analysis

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2010))+
  geom_point()+
  ggtitle("Perc_Unemployment_2010")



lm.fit_Perc_Unemployment_2010 = lm(as.numeric(Perc_Unemployment_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2010
summary(lm.fit_Perc_Unemployment_2010)
plot(lm.fit_Perc_Unemployment_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Perc_Unemployment_2010))+
  geom_boxplot()+
  ggtitle("Perc_Unemployment_2010")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2011))+
  geom_point()+
  ggtitle("Perc_Unemployment_2011")



lm.fit_Perc_Unemployment_2011 = lm(as.numeric(Perc_Unemployment_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2011
summary(lm.fit_Perc_Unemployment_2011)
plot(lm.fit_Perc_Unemployment_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Perc_Unemployment_2011))+
  geom_boxplot()+
  ggtitle("Perc_Unemployment_2011")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2012))+
  geom_point()+
  ggtitle("Perc_Unemployment_2012")

lm.fit_Perc_Unemployment_2012 = lm(as.numeric(Perc_Unemployment_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2012
summary(lm.fit_Perc_Unemployment_2012)
plot(lm.fit_Perc_Unemployment_2012)


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2013))+
  geom_point()+
  ggtitle("Perc_Unemployment_2013")



lm.fit_Perc_Unemployment_2013 = lm(as.numeric(Perc_Unemployment_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2013
summary(lm.fit_Perc_Unemployment_2013)
plot(lm.fit_Perc_Unemployment_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Perc_Unemployment_2013))+
  geom_boxplot()+
  ggtitle("Perc_Unemployment_2013")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2014))+
  geom_point()+
  ggtitle("Perc_Unemployment_2014")

lm.fit_Perc_Unemployment_2014 = lm(as.numeric(Perc_Unemployment_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2014
summary(lm.fit_Perc_Unemployment_2014)
plot(lm.fit_Perc_Unemployment_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Perc_Unemployment_2014))+
  geom_boxplot()+
  ggtitle("Perc_Unemployment_2014")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2015))+
  geom_point()+
  ggtitle("Perc_Unemployment_2015")

lm.fit_Perc_Unemployment_2015 = lm(as.numeric(Perc_Unemployment_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2015
summary(lm.fit_Perc_Unemployment_2015)
plot(lm.fit_Perc_Unemployment_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Perc_Unemployment_2015))+
  geom_boxplot()+
  ggtitle("Perc_Unemployment_2015")

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Perc_Unemployment_2016))+
  geom_point()+
  ggtitle("Perc_Unemployment_2016")

lm.fit_Perc_Unemployment_2016 = lm(as.numeric(Perc_Unemployment_2016) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Perc_Unemployment_2016
summary(lm.fit_Perc_Unemployment_2016)
plot(lm.fit_Perc_Unemployment_2016)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Perc_Unemployment_2016))+
  geom_boxplot()+
  ggtitle("Perc_Unemployment_2016")

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2010))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2010")

lm.fit_No_Employment_in_Arts_n_Entertainment_2010 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2010
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2010)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2010))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2010")

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2011))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2011")


lm.fit_No_Employment_in_Arts_n_Entertainment_2011 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2011
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2011)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2011))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2011")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2012))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2012")

lm.fit_No_Employment_in_Arts_n_Entertainment_2012 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2012
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2012)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2012)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2012))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2012")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2013))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2013")

lm.fit_No_Employment_in_Arts_n_Entertainment_2013 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2013
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2013)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2013))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2013")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2014))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2014")

lm.fit_No_Employment_in_Arts_n_Entertainment_2014 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2014
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2014)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2014))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2014")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2015))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2015")

lm.fit_No_Employment_in_Arts_n_Entertainment_2015 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2015
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2015)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2015))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2015")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Employment_in_Arts_n_Entertainment_2016))+
  geom_point()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2016")

lm.fit_No_Employment_in_Arts_n_Entertainment_2016 = lm(as.numeric(No_Employment_in_Arts_n_Entertainment_2016) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Employment_in_Arts_n_Entertainment_2016
summary(lm.fit_No_Employment_in_Arts_n_Entertainment_2016)
plot(lm.fit_No_Employment_in_Arts_n_Entertainment_2016)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Employment_in_Arts_n_Entertainment_2016))+
  geom_boxplot()+
  ggtitle("Num_Employment_in_Arts_n_Entertainment_2016")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2000))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2000")


lm.fit_No_Educated_High_School_2000 = lm(as.numeric(No_Educated_High_School_2000) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2000
summary(lm.fit_No_Educated_High_School_2000)
plot(lm.fit_No_Educated_High_School_2000)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2000))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2000")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2010))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2010")


lm.fit_No_Educated_High_School_2010 = lm(as.numeric(No_Educated_High_School_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2010
summary(lm.fit_No_Educated_High_School_2010)
plot(lm.fit_No_Educated_High_School_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2010))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2010")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2011))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2011")

lm.fit_No_Educated_High_School_2011 = lm(as.numeric(No_Educated_High_School_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2011
summary(lm.fit_No_Educated_High_School_2011)
plot(lm.fit_No_Educated_High_School_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2011))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2011")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2012))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2012")

lm.fit_No_Educated_High_School_2012 = lm(as.numeric(No_Educated_High_School_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2012
summary(lm.fit_No_Educated_High_School_2012)
plot(lm.fit_No_Educated_High_School_2012)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2012))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2012")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2013))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2013")

lm.fit_No_Educated_High_School_2013 = lm(as.numeric(No_Educated_High_School_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2013
summary(lm.fit_No_Educated_High_School_2013)
plot(lm.fit_No_Educated_High_School_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2013))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2013")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2014))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2014")

lm.fit_No_Educated_High_School_2014 = lm(as.numeric(No_Educated_High_School_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2014
summary(lm.fit_No_Educated_High_School_2014)
plot(lm.fit_No_Educated_High_School_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2014))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2014")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2015))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2015")

lm.fit_No_Educated_High_School_2015 = lm(as.numeric(No_Educated_High_School_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2015
summary(lm.fit_No_Educated_High_School_2015)
plot(lm.fit_No_Educated_High_School_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2015))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2015")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Educated_High_School_2016))+
  geom_point()+
  ggtitle("Num_Educated_High_School_2016")

lm.fit_No_Educated_High_School_2016 = lm(as.numeric(No_Educated_High_School_2016) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Educated_High_School_2016
summary(lm.fit_No_Educated_High_School_2016)
plot(lm.fit_No_Educated_High_School_2016)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Educated_High_School_2016))+
  geom_boxplot()+
  ggtitle("Num_Educated_High_School_2016")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2010))+
  geom_point()+
  ggtitle("Num_Unemployed_2010")

lm.fit_No_Unemployed_2010 = lm(as.numeric(No_Unemployed_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2010
summary(lm.fit_No_Unemployed_2010)
plot(lm.fit_No_Unemployed_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2010))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2010")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2011))+
  geom_point()+
  ggtitle("Num_Unemployed_2011")

lm.fit_No_Unemployed_2011 = lm(as.numeric(No_Unemployed_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2011
summary(lm.fit_No_Unemployed_2011)
plot(lm.fit_No_Unemployed_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2011))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2011")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2012))+
  geom_point()+
  ggtitle("Num_Unemployed_2012")

lm.fit_No_Unemployed_2012 = lm(as.numeric(No_Unemployed_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2012
summary(lm.fit_No_Unemployed_2012)
plot(lm.fit_No_Unemployed_2012)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2012))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2012")

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2013))+
  geom_point()+
  ggtitle("Num_Unemployed_2013")

lm.fit_No_Unemployed_2013 = lm(as.numeric(No_Unemployed_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2013
summary(lm.fit_No_Unemployed_2013)
plot(lm.fit_No_Unemployed_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2013))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2013")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2014))+
  geom_point()+
  ggtitle("Num_Unemployed_2014")

lm.fit_No_Unemployed_2014 = lm(as.numeric(No_Unemployed_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2014
summary(lm.fit_No_Unemployed_2014)
plot(lm.fit_No_Unemployed_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2014))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2014")

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2015))+
  geom_point()+
  ggtitle("Num_Unemployed_2015")

lm.fit_No_Unemployed_2015 = lm(as.numeric(No_Unemployed_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2015
summary(lm.fit_No_Unemployed_2015)
plot(lm.fit_No_Unemployed_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2015))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2015")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = No_Unemployed_2016))+
  geom_point()+
  ggtitle("Num_Unemployed_2016")

lm.fit_No_Unemployed_2016 = lm(as.numeric(No_Unemployed_2016) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_No_Unemployed_2016
summary(lm.fit_No_Unemployed_2016)
plot(lm.fit_No_Unemployed_2016)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = No_Unemployed_2016))+
  geom_boxplot()+
  ggtitle("Num_Unemployed_2016")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2010))+
  geom_point()+
  ggtitle("Population_2010")

lm.fit_Population_2010 = lm(as.numeric(Population_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2010
summary(lm.fit_Population_2010)
plot(lm.fit_Population_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2010))+
  geom_boxplot()+
  ggtitle("Population_2010")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2011))+
  geom_point()+
  ggtitle("Population_2011")

lm.fit_Population_2011 = lm(as.numeric(Population_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2011
summary(lm.fit_Population_2011)
plot(lm.fit_Population_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2011))+
  geom_boxplot()+
  ggtitle("Population_2011")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2012))+
  geom_point()+
  ggtitle("Population_2012")

lm.fit_Population_2012 = lm(as.numeric(Population_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2012
summary(lm.fit_Population_2012)
plot(lm.fit_Population_2012)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2012))+
  geom_boxplot()+
  ggtitle("Population_2012")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2013))+
  geom_point()+
  ggtitle("Population_2013")

lm.fit_Population_2013 = lm(as.numeric(Population_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2013
summary(lm.fit_Population_2013)
plot(lm.fit_Population_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2013))+
  geom_boxplot()+
  ggtitle("Population_2013")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2014))+
  geom_point()+
  ggtitle("Population_2014")


lm.fit_Population_2014 = lm(as.numeric(Population_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2014
summary(lm.fit_Population_2014)
plot(lm.fit_Population_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2014))+
  geom_boxplot()+
  ggtitle("Population_2014")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2015))+
  geom_point()+
  ggtitle("Population_2015")

lm.fit_Population_2015 = lm(as.numeric(Population_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2015
summary(lm.fit_Population_2015)
plot(lm.fit_Population_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2015))+
  geom_boxplot()+
  ggtitle("Population_2015")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Population_2016))+
  geom_point()+
  ggtitle("Population_2016")

lm.fit_Population_2016 = lm(as.numeric(Population_2016) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Population_2016
summary(lm.fit_Population_2016)
plot(lm.fit_Population_2016)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Population_2016))+
  geom_boxplot()+
  ggtitle("Population_2016")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2000))+
  geom_point()+
  ggtitle("Total_Household_Income_2000")

lm.fit_Total_Household_Income_2000 = lm(as.numeric(Total_Household_Income_2000) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2000
summary(lm.fit_Total_Household_Income_2000)
plot(lm.fit_Total_Household_Income_2000)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2000))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2000")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2010))+
  geom_point()+
  ggtitle("Total_Household_Income_2010")


lm.fit_Total_Household_Income_2010 = lm(as.numeric(Total_Household_Income_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2010
summary(lm.fit_Total_Household_Income_2010)
plot(lm.fit_Total_Household_Income_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2010))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2010")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2011))+
  geom_point()+
  ggtitle("Total_Household_Income_2011")

lm.fit_Total_Household_Income_2011 = lm(as.numeric(Total_Household_Income_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2011
summary(lm.fit_Total_Household_Income_2011)
plot(lm.fit_Total_Household_Income_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2011))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2011")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2012))+
  geom_point()+
  ggtitle("Total_Household_Income_2012")


lm.fit_Total_Household_Income_2012 = lm(as.numeric(Total_Household_Income_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2012
summary(lm.fit_Total_Household_Income_2012)
plot(lm.fit_Total_Household_Income_2012)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2012))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2012")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2013))+
  geom_point()+
  ggtitle("Total_Household_Income_2013")


lm.fit_Total_Household_Income_2013 = lm(as.numeric(Total_Household_Income_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2013
summary(lm.fit_Total_Household_Income_2013)
plot(lm.fit_Total_Household_Income_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2013))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2013")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2014))+
  geom_point()+
  ggtitle("Total_Household_Income_2014")

lm.fit_Total_Household_Income_2014 = lm(as.numeric(Total_Household_Income_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2014
summary(lm.fit_Total_Household_Income_2014)
plot(lm.fit_Total_Household_Income_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2014))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2014")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2015))+
  geom_point()+
  ggtitle("Total_Household_Income_2015")


lm.fit_Total_Household_Income_2015 = lm(as.numeric(Total_Household_Income_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2015
summary(lm.fit_Total_Household_Income_2015)
plot(lm.fit_Total_Household_Income_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2015))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2015")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Total_Household_Income_2016))+
  geom_point()+
  ggtitle("Total_Household_Income_2016")

lm.fit_Total_Household_Income_2016 = lm(as.numeric(Total_Household_Income_2016) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Total_Household_Income_2016
summary(lm.fit_Total_Household_Income_2016)
plot(lm.fit_Total_Household_Income_2016)


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Total_Household_Income_2016))+
  geom_boxplot()+
  ggtitle("Total_Household_Income_2016")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2000))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2000")

lm.fit_Per_Capita_Household_Income_2000 = lm(as.numeric(Per_Capita_Household_Income_2000) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2000
summary(lm.fit_Per_Capita_Household_Income_2000)
plot(lm.fit_Per_Capita_Household_Income_2000)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2000))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2000")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2010))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2010")

lm.fit_Per_Capita_Household_Income_2010 = lm(as.numeric(Per_Capita_Household_Income_2010) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2010
summary(lm.fit_Per_Capita_Household_Income_2010)
plot(lm.fit_Per_Capita_Household_Income_2010)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2010))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2010")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2011))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2011")

lm.fit_Per_Capita_Household_Income_2011 = lm(as.numeric(Per_Capita_Household_Income_2011) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2011
summary(lm.fit_Per_Capita_Household_Income_2011)
plot(lm.fit_Per_Capita_Household_Income_2011)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2011))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2011")



LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2012))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2012")

lm.fit_Per_Capita_Household_Income_2012 = lm(as.numeric(Per_Capita_Household_Income_2012) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2012
summary(lm.fit_Per_Capita_Household_Income_2012)
plot(lm.fit_Per_Capita_Household_Income_2012)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2012))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2012")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2013))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2013")

lm.fit_Per_Capita_Household_Income_2013 = lm(as.numeric(Per_Capita_Household_Income_2013) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2013
summary(lm.fit_Per_Capita_Household_Income_2013)
plot(lm.fit_Per_Capita_Household_Income_2013)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2013))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2013")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2014))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2014")

lm.fit_Per_Capita_Household_Income_2014 = lm(as.numeric(Per_Capita_Household_Income_2014) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2014
summary(lm.fit_Per_Capita_Household_Income_2014)
plot(lm.fit_Per_Capita_Household_Income_2014)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2014))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2014")


LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=No_of_Art_Projects, y = Per_Capita_Household_Income_2015))+
  geom_point()+
  ggtitle("Per_Capita_Household_Income_2015")

lm.fit_Per_Capita_Household_Income_2015 = lm(as.numeric(Per_Capita_Household_Income_2015) ~ as.numeric(No_of_Art_Projects), data = LA_KPI_DATA)
lm.fit_Per_Capita_Household_Income_2015
summary(lm.fit_Per_Capita_Household_Income_2015)
plot(lm.fit_Per_Capita_Household_Income_2015)

LA_KPI_DATA%>%
  group_by(No_of_Art_Projects)%>%
  ggplot(aes(x=factor(No_of_Art_Projects), y = Per_Capita_Household_Income_2015))+
  geom_boxplot()+
  ggtitle("Per_Capita_Household_Income_2015")






