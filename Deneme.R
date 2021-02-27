# Lets clean the unnecessary items
gc()
rm(list = ls(all = TRUE))


packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(tidyverse)
packages(dplyr)
packages(ggplot2)
packages(broom)
packages(rpart)
packages(rpart.plot) 
packages(rattle)
packages(cowplot)
packages(knitr)
packages(corrplot)
packages(gridExtra)
packages(GGally)
packages(cluster) # clustering algorithms 
packages(factoextra) # clustering algorithms & visualization
packages(funModeling) 
packages(Hmisc)
packages(FactoMineR)
packages(factoextra)
packages(plot3D)


getwd()
setwd("C:/Users/ALPARSLAN/Desktop/Alparslan/MERVE/irem&Onur/Project 2/Project")

cars <- read.csv('C:/Users/ALPARSLAN/Desktop/Alparslan/MERVE/irem&Onur/Project 2/Data_3000.csv', sep=";", dec=".", header=TRUE)

kable(head(cars))
str(cars)
cars[sapply(cars, is.character)] <- lapply(cars[sapply(cars, is.character)],as.factor)
str(cars)

glimpse(cars)
status(cars)
freq(cars)
plot_num(cars)
profiling_num(cars)
describe(cars)

cars %>% dplyr::select(where(is.factor)) %>%
  gather(attributes, value, 1:6) %>%
  ggplot(aes(x = value)) +
  geom_bar(fill = 'lightblue2', color = 'black') +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x="Values", y="Frequency") +
  theme_bw()

cars %>% dplyr::select(where(is.numeric)) %>%
  gather(attributes, value, 1:5) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = 'lightblue2', color = 'black') +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x="Values", y="Frequency") +
  theme_bw()

cars$Engine <- as.factor(cars$Engine)
str(cars)

corrplot(cor(cars %>% dplyr::select(where(is.numeric))), type = 'upper', method = 'number', tl.cex = 0.9)

ggplot(cars, aes(x = Price, y = Year)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_bw()

cars[cars$Price > 100000, ]$Price = median(cars$Price)

ggplot(data = cars, aes(x=Fuel_Type, y=Price, color=Fuel_Type)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Title',
       y='Price',x='Fuel_Type')

ggplot(data = cars, aes(x=Fuel_Type,y=Price, fill=Fuel_Type)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Green") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Title',
       y='Price',x='Year')+
  facet_wrap(~Gear_Type,nrow = 1)

ggplot(data = cars, aes(x=Fuel_Type,y=Price, fill=Fuel_Type)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Green") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Title',
       y='Price',x='Year')+
  facet_wrap(~Year,nrow = 1)

x <- cars$Price
y <- cars$Km
z <- cars$Engine_Capacity 
scatter3D(cars$Price, cars$Km,cars$Year,
          xlab = "Price", ylab = "Km", zlab = "Year",
          phi = 0, bty = "g",pch = 20,cex = 2,
          ticktype = "detailed", colvar=as.numeric(cars$Year))

car <- cars
head(car)
str(car)
indx <- sapply(car, is.factor)
car[indx] <- lapply(car[indx], function(x) as.numeric(factor(x)))
head(car)
str(car)

# Dimensionality Reduction
profiling_num(car)
describe(car)

# There are two columns that have zero variance -> Brand, Model
# Drop these zero variance columns for dimension reduction.
drops <- c("Brand","Model")
car <- car[ , !(names(car) %in% drops)]

## OUTLIER ANALYSIS YAPILMASI LAZIM -smacof::smacofSym()  
## REGRESSION MODEL KUR ONLARIN COEFF LERINE BAKARIAK YORUMLA

head(car)
# correlation analysis
corrplot(cor(car[sapply(car, is.numeric)]), type = 'upper', method = 'number', tl.cex = 0.9)
# From correlation analysis, there is a high correlation between Price and Year which is expected also Engine and Engine Capacity.
# Candidates for dimentionality reduction are: Price&Year and  Engine&Engine_Capacity
car <- car[!names(car) %in% c("Engine","Year")]
str(car)
corrplot(cor(car[sapply(car, is.numeric)]), type = 'upper', method = 'number', tl.cex = 0.9)


# PCA to dimentionality reduction
res.pca <- PCA(car)
# Extract eigenvalues/variances
get_eig(res.pca)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Extract the results for variables
var <- get_pca_var(res.pca)
# Coordinates of variables
kable(var$coord)
# Contribution of variables
kable(var$contrib)
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)



# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind
# Coordinates of individuals
head(ind$coord)
# Contribution of variables
head(ind$contrib)

var<-get_pca_var(res.pca)
a<-fviz_contrib(res.pca, "var", axes=1, xtickslab.rt=90) # default angle=45°
b<-fviz_contrib(res.pca, "var", axes=2, xtickslab.rt=90)
grid.arrange(a,b,top='Contribution to the first two Principal Components')

car <- car[!names(car) %in% c("WD", "Color","Seller")]
str(car)
head(car)


#### K_MEANS #####
set.seed(123)

cars_K2 <- kmeans(car, centers = 2, nstart = 25)
print(cars_K2)

fviz_cluster(cars_K2, data = car)
# Clusters to which each point is associated
cars_K2$cluster
cars_K2$centers
cars_K2$size
# Between clusters sum of square
cars_K2$betweenss

cars_K3 <- kmeans(car, centers = 3, nstart = 25)
cars_K4 <- kmeans(car, centers = 4, nstart = 25)
cars_K5 <- kmeans(car, centers = 5, nstart = 25)

p1 <- fviz_cluster(cars_K2, geom = "point", data = car) + ggtitle(" K = 2")
p2 <- fviz_cluster(cars_K3, geom = "point", data = car) + ggtitle(" K = 3")
p3 <- fviz_cluster(cars_K4, geom = "point", data = car) + ggtitle(" K = 4")
p4 <- fviz_cluster(cars_K5, geom = "point", data = car) + ggtitle(" K = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)

f1 <- fviz_nbclust(car, FUNcluster = kmeans, method = "silhouette") + 
  ggtitle("Optimal number of clusters \n K-means")
f2 <- fviz_nbclust(car, FUNcluster = kmeans, method = "wss") + 
  ggtitle("Optimal number of clusters \n K-means")
grid.arrange(f1, f2, ncol=2)

km2 <- eclust(car, k=2, FUNcluster="kmeans", hc_metric="euclidean", graph=F)
c2 <- fviz_cluster(km2, data=car, elipse.type="convex", geom=c("point")) + ggtitle("K-means with 3 clusters")
s2 <- fviz_silhouette(km2)
grid.arrange(c2, s2, ncol=2)

km3 <- eclust(car, k=3, FUNcluster="kmeans", hc_metric="euclidean", graph=F)
c3 <- fviz_cluster(km3, data=car, elipse.type="convex", geom=c("point")) + ggtitle("K-means with 3 clusters")
s3 <- fviz_silhouette(km3)
grid.arrange(c3, s3, ncol=2)


pam2 <- eclust(car, k=2 , FUNcluster="pam", hc_metric="euclidean", graph=F)
cp2 <- fviz_cluster(pam2, data=car, elipse.type="convex", geom=c("point")) + ggtitle("PAM with 2 clusters")
sp2 <- fviz_silhouette(pam2)
grid.arrange(cp2, sp2, ncol=2)

pam3 <- eclust(car, k=3 , FUNcluster="pam", hc_metric="euclidean", graph=F)
cp3 <- fviz_cluster(pam3, data=car, elipse.type="convex", geom=c("point")) + ggtitle("PAM with 3 clusters")
sp3 <- fviz_silhouette(pam3)
grid.arrange(cp3, sp3, ncol=2)





###### HIERARCHICAL #########

##
discar<- dist(car, method = "euclidean")
hcar<-hclust(discar, method="ward.D2")
plot(hcar)
groups <- cutree(hcar, k=2) 
# draw dendogram with red borders around the
rect.hclust(hcar, k=2, border="red") 

##
discar<- dist(car, method = "euclidean")
hcar<-hclust(discar, method="ward.D2")
plot(hcar)
groups <- cutree(hcar, k=3)
# draw dendogram with red borders around
rect.hclust(hcar, k=3, border="red") 

d <- dist(car, method = "manhattan") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward.D2")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=2) 
# draw dendogram with red borders around
rect.hclust(H.fit, k=2, border="red") 

d <- dist(car, method = "manhattan") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward.D2")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3)
# draw dendogram with red borders around
rect.hclust(H.fit, k=3, border="red") 



# Compute k-means clustering with k = 3
set.seed(123)
final <- kmeans(car, centers = 3, nstart = 25)
print(final$centers)

plot(car,col=final$cluster)
f1 <- fviz_cluster(final, car)  ######## 
f2 <- fviz_silhouette(silhouette(final$cluster,discar))
grid.arrange(f1, f2, ncol=2)

###checking with other distances 
hcar.a<- hclust(discar,method = "average")
hcaralabel<- cutree(hcar.a,3)
hcar.labels<- cutree(hcar,3)

car<- data.frame(car)
ggplot(car,aes(car$Price,car$Km))+geom_point(aes(col=hcar.labels),show.legend = F)+
  scale_color_gradient(low="blue", high="red")##change the colour by the scale
