options(scipen = 999)

install.packages("dplyr")
library(dplyr)

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

str(hbcu_all)


summary(hbcu_all)


colnames(hbcu_all)

View(hbcu_all)


year = hbcu_all$Year
grad_4 = hbcu_all$`4-year - Public`
grad_2 = hbcu_all$`2-year - Public`
total_public = hbcu_all$`Total - Public`
total_male = hbcu_all$Males
total_female = hbcu_all$Females


year = as.integer(year)

hbcu_all_public = data.frame(year,total_public,grad_4,grad_2,total_male,total_female)


str(hbcu_all_public)
View(hbcu_all_public)

set.seed(256595)


hbcu_all_public1 = hbcu_all_public %>% select(grad_4,grad_2) %>% group_by(hbcu_all_public$year)


hbcu_all_kmeans = kmeans(hbcu_all_public1, centers = 2 )


hbcu_all_kmeans_25 = kmeans(hbcu_all_public1, centers = 2 , nstart = 25)

install.packages("useful")
library(useful)

plot(hbcu_all_kmeans, data = hbcu_all_public1)
plot(hbcu_all_kmeans_25, data = hbcu_all_public1)


###Installation of Packages for fviz_cluster analysis:
install.packages("factoextra")
library(factoextra)

###View of cluster analysis using dimensional view of grad 4 and grad 2 students 
fviz_cluster(hbcu_all_kmeans, data = hbcu_all_public1 )


str(hbcu_all_public1)


hbcu_all_public1 %>%
  as_tibble() %>% 
  mutate(cluster = hbcu_all_kmeans$cluster)
  ggplot(data = hbcu_all_public1,aes(x = hbcu_all_public$grad_4, y = hbcu_all_public1$grad_2, label = hbcu_all_public1$`hbcu_all_public$year`))  +
    labs(subtitle = "Graduate Program Students Enrolled Analysis", x = "4 Year Graduate Program" , y = "2 Year Graduate Program") + geom_text()

###Creating multiple clusters for graphical view:
hbcu_all_k3_public = kmeans(hbcu_all_public1, centers = 3,nstart = 25)
hbcu_all_k4_public = kmeans(hbcu_all_public1, centers = 4,nstart = 25)
hbcu_all_k5_public = kmeans(hbcu_all_public1, centers = 5,nstart = 25)
hbcu_all_k6_public = kmeans(hbcu_all_public1, centers = 6,nstart = 25)

###Plots to compare:
p1 <- fviz_cluster(hbcu_all_k3_public, geom = "point", data = hbcu_all_public1) + ggtitle("Clustering Analysis for 3 years period")
p2 <- fviz_cluster(hbcu_all_k4_public, geom = "point", data = hbcu_all_public1) + ggtitle("Clustering Analysis for 4 years period")
p3 <- fviz_cluster(hbcu_all_k5_public, geom = "point", data = hbcu_all_public1) + ggtitle("Clustering Analysis for 5 years period")
p4 <- fviz_cluster(hbcu_all_k6_public, geom = "point", data = hbcu_all_public1) + ggtitle("Clustering Analysis for 6 years period")

###Creating Grid View of clusters:
install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow = 2)


set.seed(123)

###function to compute within clusters:
wss <- function(k) {
  kmeans(hbcu_all_public1, k, nstart = 10 )$tot.withinss
}

### Compute and plot wss for k = 1 to k = 15
k.values = 1:15

###Installation package for map_dbl:
install.packages("purrr")
library(purrr)

### extract wss for 2-15 clusters
wss_values = map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, main = "Clustering Analysis 4 year Graduate vs 2 Year Graduate",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


final = kmeans(hbcu_all_public1, 4, nstart = 25)
print(final)

fviz_cluster(final, data = hbcu_all_public1)

#### To be checked because fitment
hbcu_all_kmeans_25

hbc_all_public_fitment = FitKMeans(hbcu_all_kmeans, max.clusters = 10,nstart = 25, seed=256595) 

str(k2)


###Private_Graduates Data:

year_private = hbcu_all$Year
grad_4_private = hbcu_all$`4-year - Private`
grad_2_private = hbcu_all$`2-year - Private`
total_private = hbcu_all$`Total - Private`
total_male_private = hbcu_all$Males
total_female_private = hbcu_all$Females

hbcu_all_private = data.frame(year_private,grad_4_private,grad_2_private,total_private,total_male_private,total_female_private)

str(hbcu_all_private)
View(hbcu_all_private)



