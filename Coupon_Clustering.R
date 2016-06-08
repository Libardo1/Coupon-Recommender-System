
# Prepare data
coupon_train <- read.csv("coupon_list_train_en.csv", as.is = T)
coupon_test <- read.csv("coupon_list_test_en.csv", as.is = T)

coupon_train$is_train <- 1
coupon_test$is_train <- 0
#Combining both test and train data
all_coupon <- rbind(coupon_train, coupon_test)


# Prepare for cluster
str(all_coupon)

# Add attributes to be clustered here. Avoid categorical data.
attributes <- c("PRICE_RATE", "CATALOG_PRICE", "DISCOUNT_PRICE", "DISPPERIOD")
#mathcing attributes from testand train data to new data frame
coupon_cluster <- all_coupon[names(all_coupon) %in% attributes]

wss <- vector(mode = "numeric", length = 10)
for (n in 1:10) {
  wss[n] <- sum(kmeans(coupon_cluster, n)$withinss)
}
plot(1:10, wss)

coupon_kmeans <- kmeans(coupon_cluster, 4)
coupon_kmeans$centers   # 1: low price low rate, 4: high rate high price.

all_coupon$cluster <- coupon_kmeans$cluster


# Descriptive statistics
all_coupon$en_capsule <- as.factor(all_coupon$en_capsule)
all_coupon$en_genre <- as.factor(all_coupon$en_genre)
all_coupon$is_train <- as.factor(all_coupon$is_train)

show_attr <- c("en_capsule", "en_genre", "VISIT", "PURCH", "is_train", "PRICE_RATE", "CATALOG_PRICE", "DISCOUNT_PRICE")

c <- 1
summary(all_coupon[all_coupon$cluster==c, names(all_coupon) %in% show_attr])
c <- 2
summary(all_coupon[all_coupon$cluster==c, names(all_coupon) %in% show_attr])
c <- 3
summary(all_coupon[all_coupon$cluster==c, names(all_coupon) %in% show_attr])
c <- 4
summary(all_coupon[all_coupon$cluster==c, names(all_coupon) %in% show_attr])


# Create cluster table to be join later
coupon_cluster <- all_coupon[,names(all_coupon) %in% c("COUPON_ID_hash", "cluster")]
write.csv(coupon_cluster, "coupon_cluster.csv")

# Remove unused variables to save memory
rm(all_coupon)
rm(coupon_test)
rm(coupon_train)
rm(coupon_kmeans)
rm(attributes)
rm(c)
rm(n)
rm(wss)
rm(show_attr)
