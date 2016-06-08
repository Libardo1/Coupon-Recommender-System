library(data.table)

attrs <- c("COUPON_ID_hash", "PRICE_RATE", "CATALOG_PRICE", "DISCOUNT_PRICE", "en_genre", "en_ken")
coupon_train <- coupon_list_train[,names(coupon_train) %in% attrs]
coupon_test <- coupon_test[,names(coupon_test) %in% attrs]

#Hypothesis 2
attrs_list <- c("COUPON_ID_hash", "PRICE_RATE")
coupon_train1 <- coupon_train[,names(coupon_train) %in% attrs_list]
attrs_visit <- c("VIEW_COUPON_ID_hash", "PURCHASE_FLG")
coupon_train2 <- visit[,names(visit) %in% attrs_visit]

hypothesis_2 <- merge(coupon_train1,coupon_train2, by.x = "COUPON_ID_hash",
                      by.y = "VIEW_COUPON_ID_hash")
hypothesis_2 <- table(hypothesis_2$PRICE_RATE,hypothesis_2$PURCHASE_FLG)
write.csv(hypothesis_2,"hypothesis2.csv")

#Hypothesis 3
attrs_list2 <- c("COUPON_ID_hash", "VALIDPERIOD")
coupon_train3 <- coupon_train[,names(coupon_train) %in% attrs_list2]
attrs_visit <- c("VIEW_COUPON_ID_hash", "PURCHASE_FLG")
coupon_train2 <- visit[,names(visit) %in% attrs_visit]

hypothesis_3 <- merge(coupon_train3,coupon_train2, by.x = "COUPON_ID_hash",
                      by.y = "VIEW_COUPON_ID_hash")
hypothesis_3 <- table(hypothesis_3$VALIDPERIOD,hypothesis_3$PURCHASE_FLG)
write.csv(hypothesis_3,"hypothesis3.csv")

# Number of visit and purchase of each coupon
attrs <- c("PURCHASE_FLG", "VIEW_COUPON_ID_hash", "USER_ID_hash")
visit <- visit[,names(visit) %in% attrs]
visit_table <- data.table(visit)
group_table <- visit_table[,list(C_VISIT = .N, C_PURCH = sum(PURCHASE_FLG==1)),by=VIEW_COUPON_ID_hash]
coupon_train <- merge(coupon_train, group_table, by.x = "COUPON_ID_hash", by.y = "VIEW_COUPON_ID_hash", all.x = T)
coupon_test <- merge(coupon_test, group_table, by.x = "COUPON_ID_hash", by.y = "VIEW_COUPON_ID_hash", all.x = T)
coupon_train[is.na(coupon_train)] <- 0
coupon_test[is.na(coupon_test)] <- 0
plot(coupon_train$C_VISIT, coupon_train$C_PURCH) #Looks like there is an outlier
cor(coupon_train$C_VISIT, coupon_train$C_PURCH) #0.7789

# Number of visit and purchase of each user and coupon
group_table <- visit_table[,list(VISIT = .N, PURCH = sum(PURCHASE_FLG==1)), by=list(VIEW_COUPON_ID_hash, USER_ID_hash)]
visit_user_count <- group_table 
write.csv(visit_user_count,"group_table.csv")
# Number of visit and purchanse of each genre
genre_table <- data.table(coupon_train)[,list(COUPON = .N, VISIT = sum(C_VISIT), PURCH = sum(C_PURCH)), by=en_genre]
write.csv(genre_table, "genre_table.csv")

# Number of visit and purchanse of each user
group_table <- visit_table[,list(VISIT = .N, PURCH = sum(PURCHASE_FLG==1)), by=USER_ID_hash]
user_list <- merge(user_list, group_table, by="USER_ID_hash", all.x = T, all.y = F)
hypothesis_1 <- table(user_list$AGE, user_list$SEX_ID, user_list$PURCH)
write.csv(user_list,"user_groups.csv")
hist(user_list$PURCH)
hist(user_list$VISIT)
hist(user_list$AGE)

# How many time user visit each genre
coupon.genre <- coupon_train[,names(coupon_train) %in% c("COUPON_ID_hash", "en_genre")]
coupon.genre <- rbind(coupon.genre, coupon_test[,names(coupon_test) %in% c("COUPON_ID_hash", "en_genre")])
visit.genre <- merge(visit, coupon.genre, by.x = "VIEW_COUPON_ID_hash", by.y = "COUPON_ID_hash", all.x = T, all.y = F)
visit.genre <- visit.genre[!is.na(visit.genre$en_genre),]
visit.genre <- data.table(visit.genre)
visit.genre.count <- visit.genre[,list(G_VISIT = .N, G_PURCH = sum(PURCHASE_FLG==1)), by=list(USER_ID_hash, en_genre)]
visit.genre.count <- as.data.frame(visit.genre.count)
rm(visit.genre)

# How many time user visit each cluster
visit.cluster <- merge(visit, coupon_cluster, by.x = "VIEW_COUPON_ID_hash", by.y = "COUPON_ID_hash", all.x = T, all.y = F)
visit.cluster <- visit.cluster[!is.na(visit.cluster$cluster)]
visit.cluster <- data.table(visit.cluster)
visit.cluster.count <- visit.cluster[,list(Cl_VISIT = .N, Cl_PURCH = sum(PURCHASE_FLG==1)), by=list(USER_ID_hash, cluster)]
visit.cluster.count <- as.data.frame(visit.cluster.count)
