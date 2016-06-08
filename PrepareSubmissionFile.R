
# Prepare submission file
output <- data.frame(USER_ID_hash = character(22873), PURCHASED_COUPONS = character(22873), stringsAsFactors = F)
batch_size <- 1000
i <- 1
while (i <= 22873) {
  
  j <- min(c(i+batch_size-1, 22873))
  a.user <- user_list[i:j,]
  test_data <- merge(a.user, coupon_test, by = NULL)
  
  test_data2 <- merge(test_data, visit.genre.count, by = c("USER_ID_hash", "en_genre"), all.x = T)
  test_data2 <- merge(test_data2, coupon_cluster, by = "COUPON_ID_hash")
  test_data2 <- merge(test_data2, visit.cluster.count, by = c("USER_ID_hash", "cluster"), all.x = T)
  test_data2[is.na(test_data2$G_PURCH),names(test_data2)=="G_PURCH"] <- 0
  test_data2[is.na(test_data2$Cl_PURCH),names(test_data2)=="Cl_PURCH"] <- 0
  test_data2$G_PURCH[test_data2$G_PURCH > 0] <- 1
  test_data2$Cl_PURCH[test_data2$Cl_PURCH > 0] <- 1
  test_data2$G_PURCH <- as.factor(test_data2$G_PURCH)
  test_data2$Cl_PURCH <- as.factor(test_data2$Cl_PURCH)
  test_data2[is.na(test_data2$C_PURCH),names(test_data2)=="C_PURCH"] <- 0
  test_data2[is.na(test_data2$VISIT),names(test_data2)=="VISIT"] <- 0
  test_data2$Same_Pref <- 0
  test_data2$Same_Pref[as.character(test_data2$en_ken) == as.character(test_data2$en_pref_name)] <- 1
  test_data2$Same_Pref <- as.factor(test_data2$Same_Pref)
  
  pr <- predict.glm(lm_model, test_data2)
  summary(pr)
  test_data <- cbind(test_data2[,names(test_data2) %in% c("USER_ID_hash", "COUPON_ID_hash")],pr)
  
  test_data <- data.table(test_data, key = "USER_ID_hash")
  test_data <- test_data[order(test_data$pr),]
  max_pr <- test_data[, tail(.SD, 10), by = USER_ID_hash]
  
  out_coupon <- max_pr[, paste(COUPON_ID_hash, collapse = " "), by = "USER_ID_hash"]
  
  output[i:j,1] <- out_coupon$USER_ID_hash
  output[i:j,2] <- out_coupon$V1
  
  rm(test_data)
  rm(test_data2)
  rm(pr)
  
  i <- i + batch_size
  write.csv(output, "../Output/output.csv")
  cat(paste(c("...", i), collapse = ""))
}