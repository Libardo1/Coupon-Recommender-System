# Retrieve raw data
user_list <- read.csv("user_list_en.csv", as.is = T)
coupon_train <- read.csv("coupon_list_train_en.csv", as.is = T)
coupon_test <- read.csv("coupon_list_test_en.csv", as.is = T)
visit <- read.csv("coupon_visit_train.csv", as.is = T)

# Clean & Explore data
# User data
user_list$REG_DATE = as.Date(user_list$REG_DATE)
user_list$WITHDRAW_DATE = as.Date(user_list$WITHDRAW_DATE)
user_list$SEX_ID <- as.factor(user_list$SEX_ID)
user_list$en_pref_name <- as.factor(user_list$en_pref_name)
str(user_list)
summary(user_list)

# Coupon data
to_date <- c("DISPFROM", "DISPEND", "VALIDFROM", "VALIDEND")
for (a in to_date) {
  coupon_train[,names(coupon_train)==a] <- as.Date(coupon_train[,names(coupon_train)==a])
  coupon_test[(coupon_test)==a] <- as.Date(coupon_test[,names(coupon_test)==a])
}

to_factor <- c("USABLE_DATE_MON", "USABLE_DATE_TUE", "USABLE_DATE_WED", "USABLE_DATE_THU", "USABLE_DATE_FRI", "USABLE_DATE_SAT", "USABLE_DATE_SUN", "USABLE_DATE_HOLIDAY", "USABLE_DATE_BEFORE_HOLIDAY", "en_capsule", "en_genre", "en_small_area", "en_ken", "en_large_area")
for (a in to_factor) {
  coupon_train[,names(coupon_train)==a] <- as.factor(coupon_train[,names(coupon_train)==a])
  coupon_test[,names(coupon_test)==a] <- as.factor(coupon_test[,names(coupon_test)==a])
}

str(coupon_train)
summary(coupon_train)


# Visit Data
visit$PURCHASE_FLG <- as.factor(visit$PURCHASE_FLG)
visit$I_DATE <- as.Date(visit$I_DATE)
str(visit)
summary(visit)

