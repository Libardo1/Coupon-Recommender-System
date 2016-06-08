# Check the performance
library(ROCR)
fit.pr = predict.glm(lm_model, newdata =  visit.val)
fit.pred = prediction(fit.pr, visit.val$PURCHASE_FLG)
fit.perf = performance(fit.pred,"tpr","fpr")
plot(fit.perf,lwd=2,col="blue",
     main="ROC")
abline(a=0,b=1)
performance(fit.pred, "auc")

# Plot lift chart
fit.perf <- performance(fit.pred,"lift","rpp")
plot(fit.perf, main="lift curve", colorize=F)
