library(pROC)

fit <- glm(HeartDisease~.,family = binomial(link = "logit") ,data = heart.train)
pred.dt <- predict.glm(fit, type = "response", newdata = heart.test)

auc(model.matrix(~., data = heart.test)[,2],pred.dt)
roc(model.matrix(~., data = heart.test)[,2],pred.dt, plot =T)