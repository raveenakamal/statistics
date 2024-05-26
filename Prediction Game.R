bnb <- read.csv("airbnb_sf_training.csv")
str(bnb)


set.seed(153)
train <- bnb[ sample(1:nrow(bnb), size = .7 * nrow(bnb), replace = F), ]
test <- bnb[(bnb$id %in% train$id) == F, ]

m1 <- lm(data = train, stays_LQ ~ availability_30   + review_scores_accuracy)
m2 <- lm(data = train, stays_LQ ~ availability_30  +  review_scores_accuracy + number_of_reviews_l30d)

test$m1_predictions <- predict(m1, newdata = test, type = 'response')
test$m1_error <- test$stays_LQ - test$m1_predictions


test_m1_sse <- sum(test$m1_error^2, na.rm = T)

test$m2_predictions <- predict(m2, newdata = test, type = 'response')
test$m2_error <- test$stays_LQ - test$m2_predictions

test_m2_sse <- sum(test$m2_error^2, na.rm = T)

print("M1 Error:"); print(test_m1_sse)
print("M2 Error:"); print(test_m2_sse)

best_model<- lm(data = train, stays_LQ ~ availability_30  +  review_scores_accuracy + number_of_reviews_l30d)
save(best_model, file = "Raveena.rda")



