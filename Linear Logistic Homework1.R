dating<-read.csv("dating_DF.csv")
head("dating_DF.csv")
pkgs <- c('dplyr', 'ggplot2')
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, new.pkgs)
library(dplyr)
library (ggplot2)

hist(dating.csv$income)
hist(dating$first_contact)

ggplot(data = dating,
       aes( x = height_taller,
            y = first_contact)) +
  geom_point(alpha = .3,
             size = 2) +
  geom_smooth(method = 'lm', se = F) +
  theme_minimal()


ggplot(data = dating,
       aes( x = yrs_education,
            y = first_contact)) +
  geom_point(alpha = .3,
             size = 2) +
  geom_smooth(method = 'glm',
              formula = y ~ x,
              method.args = list(family = 'binomial'),
              se = F) +
  theme_minimal()


hist(dating$yrs_education)
mean(dating$yrs_education)

glm(data = dating, 
    first_contact ~ yrs_education,
    family = 'binomial') |>
  summary()

probability <- exp(0.0029) - 1

int<- 1 / (1 + exp(2.35))



train <- dating[ sample(1:nrow(dating), size = .7 * nrow(dating), replace = F), ]
test <- dating[(dating$X %in% train$X) == F, ]

logistic <- glm(formula = first_contact ~ yrs_education,
                family = "binomial", 
                data = train)

linear <- lm(formula = first_contact ~yrs_education, 
             data = train)

test$predict.logistic <- predict(logistic, newdata = test, type = 'response')
test$logistic.error <- (test$first_contact - test$predict.logistic) ^ 2
test_logistic_sse <- sum(test$logistic.error, na.rm = T)
test_logistic_sse

test$predict.linear <- predict(linear, newdata = test, type = 'response')
test$predict.linear <- ifelse(test$predict.linear > 1, 1,
                              ifelse(test$predict.linear < 0, 0, test$predict.linear))
test$linear.error <- (test$first_contact - test$predict.linear) ^ 2
test_linear_sse <- sum(test$linear.error, na.rm = T)
test_linear_sse

lm(formula = first_contact ~ yrs_education, 
   data = train) |>
summary()





