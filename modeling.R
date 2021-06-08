library(randomForest)
data("mtcars")

input <- mtcars

# transmission
input$am <- as.factor(input$am)
levels(input$am) <-c("AT", "MT")  # Automatic and Manual


# Linear Model -------------

# build a regression model between mpg as outcome 
fit_all <- lm(mpg~., data=input)
summary(fit_all)

fit_best <- step(fit_all, direction = "backward", trace = 0)
summary(fit_best)
# miles per gallon mostly depends on three variables - wt, qsec, am


# Random Forest -------------
set.seed(4543)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
importance(mtcars.rf)
importance(mtcars.rf, type=1)

models <- list(mtcars.lm = fit_best,
               mtcars.rf = mtcars.rf)

saveRDS(models, file = "models.rds")
