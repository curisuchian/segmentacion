install.packages("liquidSVM")
library(liquidSVM)

model <- mcSVM(Species ~ ., iris)
predict(model, iris)

model <- lsSVM(Height ~ ., trees)
y <- predict(model, trees)

model <- svmQuantileRegression(Height ~ ., trees)
y <- test(model, trees)

yaboty1 <- read_delim("E:/CHB/Segmentación por texturas/yaboty1.jpeg","\t", escape_double = FALSE, col_types = cols_only(`????????????` = col_integer()), trim_ws = TRUE)