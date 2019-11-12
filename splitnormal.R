require(stats); require(graphics)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(as.numeric(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)

require(VGAM)
set.seed(12345)

# Create a binormal distribution with means 10 and 20
data <- c(rnorm(100, 10, 1.5), rnorm(200, 20, 3))

# Initial parameters for minimization algorithm
# You may want to create some logic to estimate this a priori... not always easy but possible
# m, m2: Means - s, s2: SDs - w: relative weight of the first distribution (the second is 1-w)
init.params <- list(m=5, m2=8, s=1, s2=1, w=0.5)

fit <<- vglm(data ~ 1, mix2normal(eq.sd=FALSE), 
             iphi=init.params$w, imu=init.params$m, imu2=init.params$m2, 
             isd1=init.params$s, isd2=init.params$s2)

# Calculated parameters
pars = as.vector(coef(fit))
w = logit(pars[1], inverse=TRUE)
m1 = pars[2]
sd1 = exp(pars[3])
m2 = pars[4]
sd2 = exp(pars[5])

# Plot an histogram of the data
hist(data, 30, col="black", freq=F)
# Superimpose the fitted distribution
x <- seq(0, 30, 0.1)
points(x, w*dnorm(x, m1, sd1)+(1-w)*dnorm(x,m2,sd2), "l", col="red", lwd=2)