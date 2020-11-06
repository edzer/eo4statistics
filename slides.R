## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(1331)
options(digits=3)
point_col = "orange"


## ----echo=FALSE---------------------------------------------------------------
suppressPackageStartupMessages(library(gstat))
suppressPackageStartupMessages(library(stars))
suppressPackageStartupMessages(library(caret))
grd = st_as_stars(expand.grid(x = 1:100, y = 1:100))
grd$x = 1
v <- vgm(1, "Sph", 40)
x <- krige(x ~ 1, locations = NULL, newdata = grd, model = v, 
	nmax = 20, beta = 0, nsim = 6, dummy = TRUE, debug.level = 0)
x$f = factor((x < 0.5)[[1]], labels = c("Forest", "Non-Forest"))
plot(x["f",,,1], reset = FALSE, key.pos = 4, key.length = .4, key.width = lcm(5), main = NULL)
aoi = st_as_sfc(st_bbox(x))
plot(aoi, col = NA, border = 'red', add = TRUE, lwd = 2)


## ----echo=FALSE---------------------------------------------------------------
n = 50
set.seed(100)
s = st_sample(aoi, n)
plot(x["f",,,1], reset = FALSE, key.pos = 4, key.length = .4, key.width = lcm(5), main = NULL)
plot(s, add = TRUE, col = point_col, pch = 3)
v = st_extract(x["f",,,1], s)


## -----------------------------------------------------------------------------
(m = mean(v$f == "Forest"))


## -----------------------------------------------------------------------------
se = sqrt(m * (1 - m) / n)
m + qnorm(c(.025,.975)) * se


## ----echo=FALSE---------------------------------------------------------------
population_mean = mean(x["f",,,1][[1]] == "Forest")
c(population_mean = population_mean)


## ----echo=FALSE,fig.width=9.5,fig.height=2.5----------------------------------
nr = 6
layout(matrix(1:11, 1), widths = c(1,.1,1,.1,1,.1,1,.1,1,.1,1))
for (i in 1:nr) {
  plot(x["f",,,1], reset = FALSE, key.pos = NULL, main = NULL)
  plot(st_sample(aoi, n), add = TRUE, col = point_col, pch = 3)
  if (i < nr) plot.new()
}


## ----echo=FALSE,fig.width=9.5,fig.height=5------------------------------------
m = sapply(1:100, function(y) mean(st_extract(x["f",,,1], st_sample(aoi, n))$f == "Forest", 
	na.rm = TRUE))
se = sqrt(m * (1 - m) / n)
int = cbind(m + qnorm(.025) * se, m + qnorm(.975) * se)
plot(1:100, m, pch = 3, cex = .5, xlim = c(0,101), ylim = c(0, .7), xlab = "sample", ylab = "fraction forest")
segments(1:100, int[,1], y1 = int[,2])
abline(h = population_mean, lty = 2, col = 'red')


## ----echo=FALSE,fig.width=9.5,fig.height=2.5----------------------------------
nr = 6
layout(matrix(1:11, 1), widths = c(1,.1,1,.1,1,.1,1,.1,1,.1,1))
for (i in 1:nr) {
  plot(x["f",,,i], reset = FALSE, key.pos = NULL, main = NULL)
  plot(s, add = TRUE, col = point_col, pch = 3)
  if (i < nr) plot.new()
}


## ----echo=FALSE---------------------------------------------------------------
vg = variogram(f == "Forest" ~1, na.omit(v))
v.fit = fit.variogram(vg, vgm(1, "Sph", 30))
plot(vg, v.fit)


## ----echo=FALSE,fig.width=10--------------------------------------------------
kr = krige(f == "Forest" ~ 1, v, grd, v.fit, debug.level = 0)
kr$Population = x["f",,,1] == "Forest"
names(kr)[1] = "Kriging prediction"
hook = function(x) plot(s, add = TRUE, col = point_col, pch = 3)
plot(merge(kr[c(1,3)]), hook = hook, breaks = "equal", col = rev(grey(2:10/12)))


## ----echo=FALSE---------------------------------------------------------------
bl = krige(f == "Forest" ~ 1, v, aoi, v.fit, debug.level = 0)

## ----echo=FALSE---------------------------------------------------------------
c(predicted_mean = bl$var1.pred)
ci = bl$var1.pred + qnorm(c(.025,.975)) * sqrt(bl$var1.var)
setNames(ci, c("p.025", "p.975"))


## ----echo=FALSE---------------------------------------------------------------
c(estimated_mean = m <- mean(v$f == "Forest"))
se = sqrt(m * (1 - m) / n)
setNames(m + qnorm(c(.025,.975)) * se, c("p.025", "p.975"))


## ----echo=FALSE,fig.width=9.5,fig.height=2.1----------------------------------
cs <- krige(f == "Forest" ~ 1, v, grd, v.fit, 
	nmax = 20, beta = .27, nsim = 6, debug.level = 0, indicators = TRUE)
cs$f = factor((cs < 0.5)[[1]], labels = c("Forest", "Non-Forest"))
plot(cs["f"], hook = hook, key.pos = 1, key.length = .4, key.width=lcm(1))


## ----echo=FALSE,results='hide',message = FALSE, warnings = FALSE--------------
# corners <- st_bbox(x)
# spp <- expand.grid(data.frame("x"=c(corners[1],corners[3]),"y"=c(corners[2],corners[4])))
# spp <- st_as_sf(spp, coords = c('x', 'y'))
spp = st_make_grid(x, n = c(7,7))

di = st_distance(spp, st_as_sf(x[,,,1], as_points = TRUE))
pr_x = adrop(x[,,,1])
for (i in 1:49) {
  m = di[i,]
  dim(m) = dim(pr_x)
  pr_x[[i]] = m
}
names(pr_x) = paste0("distance_", 1:49)
pr_x$f = x["f",,,1]

data <- data.frame(st_coordinates(v),  st_extract(pr_x, v))
model <- train(data[, names(data) %in% c("X","Y",paste0("distance_",1:49))], data$f, method="rf", trControl = trainControl(method="cv"),
               savePredictions=TRUE, importance=TRUE)

prediction_dat <- data.frame(st_coordinates(pr_x),data.frame(pr_x))
names(prediction_dat)[names(prediction_dat)=="x"] <- "X"
names(prediction_dat)[names(prediction_dat)=="y"] <- "Y"
x$prediction = predict(model, prediction_dat, type = "prob")[["Forest"]]


## ----echo=FALSE, fig.show="hold", out.width="50%",message=FALSE,warning=FALSE----

plot(x["f",,,1], reset = FALSE, key.pos = 4, key.length = .4, key.width = lcm(5), main = "True Forest")
plot(s, add = TRUE, col = point_col, pch = 3)

plot(x["prediction",,,1], reset = FALSE, key.pos = 4, main = "Random Forest, p(Forest)", col = rev(grey(2:10/12)), breaks = "equal")
plot(s, add = TRUE, col = point_col, pch = 3)

