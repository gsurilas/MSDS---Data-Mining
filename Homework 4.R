library(tidyverse)


LAozone = read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/LAozone.data",sep=",",head=T)
dpg <- LAozone$dpg
ozone <- LAozone$ozone


#Make smoothing splines with df=5 and df=11
fit1 <- smooth.spline(dpg, ozone, df = 5)
fit2 <- smooth.spline(dpg, ozone, df = 11)



##5.7 Top Plot
par(mfrow=c(1,1))
plot(dpg, ozone, xlab = "DPG", ylab = "Ozone Concentration", col = "black", pch = 19)

lines(fit1, col = "red")
lines(fit2, col = "green")

#HatMat returns smoother matrix
library(sfsmisc)
smoother.df5 <- hatMat(dpg, df = 5)
smoother.df11 <- hatMat(dpg, df = 11)

#Get eigen values/vectors of smoother matrices
eigen5 <- eigen(smoother.df5)
eigen11 <- eigen(smoother.df11)

#5.7 Bottom Left Plot
plot(eigen5$values[1:25], pch = 19, col = "red", ylim = c(-.2,1.2))
lines(eigen5$values[1:25], col = "red")
points(eigen11$values[1:25], pch = 19, col = "green")
lines(eigen11$values[1:25], col = "green")
abline(h=0, lty = 2)
abline(h=1, lty = 2)




#5.7 Bottom Right Plot
par(mfrow=c(2,2))
plot(dpg[order(dpg)], eigen5$vectors[,3][order(dpg)], type = "l", col = "red", xlab = NULL, ylab = NULL)
lines(dpg[order(dpg)], eigen11$vectors[,3][order(dpg)], type = "l", col = "blue", xlab = NULL, ylab = NULL)
plot(dpg[order(dpg)], eigen5$vectors[,4][order(dpg)], type = "l", col = "red", xlab = NULL, ylab = NULL)
lines(dpg[order(dpg)], eigen11$vectors[,4][order(dpg)], type = "l", col = "blue", xlab = NULL, ylab = NULL)
plot(dpg[order(dpg)], eigen5$vectors[,5][order(dpg)], type = "l", col = "red", xlab = NULL, ylab = NULL)
lines(dpg[order(dpg)], eigen11$vectors[,5][order(dpg)], type = "l", col = "blue", xlab = NULL, ylab = NULL)
plot(dpg[order(dpg)], eigen5$vectors[,6][order(dpg)], type = "l", col = "red", xlab = NULL, ylab = NULL)
lines(dpg[order(dpg)], eigen11$vectors[,6][order(dpg)], type = "l", col = "blue", xlab = NULL, ylab = NULL)


#5.8 Right panel
par(mfrow=c(3,1))
plot(dpg[order(dpg)], smoother.df5[12,][order(dpg)], ann = FALSE)
title(main = "Row 12")
plot(dpg[order(dpg)], smoother.df5[26,][order(dpg)], ann = FALSE)
title(main = "Row 26")
plot(dpg[order(dpg)], smoother.df5[50,][order(dpg)], ann = FALSE)
title(main = "Row 50")
par(mfrow=c(3,1))
plot(dpg[order(dpg)], smoother.df5[75,][order(dpg)], ann = FALSE)
title(main = "Row 75")
plot(dpg[order(dpg)], smoother.df5[100,][order(dpg)], ann = FALSE)
title(main = "Row 100")
plot(dpg[order(dpg)], smoother.df5[115,][order(dpg)], ann = FALSE)
title(main = "Row 115")

