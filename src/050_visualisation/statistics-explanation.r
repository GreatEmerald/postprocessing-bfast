# Aid for the visualisation of the statistics explanation chart
exampledata = data.frame(reference=c(91,92,91,17,18), prediction=c(40,38,36,34,17), year=c(2015.035,2016.035,2017.035,2018.035, 2019.035))

plot(reference~year, exampledata, type="b", ylim=c(0,100), xlim=c(2015,2020))
lines(prediction~year, exampledata, type="b", col="blue")

refmodel = lm(reference~year, exampledata)
summary(refmodel)
coef(refmodel)
fitted(refmodel)
predmodel = lm(prediction~year, exampledata)
summary(predmodel)
coef(predmodel)
fitted(predmodel)

abline(coef(refmodel)[[1]], coef(refmodel)[[2]])

abline(coef(predmodel)[[1]], coef(predmodel)[[2]], col="blue")

source("../utils/accuracy-statistics.r")
AccuracyStats(exampledata[["prediction"]], exampledata[["reference"]])


round(exampledata[["reference"]] - fitted(refmodel))
round(exampledata[["prediction"]] - fitted(predmodel))
