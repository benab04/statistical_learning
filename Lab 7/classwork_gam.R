# GAM (Generalized Additive Models)
library(mgcv)
attach(LungCapData2)
head(LungCapData2)

gam1 = gam(LungCap ~ s(Age, k = 4), data = LungCapData2)
summary(gam1)

gam2 = gam(LungCap ~ s(Age, k = 4) + s(Height, k = 4), data = LungCapData2)
summary(gam2)

par(mfrow = c(1, 2))

plot(gam2, se = TRUE, col = "blue")
anova(gam1, gam2, test = "F")
