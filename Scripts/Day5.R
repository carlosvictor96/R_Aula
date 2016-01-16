# Day 5

## introduction to statistics in R


# load data ---------------------------------------------------------------

gapminder <- read.csv("data/gapminder.csv")


# load packages -----------------------------------------------------------

library(dplyr)
library(ggplot2)
install.packages("car")
library(car)

# probability distributions -----------------------------------------------

# normal distribution
random_normal <- rnorm (n = 300, mean = 10, sd = 4)

rand_df <- data.frame(rn=random_normal)
rand_df
ggplot(rand_df, aes (x=rn)) + geom_density ()


## poisson distribution
rand_df$randpois <- rpois(300, 4)

ggplot (rand_df, aes(x=randpois)) +geom_density()

### r, q, d
?rpois

plot(curve(dnorm(x), from=-2, to=2), type="l", xlim = c(-3,3))

### curve for lines

pop <- function(x) {
  2*x^0.75
}

pop(4)
pop(40)
pop(400)

plot(curve(pop(x)))

# linear models -----------------------------------------------------------

head(gapminder)
model <- lm(pop~year, data= gapminder)

summary(model)

model2 <- lm(pop~year*continent, data=gapminder)
#similar: year +continent+year:continent

summary(model2)


anova(model2)

model3 <- lm(pop~gdpPercap*year, data=gapminder)
summary(model3)
anova(model3)


### sums of square with the car packages
Anova(model3, type="III")


# checking residual -------------------------------------------------------

model4 <- lm(pop~year, data=gapminder)
summary(model4)
plot(pop~year, data=gapminder)
plot(model4)

qqnorm(resid(model4));qqline(resid(model4))
hist(resid(model4))
locator()

### fitting count data!!

#### transform the response variable
model5 <-  lm(log(pop)~year, data =gapminder)
plot(model5)


qqnorm(resid(model5)); qqline(resid(model5))
hist(resid(model5), col="blue")

## GLMs with poisson errors
model6 <-  glm(pop ~year, data = gapminder, family = poisson)

model6$deviance/model6$df.residual
plot(model6)

summary (model6)



model7  <- glm(pop ~year, data=gapminder, family = quasipoisson())
plot(model7)
summary (model7)



# multivariate analyses ---------------------------------------------------



library(vegan)
data("varechem")
View(varechem)

data("varespec")
View(varespec)

data("dune")
View(dune)

data("dune.env")
View(dune.env)

model8 <- lm(log(pop) ~1, data=gapminder)
model9 <- lm(log(pop) ~year, data=gapminder)
model10 <- lm(log(pop) ~year*country, data=gapminder)
model11 <- lm(log(pop) ~year*country*continent, data=gapminder)

anova(model8,model9, model10)
AIC(model8,model9, model10)

#AICcmodavg  is a good package for model averaging
#check aictab() function

install.packages("AICcmodavg")



dist1 <- vegdist(varespec, method = "bray")
dist1
dist1 <- vegdist(varespec, method = "bray",  diag = TRUE)

#PCA

pca1 <- rda(varechem)
pca1 <- rda(varechem, scale=TRUE)

plot(pca1)

#ordenar
ordiplot(pca1)

#check ?ordihull

ordiplot(pca1, type = "text")
ordiplot(pca1, display = "species")
ordiplot(pca1, scaling = 2)
pca1

summary(pca1)
scores(pca1)
scores(pca1)$sites

rda1 <- rda(varespec ~ K + pH + P, data = varechem)
plot(rda1)

rda2 <- rda(log(varespec+1)~ K + pH + P, data = varechem)
plot(rda2)

rda3 <- rda(decostand(varespec, method ="hellinger") ~
              + K + pH + P, data = varechem)

plot(rda3)

anova (rda3, by ="axis")
anova(rda3, by = "terms") #type I
anova(rda3, by = "margin") #Type II

RsquareAdj(rda3)

cca1 <- cca(decostand(varespec, method = "hellinger")~ 
              + K + pH + P, data = varechem)
# mds

dist1
dist2 <- vegdist(varechem, method = "euclid")
mds1 <- cmdscale(dist2)

plot(mds1)

# variance partitioning
#?varpart

#?mantel for mantel test
#?anosim for anosim

#permanova + permdisp

head(dune)
head(dune.env)

## For PERMANOVA
perm1 <- adonis(dune ~ Management, data = dune.env, method = "bray")


# For PERMDISP
dist3 <- vegdist(dune, method = "bray")
disper1 <- betadisper(d = dist3, group = dune.env$Management)
permutest(disper1, pairwise = TRUE)
plot(disper1)

levels(dune.env$Management)

boxplot(disper1)

scores(disper1)
disper1$distances

dune.env %>%
  mutate(dispersao = disper1$distances)%>%
  ggplot(aes(x = Use, y = dispersao)) +
  geom_boxplot()

dune.env %>%
  mutate(dispersao = disper1$distances)%>%
  ggplot(aes(x = A1, y = dispersao)) +
  geom_point()

#don't forget: ANOSIN is not the same as PERMANOVA, which is not the same as MANTEL


?family
?predict

# draw a poisson distribution
pois_resp  <- data.frame(xs = runif(50, min= 10, max=100))

ys <-  vector ("numeric", length =50)
for (i in 1:length(pois_resp$xs)){
  ys[i]  <- rpois(1, lambda = pois_resp$xs[i])
  } 
  
pois_resp$ys



library (maptools)
library(ggmap)
library(maps)
library(raster)

# multilevel --------------------------------------------------------------

library(broom)
library(dplyr)

gapminder%>%
  group_by(continent, country)%>%
  do(tidy(glm(pop~year, data = ., family = poisson)))%>%
  filter(term=="gdpPercap") %>%
  data.frame%>%
  ggplot(aes(x=continent, y=estimate)) +geom_point()