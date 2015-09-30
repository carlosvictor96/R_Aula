## Day 2 Of R Workshop
#### 29 Sept 2015

library(ggplot2)

# load packages -----------------------------------------------------------
library(ggplot2)

# load date ---------------------------------------------------------------

# load gapminder date -----------------------------------------------------

gapminder  <-  read.csv ("data/gapminder.csv")

# load functions ----------------------------------------------------------

source("Scripts/functions.R")

# substting review --------------------------------------------------------

# methods of subsettting

#x[a] for vetors
x <- c(10:1)
x [3]

#x[a, b] matrices, Data frames
x <-  list(c(1:10), c(10:20)
x
x[1,2]

# x ["a"] or x["a",] vecor, data frame, or matrix with names


x <-  c(1:10)
names(x)  <-  letters [1:10]
x
x["b"]


# x[[a]] for list data frame
x <-  list(c(1:10), c (10:20))
x
x[[1]]

# x$a


names(x)  <-  c ( "one", "two")
x
x$two


## more complex subsetting

#<, >, ==

x [x>3]
gapminder

# %in%

x <-  c(1:10)
names(x)<-  letters[1:10]
x

# this doesn't work

x == c(4,5,6)
x[x %in% c(4,5,6)]
x [names(x) %in% c("d", "e", "f")]

##challenge

gapminder[gapminder$year == 1957,]

#gapminder[,-(1:4)]

gapminder

#gapminder[gapminder$lifeExp>80,]

# gapminder[1, c(,4,5)]
gapminder[1,c("lifeExp", "gdpPercap")]
gapminder[1, c(,4,5)]
gapminder[1,c("lifeExp", "pop)]

#
1
g1 <- gapminder[gapminder$year == 2002,]
g2 <- gapminder[gapminder$year == 2007,]
rbind(g1, g2)

2
gmm2  <-  gapminder[gapminder$year == 2002|gapminder$year == 2007,]
head(gmm2)
gmm2$year
gmm2

3
gapminder$year %in% c(2002,2007)

## prefix
sum (2+3)
## infix
2+3
## infix como um prefix
'+' (2,3)

# plotting review ---------------------------------------------------------
#

# plotting review ---------------------------------------------------------
ggplot(data = gapminder, aes(x = lifeExp, y= gdpPercap, colour=continent)) + geom_point () + geom_line ()

ggplot(data = gapminder, aes(x = lifeExp, y= gdpPercap, colour=continent, group = country)) + geom_point () + geom_line ()

# scale
ggplot(data = gapminder, aes(x = year, y= pop, colour=continent)) + geom_point () + geom_line ()
ggplot(data = gapminder, aes(x = lifeExp, y= gdpPercap, colour=continent, group=country)) + geom_point () + geom_line () +scale_y_log10

#color Black

ggplot(data = gapminder, aes(x = lifeExp, y= gdpPercap, colour=continent, group=country)) + geom_point (colour=black") + geom_line () +scale_y_log10

# slip 
ggplot(data = gapminder, aes(x = lifeExp, y= gdpPercap, colour=continent, group=country)) + geom_point (colour="black") + geom_line () + scale_y_log10

## Challenge
ggplot(data = gapminder, aes (x = lifeExp, y = gdpPercap, colour=country)) + geom_point (colour="green") + scale_y_log10 () + geom_smooth (method="lm", size=1)
ggplot(data = gapminder, aes (x = lifeExp, y = gdpPercap )) + geom_point (colour="green") + scale_y_log10 () + geom_smooth (method="lm", size=1, alpha=0.3)

#
ggplot(data = gapminder, aes(x = gdpPercap, group=continent, colour=continent, )) + geom_density (alpha=0.5) + scale_x_log10 () +facet_wrap (~year)

ggplot(data = gapminder, aes(x = gdpPercap, group=continent, colour=continent, )) + geom_density (alpha=0.5) + scale_x_log10 () +facet_wrap (~year) + ggtitle("PIB por continente") + ylab("Densidade de PIB") + xlab("PIB") + scale_fill_manual(values = c("red", "green", "blue", "gray"))


## saving a plot
ggsave("figures/density_plot.pdf")
ggsave("figures/density_plot.jpg")

pdf(file = "figures/pdf_densityplot.pdf, height = 7")
ggplot(data = gapminder, aes(x = gdpPercap, group=continent, colour=continent, )) + geom_density (alpha=0.5) + scale_x_log10 () +facet_wrap (~year),
dev.off()

###Challenge

pdf(file = "figures/plot4.pdf", height = 8)
ggplot(data = gapminder, aes(x = gdpPercap, group=continent, colour=continent, )) +
  geom_density (alpha=0.5) + scale_x_log10 () +
  facet_grid (continent~year)

ggplot(data = gapminder, aes(x = gdpPercap, group=continent, colour=continent, )) + 
  geom_density (alpha=0.5) + 
  scale_x_log10 () +
  facet_wrap (~year)
dev.off()

###PNG or JPG rather of pdf

png(file = "figures/png_plot.pdf", height =7)
ggplot(data = gapminder, aes(x = gdpPercap, group=continent, colour=continent, )) + 
  geom_density (alpha=0.5) + 
  scale_x_log10 () +
  facet_wrap (~year)
dev.off()

###multiplot
###grid.arrrange
####gridExtra::gridarrange ()


# Working with data frames ------------------------------------------------

#rbind and cbind can add rows and columns to data frames
iris
head(iris)

## add a new column

iris$family  <- "Iridaceae"
head(iris)

# add a column using data from the data frame
iris$Petal.Area <- iris$Petal.Width * iris$Petal.Length
head(iris)

iris$Seplal.area  <-  with(iris, Sepal.Width * Sepal.Length)
head(iris)

# summary stats
mean(iris$Petal.Width)
sd(iris$Petal.Width)
median (iris$Petal.Width)

mean_petal_width <- mean(iris$Petal.Width)
sd_petal_width <- sd(iris$Petal.Width)

sd(iris$Petal.Width)/mean_petal_width

###Challenge
mean(iris$Sepal.Length)
sd(iris$Sepal.Width)

mean_sepal_width <- mean(iris$Sepal.Length)
sd_sepal_wifth <- sd(iris$Sepal.Width)

mean_sepal_length/sd_sepal_width


# R Functions -------------------------------------------------------------

cal_CV(iris$Petal.Length)
cal_CV(iris$Petal.Width)
cal_CV(iris$Petal.Area)

cal_CV(gapminder$gdpPercap)
cal_CV(rnorm(2000))

##a area of a circle
circum_to_diametre(30)
circum_to_diameter(3)

circum_to_diametrer(30)
circum_to_diametrer(3)

##challenge: Write a function that calculates the results from the diamettre!
radius_to_diametre(30)
radius_to_area(30)
#challenge
area_from_circum(40)
area_to_circum(40)

### MAke a data frame

circs <- data.frame(xs = 10:200)
head(circs)
circs$area <- area_from_circum(circum+circs$xs)
head(circs)
ggplot(circs,aes(x = xs, y = areas)) + geom_line()


#saving data frame
gapminder
brazil <- gapminder[gapminder$country =="Brazil",]
brazil

write.table(brazil, file = "cleaned_data/brazil_.csv", sep=",")
write.table(x=brazil,
            file = "cleaned_data/brazil_data.csv",
            sep = ",",
            row.names = FALSE,
            quote = FALSE)


write.table(x=brazil,
            file = "cleaned_data/brazil_data_tabbed.csv", sep = "\t",
            row.names = FALSE,
            quote =FALSE)

write.table(x=brazil,
            file = "cleaned_data/brazil_data_semicolon.csv",
            sep = ";",
            row.names = FALSE,
            quote =FALSE)

brazil2  <-  read.table("cleaned_data/brazil_data_tabbed.csv",sep = "\t")

#challenge
since_1990 <-gapminder[gapminder$year > 1990,]

since_1990

write.table(x=since_1990,
            file = "cleaned_data/gapminder_since_1990.csv",
            sep ="\t",
            row.names = FALSE,
            quote = FALSE)
