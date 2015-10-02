##Day4
## 1 october 2015



# Load Packages -----------------------------------------------------------


# Install dplyr
install.packages("dplyr")

# Load dplyr
library(dplyr)


# Import data -------------------------------------------------------------

gapminder <- read.csv("data/gapminder.csv")



# Using dplyr -------------------------------------------------------------

asia <- gapminder[gapminder$continent =="Asia",]
mean_asia_gdp <- mean(asia$gdpPercap)

africa <- gapminder[gapminder$continent =="Africa",]
mean_africa_gdp <- mean(africa$gdpPercap)

mean_asia_gdp
mean_africa_gdp


## select ()
gdp_df <- select(gapminder, gdpPercap)
gdp_df
View(gdp_df)

gdp_by_country <- select(gapminder, gdpPercap, country)
View(gdp_by_country)


gapminder$gdpPercap
View(select(gapminder, -pop))

## filter ()
gapminder_europe <- filter(gapminder, continent =="Europe")

View(gapminder_europe)

gm_europe_gdp <- select(gapminder_europe, year, country, gdpPercap)
View(gm_europe_gdp)

## pipe Operator %>%
##ctrl+shift+m or command+shift+m

gm_europe_gdp <- gapminder %>%
  filter(continent =="Europe") %>%
  select(year, country, gdpPercap)
View(gm_europe_gdp)

#challenge
gm_africa_dt <- gapminder%>%
  filter(continent =="Africa") %>%
  select(lifeExp, country, year)
View(gm_africa_dt)

## group_by () : separete the data.frame into groups in order 
to perform some analysis on each group at the same time

gapminder_grouped <- gapminder %>%
  group_by(continent)

gapminder_grouped

## summarize ()
gapminder_mean_gdp <- gapminder%>%
  group_by(continent) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))
View(gapminder_mean_gdp)

##challenge
gapminder_mean_lifeExp <- gapminder%>%
  group_by(country) %>%
  summarize(mean_lifeExp = mean(lifeExp)) %>%
  arrange(desc(mean_lifeExp))

View(gapminder_mean_lifeExp)

gdp_continent_year <- gapminder%>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))

View(gdp_continent_year)

gdp_continent_year <- gapminder%>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap))

head(gdp_continent_year)

## mutate () : add new column inside a pipe
gdp_pop_millions <- gapminder%>%
  mutate(pop_millions = pop/10^6)

View(gdp_pop_millions)


###Advanced Challenge
set.seed(1)
mean_lifeExp_2002  <- gapminder%>%
  filter(year == 2002)%>%
  group_by (continent)%>%
  sample_n(2)%>%
  summarize(mean_lifeExp = mean(lifeExp))%>%
  arrange(desc(continent))

View(mean_lifeExp_2002)


# Tidying data with tidyr -------------------------------------------------

install.packages("tidyr")
library(tidyr)


write.csv(gap_wide, file = "cleaned_data/")

gap_wide <- read.csv("cleaned_data/gap_wide.csv")

head(gap_wide)

View(gap_wide)

gap_long <- gather(gap_wide, obstype_year, obs_values,-continent, -country)

head(gap_long)
gap_long_sep <- separate(gap_long, obstype_year, into = c("obstype", "year"), sep ="_")

# look at the top
head(gap_long_sep)

## look at the atructure
str(gap_long_sep)

### force the year to become a number again
gap_long_sep$year <-  as.numeric(gap_long_sep$year)
str(gap_long_sep)

####challenge
separate
mean_LPG  <- 
  gap_long_sep%>%
  group_by (continent,country, obstype)%>%
  summarize(mean_obs = mean(obs_values))
  
#put observations into columns

## simple example of the
exemplo <- data.frame(grp =letters[1:4],
                   foo =c(1,2,3,4),
                   bar =c(5,6,7,8))

gather(exemplo, key = "variable", value= "value", foo:bar)
## would work for 3rd column

### also by *excluding* a column
comprido <- gather(exemplo, key="variable", value="value", -grp)

gordo <- spread(comprido,key = "variable", value="value", )
comprido%>%
  group_by(grp, variable) %>%
  summarise(mean_value = mean(value))%>%
  spread(key = variable, value =mean_value)

exemplo
comprido%>%
  group_by(grp, variable) %>%
  mutate (new_name = seq_along(value))%>%
  spread(variable, value)


##### challenge

### parte a
#### Calculate the mean population for each countries(within continents)
####
pop_means <- gapminder %>%
  group_by(continent, country) %>%
  summarise(meanpop = mean(pop))

pop_means
  
### make a column for each country, and a row for each continent
### ADVANCED fill en missing values with 0

pop_countries <- spread(pop_means, country, meanpop)

View(pop_countries)
pop_countries <- spread(pop_means, country, meanpop, fill = 0)
?spread

#challenge
#Organizing  Tables
### not (necessarily) for students: messing up gapminder into a wide format:  

