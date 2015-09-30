#### Functions
#### for analyzing gapminder data
#### Carlos Victoe
#### Sept

# function to calculate th Cv
## takes a vector of numbers
###return to CV
cal_CV <- function(x){
 mean_x <- mean(x)
 sd_x <- sd(x)
 CV <- sd_x / mean_x
 return(CV)
}



## go from circunference to diameter
circum_to_diametrer <- function(circ){
  diam <- circ/pi
  return(diam)
}

##Calculate Radius from diametre
radius_to_diametre <- function(diam){
  radius <- diam/2
  return(radius)
}

radius_to_area <- function(radius){
  area <- pi*radius^2
  return(area)
}

# start with circunference and get the area
area_to_circum <- function(circum){
  ## get the diametre from circumference
  diam <- circum_to_diametrer(circum)
  ##get the radius
  rad <- radius_to_diametre(diam)
  ##calculate area from radius
  Area <- radius_to_area(rad)
  return(Area)
}