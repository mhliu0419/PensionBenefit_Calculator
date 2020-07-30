# Load data
interest <- 0.05

active.employee.data <- read.csv('/Users/mhliu/Desktop/Pension/Active\ Employee\ Info.csv')
retiree.data  <- read.csv('/Users/mhliu/Desktop/Pension/Retiree\ Info.csv')

retirement_termination.rate <- read.csv('/Users/mhliu/Desktop/Pension/Retirement\ and\ Termination\ Rate.csv')

post.retirementrate.female <- read.csv('/Users/mhliu/Desktop/Pension/post\ retirement\ female.csv')
post.retirementrate.male <- read.csv('/Users/mhliu/Desktop/Pension/post\ retirement\ male.csv')

pre.retirementrate.female <- read.csv('/Users/mhliu/Desktop/Pension/pre\ retirement\ female.csv')
pre.retirementrate.male <- read.csv('/Users/mhliu/Desktop/Pension/pre\ retirement\ male.csv')

#date.change <- function(x){
#  return(as.Date(x, tryFormats = "%m/%d/%y"))
#  }

# Calculate pension benefit of retiree firstly

## Create a function to get generational mortality based on employee's gender, current age and current year

Generational.Mortality <- function(gender, current_age, current_year){
  
  if (gender == 'M'){
    return(post.retirementrate.male[current_age - 19, current_year - 2012])
  }
  else if (gender == 'F'){
    return(post.retirementrate.female[current_age - 19, current_year - 2012])
  }
  else{
    return(NaN)
  }
}


Cumulative.Generational.Mortality <- function(gender, age, current_year, )
































































































