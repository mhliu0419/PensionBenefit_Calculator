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


Cumulative.Generational.Survival.Rate <- function(gender, current_age, current_year, target_age){
  
  c.g.s.r <- 1
  if ((target_age - current_age) > 0){
    for (i in current_age:(target_age-1)) {
      c.g.s.r <- c.g.s.r * (1 - Generational.Mortality(gender, i, current_year + i - current_age))
    }
  }
  return(c.g.s.r)
}

Pension.Benefit.Retiree <- function(service.year, salary, current_age, target_age){
  
  p.b.r <- 0.01 * (service.year + target_age - current_age) * (salary *(1+0.025)^(target_age - current_age))
  return(p.b.r)
}



PVB.retiree <- function(gender, current_age, current_year, benefit){
  
  r.pvb <- 0
  for (i in current_age:120) {
    if (i >= 65){
      r.pvb <- r.pvb + Cumulative.Generational.Survival.Rate(gender, current_age, current_year, i) * benefit * (1 + interest)^(-i + current_age)
    }
    else if (i < 65){
      r.pvb <- r.pvb + Cumulative.Generational.Survival.Rate(gender, current_age, current_year, i) * benefit * (1 + interest)^(-i + current_age)
    }  
  }
  return(r.pvb)
}

# (0.5 + 0.05 * (i - 55)) * 

PVB.retiree('M', 72, 2019, 16444)
Cumulative.Generational.Survival.Rate('M', 66, 2019, 116)
























































































