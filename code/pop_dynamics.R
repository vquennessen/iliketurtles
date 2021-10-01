# base population dynamics model

###### parameters ##############################################################

# life history parameters
max_age <- X             # maximum age  

# model parameters
y <- 200                 # number of years

##### initialize arrays ########################################################

# numbers at age; dimensions 2 (sex) * maximum age * number of years
N <- array(rep(NA, 2*max_age*y))

# extract temperature estimates from IPCC prediction scenarios

##### model ####################################################################

# calculate number of hatchlings given male and female adults

# adjust hatchling mortality and sex ratio given predicted temperature

# step rest of population forward one year given natural mortality estimates