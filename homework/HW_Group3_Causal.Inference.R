# Practice Problems for Gelman and Hill 
# Chapter 9: #1 - Cat

# 1
# Ti -> 1 = schools with vending machines
#       0 = schools without vending machines
# i = children
# yi = B0 + B1Ti + B2xi + error
# xi = weight/health prior to going to school
# An ideal experiment would be generalizeable to the entire population, also known as externally valid. 
# Assuming that our population of interest is people living in the US, this means that we will need a random sample 
#of all American schools. It would have to be large enough of a sample to properly reflect variation in geographic 
#region, socio-economic status, etc. In order to look at the effect over time, we would randomly select school 
#systems instead of individual schools. After we compiled this randomly selected list of school systems, we would 
#randomly divide the list in half, with one group being experimental and one being control. We would then go to 
#these school districts, and install vending machines in the experimental school, and ban all vending machines from
#the other schools. We are primarily interested in the effect of the presence/absence of vending machines, so we 
#wouldn't worry about varying to number of vending machines per capita across schools. We would want to make sure 
#that our data wasn't marred by prior experiences, so we would only start collecting data on kids who hadn't yet 
#entered school. We would then track their BMI from kindergarten through high school (assuming no one moved into or 
#out of each school since it's a perfect world). Ideally, we would see an effect of vending machines on BMI without 
#including other variables in our model, but we would collect other information such as activity level and mental 
#health/stress in order to have a more complete and interesting data set. Our final model would be centered around 
#explaining BMI with the presence of vending machines. Specifically, we want to look at the difference in average 
#BMI between kids who went to a school with vending machines, and kids who did not. We would likely look at this 
#relationship separately for each age group, as we would expect age-related differences in decision making as well 
#as discretionary spending funds. We would want to look at the average BMI of each group before the experiment to 
#make sure that we don't have to control for this in the model. However, since we have a large sample that was 
#taken totally at random, we expect their to be no difference in starting conditions.

