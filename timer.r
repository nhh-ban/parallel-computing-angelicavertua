# timer.r
getwd()
library(tictoc)

# Source the three methods
source("scripts/method1.r")
source("scripts/method2.r")
source("scripts/method3.r")

# Time Method 1
tic()
source("scripts/method1.r", chdir = TRUE)
toc()

# Time Method 2
tic()
source("scripts/method2.r", chdir = TRUE)
toc()

# Time Method 3
tic()
source("scripts/method3.r", chdir = TRUE)
toc()


### Results
# Method 1 (original): 174.5 sec
# Method 2: 131 sec
# Method 3: 303.3 sec

#the slowness is probably due to my computer, that isn't one of the best 

# the method 2 is the fastest probably because it effectively leverages 
#parallel computing to distribute the simulations across multiple CPU cores. 
#This allows for concurrent execution of the simulations, significantly speeding
#up the computation when dealing with a large number of simulations (M). 

# the final loop is parallelized using the doParallel package and foreach package. 
#This means that multiple simulations can run simultaneously on multiple CPU cores. 
#It efficiently utilizes the available resources for parallel execution

#The simulations in Method 2 run concurrently, meaning that they are executed simultaneously, 
#and their results are combined afterward