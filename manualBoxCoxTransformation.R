# Set seed for reproducibility
set.seed(123)


################################################################################
# this function generates random data with a right skew
################################################################################
generate_data = function(){
  # Generate a right-skewed dataset using exponential distribution
  right_skewed_exp <- rexp(1000, rate = 0.5)  # rate parameter controls the spread
  
  # Generate a right-skewed dataset using gamma distribution
  #right_skewed_gamma <- rgamma(1000, shape = 2, rate = 0.5)  # shape < 3 gives a right skew
  
  return (right_skewed_exp)
  
}

data = generate_data()

head(data)

###############################################################################
# Manually applies a BoxCox transformation to nomalize the data
# Some models assume data is normally distributed and as such results 
# - can have bias when the assumption is wrong.
# It is a logarithmic transformation that stretches out the smaller range to 
# enlarge its variability. And it shrinks the larger range to reduce its variability.

# Here is the underlying function: # t(y) = (y^lambda - 1)/lambda
# lambda is a dynamic variable

###############################################################################

boxCox = function(data, l){
  x = data
  originalData = data
  
  # Plot the histograms to visualize the skewness
  par(mfrow = c(1, 2))  # Set up 1x2 plotting space
  
  lambda = l # this can be changed
  boxCox = (x^lambda - 1)/lambda
  
  hist(boxCox, breaks=30)
  hist(x, breaks=30)
    
}

boxCox(data, 0.35)
