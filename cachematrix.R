## 
## This function takes a matrix and calculates the inverse.
## Both the matrix and inverse are stored in a list as a cache.
## 
makeCacheMatrix <- function(x = matrix()) {

	## 
	## The matrix
	## 
	matrix <- NULL

	## 
	## The matrix inverse
	## 
	inverse <- NULL

	## 
	## Set matrix. Use the solve() function to
	## generate the inverse of the matrix.
	## 
    set <- function(m) {
 		matrix <<- m
		inverse <<- solve(m)
 	}

	## 
	## Get matrix.
	##  
	## Loops through the list of matrix in the list to
	## see if it exist. If so, return 
	## 
	## 
	get <- function() {
		return matrix
	}

	## 
	## Set inverse of matrix
	## 
	setInverse <- function(inv){
		inverse <- inv
	}

	## 
	## Get inverse of matrix
	## 
	getInverse <- function() {
		return 	inverse
	}

    ## 	
    ## list functions
    ## 	
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


##
## This function will take the custom matric with the cached inverse.
## If the matrix already exists in the list, the inverse will be returned.
## IF the matrix does not exist, it will add the matrix to the list and 
## calculate the inverse.
##  
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    matrix <- x$get

    ## 
    ## Check if the 2 matrix are the same. 
    ## Use method to compare the 2 matrix defined here:
    ## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
    ## 
    ## Compare the following
    ##    1. The both objects are actually a matrix
    ##    2. That the dimensions are equal
    ##    3. The all the values are the same
    ## 
    if(is.matrix(x) && 
       is.matrix(matrix) && 
       dim(x) == dim(matrix) && 
       all(x == matrix)){

       	## Match is found, return the inverse of the matrix
        return x$getInverse
    }

    ## 
    ## Inverse not found, call set() that will set the matrix and 
    ## calculate the inverse
    ## 
    x$set(m)

    ## Get the inverse, this is returned from the function
    x$getInverse
}
