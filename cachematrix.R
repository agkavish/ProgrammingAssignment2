## This function implementes solution to the assignment 2 - Catching the inverse of matrix.
## How to use:
## create a square matrix for example:
## b <- matrix(c(2,4, 3, 1, 6,8, 0, 12,14),
## nrow = 3,
## ncol = 3
##) 
## create a variable to hold cached matrix
## m <- makeCachedMatrix(b)
## call cacheSolve(m), it will print inverted matrix of b.
## prinitng m$getInvertedMatrix() will also give the value of inverted matrix


## makeCaceMatrix function creates cached inverted matrix. This will provide
## functions to set/get matrix and inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  iMat <- NULL; #Holds inverted matrix
  
  set <- function(y){
    x <<- y
    iMat <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInvertedMatrix <- function(invertedMatrix) {
    iMat <<- invertedMatrix
  }
  
  getInvertedMatrix <- function(){
    iMat
  }
  
  list ( set = set, get = get,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix
    )
}


## cacheSolve function checks if the given matix x has inverted matrix set, if not 
## this function inverts the matrix using solve function and set it to orinigial matrix. 
## This function returs inverted matrix. This function assumes that supplied matrix is invertiable hence does
## make any explicit check to validate the same.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInvertedMatrix()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  
  data <- x$get()
  im <- solve(data)
  x$setInvertedMatrix(im)
  im
}

