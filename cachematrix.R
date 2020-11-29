## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It uses lexical scoping to its benefit, through creating 'x' and 'i' 
## outside of their current environment by moving them up to the parent
## environment. Objects 'x' and 'i' can be retrieved from the enclosing 
## environment for functions 'set', 'get', 'setinverse' and 'getinverse'.

## Here are the steps that it takes to achieve this:

## 1. Sets the value of the matrix (using deep assignment operator <<-);  
## 2. Gets the value of the matrix ('x' stored in enclosing environment);
## 3. Sets the value of the inverse (using deep assignment operator <<-);
## 4. Gets the value of the inverse ('i' stored in enclosing environment). 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
          x <<- y
          i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function()i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve: calculates the inverse of the special matrix created in the
## first function 'makeCacheMatrix'. Through using the cached inverse of this
## matrix, it saves on computing time and power compared to running the 
## computation of the solve function for multiple iterations. 

## Here are the steps that it takes to do so:

## 1 It checks to see if the inverse has already been calculated.
## 2. If it has, then it is retrieved from cached data and skips carrying out 
## the calculation again.
## 3. If the inverse has not already been calculated, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the 
## setinverse function.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
