## This is the assignment that seeks to provide a pair of functions...
## ...to cache the inverse of the matrix


## Example on how this works:
## (1) First we use the assign the makeCacheMatrix function to a variable
## amatrix <- makeCacheMatrix(matrix(c(1,3,4,5),2,2))

## (2) We can use functions  to get and set matrix and its inverse
## amatrix$get()             : it will give you the matrix initially set
## amatrix$getinverse()    : will give you "NULL" because there is nothing stored

## (3) We use the cacheSolve function will calculate the inverse or return the cached value
## cacheSolve(amatrix)     :will give you the inverse


## (4) We can use functions within the makeCacheMatrix function to set a new matrix
## or obtain the inverse one
## amatrix$inverse()            :it will give you the cached inverse matrix
## amatrix$set(matrix(c(75,4,22,3),2,2))   : it will set a new matrix


## makeCacheMatrix
## This is the function that creates a matrix object that can cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) inv_matrix <<- solve
  getinverse <- function() inv_matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## This function will use the Solve() function to obtain the inverse of the matrix
## If the matrix provided in the set() function from makeCacheMatrix is not square 
## or its determinant is equal to 0, then the function will produce an error 
## If there is a new matrix, it will calculate a new inverse. 
## Otherwise, it will retrieve the inverse in cache

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if (!is.null(inv_matrix)) {
    message("getting cached inversed matrix")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}