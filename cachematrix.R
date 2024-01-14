#makeCacheMatrix function creates a cacheable matrix for purposes of calculating its inverse and then caching the result:

makeCacheMatrix <- function(x = matrix()) {

#Initializes a variable "inv" to store matrix inverse
#Creates a function named "set" that sets the matrix in the cache
#x<<-y caches the matrix
#inv<<-Null invalidates the cached inverse when the matrix x changes:
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL
  }

#"get" function gets the cached matrix x
#"set_inv" function calls the (inverse) function to set the inverse and then stores the inverse of the matrix in cache with <<- operator
#"get_inv" function gets the stored inverse from the cache

  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set,
       get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}



#cacheSolve function uses the makeCacheMatrix function to:
#1)solve for the inverse OR 2)if the inverese has allready been obtained it checks in the cache and returns cached inverse:

cacheSolve <- function(x, ...) {

#calls get_inv function to check the cache:
  inv<- x$get_inv()

#if there is cached inverse (inv variable is not null) function returns the cached inverse matrix:
  if (!is.null(inv)) {
          message("Cached inverse")
          return(inv)
  }

#otherwsie it solves for inverse and returns freshly computed inverse matrix:
  message("Calculating inverse")
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)

  inv
}

#Test:
test_matrix<-matrix(1:4,2,2)
inv_test_matrix<-makeCacheMatrix(test_matrix)
cacheSolve(inv_test_matrix)

#expected output:
#Calculating inverse
#    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
cacheSolve(inv_test_matrix)

#expected output:
#Cached inverse
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5