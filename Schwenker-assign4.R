library(QZ)
library(matrixcalc)
library(generalCorr)

#PROBLEM 1
X<-A%*%t(A)
Y<-t(A)%*%A
X
Y
eX<-eigen(X)
print(paste("eigen values of X are, "))
eigen_val_X <-eX$values
eigen_val_X

print("eigen vectors of X are")
eigen_vec_X <-eX$vectors
eigen_vec_X

eY<-eigen(Y)
print(paste("eigen values of Y are, "))
eigen_val_Y <-eY$values
eigen_val_Y

print("eigen vectors of Y are")
eigen_vec_Y <-eY$vectors
eigen_vec_Y

A_svd<-svd(A, nu=2,nv = 3)

print("left singular vectors of A are")
left_sing_A <- A_svd$u
left_sing_A
print("singular values of A are")
Sing_val_A<- A_svd$d
Sing_val_A
print("right singular vectors of A are")
Right_Sing_A<- A_svd$v
Right_Sing_A
all.equal(left_sing_A,eigen_vec_X )
#are not the same, because of negatives
print("taking the absolute value to remove negatives and show similarity")
eigen_vec_X_abs <- abs(eigen_vec_X)
eigen_vec_X_abs
left_sing_A_abs <- abs(left_sing_A)
left_sing_A_abs
print("when absolute values are compared")
all.equal(eigen_vec_X_abs,left_sing_A_abs)

Right_Sing_A
eigen_vec_Y
all.equal(Right_Sing_A,eigen_vec_Y )
print("taking the absolute value to remove negatives and show similarity")
#are not the same, because of negatives
eigen_vec_Y_abs <- abs(eigen_vec_Y)
eigen_vec_Y_abs
right_sing_A_abs <- abs(Right_Sing_A)
right_sing_A_abs
print("when absolute values are compared")
all.equal(eigen_vec_Y_abs,right_sing_A_abs)

#####################################################################################

#PROBLEM 2

conjugate_matrix <- function(A) {
  n <- nrow(A)
  B <- matrix(NA, n, n)
  for( i in 1:n )
    for( j in 1:n )
      B[i,j] <- cofactor(A, i, j)
  return(B)
}

myinverse <- function(A){ if(!is.singular.matrix(A) && is.square.matrix(A)){
  C<- conjugate_matrix(A)
  A_inv<-t(C)/det(A)
  return(A_inv)
}else{print(" matrix was either not square or could not get an inverse")}}
#run
funct_outcome <- myinverse(A)
#set sample matrix
A<-matrix(c(1,1,2,1),nrow=2,ncol=2)
A
#test agaist R package
built_in_outcome <-solve(A)
funct_outcome
built_in_outcome
#test for equality
all.equal(funct_outcome,built_in_outcome)