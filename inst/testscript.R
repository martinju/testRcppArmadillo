
library(testRcppArmadillo)

# Simple C examples compile issues time --------------------------------------------------------------------------------
Rcpp::sourceCpp(
  code = '
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
Rcpp::NumericVector addVectors_sourceCpp(const Rcpp::NumericVector& vec1, const Rcpp::NumericVector& vec2) {
  // Check if the input vectors are of the same length
  if (vec1.size() != vec2.size()) {
    Rcpp::stop("Vectors must be of the same length.");
  }

  // Create a result vector of the same length as the input vectors
  Rcpp::NumericVector result(vec1.size());

  // Perform element-wise addition
  for (int i = 0; i < vec1.size(); ++i) {
    result[i] = vec1[i] + vec2[i];
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix addMatrices_sourceCpp(const Rcpp::NumericMatrix& mat1, const Rcpp::NumericMatrix& mat2) {
  // Check if the input matrices have the same dimensions
  if (mat1.nrow() != mat2.nrow() || mat1.ncol() != mat2.ncol()) {
    Rcpp::stop("Matrices must have the same dimensions.");
  }

  // Create a result matrix of the same dimensions as the input matrices
  Rcpp::NumericMatrix result(mat1.nrow(), mat1.ncol());

  // Perform element-wise addition
  for (int i = 0; i < mat1.nrow(); ++i) {
    for (int j = 0; j < mat1.ncol(); ++j) {
      result(i, j) = mat1(i, j) + mat2(i, j);
    }
  }

  return result;
}

// [[Rcpp::export]]
arma::mat addMatricesArmadillo_sourceCpp(const arma::mat& mat1, const arma::mat& mat2) {
  // Check if the input matrices have the same dimensions
  if (mat1.n_rows != mat2.n_rows || mat1.n_cols != mat2.n_cols) {
    Rcpp::stop("Matrices must have the same dimensions.");
  }

  // Perform element-wise addition using Armadillo
  arma::mat result = mat1 + mat2;

  return result;
}')


# Dimension of matrix
n = 10^4
m = 10^3

# Make matrices
mat1 = matrix(rnorm(n*m), n, m)
mat2 = matrix(rnorm(n*m), n, m)

rbenchmark::benchmark(
  inPackage_vec = testRcppArmadillo::addVectors_inPackage(mat1[,1], mat2[,1]),
  sourceCpp_vec = addVectors_sourceCpp(mat1[,1], mat2[,1]),
  replications = 10^4,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self")
)
# Windows-laptop
#test replications elapsed relative user.self sys.self
#1 inPackage_vec        10000    1.50    1.087      0.68     0.81
#2 sourceCpp_vec        10000    1.38    1.000      0.56     0.81



rbenchmark::benchmark(
  inPackage_mat_rcpp = testRcppArmadillo::addMatrices_inPackage(mat1, mat2),
  sourceCpp_mat_rcpp = addMatrices_sourceCpp(mat1, mat2),
  replications = 50,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self")
)
# Windows-laptop
#test replications elapsed relative user.self sys.self
#1 inPackage_mat_rcpp           50   13.32    1.047     12.14     0.72
#2 sourceCpp_mat_rcpp           50   12.72    1.000     11.75     0.75


rbenchmark::benchmark(
  inPackage_mat_arma = testRcppArmadillo::addMatricesArmadillo_inPackage(mat1, mat2),
  sourceCpp_mat_arma = addMatricesArmadillo_sourceCpp(mat1, mat2),
  replications = 100,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self")
)
# Windows-laptop
#test replications elapsed relative user.self sys.self
#1 inPackage_mat_arma          100    4.84    1.000      2.08     2.74
#2 sourceCpp_mat_arma          100    5.75    1.188      2.45     3.05


