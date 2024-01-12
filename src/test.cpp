#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
  Rcpp::NumericVector addVectors_inPackage(const Rcpp::NumericVector& vec1, const Rcpp::NumericVector& vec2) {
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
Rcpp::NumericMatrix addMatrices_inPackage(const Rcpp::NumericMatrix& mat1, const Rcpp::NumericMatrix& mat2) {
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
arma::mat addMatricesArmadillo_inPackage(const arma::mat& mat1, const arma::mat& mat2) {
  // Check if the input matrices have the same dimensions
  if (mat1.n_rows != mat2.n_rows || mat1.n_cols != mat2.n_cols) {
    Rcpp::stop("Matrices must have the same dimensions.");
  }

  // Perform element-wise addition using Armadillo
  arma::mat result = mat1 + mat2;

  return result;
}
