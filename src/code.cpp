#include "hiperglm_types.h"
// [[Rcpp::depends(RcppEigen)]]


// [[Rcpp::export]]
Eigen::VectorXd qr_eigen(Eigen::MatrixXd A, Eigen::VectorXd y) {
  Eigen::HouseholderQR<Eigen::MatrixXd> qr(A); // compute QR decomposition of A
  return qr.solve(y); // solve for least squares solution
}
