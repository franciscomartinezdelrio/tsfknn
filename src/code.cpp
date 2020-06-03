#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

/* This function finds the indexes of the k nearest neighbors and computes
 * the distances to all the examples
 */
// [[Rcpp::export]]
List first_n(NumericMatrix m, NumericVector v, int k) {
  NumericVector d(m.nrow());
  NumericVector d_copy(m.nrow());
  for (int i = 0; i < m.nrow(); ++i) {
    d[i] = sum(pow(m(i, _ ) - v, 2));
    d_copy[i] = sum(pow(m(i, _ ) - v, 2));
  }
  double maxi = max(d);
  IntegerVector ind(k);
  for (int i = 0; i < k; ++i) {
    NumericVector::iterator it = std::min_element(d.begin(), d.end());
    ind[i] = std::distance(d.begin(), it) + 1;
    *it = maxi + 1;
  }
  List ret;
  ret["indexes"] = ind;
  ret["distances"] = d_copy;
  return ret;
}

// [[Rcpp::export]]
List build_examples2(NumericVector timeS, NumericVector lags, int nt) {
  const int MAXLAG = lags[0];
  const int NCOL   = lags.size();
  const int NROW   = timeS.size() - MAXLAG - nt + 1;
  NumericMatrix patterns(NROW, NCOL);
  NumericMatrix targets(NROW, nt);
  IntegerVector targetsI(NROW);
  int row = 0;
  for (int ind = MAXLAG + nt -1; ind < timeS.size(); ++ind) {
    for (int col = 0; col < NCOL; ++col)
       patterns(row, col) = timeS[ind - nt + 1 - lags[col]];
    targets(row, _)  = timeS[Range(ind - nt + 1, ind + 1)];
    targetsI[row] = ind - nt + 2;
    row++;
  }
  List ret;
  ret["patterns"] = patterns;
  ret["targets"]  = targets;
  ret["targetsI"] = targetsI;
  return ret;
}
