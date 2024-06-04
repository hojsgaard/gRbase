#include <RcppArmadillo.h>
#include <algorithm>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


// TEMPLATED VERSION

// Template function to check if one vector is a subset of another
template <typename T>
bool isSubset(const std::vector<T>& vec1, const std::vector<T>& vec2) {
  return std::includes(vec2.begin(), vec2.end(), vec1.begin(), vec1.end());
}

// Template function to handle both character and numeric vectors
template <typename T>
SEXP filter_maximal_vectors_Template(List setlist, bool index = false) {
  int n = setlist.size();
  std::vector<std::vector<T>> vectors(n);

  // Convert Rcpp::List to std::vector<std::vector<T>>
  for (int i = 0; i < n; i++) {
    vectors[i] = as<std::vector<T>>(setlist[i]);
    std::sort(vectors[i].begin(), vectors[i].end()); // Sort for std::includes to work
  }

  std::vector<bool> isMaximal(n, true);

  // Compare each vector with all others
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i != j && isMaximal[i]) {
        if (isSubset(vectors[i], vectors[j])) {
          isMaximal[i] = false;
          break;
        }
      }
    }
  }

  // Collect the maximal vectors or their indices
  if (index) {
    std::vector<int> indices;
    for (int i = 0; i < n; i++) {
      if (isMaximal[i]) {
        indices.push_back(i + 1); // R indices are 1-based
      }
    }
    return wrap(indices);
  } else {
    List result;
    for (int i = 0; i < n; i++) {
      if (isMaximal[i]) {
        result.push_back(wrap(vectors[i])); // Convert back to List format
      }
    }
    return result;
  }
}

// [[Rcpp::export]]
SEXP filter_maximal_vectors_(List setlist, bool index = false) {
  if (setlist.size() == 0) {
    return List::create(); // Return an empty list if input is empty
  }
  
  SEXP firstElement = setlist[0];
  
  if (TYPEOF(firstElement) == STRSXP) {
    return filter_maximal_vectors_Template<std::string>(setlist, index);
  } else if (TYPEOF(firstElement) == INTSXP) {
    return filter_maximal_vectors_Template<int>(setlist, index);
  } else if (TYPEOF(firstElement) == REALSXP) {
    return filter_maximal_vectors_Template<double>(setlist, index);
  } else {
    stop("Unsupported vector type");
  }
}





/*

// PRIMITIVE VERSION


// Function to check if one vector is a subset of another
bool isSubset(const std::vector<std::string>& vec1, const std::vector<std::string>& vec2) {
  return std::includes(vec2.begin(), vec2.end(), vec1.begin(), vec1.end());
}

// [[Rcpp::export]]
List filter_maximal_vectors_(List setlist) {
  int n = setlist.size();
  std::vector<std::vector<std::string>> vectors(n);

  // Convert Rcpp::List to std::vector<std::vector<std::string>>
  for (int i = 0; i < n; i++) {
    vectors[i] = as<std::vector<std::string>>(setlist[i]);
    std::sort(vectors[i].begin(), vectors[i].end()); // Sort for std::includes to work
  }

  std::vector<bool> isMaximal(n, true);

  // Compare each vector with all others
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i != j && isMaximal[i]) {
        if (isSubset(vectors[i], vectors[j])) {
          isMaximal[i] = false;
          break;
        }
      }
    }
  }

  // Collect the maximal vectors
  List result;
  for (int i = 0; i < n; i++) {
    if (isMaximal[i]) {
      result.push_back(wrap(vectors[i])); // Convert back to List format
    }
  }

  return result;
}


// VERSION WITH INDEX

#include <RcppArmadillo.h>
#include <algorithm>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// Function to check if one vector is a subset of another
bool isSubset(const std::vector<std::string>& vec1, const std::vector<std::string>& vec2) {
  return std::includes(vec2.begin(), vec2.end(), vec1.begin(), vec1.end());
}

// [[Rcpp::export]]
SEXP filter_maximal_vectors_(List setlist, bool index = false) {
  int n = setlist.size();
  std::vector<std::vector<std::string>> vectors(n);

  // Convert Rcpp::List to std::vector<std::vector<std::string>>
  for (int i = 0; i < n; i++) {
    vectors[i] = as<std::vector<std::string>>(setlist[i]);
    std::sort(vectors[i].begin(), vectors[i].end()); // Sort for std::includes to work
  }

  std::vector<bool> isMaximal(n, true);

  // Compare each vector with all others
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (i != j && isMaximal[i]) {
        if (isSubset(vectors[i], vectors[j])) {
          isMaximal[i] = false;
          break;
        }
      }
    }
  }

  // Collect the maximal vectors or their indices
  if (index) {
    std::vector<int> indices;
    for (int i = 0; i < n; i++) {
      if (isMaximal[i]) {
        indices.push_back(i + 1); // R indices are 1-based
      }
    }
    return wrap(indices);
  } else {
    List result;
    for (int i = 0; i < n; i++) {
      if (isMaximal[i]) {
        result.push_back(wrap(vectors[i])); // Convert back to List format
      }
    }
    return result;
  }
}

*/ 

