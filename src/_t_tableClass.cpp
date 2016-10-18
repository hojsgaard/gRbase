#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

#include "_t_tableCore.h"

class Ptable {
private:
  NumericVector data;

public:
  Ptable( const Ptable& org){// maybe clone
    data = org.data;
  }

  Ptable( NumericVector v ){ // maybe clone
    data = v;
  }

  friend Ptable operator*(const Ptable &t1, const Ptable &t2);
  friend Ptable operator/(const Ptable &t1, const Ptable &t2);
  friend Ptable operator+(const Ptable &t1, const Ptable &t2);
  friend Ptable operator-(const Ptable &t1, const Ptable &t2);
  friend Ptable operator%(const Ptable &t1, const Ptable &t2);
  friend bool   operator==(const Ptable &t1, const Ptable &t2);

  void print() {
    Rf_PrintValue( data );
  }

  NumericVector value(){
    return data;
  }
};

Ptable operator*(const Ptable &t1, const Ptable &t2) {
  NumericVector out = tabMult__( t1.data, t2.data );
  // eller NumericVector out = tabMult( t1.value(), t2.value() );
  // men ovenstående virker fordi operatorerne er friends med objektet
  return Ptable( out );
}

Ptable operator/(const Ptable &t1, const Ptable &t2) {
   NumericVector out = tabDiv__( t1.data, t2.data );
   return Ptable( out );
}

Ptable operator%(const Ptable &t1, const Ptable &t2) {
   NumericVector out = tabDiv0__( t1.data, t2.data );
   return Ptable( out );
}

Ptable operator+(const Ptable &t1, const Ptable &t2) {
   NumericVector out = tabAdd__( t1.data, t2.data );
   return Ptable( out );
}

Ptable operator-(const Ptable &t1, const Ptable &t2) {
   NumericVector out = tabSubt__( t1.data, t2.data );
   return Ptable( out );
}

bool operator==(const Ptable &t1, const Ptable &t2) {
   bool out = tabEqual__( t1.data, t2.data );
   return out ;
}


//[[Rcpp::export(.testOperations)]]
NumericVector testOperations(NumericVector t1, NumericVector t2){
  Ptable p1( t1 );
  Ptable p2( t2 );

  Ptable out1 = p1 * p2;
  Rprintf("mult :\n");  Rf_PrintValue( out1.value() );

  Ptable out2 = p1 / p2;
  Rprintf("div  :\n");  Rf_PrintValue( out2.value() );

  Ptable out3 = p1 % p2;
  Rprintf("div0 :\n");  Rf_PrintValue( out3.value() );

  Ptable out4 = p1 + p2;
  Rprintf("add  :\n");  Rf_PrintValue( out4.value() );

  Ptable out5 = p1 - p2;
  Rprintf("subt :\n");  Rf_PrintValue( out5.value() );

  return out1.value();
}




// //[[Rcpp::export]]
// NumericVector testEqual(NumericVector t1, NumericVector t2){
//   Ptable p1( t1 );
//   Ptable p2( t2 );
//   Ptable out = p1 * p2;
//   return out.value();
// }
