
# Libraries ---------------------------------------------------------------

library("Rcpp")
library("RcppArmadillo")
library("Matrix")


# Compile -----------------------------------------------------------------
library("Rcpp")
cppFunction(depends = "Rcpp",
            code = 'NumericVector dvnorm(NumericVector x, NumericVector means, NumericVector sds, bool log){
            int n = x.size();
            NumericVector res(n);
            if (sds.size() < n){
                for (int i=0; i<n; i++) res[i] = R::dnorm( x[i], means[i], sds[0], log);
            } else {
                for (int i=0; i<n; i++) res[i] = R::dnorm( x[i], means[i], sds[i], log);
            }
            return res;
            }')

#Vectorized multinomial density function
cppFunction(depends = "Rcpp",
            code = 'Rcpp::NumericVector dMultinom(Rcpp::NumericMatrix probs, Rcpp::NumericVector y, bool lg){
            // get dimensions of probs
            int n = probs.nrow();
            
            // initialize
            Rcpp::NumericVector ans(n);
            
            // extract probability corresponding to column (warning: zero indexing!)        
            for (int i = 0; i < n; i++) {
                ans(i) = probs(i, y(i)-1);
            }
            
            // return
            if (lg) {
                return(log(ans));
            } else {
                return(ans);
            }
            }')

#Normalize rows
cppFunction(depends = "RcppArmadillo",
            code = 'arma::mat normalize_rows(arma::mat M){
            // sum the rows to get the total we need to divide
            arma::vec rs = sum(M, 1);

            // in-place division of by the row sum
            M.each_col() /= rs;

            // return the matrix with rows summing to 1
            return(M);
            }')




#Multinomial sampling function
#Pass in a NxK matrix of probabilities and it will sample N states from
cppFunction('IntegerMatrix rMultinom(Rcpp::NumericMatrix probs, int m=1) {
            // set RNG
            Rcpp::RNGScope scope;
            
            // get dimensions of probs
            int n = probs.nrow();
            int k = probs.ncol();
            
            Rcpp::IntegerMatrix ran(n, m);
            
            Rcpp::NumericVector z(n);
            Rcpp::NumericMatrix U(k, n);
            
            for (int i = 0; i < n; i++) {
            z[i] = Rcpp::sum(probs(i, Rcpp::_));
            Rcpp::NumericVector cumsum_temp = Rcpp::cumsum(probs(i, Rcpp::_));
            U(Rcpp::_, i) = cumsum_temp;
            }
            
            for (int i = 0; i < m; i++) {
            Rcpp::NumericVector rand = Rcpp::runif(n);
            Rcpp::NumericVector un(k * n);
            int index = 0;
            
            Rcpp::IntegerMatrix compare(k, n);
            int ind = 0;
            
            // C++ equivalent of `un <- rep(rand, rep(k, n))`
            for (int a = 0; a < n; a++) {
            std::fill(un.begin() + index, un.begin() + index + k, rand[a]);
            index += k;
            
            for (int b = 0; b < k; b++) {
            compare(b, a) = un[ind] > U(b, a);
            ind++;
            }
            
            ran(a, i) = Rcpp::sum(compare(Rcpp::_, a)) + 1;
            }
            }
            
            return ran;
            }')


cppFunction(depends = "Rcpp",
            code = 'Rcpp::NumericVector dMultinom(Rcpp::NumericMatrix probs, Rcpp::NumericVector y, bool lg){
            // get dimensions of probs
            int n = probs.nrow();
            
            // initialize
            Rcpp::NumericVector ans(n);
            
            // extract probability corresponding to column (warning: zero indexing!)        
            for (int i = 0; i < n; i++) {
            ans(i) = probs(i, y(i)-1);
            }
            
            // return
            if (lg) {
            return(log(ans));
            } else {
            return(ans);
            }
            }')