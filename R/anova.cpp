#include <TMB.hpp>


template<class Type>
Type objective_function<Type>::operator() ()
{
    DATA_VECTOR(y);
    DATA_FACTOR(group);
    PARAMETER_VECTOR(theta);
    PARAMETER(ln_sd);

    Type sd = exp(ln_sd);
    Type nll = 0;
    int n = y.size();
    for(int i=0; i<n; i++){
        nll -= dnorm(y[i], theta[group[i]], sd, true);
    }
    return nll;
}
