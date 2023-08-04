#include <TMB.hpp>


template<class Type>
Type objective_function<Type>::operator() ()
{
    DATA_VECTOR(y);
    DATA_FACTOR(group);
    PARAMETER_VECTOR(beta);
    PARAMETER(ln_sd);

    Type sd = exp(ln_sd);
    Type nll = 0;
    int n = y.size();
    for(int i=0; i<n; i++){
        nll -= dnorm(y[i], beta[group[i]], sd, true); //total negative log-likelihood
        // may see the log likelihood written out, as not everyone uses the dnorm function
        // -log(pow(2*pi, 0.5)) is a constant but is necessary for correctly calculating AIC 
        // because if you are comparing 2 different distributions then the constants are different
        // nll -= -log(pow(2*pi, 0.5)) - log(sd) - Type(0.5) * pow((y[i] - beta[group[i]])/sd,2)
    }
    return nll;
}
