// State-space dynamic linear model
#include <TMB.hpp>
// note this file actully lives in the TMB repo:
// https://github.com/kaskr/adcomp/blob/master/TMB/inst/include/TMB.hpp

template<class Type>
Type objective_function<Type>::operator() () {

    DATA_VECTOR(y);
    
    // PARAMETER() is for single values
    PARAMETER(growth_rate); // typically r in most models
    PARAMETER(log_sigma_obs); // observation error
    PARAMETER(log_sigma_re); // random effect error

    // PARAMETER_VECTOR is for a vector of values
    PARAMETER_VECTOR(re); // random effect 
    
    // transform parameters
    // exp() of parameter puts bounds on parameter to keep it away from 0
    Type sigma_obs = exp(log_sigma_obs);
    Type sigma_re = exp(log_sigma_re);

    // report
    // ADREPORT applies the delta method, a back transformation (applied in the transfrom parameters section) 
    // for the se's to get what you are actually interested in
    ADREPORT(sigma_re);
    ADREPORT(sigma_obs);
    
    int n = y.size(); // length of time series
    // Type keeps the generic type generic
    // Type corresponds to the class named used at the top of the tmb file
    // in this case, template<class Type>
    // Have to use it for tracking derivatives
    // using 0.0 instead of 0 is a good practice, although tmb probably
    // would convert your 0 to 0.0 internally?
    Type nll = 0.0;
    
    // Random Effect Model (has correlation structure)
    // Also an AR1 model because based on the previous observation
    // In State-Space Modeling, this is referred to as the "Process" Model
    // i++ is the same as i = i + 1
    for(int i=1; i<n; i++){ // ++ means that we will iterate i by 1 and will stop right before we get to n (i<n)
        Type expected_re_i = growth_rate * re[i-1]; // growth rate * previous re vector (time-step); growth rate is constant
        nll -= dnorm(re[i], expected_re_i, sigma_re, true); // likelihood for the random effect
    }
    
    // Observation Model (iid model; no correlation structure)
    for(int i=0; i<n; i++){
        nll -= dnorm(y[i], re[i], sigma_obs, true);        
        //the same as
        //nll = nll - dnorm(y[i], re[i], sigma_obs, true);
    }

    return nll;

}
