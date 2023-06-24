// M1.1 Single Note Accuracy Model
// 

data {
  int< lower = 1> n;                                                            // Number of Observations, can't go lower than 1 
  
  int< lower = 12 > n_scale_degrees;                                            // Number of Scale Degrees (aka the number 12), lower bound is number 
  int< lower = 39 > n_participants;                                             // Number of Participants (39)
  int< lower = 3 > n_keys;                                                      // Number of Keys (3)
  
  int< lower = 1, upper = n_scale_degrees > scale_degree[n];                    // Make vector of lenght n, that holds numbers 1:12 
  int< lower = 1, upper = n_participants> participant[n];                       // "" ... 1:39
  int< lower = 1, upper = n_keys > key[n];                                      // "" ... 1:3
  
  int correct[n];                                                               // Correct or incorrect for each trial, n observations long // JAB: Why int? 
}

parameters{
  vector[n_scale_degrees] alpha;                                                // Estimate separate alpha (intercept) for each scale degree
  vector[n_participants] beta;                                                  // Estimate separate beta (intercept) for each participant
  vector[n_keys] gamma;                                                         // Estimate separate gamma (intercet) for each key 
  
  real beta_bar;                                                                // Estimate different slope for each participant
  real<lower = 0> sigma_beta;                                                   // Estimate single variance parameter for participannt 
  real<lower = 0> sigma_gamma;                                                  // Estimate single variance parameter for key 
  real<lower = 0> sigma_alpha;                                                  // Estimate single variance parameter for scale degrees 
  }
  

model {
    // Likelihood 
    vector[n] p;
    
    // Priors
    
    // Alpha: Treatment, want to estimate a fixed effect
    
    alpha ~ normal( 0 , 1.5 ); // For now 

    // Adaptive priors 
    
    // Beta: Participant, allow this to vary 
    beta ~ normal( beta_bar, sigma_beta);
    gamma ~ normal(0, sigma_gamma);
    
    // Hyper Priors 
    beta_bar ~ normal(0, 1.5);
   
    //sigma_alpha ~ exponential(1);
    sigma_beta ~ exponential(1);
    sigma_gamma ~ exponential(1);
    
    // Linear Combination 
    for ( i in 1:n ) {
        p[i] = alpha[n_scale_degrees] + beta[n_participants]  + gamma[n_keys];
        p[i] = inv_logit(p[i]);
    }
    
    correct ~ binomial( 1 , p);
}

// TODO: What is this? 

generated quantities{
    vector[n] log_lik;
    vector[n] p;
    for ( i in 1:n ) {
        p[i] = alpha[n_scale_degrees] + beta[n_participants]  + gamma[n_keys];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:n) log_lik[i] = binomial_lpmf(correct[i] | 1 , p[i] );
}
