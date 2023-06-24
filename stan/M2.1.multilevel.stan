// Model 2.1 -- Why not put single tone here as well? TODO 
/////////////////////
data {
  int< lower = 1 > n;                                                           // Number of Observations, can't go lower than 1 
  int n_lengths;                                                                // The Number 5, 5 Lengths (2,3,5,7,9)
  int n_participants;                                                           // The number 39, 39 Participants
  int n_scale_degrees;                                                          // The number 12, 12 scale degrees
  
  int length[n];                                                                // Array for all length/block index variables
  int participant[n];                                                           // Array for all participant id data 
  int scale_degree[n];                                                          // Array to hold each scale degree in each trial 
  int correct[n];                                                               // Array to hold all response variables (0 or 1)
  
  int scale_degrees[n_scale_degrees];                                           // Array of length 12 for all twelve keys (TODO: Get group here to work)
  int participants[n_participants];                                             // Array of length 39 to index through participants
  int lengths[n_lengths];                                                       // Array of lenght 5 to index through all block-lengths 
}

parameters{
  vector[n_participants] alpha;                                                 // Estimate 39 intercepts for participants 
  vector[n_scale_degrees] beta;                                                 // Estimate 12 intercepts for SD
  vector[n_lengths] kappa;                                                      // Estimate 5 intercepts for blocks/lenghts 
  
  real alpha_bar;                                                               // Estimate different slope for each participant
  real<lower = 0> sigma_alpha;                                                  // Estimate single variance parameter for participant 
  real<lower = 0> sigma_beta;                                                   // Estimate single variance parameter for scale degree 
  real<lower = 0> sigma_kappa;                                                  // Estimate single variance parameter for key 
  
  }
  

model {
  vector[n] p;
  
    // Priors
    
    // Beta: Fixed effecto for each scale degree 
    beta ~ normal( 0 , sigma_beta); // For now 

    // Adaptive priors 
    
    // Alpha: Participant, allow this to vary 
    alpha ~ normal(alpha_bar, sigma_alpha);
    kappa ~ normal(0, sigma_kappa);
    
    // Hyper Priors 
    alpha_bar ~ normal(0, 1.5);
   
    sigma_alpha ~ exponential(1);
    sigma_beta ~ exponential(1);
    sigma_kappa ~ exponential(1);
    
  // Priors
  // alpha ~ normal(0, 1.5);
  // beta ~ normal(0, 0.5);
  // gamma ~ normal(0, 0.5);

    // Linear Combination 
    for ( i in 1:n ) {
        p[i] = alpha[participant[i]] + beta[scale_degree[i]] + kappa[length[i]];   // Now Matches stan output, is this right style with i? If so, why not
        p[i] = inv_logit(p[i]);
    }
    correct ~ binomial( 1 , p);
  
}

generated quantities{
    vector[n] log_lik;
     vector[n] p;
    for ( i in 1:n ) {
        p[i] = alpha[participant[i]] + beta[scale_degree[i]] + kappa[length[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:n ) log_lik[i] = binomial_lpmf( correct[i] | 1 , p[i] );
}
