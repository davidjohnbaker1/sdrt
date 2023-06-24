// Model 2.2 -- Full Multi-Tone Model with Serial Order 
// TODO: Consider with Likert data here?
/////////////////////
data {
  int< lower = 1 > n;                                                           // Number of Observations, can't go lower than 1 
  int n_lengths;                                                                // The Number 5, 5 Lengths (2,3,5,7,9)
  int n_participants;                                                           // The number 39, 39 Participants
  int n_scale_degrees;                                                          // The number 12, 12 scale degrees
  int n_serials;                                                                 // The number 9, 9 possible serial order positions (should this be ordinal?) 
  int n_melodies;                                                               // Also 39, for different melodies
  
  int length[n];                                                                // Array for all length/block index variables
  int participant[n];                                                           // Array for all participant id data 
  int scale_degree[n];                                                          // Array to hold each scale degree in each trial 
  int serial[n];                                                                // Array that holds all serial position data 
  int correct[n];                                                               // Array to hold all response variables (0 or 1)
  int melody[n];                                                                // Array for all melody data 
  
  int scale_degrees[n_scale_degrees];                                           // Array of length 12 for all twelve keys (TODO: Get group here to work)
  int participants[n_participants];                                             // Array of length 39 to index through participants
  int lengths[n_lengths];                                                       // Array of lenght 5 to index through all block-lengths 
  int serials[n_serials];                                                       // Array of length 9 for each location data can be at
  int melodies[n_melodies];                                                     // Array length 39 for each melody
}

parameters{
  vector[n_participants] alpha;                                                 // Estimate 39 intercepts for participants 
  vector[n_scale_degrees] beta;                                                 // Estimate 12 intercepts for SD
  vector[n_lengths] kappa;                                                      // Estimate 5 intercepts for blocks/lenghts 
  vector[n_serials] epsilon;
  vector[n_melodies] meppa;                                                     // Estimate 39 intercepts for melodies
  
  real alpha_bar;                                                               // Estimate different slope for each participant
  real meppa_bar;                                                               // Estimate different slope for each melody
  real<lower = 0> sigma_alpha;                                                  // Estimate single variance parameter for participant 
  real<lower = 0> sigma_beta;                                                   // Estimate single variance parameter for scale degree 
  real<lower = 0> sigma_kappa;                                                  // Estimate single variance parameter for key 
  real<lower = 0> sigma_epsilon;                                                // Estimate single error for serials
  real<lower = 0> sigma_meppa;                                                  // Estimate error for melodies
  
  }
  

model {
  vector[n] p;
  
    // Priors
    
    // Beta: Fixed effecto for each scale degree 
    beta ~ normal( 0 , sigma_beta); // For now 
    kappa ~ normal(0, sigma_kappa);
    epsilon ~ normal(0, sigma_epsilon); // Should this reflect that more likey to get it right over time?  

    // Adaptive priors 
    
    // TODO: All random effects have mean of 0, but put intercept somewhere else!!!! (Mu + participant + beta )
    
    // Alpha: Participant, allow this to vary 
    alpha ~ normal(alpha_bar, sigma_alpha);
    meppa ~ normal(meppa_bar, sigma_meppa);
    
    // Hyper Priors 
    alpha_bar ~ normal(0, 1.5);
    meppa_bar ~ normal(0,1.5);
   
    sigma_alpha ~ exponential(1);
    sigma_beta ~ exponential(1);
    sigma_kappa ~ exponential(1);
    sigma_epsilon ~ exponential(1);
    
  // Priors
  // alpha ~ normal(0, 1.5);
  // beta ~ normal(0, 0.5);
  // gamma ~ normal(0, 0.5);

    // Linear Combination 
    for ( i in 1:n ) {
        p[i] = alpha[participant[i]] + beta[scale_degree[i]] + kappa[length[i]] + epsilon[serial[i]] + meppa[melody[i]];   // Now Matches stan output, is this right style with i? If so, why not
        p[i] = inv_logit(p[i]);
    }
    correct ~ binomial( 1 , p);
  
}

generated quantities{
    vector[n] log_lik;
     vector[n] p;
    for ( i in 1:n ) {
        p[i] = alpha[participant[i]] + beta[scale_degree[i]] + kappa[length[i]] + epsilon[serial[i]] + meppa[melody[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:n ) log_lik[i] = binomial_lpmf( correct[i] | 1 , p[i] );
}
