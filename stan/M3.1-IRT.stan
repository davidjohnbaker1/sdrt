// Model 3.1 IRT Model 

data {
  // Bounds 
  int<lower=1> J;                     // number of students
  int<lower=1> K;                     // number of questions
  int<lower=1> N;                     // number of observations
  
  // Observations
  int student[N];
  int question[N];
  int answer[N];
  
}

parameters {
  real delta;            // mean student ability
  real alpha[J];         // ability of student j - mean ability
  real beta[K];          // difficulty of question k
}

model {
  alpha ~ std_normal();         // informative true prior
  beta ~ std_normal();          // informative true prior
  delta ~ normal(0.75, 1);      // informative true prior
  for (n in 1:N) {
    answer[n] ~ bernoulli_logit(alpha[student[n]] - beta[question[n]] + delta);
  }
}
