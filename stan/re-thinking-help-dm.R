library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition



m11.1 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm( 0 , 10 )
  ) , data=d )

rethinking::stancode(m11.1)

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment) )

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )

stancode(m11.4)

precis( m11.4 , depth=2 )

post <- extract.samples(m11.4)
p_left <- inv_logit( post$a )

plot( precis( as.data.frame(p_left) ) , xlim=c(0,1) )


m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )

stancode(m11.4)

precis( m11.4 , depth=2 )


data_m1.1

m11.sdrt <- ulam(
  alist(
    correct ~ dbinom( 1 , p ) ,
    logit(p) <-  b[scale_degree] ,
  #  a[participant] ~ dnorm( 0 , 1.5 ),
    b[scale_degree] ~ dnorm( 0 , 0.5 )
  ) , data=data_m1.1 , chains=4 , log_lik=TRUE )

precis( m11.sdrt , depth=2 )

post <- extract.samples(m11.sdrt)
correct_x <- inv_logit( post$b )
plot( precis( as.data.frame(correct_x) ) , xlim=c(0,1) )


stancode(m11.sdrt)



