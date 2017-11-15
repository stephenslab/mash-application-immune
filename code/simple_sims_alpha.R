# ---- simple_sims_alpha ----
simple_sims_alpha = function(nsamp = 100, ncond = 5, err_sd, alpha){
  Balpha.id = matrix(rnorm(nsamp * ncond), nrow = nsamp, ncol = ncond)

  b = rnorm(nsamp)
  Balpha.all = matrix(rep(b, ncond), nrow = nsamp, ncol = ncond)

  Balpha.zero = matrix(0, nrow = nsamp, ncol = ncond)

  Balpha.one = Balpha.zero
  b2 = rnorm(nsamp)
  Balpha.one[, 1] = b2

  Balpha = rbind(Balpha.zero, Balpha.id, Balpha.one, Balpha.all)

  E.alpha= matrix(rnorm(nrow(Balpha)*nrow(Balpha), sd=err_sd^(1-alpha)), nrow = nrow(Balpha),
                  ncol = ncol(Balpha), byrow = T)
  Balpha.hat = Balpha+E.alpha

  Shat = matrix(err_sd, nrow = nrow(Balpha), ncol = ncol(Balpha), byrow=T)
  Bhat = Balpha.hat*Shat^(alpha)
  B = Balpha*Shat^(alpha)
  return(list(B=B,Bhat=Bhat,Shat=Shat))
}
