
require(longpower)
# https://cran.r-project.org/web/packages/longpower/vignettes/longpower.html
n = 3 # visits
t = c(0,2,5)
rho = c(0.2, 0.5, 0.8)
sigma2 = c(100, 200, 300)
tab.diggle = outer(rho, sigma2, 
                   Vectorize(function(rho, sigma2){
                     ceiling(diggle.linear.power(
                       d=0.5,
                       t=t,
                       sigma2=sigma2,
                       R=rho,
                       alternative="one.sided",
                       power=0.80)$n[1])}))
colnames(tab.diggle) = paste("sigma2 =", sigma2)
rownames(tab.diggle) = paste("rho =", rho)
tab.diggle

u <- list(u1 = t, u2 = rep(0,n))
v <- list(v1 = cbind(1,1,t),
          v2 = cbind(1,0,t))  
tab.ll <- outer(rho, sigma2, 
                Vectorize(function(rho, sigma2){
                  ceiling(liu.liang.linear.power(
                    delta=0.5, u=u, v=v,
                    sigma2=sigma2,
                    R=rho, alternative="one.sided",
                    power=0.80)$n[1])}))
colnames(tab.ll) <- paste("sigma2 =", sigma2)
rownames(tab.ll) <- paste("rho =", rho)
tab.ll


# Aim 2

n_sidd = 40
n_mod = 60
n_sird = 80

n = c(n_sidd,n_mod,n_sird)
R = c(0.2,0.6)
power_df = expand.grid(n,R) 


power_df$delta <- map2(power_df$Var1,power_df$Var2,
         .f = function(v1,v2){
           p_out = diggle.linear.power(
             n = v1, # median: 12 obs
             # delta = 0.02,
             t = c(0:11),
             sigma2 = 1,
             R = v2,
             sig.level=0.05,
             power = 0.8,
             alternative = "two.sided"
             
           )
           
           p_out$delta %>% 
             return()
           
         }) %>% 
  unlist()




