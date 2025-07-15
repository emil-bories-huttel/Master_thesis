stage1 <- bf(y1i_post ~ 1 + treat + y1i_prior) 
stage2 <- bf(pro_dev_post ~ 1 + y1i_post + pro_dev_prior) 

eq10_3 <- brm(data = df, 
              family = gaussian,
              stage1 + stage2 + set_rescor(TRUE),
              prior = c(
                # first stage
                prior(normal(0, 11.24), class = Intercept, resp = y1ipost),
                prior(normal(0, 11.24), class = b, resp = y1ipost),
                prior(exponential(11.24), class = sigma, resp = y1ipost),
                
                # second stage 
                prior(normal(0, 0.49), class = Intercept, resp = prodevpost),
                prior(normal(0, 0.49), class = b, resp = prodevpost),
                prior(exponential(0.49), class = sigma, resp = prodevpost),
                
                # rho
                prior(lkj(2), class = rescor)),
              
              iter = 2000, 
              warmup = 1000, 
              chains = 4, 
              cores = 4,
              seed = 14)

saveRDS(eq10_3, "Models/eq10_3.rds") 

eq10_4 <- brm(data = df, 
              family = gaussian,
              stage1 + stage2 + set_rescor(TRUE),
              prior = c(
                # first stage
                prior(normal(0, 11.24), class = Intercept, resp = y1ipost),
                prior(normal(0, 11.24), class = b, resp = y1ipost),
                prior(exponential(11.24), class = sigma, resp = y1ipost),
                
                # second stage 
                prior(normal(0, 0.49), class = Intercept, resp = prodevpost),
                prior(normal(.23, 0.49), class = b, resp = prodevpost),
                prior(exponential(0.49), class = sigma, resp = prodevpost),
                
                # rho
                prior(lkj(2), class = rescor)),
              
              iter = 2000, 
              warmup = 1000, 
              chains = 4, 
              cores = 4,
              seed = 14)

saveRDS(eq10_4, "Models/eq10_4.rds") 
