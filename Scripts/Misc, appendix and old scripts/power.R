library(pwr)
library(broom)

pwrt1 <- pwr.t.test(d=.3,n=c(1:1000),
           sig.level=.05,type="two.sample",alternative="two.sided") |> 
  tidy()

pwrt1 |>
  ggplot(aes(x=n,y=power)) +
  geom_line(colour="maroon", alpha=0.5) + 
  theme_bw() + 
  (geom_hline(yintercept = 0.8, 
              linetype = "dashed")) +
  geom_vline(xintercept = 176, 
             linetype = "dashed") +
  annotate("text", x = 250, y=.5, label="n = 176") +
  labs(y="Power",
       x=NULL) +
  ylim(.1,1) 

# see power for 230 
pwrt1 |> 
  filter(n==235) |> 
  select(power)  
