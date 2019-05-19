Sigma = matrix(c(1,0.75,0.1,
                 0.75,1,-0.25,
                 0.1,-0.25,1), nrow = 3)
X = MASS::mvrnorm(300, c(0,0,0), Sigma)

library(ggplot2)

p1 = ggplot()+
  aes(x = X[,1], y = X[,2])+
  geom_point(size = 0.5)+
  labs(x = expression(X[1]), y = expression(X[2]))+
  theme_classic()+
  theme(axis.text = element_blank())

p2 = ggplot()+
  aes(x = X[,1], y = X[,3])+
  geom_point(size = 0.5)+
  labs(x = expression(X[1]), y = expression(X[3]))+
  theme_classic()+
  theme(axis.text = element_blank())

p3 = ggplot()+
  aes(x = X[,2], y = X[,3])+
  geom_point(size = 0.5)+
  labs(x = expression(X[2]), y = expression(X[3]))+
  theme_classic()+
  theme(axis.text = element_blank())

library(gridExtra)

grid.arrange(p1, p2, p3, nrow = 1)

pc = prcomp(X)
pc$rotation

xtable::xtable(pc$rotation[,-3])
