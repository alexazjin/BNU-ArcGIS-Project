junc_ <- read_csv("~/Desktop/junc----.csv")
junc_ <- junc_ %>% select(TARGET_FID, Join_Count)
junc_
poi_ <- poi_ %>% select(TARGET_FID, Join_Count)
poi_

all1 <- read_csv("~/Desktop/total_005.csv")
all1 <- all1 %>% filter(poi<600)
#unit: number/km^2
all1 <- mutate(all1, dpoi= poi*187.5, djunc = junc * 0.847)


all1 <- all1 %>% mutate(mean1 = mean(dpoi))
all1 <- all1 %>% mutate(resid = dpoi-mean1)

ggplot(data=all1)+ geom_point(mapping = aes(x = djunc, y=resid))+
  geom_hline( yintercept = 0, color = "hotpink")

all1 <- all1 %>% mutate(djunc2 = (djunc)^2)
ggplot(data=all1)+ geom_point(mapping = aes(x = djunc2, y=dpoi)) +
  geom_smooth(aes(x=djunc2, y=dpoi), method = "lm")

  ggplot(data = all1) + 
  geom_point(mapping = aes(x = djunc, y = dpoi)) +
geom_smooth(aes(x=djunc, y=dpoi), method = "lm")+
  labs( x= "junc_density", y= "poi_density" , title = "y=437.1x + 6033.9 (fishnet_0.005)")
  
  lm(dpoi ~ djunc, data=all1)
cor(all1[["dpoi"]], all1[["djunc"]])



all2 <- read_csv("~/Desktop/total_008.csv")
all2 <- mutate(all2, dpoi= poi * 73.2, djunc = junc * 0.331)

  ggplot(data = all2) + 
  geom_point(mapping = aes(x = djunc, y = dpoi),) +
  geom_smooth(aes(x=djunc, y=dpoi), method = "lm") +
    labs( x= "junc_density", y= "poi_density", title = "y=338.9x + 4269.2(fishnet_0.008)")
  
cor(all2[["dpoi"]], all2[["djunc"]])

lm(dpoi ~ djunc, data=all2)

all3 <- read_csv("~/Desktop/total_01.csv")
all3 <- filter(all3, poi < 1170)
all3 <- mutate( all3, dpoi = poi * 46.875, djunc = junc * 0.212)
ggplot(data = all3) + 
  geom_point(mapping = aes(x = djunc, y = dpoi)) +
  geom_smooth(aes(x=djunc, y=dpoi), method = "lm") +
  labs( x= "junc_density", y= "poi_density", title = "y=423.4x + 2336.0 (fishnet_0.01)")

lm(dpoi ~ djunc, data=all3)
cor(all3[["dpoi"]], all3[["djunc"]])



all <- lm(poi ~ junction + I(junction^2), data = all)

summary(all)

library(modelr)

junction2 <- junction^2
ggplot(data = all) + geom_point(mapping = aes(x = junction2, y = poi)) +
  geom_smooth(aes(x=junction2, y=poi), method = "lm") +
  cor(all[["junction2"]], all[["poi"]])
lm(poi ~ junction, data = all)
lm(poi ~ junction2, data=all)