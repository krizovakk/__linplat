# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")

require(tidyverse) #zakladni balik  
require(readxl) #nacitani excelovych tabulek
require(reshape2) #prevod na long format
require(easynls) #linplat model auto
library(rcompanion) #plotPredy / manual linplat


sb <- read_excel("red/linplat.xlsx") #sb = sugar beet
sb$n <- paste(sb$dose, sb$treat, sep = "_")

# sb$n <- factor(sb$n, levels = c("0_Control","80_NPK1","120_NPK2","160_NPK3", "200_NPK4",
#                           "105_FYM","185_FYM+NPK1","225_FYM+NPK2","265_FYM+NPK3","305_FYM+NPK4"))

sb$n <- factor(sb$n, levels = c("0_Control","80_NPK1","105_FYM","120_NPK2","160_NPK3","185_FYM+NPK1","200_NPK4",
                          "225_FYM+NPK2","265_FYM+NPK3","305_FYM+NPK4")) # ordered

sb$year <- as.factor(sb$year)

## TUBER

tube <- sb %>% 
  select(year, treat, dose, n, tuber)

## TOP

top <- sb %>% 
  select(year, treat, dose, n,  top)

# explorative -------------------------------------------------------------

ggplot(tube, aes(n, tuber, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TUBER", x = "", y = "Tuber yield (t ha-1)")
ggsave("tuber_ordered.png", path = "plots", height = 6, width = 9, dpi = 300)  

ggplot(top, aes(n, top, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TOP", x = "", y = "Top yield (t ha-1)")
ggsave("top_ordered.png", path = "plots", height = 6, width = 9, dpi = 300)  


# linplat model -----------------------------------------------------------

# install.packages("easynls")
#require(easynls)

## create dataframe for TUBER
df_tube <- data.frame(tube$dose, tube$tuber)

## create dataframe for TUBER
df_top <- data.frame(top$dose, top$top)

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_tube, model=1) #linear
nlsplot(df_tube, model=2) #quadratic
nlsplot(df_tube, model=3) #linear-plateau

## RESULTS

nlsfit(df_tube, model=3)
nlsfit(df_top, model=3)

nlsplot(df_tube, model=3, xlab = "N dose [kg ha-1]", ylab = "tuber yield [t ha-1]")
nlsplot(df_top, model=3, xlab = "N dose [kg ha-1]", ylab = "top yield [t ha-1]")



# linplat manual ----------------------------------------------------------

## https://rcompanion.org/handbook/I_11.html

# install.packages("rcompanion") # quite long installation, 5 minutes
#library(rcompanion) # for plot part

## TOP

fit.lm    = lm(top ~ dose, data=top) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(top$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_top = nls(top ~ linplat(dose, a, b, clx), # Find best fit parameters
                   data = top,
                   start = list(a   = a.ini,
                                b   = b.ini,
                                clx = clx.ini),
                   trace = FALSE,
                   nls.control(maxiter = 1000))

summary(model_top)

#par(mar = c(2,0,0,0))
#par(oma = c(3,0,0,0))
plotPredy(data  = top,
          x     = dose,
          y     = top,
          model = model_top,
          main  = "",
          xlab  = "N dose [kg.ha-1]",
          ylab  = "top yield [t.ha-1]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = top$dose, labels = top$dose, 
     las = 2, cex.axis = 1.1)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 17.6446+0.0432(x-200.8485)", side = 3, line = 1,
      outer = FALSE, cex = 1.2, col = "blue")

## TUBER

fit.lm    = lm(tuber ~ dose, data=tube) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(tube$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_tube = nls(tuber ~ linplat(dose, a, b, clx), # Find best fit parameters
                data = tube,
                start = list(a   = a.ini,
                             b   = b.ini,
                             clx = clx.ini),
                trace = FALSE,
                nls.control(maxiter = 1000))

summary(model_tube)

#par(mar = c(2,0,0,0))
#par(oma = c(3,0,0,0))
plotPredy(data  = tube,
          x     = dose,
          y     = tuber,
          model = model_tube,
          main  = "",
          xlab  = "N dose [kg.ha-1]",
          ylab  = "tuber yield [t.ha-1]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = tube$dose, labels = tube$dose, 
     las = 2, cex.axis = 1.1)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 54.0439+0.0671(x-183.0802)", side = 3, line = 1,
      outer = FALSE, cex = 1.2, col = "blue")



# ***2ND ROUND*** --------------------------------------------------------------
## separate plots for NPK and FYM

# base --------------------------------------------------------------------

require(tidyverse) #zakladni balik  
require(readxl) #nacitani excelovych tabulek
require(reshape2) #prevod na long format
require(easynls) #linplat model auto
library(rcompanion) #plotPredy / manual linplat

sb <- read_excel("red/linplat.xlsx") 
sb$n <- paste(sb$dose, sb$treat, sep = "_")

sb$year <- as.factor(sb$year)

# sb$n <- factor(sb$n, levels = c("0_Control","80_NPK1","105_FYM","120_NPK2","160_NPK3","185_FYM+NPK1","200_NPK4",
#                                 "225_FYM+NPK2","265_FYM+NPK3","305_FYM+NPK4")) 

## separate data for NPK

tar_npk <- c("Control", "NPK1", "NPK2", "NPK3", "NPK4") # for filtering NPK
n_npk <- c("0_Control", "80_NPK1", "120_NPK2",
           "160_NPK3", "200_NPK4") # levels of n factor

npk <- sb %>% 
  filter(treat %in% tar_npk)

## separate data for FYM

tar_fym <- c("Control", "FYM", "FYM+NPK1", "FYM+NPK2", "FYM+NPK3", "FYM+NPK4")
n_fym <- c("0_Control", "105_FYM", "185_FYM+NPK1", "225_FYM+NPK2", 
           "265_FYM+NPK3", "305_FYM+NPK4")

fym <- sb %>% 
  filter(treat %in% tar_fym)

## TUBER

tube_npk <- npk %>% 
  select(year, treat, dose, n, tuber)
tube_npk$n <- factor(tube_npk$n, 
                     levels = n_npk)

tube_fym <- fym %>% 
  select(year, treat, dose, n, tuber)
tube_fym$n <- factor(tube_fym$n, levels = n_fym)

## TOP

top_npk <- npk %>% 
  select(year, treat, dose, n,  top)
top_npk$n <- factor(top_npk$n, levels = n_npk)

top_fym <- fym %>% 
  select(year, treat, dose, n,  top)
top_fym$n <- factor(top_fym$n, levels = n_fym)

# explorative -------------------------------------------------------------

##  TUBER

ggplot(tube_npk, aes(n, tuber, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TUBER / NPK", x = "", y = "Tuber yield (t ha-1)")
ggsave("tuber_npk.png", path = "plots", height = 6, width = 9, dpi = 300) 

ggplot(tube_fym, aes(n, tuber, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TUBER / FYM", x = "", y = "Tuber yield (t ha-1)")
ggsave("tuber_fym.png", path = "plots", height = 6, width = 9, dpi = 300) 

## TOP

ggplot(top_npk, aes(n, top, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TOP / NPK", x = "", y = "Top yield (t ha-1)")
ggsave("top_npk.png", path = "plots", height = 6, width = 9, dpi = 300)  

ggplot(top_fym, aes(n, top, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TOP / FYM", x = "", y = "Top yield (t ha-1)")
ggsave("top_fym.png", path = "plots", height = 6, width = 9, dpi = 300)  

# linplat model -----------------------------------------------------------

#install.packages("easynls")
#require(easynls)

## create dataframes for linplat model

df_tube_npk<- data.frame(tube_npk$dose, tube_npk$tuber) #TUBER / NPK

df_tube_fym <- data.frame(tube_fym$dose, tube_fym$tuber) #TUBER / FYM

df_top_npk <- data.frame(top_npk$dose, top_npk$top) # TOP / NPK

df_top_fym <- data.frame(top_fym$dose, top_fym$top) # TOP / FYM

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_tuber, model=1) #linear
nlsplot(df_tuber, model=2) #quadratic
nlsplot(df_tuber, model=3) #linear-plateau

## RESULTS

nlsfit(df_tube_npk, model=3)
nlsfit(df_tube_fym, model=3)

nlsfit(df_top_npk, model=3)
nlsfit(df_top_fym, model=3)

nlsplot(df_tube_fym, model=3, xlab = "N dose [kg ha-1]", 
        ylab = "tuber yield [t ha-1]")
nlsplot(df_top_fym, model=3, xlab = "N dose [kg ha-1]", 
        ylab = "top yield [t ha-1]")

nlsplot(df_top_fym, model=3, 
        start = c(a = 17.4, b = 0.0339, c = 180.8333), 
        xlab = "Explanatory Variable" , 
        ylab = "Response Variable", position = 1) # might be the path

# linplat manual ----------------------------------------------------------

## https://rcompanion.org/handbook/I_11.html

# install.packages("rcompanion") # quite long installation, 5 minutes
#library(rcompanion) # for plot part

## TOP NPK / done

fit.lm    = lm(top ~ dose, data=top_npk) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(top_npk$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_topnpk = nls(top ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = top_npk,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_topnpk)

plotPredy(data  = top_npk,
          x     = dose,
          y     = top,
          model = model_topnpk,
          main  = "NPK",
          xlab  = "N dose [kg/ha]",
          ylab  = "top yield [t/ha]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = top_npk$dose, labels = top_npk$dose, 
     las = 1, cex.axis = 1.1)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 17.4037+0.0639(x-122.2971)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")

# 
## TOP FYM /// TO BE SOLVED LATER
# 


fit.lm    = lm(top ~ dose, data=top_fym) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(top_fym$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_topfym = nls(top ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = top_fym,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            # start = list(a = 17.4, b = 0.039, clx = 180.8333),
            trace = TRUE,
            nls.control(maxiter = 1000))

summary(model_topfym)

### model is not functional; solution:

# install.packages("FertBoot")
require(FertBoot)

tf <- read_excel("red/top_fym.xlsx")   

df_top_fym <- data.frame(top_fym$dose, top_fym$top) 

f_mod <- f.linear.plateau(
  df_top_fym,
  start = list(a = 17.4, b = 0.039, c = 180.833),
  plus_minus = 10,
  n.start = 1000,
  msg = TRUE)

summary(f_mod$nls.model)

plotPredy(data  = tf,
          x     = dose,
          y     = top,
          model = f_mod$nls.model,
          main  = "FYM",
          xlab  = "N dose [kg/ha]",
          ylab  = "top yield [t/ha]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)

axis(1, at = tf$dose, labels = tf$dose, 
     las = 1, cex.axis = 1.1)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 17.4037+0.0339(x-180.8333)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")
  

## TUBE FYM / done

fit.lm    = lm(tuber ~ dose, data=tube_fym) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(tube_fym$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_tubefym = nls(tuber ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = tube_fym,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_tubefym)

plotPredy(data  = tube_fym,
          x     = dose,
          y     = tuber,
          model = model_tubefym,
          main  = "FYM",
          xlab  = "N dose [kg/ha]",
          ylab  = "tuber yield [t/ha]",
          xaxt  = "n", 
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
axis(1, at = tube_fym$dose, labels = tube_fym$dose, 
     las = 1, cex.axis = 1.1)
# text(105,50, "y = 52.9120+0.0789(x-165.4596)", col = "blue", cex=0.9)
mtext("y = 52.9120+0.0789(x-165.4596)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")

## TUBE NPK / done

fit.lm    = lm(tuber ~ dose, data=tube_npk) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(tube_npk$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_tubenpk = nls(tuber ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = tube_npk,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_tubenpk)

# par(mar = c(6,0,0,0))
# par(oma = c(1,0,0,0))
plotPredy(data  = tube_npk,
          x     = dose,
          y     = tuber,
          model = model_tubenpk,
          main  = "NPK",
          xlab  = "N dose [kg/ha]", 
          ylab  = "tuber yield [t/ha]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
axis(1, at = tube_npk$dose, labels = tube_npk$dose, 
     las = 1, cex.axis = 1.1)
# text(140,55, "y = 52.9120+0.1046(x-111.5920)", col = "blue", cex=0.9)
mtext("y = 52.9120+0.1046(x-111.5920)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")

# TOPFYM issue ------------------------------------------------------------

# install.packages("FertBoot")
require(FertBoot)

df_top_fym <- data.frame(top_fym$dose, top_fym$top) 

f_mod <- f.linear.plateau(
  df_top_fym,
  start = list(a = 17.4, b = 0.039, c = 180.833),
  plus_minus = 10,
  n.start = 1000,
  msg = TRUE)

summary(f_mod$nls.model)

##

# install.packages("easyreg")
require(easyreg)

er1(top_fym, model = 3, start = c(a = 17.4, b = 0.0390, c = 180:8333),
    mixed=FALSE, digits=6, alpha=0.05)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

##

model_topfym = nls(top ~ linplat(dose, a, b), # Find best fit parameters
                   data = top_fym,
                   start = list(a   = a.ini,
                                b   = b.ini),
                   # start = list(a = 17.4, b = 0.039, clx = 180.8333),
                   trace = TRUE,
                   nls.control(maxiter = 1000))

summary(model_topfym)

##

df_top_fym$top_fym.dose[df_top_fym$top_fym.dose == 0] <- 0.000001


tf <- read_excel("red/top_fym.xlsx")                  


plotPredy(data  = tf,
          x     = dose,
          y     = top,
          model = f_mod$nls.model,
          main  = "FYM",
          xlab  = "N dose [kg/ha]",
          ylab  = "top yield [t/ha]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)

axis(1, at = tf$dose, labels = tf$dose, 
     las = 1, cex.axis = 1.1)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 17.4+0.0339(x-180.8333)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")




plotPredy(data  = tf,
          x     = dose,
          y     = top,
          model = f_mod$nls.model,
          main  = "FYM",
          xlab  = "N dose [kg/ha]",
          ylab  = "top yield [t/ha]",
          xaxt  = "n",
          cex   = 2,
          cex.lab=2, 
          cex.axis=2, 
          cex.main=2)

axis(1, at = tf$dose, labels = tf$dose, 
     las = 1, cex.axis = 2)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 17.4+0.0339(x-180.8333)", side = 3, line = 0,
      outer = FALSE, cex = 1.5, col = "blue")



# plotPredy(data  = df_top_fym,
#           x     = c(0,105,185,225,265,305,0,105,185,225,265,305,0,105,185,225,265,305),
#           y     = c(20.360000, 23.840000, 32.780000, 33.750000, 31.910000, 30.310000,
#                     22.751323, 24.761905, 30.070547, 33.42151, 30.458554, 36.437390,
#                     9.003527,  9.576720, 13.209877, 13.527337, 13.280423, 13.756614),
#           model = f_mod$nls.model,
#           xlab  = "FYM / N dose [kg/ha]",
#           ylab  = "top yield [t/ha]")


  
  



