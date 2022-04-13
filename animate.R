if (!require(caTools)) install.packages('caTools')
library(caTools)

source("./gol.R")

storage = run(100, 100, 0.25, 200, disp=FALSE)
write.gif(storage, "gol.gif", col="jet", delay=25)
