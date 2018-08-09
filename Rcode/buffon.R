set.seed(123)
pin <- 1  # keep length <= 1
hyp <- pin/2
trials <- 10000
  angle <- runif(trials, 0, 360)
  pin.center <- runif(trials, 0, 1)
  closest.line <- ifelse(pin.center <= 0.5, pin.center, 1-pin.center)
  pin.tip <- abs(hyp * cos(angle))
  touches <- ifelse(pin.tip >= closest.line, 1, 0)
  sum(touches)/trials 
  
sum(touches)/trials -> p
2/sum(touches)*10000

s <- numeric()
len <- 200
for (i in 1:len) {
  s[i] <- (2/(sum(touches[1:i])/i))
}


#plot pi estimate
pdf(file="~/Desktop/pi.pdf"
    , height=3
    , width=3
    #, onefile=TRUE  
    #, family='Helvetica' 
    #, paper='letter'
    #, pointsize=12 
)
par(mar = c(2, 2.5, 0, 0.5) + 0.1)
par(oma = c(0, 0, 0, 0))
plot(1:len,s, type="l"
, ann=FALSE  # turn off main title and axis titles
, bty="n"  # turn off box around plot
, xaxt="n"  # turn off x-axis labels
, yaxt="n")  # turn off y-axis labels
abline(h=pi, col="red")

axis(side=1, at=c(0,200), labels=NA, tck= -0.01)
axis(side=2, at=c(2,pi,5),labels=NA, tck= -0.01, las = 1)
#
# Then plot labels and move them closer to tickmarks
axis(side = 1, at=c(0,200), labels=c(1,200), lwd = 0, line = -.7)  # line controls how close tick labels are to tick marks
axis(side = 2, at=c(2,5), labels=c(2,5), lwd = 0, line = -.5, las = 1)
axis(side = 2, at=pi, labels=expression(pi), lwd = 0, line = -.6, las = 1, cex.axis=1.5)
#
# Then place Axis Title text closer to axis labels
mtext(side = 1, "Trials", line = 1)
mtext(side = 2, "Estimate", line = 1.5)
dev.off()



## Plot all needles
pdf(file="~/Desktop/tmp.pdf"
    , height=4
    , width=4
    #, onefile=TRUE  
    #, family='Helvetica' 
    #, paper='letter'
    #, pointsize=12 
)

old.mar <- par()$mar  # original margin parameters
old.oma <- par()$oma  # original outside margin parameters
par(mar = c(0, 0, 0, 0) + 0.1)
par(oma = c(0, 0, 0, 0))

jit <- runif(trials,0,1)
x1 <- pin.center-pin.tipx
x2 <- pin.center+pin.tipx
y1 <- jit - pin.tipy
y2 <- jit + pin.tipy

dat <- data.frame(x1,x2,y1,y2,pin.center,jit)
b <- ggplot(dat, aes(pin.center, jit))
b <- b + geom_segment(aes(x=c(0,1), y=c(-.5,-.5),
                          xend = c(0,1), yend = c(1.5,1.5)),
                      colour="red", size=1.3)
b <- b + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), alpha=1/30)
b <- b + geom_segment(aes(x=x1[c(400,5000)], y=y1[c(400,5000)],
                            xend = x2[c(400,5000)], yend = y2[c(400,5000)]),
                        colour="blue", size=1.3)
b <- b + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        line = element_blank(),
        text = element_blank(), 
        rect=element_blank()) +
  labs(title = element_blank())
#print(b)
ggsave("~/Desktop/tmp2.png", b, height=5, width=5)
par(mar=old.mar) # reset original margin parameter
par(oma=old.oma)  # reset original outside margin parameters
dev.off()