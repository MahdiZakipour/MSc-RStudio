m.total = 80
l.foot = .2
l.leg = .435

# foot - kaf paa
teta = 1.37*pi/180

m <- 0.0145* m.total
l.od <- .5*l.foot
l.op <- l.foot - l.od
r.o <- .475*l.foot
i.o <- m*(r.o.foot^2)

rx3 <- m*3.25-160.25
ry3 <- -765.96 + m.foot*9.8 + m*1.78
m3 <- ry3*l.op*cos(teta)+rx3*l.op*sin(teta) - 160.25*l.od*sin(teta)-765.96*l.od*cos(teta) + i.o*(-45.35)
