### Decision Variables
x.14 <- 0
x.13 <- 0
x.23 <- 0
x.24 <- 0
x.34 <- 0

### Travel Time in Arc
t.14 <- function(x.14) {20+0.01*x.14}
t.13 <- function(x.13) {10+0.001*x.13}
t.23 <- function(x.23) {12+0.001*x.23}
t.24 <- function(x.24) {20+0.01*x.24}
t.34 <- function(x.34) {6+0.001*x.34}
x.14.f <- function(t.14.v) {10000-10*t.14.v}
x.24.f <- function(t.24.v) {8000-10*t.24.v}

### Simulation
alpha <- 0.1
t.travel <- c(t.14(x.14),t.13(x.13),t.23(x.23),t.24(x.24),t.34(x.34))
x.arc <- c(x.14,x.13,x.23,x.24,x.34)
Assignment.df <- data.frame(row.names = c("Arc.14","Arc.13","Arc.23","Arc.24","Arc.34"), "x"=x.arc, "t"=t.travel)
x.14.addition <- c(0,0,0,0,0)
x.24.addition <- c(0,0,0,0,0)
ite <- 0
x.14.prev <- 0
x.24.prev <- 0
while (ite < 30){
        x.14.addition <- c(0,0,0,0,0)
        x.24.addition <- c(0,0,0,0,0)
        t.travel <- c(t.14(x.arc[1]),t.13(x.arc[2]),t.23(x.arc[3]),t.24(x.arc[4]),t.34(x.arc[5]))
        if (t.travel[1] < t.travel[2]+t.travel[5]) {
                x.14.addition[1] <- alpha * (x.14.f(t.travel[1])-x.14.prev)
                x.arc[1] <- x.arc[1] + x.14.addition[1]
                x.14.prev <- x.14.prev + x.14.addition[1]
                }
        else if (t.travel[1] >= t.travel[2]+t.travel[5]) {
                x.14.addition[c(2,5)] <- alpha * (x.14.f(t.travel[2]+t.travel[5])-x.14.prev)
                x.arc[2] <- x.arc[2] +  x.14.addition[2]
                x.arc[5] <- x.arc[5] +  x.14.addition[5]
                x.14.prev <- x.14.prev +  x.14.addition[2]
        }
        if (t.travel[4] < t.travel[3]+t.travel[5]) {
                x.24.addition[4] <- alpha * (x.24.f(t.travel[4])-x.24.prev)
                x.arc[4] <- x.arc[4] +  x.24.addition[4]
                x.24.prev <- x.24.prev +  x.24.addition[4]
        }
        else if (t.travel[4] >= t.travel[3]+t.travel[5]) {
                x.24.addition[c(3,5)] <- alpha * (x.24.f(t.travel[3]+t.travel[5])-x.24.prev)
                x.arc[3] <- x.arc[3] + x.24.addition[3]
                x.arc[5] <- x.arc[5] + x.24.addition[5]
                x.24.prev <- x.24.prev +  x.24.addition[3]
        }
        t.travel <- c(t.14(x.arc[1]),t.13(x.arc[2]),t.23(x.arc[3]),t.24(x.arc[4]),t.34(x.arc[5]))
        Assignment.df <- cbind(Assignment.df, "add to Arc.14"=round(x.14.addition,0))
        Assignment.df <- cbind(Assignment.df, "add to Arc.24"=round(x.24.addition,0))
        Assignment.df <- cbind(Assignment.df, "x"=round(x.arc,0))
        Assignment.df <- cbind(Assignment.df, "t"=round(t.travel,1))
        
        ite <- ite+1
}
Assignment.df <- t(Assignment.df)
