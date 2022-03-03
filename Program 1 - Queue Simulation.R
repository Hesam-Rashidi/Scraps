
########################################################################################
#  Please make sure to enter the required inputs before running the rest of the code!  #
########################################################################################

### Inputs #############################################################################
library(svDialogs)
sys.type <- dlg_input("Choose the system type (enter 'MM1' or 'MD1'): ")$res
calc.type <- dlg_input("Choose the priority type (enter 'FCFS' or 'SPT'): ")$res
prog.type <- dlg_input("Choose the program type (enter 'c' or 't' for a program with a specific number of customers or runtime respectively):")$res
if (prog.type == "c") {n <- as.numeric(dlg_input("Enter the number of costumers: ")$res)}
if (prog.type == "t") {T <- as.numeric(dlg_input("Enter the desired runtime (in minutes): ")$res)}

### Random draw ########################################################################
random.draw <- function(mean, Y0, n){
        U <- c()
        Y <- Y0
        for (i in 1:n){
                Y <- (16807*Y)%%(2147483648)
                U <- rbind(U, Y/(2147483648))
        }
        -mean*log(U)
}
if (prog.type == "c") {
        A <- round(random.draw(mean=5, Y0=2062615503, n=n),1)
        if (sys.type == "MM1") {
                S <- round(random.draw(mean=4, Y0=1383670351, n=n),1)
        } else if (sys.type == "MD1") {
                S <- rep(4, n)
        } 
} else if (prog.type == 't') {
        A <- c(round(random.draw(mean=5, Y0=2062615503, n=1),1))
        if (sys.type == "MM1") {
                S <- round(random.draw(mean=4, Y0=1383670351, n=1),1)
        } else if (sys.type == "MD1") {S <- 4}
        n <- 1
        end <- 0
        t.added <- c(rep(0, length(A)))
        t.processed <- c(round(S[1]+A[1],1), rep(0, length(A)-1))
        while (end <= T) {
                n <- n+1
                A <- round(random.draw(mean=5, Y0=2062615503, n=n),1)
                if (sys.type == "MM1") {
                        S <- round(random.draw(mean=4, Y0=1383670351, n=n),1)
                } else if (sys.type == "MD1") {
                        S <- rep(4, n)
                }
                for (i in 1:length(A)) {t.added[i] <- round(sum(A[1:i]),1)}
                if (calc.type == 'FCFS') {
                        for (i in 2:length(A)) {t.processed[i] <- round(max(t.processed[i-1], t.added[i]) + S[i],1)}
                } else if (calc.type == 'SPT') {
                        j <- c(1)
                        for (i in 2:length(A)) {
                                x <- which(t.added <= t.processed[i-1])
                                x <- x[!x %in% j]
                                if (length(x) > 0){
                                        S.ite <- S[x]
                                        index <- x[which.min(S.ite)]
                                        t.processed[i] <- round(max(t.processed[i-1], t.added[index]) + S[index],1)
                                        j <- cbind(j,index)
                                } else {
                                        t.processed[i] <- round(max(t.processed[i-1], t.added[i]) + S[i],1)
                                        j <- cbind(j,i)}
                        }
                }
                end <- t.processed[length(A)]
        }
} 

### Calculations ######################################################################
if (prog.type == 'c') {
        t <- 0 #Time
        N <- 0 #Initial queue length
        Nt.df <- data.frame("N"=N, "t"=t) #data frame
        t.step <- 0.1
        t.added <- c(rep(0, length(A)))
        for (i in 1:length(A)) {t.added[i] <- round(sum(A[1:i]),1)} 
        t.processed <- c(round(S[1]+A[1],1), rep(0, length(A)-1))
        if (calc.type == 'FCFS') {
                for (i in 2:length(A)) {
                        t.processed[i] <- round(max(t.processed[i-1], t.added[i]) + S[i],1)
                        }
                } else if (calc.type == 'SPT') {
                        j <- c(1)
                        for (i in 2:length(A)) {
                                x <- which(t.added <= t.processed[i-1])
                                x <- x[!x %in% j]
                                if (length(x) > 0){
                                        S.ite <- S[x]
                                        index <- x[which.min(S.ite)]
                                        t.processed[i] <- round(max(t.processed[i-1], t.added[index]) + S[index],1)
                                        j <- cbind(j,index)
                                        } else {
                                                t.processed[i] <- round(max(t.processed[i-1], t.added[i]) + S[i],1)
                                                j <- cbind(j,i)}
                        }
                        }
        t.end <- max(t.processed)
        while (t <= t.end){
                if (t %in% t.added) {N <- N+1} else if (t %in% t.processed) {N <- N-1}
                Nt.df <- rbind(Nt.df, c(N, t))
                t <- round(t+t.step,1)
                }
} else if (prog.type == 't') {
        t <- 0 #Time
        N <- 0 #Initial queue length
        Nt.df <- data.frame("N"=N, "t"=t) #data frame
        t.step <- 0.1
        t.processed <- t.processed[t.processed<T]
        if (length(t.added)!=length(t.processed)) {
                t.added <- t.added[-length(t.added)]
                j <- j[-length(A)]}
        t.end <- max(t.processed)
        while (t <= T){
                if (t %in% t.added) {N <- N+1} else if (t %in% t.processed) {N <- N-1}
                Nt.df <- rbind(Nt.df, c(N, t))
                t <- round(t+t.step,1)
        }
}

### Outputs ###########################################################################
#1 plot
plot(y=Nt.df$N,x=Nt.df$t, xlab = "time (minutes)", ylab = "N" , main = "Queue length over time", type="l")
#2 average time in queue
avg.time.in.queue <- c(0, rep(0, length(t.processed)-1))
for (i in 2:length(t.processed)) {avg.time.in.queue[i] <- max(0, t.processed[i-1]-t.added[i])}
if (calc.type == 'SPT') {
        for (i in 2:length(j)) {avg.time.in.queue[i] <- max(0, t.processed[i-1]-t.added[j[i]])}
}
avg.time.in.queue <- sum(avg.time.in.queue)/length(t.processed)
#3 average number of events in queue
auc <- c()
for (i in 2:length(Nt.df[,1])) {auc <- cbind(auc, t.step*(Nt.df[(i-1),1]+Nt.df[i,1])/2)}
avg.num.in.queue <- 1/t.end*sum(auc)
#4 percent of events present in system more than t
time.in.sys <- c(rep(0, length(t.processed)))
if (calc.type == 'SPT') {
        for (i in 1:length(t.processed)) {
                time.in.sys[i] <- t.processed[i]-t.added[j[i]]
                }
        } else if (calc.type == 'FCFS') {
                time.in.sys <- t.processed-t.added
                }
'percent.(t>4.5.min).in.sys' <- sum(time.in.sys>4.5)/length(t.processed)
#5 Utilization
U <- (t.end - t.step*(sum(Nt.df$N==0)-1))/t.end
#Output
winDialog("ok", paste("Average time in queue (minutes):", format(avg.time.in.queue, digits=4), "
Average number of events in queue:", format(avg.num.in.queue, digits=4),"
Events present in system more than 4.5 minutes:", format(`percent.(t>4.5.min).in.sys`, digits=3),"
U:", format(U, digits=3)))


