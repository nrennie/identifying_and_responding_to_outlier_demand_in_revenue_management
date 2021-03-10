#Return Bookings data
#nested limits
#full buy down - lowest available fare
EMSRbSimulation <- function(probs1=c(0.1,0.35,0.1,0.25,0.15,0.05,0,0),probs2=c(0.05,0.05,0.1,0,0.05,0.1,0.15,0.6),alpha,beta,phi1,a1,b1,a2,b2,N,BH,limits){
  n <- length(limits)
  #Generate customer arrivals
  Cust <- NHPP(alpha,beta,phi1,a1,b1,a2,b2,N)
  Cust1 <- Cust[[1]]
  Cust2 <- Cust[[2]]
  #Accept or reject based on EMSRb protection levels
  A <- numeric(N) #High
  O <- numeric(N)
  J <- numeric(N)
  P <- numeric(N)
  R <- numeric(N)
  S <- numeric(N)
  M <- numeric(N) #Low
  Total <- numeric(N)
  #Implement emsrb protection limits
  suppressWarnings(
  for (i in 1:N){
    #deal with type 1 customer arrivals first (business)
    if (Cust1[i] != 0){
    f <- sample(0:n, Cust1[i], prob=probs1, replace=TRUE)
    #choose 0, 1, 2, 3, 4, 5, 6, 7
    #if choose no purchase take no action
    #if choose 7
    if (f == 7){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
    }
    #if choose 6
    if (f == 6){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust1[i]) <= limits[n-1]){
        S[i] <- Cust1[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
    }
    #if choose fare class 5 
    if (f == 5){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust1[i]) <= limits[n-1]){
        S[i] <- Cust1[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(S) + sum(M) + sum(R) + Cust1[i]) <= limits[n-2]){
        R[i] <- Cust1[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose fare class 4, check if capacity in 5 and allocate else check allocate to 4 if capacity
    else if (f == 4){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust1[i]) <= limits[n-1]){
        S[i] <- Cust1[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-2]){
        R[i] <- Cust1[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-3]){
        P[i] <- Cust1[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose fare class 3, check lower capacities first
    else if (f == 3){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust1[i]) <= limits[n-1]){
        S[i] <- Cust1[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-2]){
        R[i] <- Cust1[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-3]){
        P[i] <- Cust1[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-4]){ 
        J[i] <- Cust1[i]
      } 
      #else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-4])){
      #  J[i] <- limits[n-4] - (sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose fare class 2, check lower fare classes first
    else if (f == 2){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust1[i]) <= limits[n-1]){
        S[i] <- Cust1[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-2]){
        R[i] <- Cust1[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-3]){
        P[i] <- Cust1[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-4]){
        J[i] <- Cust1[i]
      } 
      #else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-4])){
      #  J[i] <- limits[n-4] - (sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-5]){
        O[i] <- Cust1[i]
      } 
      #else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-5])){
      #  O[i] <- limits[n-5] - (sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose highest fare class, check all others first
    else if (f == 1){
      if ((sum(M) + Cust1[i]) <= limits[n]){
        M[i] <- Cust1[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust1[i]) <= limits[n-1]){
        S[i] <- Cust1[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-2]){
        R[i] <- Cust1[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-3]){
        P[i] <- Cust1[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-4]){
        J[i] <- Cust1[i]
      } 
      #else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-4])){
      #  J[i] <- limits[n-4] - (sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-5]){
        O[i] <- Cust1[i]
      } 
      #else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-5])){
      #  O[i] <- limits[n-5] - (sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(A) + sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust1[i]) <= limits[n-6]){
        A[i] <- Cust1[i]
      } 
      #else if ((sum(A) + sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-6])){
      #  A[i] <- limits[n-6] - (sum(A) + sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    }
    #Now deal with type 2 customer arrivals (tourist)
    if (Cust2[i] != 0){
    g <- sample(0:n, Cust2[i], prob=probs2, replace=TRUE)
    #choose 0, 1, 2, 4, 5, 6, 7
    #if choose no purchase take no action
    #if choose gare class 7 (lowest available to tourist passengers), allocate if capacity 
    if (g == 7){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
    }
    #if choose 6
    if (g == 6){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust2[i]) <= limits[n-1]){
        S[i] <- Cust2[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
    }
    #if choose gare class 5 
    if (g == 5){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust2[i]) <= limits[n-1]){
        S[i] <- Cust2[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(S) + sum(M) + sum(R) + Cust2[i]) <= limits[n-2]){
        R[i] <- Cust2[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose gare class 4, check if capacity in 5 and allocate else check allocate to 4 if capacity
    else if (g == 4){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust2[i]) <= limits[n-1]){
        S[i] <- Cust2[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-2]){
        R[i] <- Cust2[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-3]){
        P[i] <- Cust2[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose gare class 3, check lower capacities girst
    else if (g == 3){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust2[i]) <= limits[n-1]){
        S[i] <- Cust2[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-2]){
        R[i] <- Cust2[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-3]){
        P[i] <- Cust2[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-4]){ 
        J[i] <- Cust2[i]
      } 
      #else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-4])){
      #  J[i] <- limits[n-4] - (sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose gare class 2, check lower gare classes girst
    else if (g == 2){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust2[i]) <= limits[n-1]){
        S[i] <- Cust2[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-2]){
        R[i] <- Cust2[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-3]){
        P[i] <- Cust2[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-4]){
        J[i] <- Cust2[i]
      } 
      #else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-4])){
      #  J[i] <- limits[n-4] - (sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-5]){
        O[i] <- Cust2[i]
      } 
      #else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-5])){
      #  O[i] <- limits[n-5] - (sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    #if choose hifhest gare class, check all others girst
    else if (g == 1){
      if ((sum(M) + Cust2[i]) <= limits[n]){
        M[i] <- Cust2[i]
      }
      #else if ((sum(M) < limits[n])){
      #  M[i] <- limits[n] - sum(M)
      #}
      else if ((sum(S) + sum(M) + Cust2[i]) <= limits[n-1]){
        S[i] <- Cust2[i]
      }
      #else if ((sum(S) + sum(M) < limits[n-1])){
      #  S[i] <- limits[n] - (sum(M) + sum(S))
      #}
      else if ((sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-2]){
        R[i] <- Cust2[i]
      } 
      #else if ((sum(R) + sum(S) + sum(M) < limits[n-2])){
      #  R[i] <- limits[n-2] - (sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-3]){
        P[i] <- Cust2[i]
      } 
      #else if ((sum(P) + sum(R) + sum(S) + sum(M) < limits[n-3])){
      #  P[i] <- limits[n-3] - (sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-4]){
        J[i] <- Cust2[i]
      } 
      #else if ((sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-4])){
      #  J[i] <- limits[n-4] - (sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-5]){
        O[i] <- Cust2[i]
      } 
      #else if ((sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-5])){
      #  O[i] <- limits[n-5] - (sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
      else if ((sum(A) + sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) + Cust2[i]) <= limits[n-6]){
        A[i] <- Cust2[i]
      } 
      #else if ((sum(A) + sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M) < limits[n-6])){
      #  A[i] <- limits[n-6] - (sum(A) + sum(O) + sum(J) + sum(P) + sum(R) + sum(S) + sum(M))
      #}
    }
    }
  })
  Total <- A+O+J+P+R+S+M
  #return(list(A=aggBH(A,BH),O=aggBH(O,BH),J=aggBH(J,BH),P=aggBH(P,BH),R=aggBH(R,BH),S=aggBH(S,BH),M=aggBH(M,BH),Total=aggBH(Total,BH)))
  #return(list(A=aggBH(A,BH),O=aggBH(O,BH),J=aggBH(J,BH),P=aggBH(P,BH),R=aggBH(R,BH),S=aggBH(S,BH),M=aggBH(M,BH),Total=aggBH(Total,BH), DemandA = aggBH(Demand$A,BH), DemandO = aggBH(Demand$O,BH), DemandJ = aggBH(Demand$J,BH), DemandP = aggBH(Demand$P,BH), DemandR = aggBH(Demand$R,BH), DemandS = aggBH(Demand$S,BH), DemandM = aggBH(Demand$M,BH), DemandTotal = aggBH(Demand$Total,BH)))
  return(matrix(c(A=aggBH(A,BH),O=aggBH(O,BH),J=aggBH(J,BH),P=aggBH(P,BH),R=aggBH(R,BH),S=aggBH(S,BH),M=aggBH(M,BH),Total=aggBH(Total,BH)), ncol=8))
}

