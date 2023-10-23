#install.packages("dplyr")
library(dplyr)

#setwd("H:/BAOVE/pr 1")
setwd("C:/Users/DANIIL KHOKHRUNOV/Downloads/projekt1")
dat <- read.csv("eurotrip-flights.csv")

cas <- unique(dat$date)
castonum <- seq(1:45)

### vymenime daty na t = cas
for(i in 1:nrow(dat)){
  for (j in 1:length(cas)){
    if (dat$date[i] == cas[j]){dat$date[i] <- j}
  }
}

odkud <- unique(dat$from.city)
kam <- unique(dat$to.city)

matice <- matrix(data = 0, nrow = length(odkud), ncol = length(kam))
colnames(matice) <- kam
rownames(matice) <- odkud



######## metoda souseda ########
Mestavse <- unique(dat$from.city)


Mesta <- c("Prague")
cena <- c() 
l = 2  #############
place = "Prague"
konec <- 0
while(konec == 0){
for(t in (l-1):44){ #### 
mesto <- Mesta[t]
ls <- filter(dat, date == t, from.city == mesto, !(to.city %in% Mesta), to.city != place)
new.city <- filter(ls, price.czk == min(ls$price.czk))$to.city
cena <- append(cena, min(ls$price.czk))
Mesta <- append(Mesta, new.city)
#########
}
###
if(length(Mesta) != length(Mestavse)){
  l <- sample(c(2:length(Mesta)), 1)
  place <- Mesta[l]
  Mesta <- Mesta[1:(l-1)]
  lst <- filter(dat,date == l, from.city == Mesta[l-1], !(to.city %in% Mesta))
  cena <- cena[1:(l-1)]
  print("Reykjavik" %in% Mesta)
  print(length(Mesta))
} else {
  konec <-1
}}
##

ls <- filter(dat, date == 44, from.city == Mesta[45], to.city == "Prague")
cena <- append(cena, ls$price.czk)
Mesta <- append(Mesta, "Prague")
sum(cena)



################################### nahoda #########

cesta <- c()
mincena <- 100000
for(i in 1:10000){
  cena <- c()
Mestavse <- unique(dat$from.city)

Mesta <- c("Prague")

for(t in 1:44){ 
  mesto <- Mesta[t]
  ls <- filter(dat, date == t, from.city == mesto, !(to.city %in% Mesta))
  ls <- ls[order(ls$price.czk), ]
  ls <- head(ls, 2)
  if(nrow(ls) > 1){
    new.city <- ls[sample(nrow(ls), 1, prob = c(0.94, 0.06)), ]
    cena <- append(cena, new.city$price.czk)
    Mesta <- append(Mesta, new.city$to.city)
  } else if (nrow(ls) > 0) {
    new.city <- ls[sample(nrow(ls), 1), ]
    cena <- append(cena, new.city$price.czk)
    Mesta <- append(Mesta, new.city$to.city)
  } else {
    t <- 44
  }
}
ls <- filter(dat, date == 44, from.city == Mesta[45], to.city == "Prague")
cena <- append(cena, ls$price.czk)
Mesta <- append(Mesta, "Prague")
suma <- sum(cena)
if(length(unique(Mesta)) == length(Mestavse) & length(cena) == 45){
  if(suma < mincena){
    print(length(cena))
    mincena <- suma
    cesta <- Mesta
    print(mincena)
    print(cesta)
    rec <- list("cena" = cena, "cesta" = cesta)
  }
}
}
print(mincena)
print(cesta)
write.csv = rec

sum(rec$cena)
check <- data.frame(matrix(NA, 1, 6))
for(i in 1:45){
 check[nrow(check)+1, ] <- filter(dat, date == i, from.city == rec$cesta[i], to.city == rec$cesta[i+1])
}
write.csv(check,"check.csv")

###################################################

matice
lt <- list(matice1, matice2, matice3, matice4, matice5, matice6, matice7, matice8, matice9, matice10,
           matice11, matice12, matice13, matice14, matice15, matice16, matice17, matice18, matice19, matice20,
           matice21, matice22, matice23, matice24, matice25, matice26, matice27, matice28, matice29, matice30,
           matice31, matice32, matice33, matice34, matice35, matice36, matice37, matice38, matice39, matice40,
           matice41, matice42, matice43, matice44, matice45)

for (t in 1:1){
  ls <- filter(dat, date == t)
  for(i in 1:nrow(ls)){ 
    n <- ls$from.city[i]
    m <- ls$to.city[i]
    matice[rownames(matice1) %in% n, colnames(matice1) %in% m] <- ls$price.czk[i]
  }
  for(i in 1:nrow(matice)){
    for(j in 1:ncol(matice)){
      if(matice[i,j] == 0){
        matice[i,j] <- 100000
      }
    }
  }
}

t = 1
a <- filter(dat, date == 35, from.city == "Tbilisi")

#########

Mestavse <- unique(dat$from.city)


Mesta <- c("Prague")
cena <- c()
for(t in 1:44){ 
  mesto <- Mesta[t]
  ls <- filter(dat, date == t, from.city == mesto, !(to.city %in% Mesta))
  new.city <- filter(ls, price.czk == min(ls$price.czk))$to.city
  cena <- append(cena, min(ls$price.czk))
  Mesta <- append(Mesta, new.city)
}

ls <- filter(dat, date == 44, from.city == Mesta[45], to.city == "Prague")
cena <- append(cena, ls$price.czk)
Mesta <- append(Mesta, "Prague")
length(cena)
sum(cena)

if(length(Mesta) != length(Mestavse)){
  no <- Mestavse[!(Mestavse %in% Mesta)]
}

k = NA
u = NA

while(length(Mesta) != length(Mestavse)){
  Mesta <- c("Prague")
  cena <- c()
  for(t in 1:44){ 
    mesto <- Mesta[t]
    ls <- filter(dat, date == t, from.city == mesto, !(to.city %in% Mesta))
    
    for(i in 1:nrow(ls)){
      if (ls$from.city == k & ls$to.city == u){
        ls <- ls[-i, ]
      }
    }
    
    new.city <- filter(ls, price.czk == min(ls$price.czk))$to.city
    cena <- append(cena, min(ls$price.czk))
    Mesta <- append(Mesta, new.city)
  }
  
  ls <- filter(dat, date == 44, from.city == Mesta[45], to.city == "Prague")
  cena <- append(cena, ls$price.czk)
  Mesta <- append(Mesta, "Prague")
  length(cena)
  sum(cena)
  
  if(length(Mesta) != length(Mestavse)){
    k <- Mesta[sample(length(Mesta), 1)]
    u <- Mesta[sample(length(Mesta), 1)+1]
  }
}



