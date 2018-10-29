permutations(n=2, r=length(survival.s), repeats.allowed = T)

permuttaion(seq(1, length(survival.b)))


combs.b <- lapply(seq(1, length(survival.b)), function(i) combinations(n=length(survival.b), r=i))
combs.s <- lapply(seq(length(survival.b), 1), function(i) combinations(n=length(survival.b), r=i))

lapply(seq_along(combs), function(j){
  sapply(seq(1, nrow(combs[[j]])), function(r){
    c(survival.b[combs.b[[j]][r, ]], survival.s[combs.s[[j]][r, ]])
  })
})


