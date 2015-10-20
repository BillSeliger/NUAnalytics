require(lpSolveAPI)

## Documentation for lpSolveAPI can be found at this link
## http://cran.r-project.org/web/packages/lpSolveAPI/lpSolveAPI.pdf

## test2 - 4 orders, 5 plants with BigM constraint
## create the model - constraints first arg, decision variables 2nd arg
## constraints are 0 as we will add them below
test2 <- make.lp(0, 35)

## add supply must meet demand constraints
add.constraint(test2, c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, rep(0,15)), "=", 10)
add.constraint(test2, c(0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, rep(0,15)), "=", 16)
add.constraint(test2, c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, rep(0,15)), "=", 23)
add.constraint(test2, c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, rep(0,15)), "=", 31)
## add demand must not exceed supply constraints
add.constraint(test2, c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, rep(0,15)), "<=", 40)
add.constraint(test2, c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, rep(0,15)), "<=", 40)
add.constraint(test2, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, rep(0,15)), "<=", 40)
add.constraint(test2, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, rep(0,15)), "<=", 40)
add.constraint(test2, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, rep(0,15)), "<=", 40)

## add BigM constraints for each plant-version 
add.constraint(test2, c(1,1,c(rep(0,18)),-99999,rep(0,14)), "<=", 0)
add.constraint(test2, c(0,0,1,c(rep(0,18)),-99999,rep(0,13)), "<=", 0)
add.constraint(test2, c(0,0,0,1,c(rep(0,18)),-99999,rep(0,12)), "<=", 0)
add.constraint(test2, c(c(rep(0,4)),1,1,c(rep(0,17)),-99999,rep(0,11)), "<=", 0)
add.constraint(test2, c(c(rep(0,6)),1,c(rep(0,17)),-99999,rep(0,10)), "<=", 0)
add.constraint(test2, c(c(rep(0,7)),1,c(rep(0,17)),-99999,rep(0,9)), "<=", 0)
add.constraint(test2, c(c(rep(0,8)),1,1,c(rep(0,16)),-99999,rep(0,8)), "<=", 0)
add.constraint(test2, c(c(rep(0,10)),1,c(rep(0,16)),-99999,rep(0,7)), "<=", 0)
add.constraint(test2, c(c(rep(0,11)),1,c(rep(0,16)),-99999,rep(0,6)), "<=", 0)
add.constraint(test2, c(c(rep(0,12)),1,1,c(rep(0,15)),-99999,rep(0,5)), "<=", 0)
add.constraint(test2, c(c(rep(0,14)),1,c(rep(0,15)),-99999,rep(0,4)), "<=", 0)
add.constraint(test2, c(c(rep(0,15)),1,c(rep(0,15)),-99999,rep(0,3)), "<=", 0)
add.constraint(test2, c(c(rep(0,16)),1,1,c(rep(0,14)),-99999,rep(0,2)), "<=", 0)
add.constraint(test2, c(c(rep(0,18)),1,c(rep(0,14)),-99999,rep(0,1)), "<=", 0)
add.constraint(test2, c(c(rep(0,19)),1,c(rep(0,14)),-99999), "<=", 0)

## set constraint type
set.type(test2, c(21:35), type = "binary")

## set the objective function
set.objfn(test2, c(99, 98, 97, 96, 
                   89, 88, 87, 86,
                   79, 78, 77, 76,
                   74, 68, 67, 66,
                   59, 58, 57, 50,
                   rep(2000,15)
))

## write the lp model to a file
write.lp(test2, "test2.lp",type='lp')

## solve the model, a [0] indicates a solution was found
solve(test2)
## get the objective
get.objective(test2)
## get the decision variables
get.variables(test2)

## make a matrix of the decisions and put in readable format
results <- get.variables(test2)
results <- matrix(unlist(results[1:20]), nrow = 5, ncol = 4, byrow = TRUE)
## name the rows and columns
rownames(results) <- c("plant1","plant2", "plant3", "plant4", "plant5")
colnames(results) <- c("order1","order2", "order3", "order4")
## print the results to the display
print(results)

## it is good practice to delete the model in R when done with it
delete.lp(test2)
