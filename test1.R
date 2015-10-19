require(lpSolveAPI)

## Documentation for lpSolveAPI can be found at this link
## http://cran.r-project.org/web/packages/lpSolveAPI/lpSolveAPI.pdf

## Test 1 - 4 orders, 5 plants
## create the model - constraints first arg, decision variables 2nd arg
## constraints are 0 as we will add them below
test1 <- make.lp(0, 20)
set.objfn(test1, c(99, 98, 97, 96, 
                   89, 88, 87, 86,
                   79, 78, 77, 76,
                   74, 68, 67, 66,
                   59, 58, 57, 57
))

## supply must meet demand constraints
add.constraint(test1, c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), "=", 10)
add.constraint(test1, c(0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0), "=", 16)
add.constraint(test1, c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0), "=", 23)
add.constraint(test1, c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), "=", 31)
## demand must not exceed supply constraints
add.constraint(test1, c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 40)
add.constraint(test1, c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 40)
add.constraint(test1, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0), "<=", 40)
add.constraint(test1, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0), "<=", 40)
add.constraint(test1, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1), "<=", 40)

## set constraint type
set.type(test1, c(1:20), "integer")

## name the variables - this isn't necessary but is done for readability here
dimnames(test1) <- list(c("order1demand", "order2demand", "order3demand", "order4demand",
                          "capacityplant1", "capacityplant2", "capacityplant3", "capacityplant4", 
                          "capacityplant5"),
                        c("plant1order1", "plant1order2", "plant1order3", "plant1order4",
                          "plant2order1", "plant2order2", "plant2order3", "plant2order4",
                          "plant3order1", "plant3order2", "plant3order3", "plant3order4",
                          "plant4order1", "plant4order2", "plant4order3", "plant4order4",
                          "plant5order1", "plant5order2", "plant5order3", "plant5order4")
                          )

## write the lp model to a 
write.lp(test1, "test1.lp",type='lp')

## solve the model a [0] indicates a solution was found
solve(test1)
## get the objective
get.objective(test1)
## get the decision variables
get.variables(test1)

## make a matrix of the decisions and put in readable format
results <- get.variables(test1)
results <- matrix(unlist(results), nrow = 5, ncol = 4, byrow = TRUE)
rownames(results) <- c("plant1","plant2", "plant3", "plant4", "plant5")
colnames(results) <- c("order1","order2", "order3", "order4")

print(results)

delete.lp(test1)
