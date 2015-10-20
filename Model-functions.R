
## this code reads the cust file after it has been converted from xml to xlsx file and structure the data so that 
## a model file can be created for optimization


Modelfile <- function(title, event, filename, paging, piece_weight){

require(xlsx)
require(dplyr)
require(ggplot2)
require(lpSolveAPI)

## create a dataframe for plant capacity constraints
## consider user input to capture plant capacity constraints
plants <- c( "Plant1", "Plant2", "Plant3", "Plant4", "Plant5")
capacity <- c(5000000,50000000,5000000,5000000,5000000)
capacity <- data.frame(plants, capacity)

##Set to the local directory where the necessaru files reside 
## necessary files are Manf Costs, Dist Costs & Customer Order Detail
setwd("C:/Users/rr046302/Documents/R/lpSolve testing/Plant-Assignment")

## Read in Manufacturing Costs 
manf_costs <- read.csv("Compiled cust and cust - Optimization Inputs r4.csv")

## create the MR costs - in the future read from user input
MR <- 150

## Read in the Distribution Costs
dist_costs <- read.csv("Gravure Retail Distribution Cost by Origin-4.22.2015.csv")
colnames(dist_costs) <- c("city.st", "Plant1.CWT", "Plant2.CWT", "Plant3.CWT",
                          "Plant4.CWT","Plant4.CWT")
LTL <- 20

## read in the cust xlsx file - need to be able to read xml file
setwd("C:/Users/rr046302/Documents/R/lpSolve testing/cust-cust testing")
cust_raw <- read.xlsx2(file = filename, sheetName="Sheet1")

setwd("C:/Users/rr046302/Documents/R/lpSolve testing/Plant-Assignment")
## drop a lot of unneeded variables
drops <- c("ActivityId", "ActivityTitle", "ActivityStartDate", "Configuration", "MarketName", "Printer",
           "Freight", "RoutingID", "NewsPaperId", "NewsPaperName", "Address1", "Address2", "Zip", "Country",
           "DueDate", "RoutingInstruction", "ShippingRequirements", "QuarterFoldingInstuction", "StoreId", 
           "StoreName", "Address12", "Address23", "City4", "State5", "Zip6", "Country7", "SendQuantity8", "ShipsWith", 
           "InsertionDate9", "DueDate10", "SampleLocationId", "SampleLocationName", "Address111", "Address212", "City13", 
           "State14", "Zip15", "Country16", "SendQuantity17", "InsertionDate18", "DueDate19", "ShipsWith20", "SNLFileVersion")

cust <- cust_raw[,!(names(cust_raw) %in% drops)]

## change several of the column names
colnames(cust)[2] <- "merchArea"
colnames(cust)[3] <- "plant"
colnames(cust)[6] <- "copies"

## change the quantity to a integer variable
cust$copies <- as.numeric(as.character(cust$copies))
cust$copies <- cust$copies/1000

## create the city.st variable for looking up distribution costs
cust$city.st <- paste (cust$City, cust$State, sep = " ", collapse = NULL)

## create the weight variable for every observation
cust$weight <- cust$copies*piece_weight

## aggregate by RunCode and City St and summarise copies and weight
cust2 <- cust %>% group_by(RunCode, plant, city.st) %>% summarise (copies = sum(copies), weight = sum (weight))

## drop incomplete cases - rows that contain no copy count
cust2 <- cust2[complete.cases(cust2),]

## In the Shiny deployment we will want to read thes from Manf Costs table
## add manufacturing costs - read this from Dave Coggins dataframe in a future enhancement
cust2$Plant1.Manf.cost <- manf_costs[,paging][1]
cust2$Plant2.Manf.cost <- manf_costs[,paging][2]
cust2$Plant3.Manf.cost <- manf_costs[,paging][3]
cust2$Plant4.Manf.cost <- manf_costs[,paging][4]
cust2$Plant4.Manf.cost <- manf_costs[,paging][5]

## change all city.st variable to lower case for merge
dist_costs$city.st <- tolower(dist_costs$city.st)
cust2$city.st <- tolower(cust2$city.st)

## merge the distribution cost/cwt for each cust destination
cust2 <- merge(cust2, dist_costs, by = "city.st")

## calculate the distribution cost for each origin
## If NA use LTL as default rate
cust2$Plant1.Dist.Cost <- ifelse(is.na(cust2$Plant1.CWT), 
                                round(cust2$weight/100 * LTL / cust2$copies, 2),
                                round(cust2$weight/100 * cust2$Plant1.CWT/cust2$copies, 2))
cust2$Plant2.Dist.Cost <- ifelse(is.na(cust2$Plant2.CWT), 
                                round(cust2$weight/100 * LTL / cust2$copies, 2),
                                round(cust2$weight/100 * cust2$Plant2.CWT/cust2$copies, 2))
cust2$Plant4.Dist.Cost <- ifelse(is.na(cust2$Plant4.CWT), 
                                round(cust2$weight/100 * LTL / cust2$copies, 2),
                                round(cust2$weight/100 * cust2$Plant4.CWT/cust2$copies, 2))
cust2$Plant3.Dist.Cost <- ifelse(is.na(cust2$Plant3.CWT), 
                                round(cust2$weight/100 * LTL / cust2$copies, 2),
                                round(cust2$weight/100 * cust2$Plant3.CWT/cust2$copies, 2))
cust2$Plant4.Dist.Cost <- ifelse(is.na(cust2$Plant4.CWT), 
                                round(cust2$weight/100 * LTL / cust2$copies, 2),
                                round(cust2$weight/100 * cust2$Plant4.CWT/cust2$copies, 2))

## sum the manf cost and distribution cost for total cost  
cust2$Plant1.Cost <- cust2$Plant1.Manf.cost + cust2$Plant1.Dist.Cost
cust2$Plant2.Cost <- cust2$Plant2.Manf.cost + cust2$Plant2.Dist.Cost
cust2$Plant3.Cost <- cust2$Plant3.Manf.cost + cust2$Plant3.Dist.Cost
cust2$Plant4.Cost <- cust2$Plant4.Manf.cost + cust2$Plant4.Dist.Cost
cust2$Plant4.Cost <- cust2$Plant4.Manf.cost + cust2$Plant4.Dist.Cost

## calculate baseline Distribution costs
cust2 <- mutate(cust2, base_dist_cost =  ifelse(cust2$plant == "Plant1", cust2$Plant1.Dist.Cost * cust2$copies,
                                                  ifelse(cust2$plant == "Plant2", cust2$Plant2.Dist.Cost * cust2$copies,
                                                         ifelse(cust2$plant == "Plant3", cust2$Plant3.Dist.Cost * cust2$copies,
                                                                ifelse(cust2$plant == "Plant4", cust2$Plant4.Dist.Cost * cust2$copies, 
                                                                       cust2$Plant4.Dist.Cost * cust2$copies)))))

## calculate baseline Manufacturing costs
cust2 <- mutate(cust2, base_manf_cost =  ifelse(cust2$plant == "Plant1", cust2$Plant1.Manf.cost * cust2$copies,
                                                  ifelse(cust2$plant == "Plant2", cust2$Plant2.Manf.cost * cust2$copies,
                                                         ifelse(cust2$plant == "Plant3", cust2$Plant3.Manf.cost * cust2$copies,
                                                                ifelse(cust2$plant == "Plant4", cust2$Plant4.Manf.cost * cust2$copies, 
                                                                       cust2$Plant4.Manf.cost * cust2$copies)))))

## calculate total baseline costs
cust2 <- mutate(cust2, base_cost =  ifelse(cust2$plant == "Plant1", cust2$Plant1.Cost * cust2$copies,
                                             ifelse(cust2$plant == "Plant2", cust2$Plant2.Cost * cust2$copies,
                                                    ifelse(cust2$plant == "Plant3", cust2$Plant3.Cost * cust2$copies,
                                                           ifelse(cust2$plant == "Plant4", cust2$Plant4.Cost * cust2$copies, 
                                                                  cust2$Plant4.Cost * cust2$copies)))))

## To create the entire dataset to solve with Gurobi or CPLEX NEOS public solver
cust3 <- cust2

## sort the dataframe in Version - city.st order for creating the MIP model
cust3 <- arrange(cust3, RunCode, city.st)

## create a dataframe of the versions
RunCodes <- unique(cust3$RunCode)

## create the lprec object
## make the lprec object - constraints first arg, decision variables 2nd arg
## constraints - demand = supply, Big-M fixed cost per RunCode, overall plant capacity
## variables for each (order * plant) + plant fixed cost per RunCode
lprec <- make.lp(sum(nrow(cust3), nrow(capacity), length(RunCodes)*nrow(capacity)), 
                 sum(NROW(capacity)*NROW(cust3), (nrow(capacity) * (length(RunCodes)))))
print(lprec)

## set RHS copies constraints and types - we must fulfill all demand for each RunCode-Order combination
set.constr.value(lprec, rhs = cust3$copies, constraints=seq(1,NROW(cust3)))

## set the LHS binary Assignment/Demand must be met constraints
cst <- 0
for(i in 1:(nrow(cust3))){
  cst <- cst + 1
  set.row(lprec, cst, rep(1, nrow(capacity)), indices = seq(cst, nrow(cust3)*nrow(capacity), by = nrow(cust3)))
}

## set the RHS constraints for plant capacity
cst <- 0
for(plant in capacity$capacity){
  cst <- cst + 1
  set.constr.value(lprec, rhs = capacity$capacity[cst], constraints = sum(cst, NROW(cust3)))
}

## set the LHS plant capacity constraints 
cst <- 0
for(i in capacity$capacity){
  cst <- cst + 1
  set.row(lprec, cst+nrow(cust3), rep(1, nrow(cust3)),
          indices = seq(from=1+((cst-1)*nrow(cust3)), 
                        to=nrow(cust3)+(cst-1)*nrow(cust3), by = 1))
}

## set RHS Big-M constraints for fixed startup cost for each RunCode
set.constr.value(lprec, rep(0, length(RunCodes)), 
                 constraints = c(seq(from = sum(NROW(cust3), NROW(capacity), 1), length = length(RunCodes))))

## set rhs constraint types - all of them 
set.constr.type(lprec, c(rep("=", nrow(cust3)), rep("<=", sum(nrow(capacity), length(RunCodes)*nrow(capacity)))))

## set the objective types
set.type(lprec, c(seq(from = sum(NROW(cust3) * NROW(capacity), 1), 
                      length = (NROW(capacity)*length(RunCodes)))), type = "binary")

## set objective coefficients for all costs
## Note that you must use a single call to set all objective costs
set.objfn(lprec, c(cust3$Plant1.Cost, cust3$Plant2.Cost, cust3$Plant3.Cost,
                   cust3$Plant4.Cost, cust3$Plant4.Cost, 
                   rep(MR, length(RunCodes)*nrow(capacity))))

## set the constraints for BigM fixed costs
## set Big-M constraints for fixed startup cost for each Version
## cnt1 is row counter - accretes by each plant-version combination
## cnt2 is the RunCodes lookup - accretes by each RunCode
## cnt3 is indices counter - accretes by each plant-order combination

cnt1 <- 0
cnt3 <- 1
for (p in 1:(length(plants))) {cnt2 <- 1
                               for(r in 1:(length(RunCodes))){
                                 cnt1 <- cnt1 + 1
                                 set.row(lprec, nrow(cust3) + length(plants) + cnt1,   
                                         xt = c(rep(1, sum(cust3$RunCode == RunCodes[cnt2])),-999999),
                                         indices = c(seq(cnt3,(-1+cnt3+sum(cust3$RunCode == RunCodes[cnt2])), by=1),
                                                     cnt1+(length(plants)*nrow(cust3)))
                                 )
                                 cnt3 <- cnt3 + sum(cust3$RunCode == RunCodes[cnt2]) 
                                 cnt2 <- cnt2 + 1
                               }
}

modelname <- paste(title, event,"model.mps", sep = "")
write.lp(lprec, modelname,type='mps')

assign("cust3", cust3, envir = .GlobalEnv)
assign("capacity", capacity, envir = .GlobalEnv)
assign("plants", plants, envir = .GlobalEnv)

print(paste(modelname, "is written"))


}


SKresults <- function(title, event){
## read in the NEOS solution from a text file
NEOS_sol <- read.table("NEOS Solution.txt", sep = " ")
NEOS_sol$V2 <- ifelse(NEOS_sol$V2 < 0.0001, 0, NEOS_sol$V2)

## write the NEOS objectives to a dataframe and cbind back with the original dataframe for comparison
NEOS_matrix <- matrix(unlist(NEOS_sol$V2[1:sum(NROW(cust3) * NROW(capacity))]), 
                      nrow = NROW(cust3), ncol = NROW(capacity), byrow = FALSE)
colnames(NEOS_matrix) <- plants

NEOS_decision <- cbind(cust3, NEOS_matrix)

NEOS_decision$optPlant <- ifelse(NEOS_decision$Plant1>0, "Plant1",
                                 ifelse(NEOS_decision$Plant2>0, "Plant2",
                                        ifelse(NEOS_decision$Plant3>0, "Plant3",
                                               ifelse(NEOS_decision$Plant4>0, "Plant4", 
                                                      "Plant5"))))

## calculate Optimal Distribution costs
NEOS_decision <- mutate(NEOS_decision, opt_dist_cost <-  
                          ifelse(NEOS_decision$optPlant == "Plant1", NEOS_decision$Plant1.Dist.Cost * NEOS_decision$copies,
                                 ifelse(NEOS_decision$optPlant == "Plant2", NEOS_decision$Plant2.Dist.Cost * NEOS_decision$copies,
                                        ifelse(NEOS_decision$optPlant == "Plant3", NEOS_decision$Plant3.Dist.Cost * NEOS_decision$copies,
                                               ifelse(NEOS_decision$optPlant == "Plant4", NEOS_decision$Plant4.Dist.Cost * NEOS_decision$copies, 
                                                      NEOS_decision$Plant4.Dist.Cost * NEOS_decision$copies)))))

## calculate Optimal Manufacturing costs
NEOS_decision <- mutate(NEOS_decision, opt_manf_cost <-
                          ifelse(NEOS_decision$optPlant == "Plant1", NEOS_decision$Plant1.Manf.cost * NEOS_decision$copies,
                                 ifelse(NEOS_decision$optPlant == "Plant2", NEOS_decision$Plant2.Manf.cost * NEOS_decision$copies,
                                        ifelse(NEOS_decision$optPlant == "Plant3", NEOS_decision$Plant3.Manf.cost * NEOS_decision$copies,
                                               ifelse(NEOS_decision$optPlant == "Plant4", NEOS_decision$Plant4.Manf.cost * NEOS_decision$copies, 
                                                      NEOS_decision$Plant4.Manf.cost * NEOS_decision$copies)))))

## calculate total Optimal costs
NEOS_decision <- mutate(NEOS_decision, opt_cost <-  
                          ifelse(NEOS_decision$optPlant == "Plant1", NEOS_decision$Plant1.Cost * NEOS_decision$copies,
                                 ifelse(NEOS_decision$optPlant == "Plant2", NEOS_decision$Plant2.Cost * NEOS_decision$copies,
                                        ifelse(NEOS_decision$optPlant == "Plant3", NEOS_decision$Plant3.Cost * NEOS_decision$copies,
                                               ifelse(NEOS_decision$optPlant == "Plant4", NEOS_decision$Plant4.Cost * NEOS_decision$copies, 
                                                      NEOS_decision$Plant4.Cost * NEOS_decision$copies)))))

## count the number of cylinders/versions per plant
## This count * MR yields base MR costs
baseline_cyl <- count(distinct(select(cust3,  RunCode, plant)))
print(baseline_cyl)

## number of optimized cylinder sets
optimal_cyl <- count(distinct(select(NEOS_decision,  RunCode, optPlant)))
print(optimal_cyl)

print(sum(NEOS_decision$base_manf_cost))
print(sum(NEOS_decision$base_dist_cost))
print(sum(NEOS_decision$base_cost))

print(sum(NEOS_decision$opt_manf_cost))
print(sum(NEOS_decision$opt_dist_cost))
print(sum(NEOS_decision$opt_cost))

## write the final model to a text file
decisionname <- paste(title, event,"_decision.csv", sep = "")
write.csv(NEOS_decision, file = decisionname)

library(gridExtra)

plotname <- paste(title, event,"_plot.png", sep = "")
png(plotname, width = 960, height = 960)

myplot1 <- ggplot(NEOS_decision, aes(x=plant, y=sum(copies), fill=RunCode)) +
  geom_bar(stat="identity") + labs (y = "Copies") + ggtitle ("Baseline Copies by Version & Plant") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

myplot2 <- ggplot(NEOS_decision, aes(x=optPlant, y=sum(copies), fill=RunCode)) +
  geom_bar(stat="identity") + labs (y = "Copies") + ggtitle ("Optimized Copies by Version & Plant") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

options(scipen = 9)
print(grid.arrange(myplot1, myplot2, ncol=2))
dev.off()

}
