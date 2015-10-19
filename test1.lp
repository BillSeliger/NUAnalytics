/* Objective function */
min: +99 plant1order1 +98 plant1order2 +97 plant1order3 +96 plant1order4 +89 plant2order1 +88 plant2order2
 +87 plant2order3 +86 plant2order4 +79 plant3order1 +78 plant3order2 +77 plant3order3 +76 plant3order4
 +74 plant4order1 +68 plant4order2 +67 plant4order3 +66 plant4order4 +59 plant5order1 +58 plant5order2
 +57 plant5order3 +57 plant5order4;

/* Constraints */
order1demand: +plant1order1 +plant2order1 +plant3order1 +plant4order1 +plant5order1 = 10;
order2demand: +plant1order2 +plant2order2 +plant3order2 +plant4order2 +plant5order2 = 16;
order3demand: +plant1order3 +plant2order3 +plant3order3 +plant4order3 +plant5order3 = 23;
order4demand: +plant1order4 +plant2order4 +plant3order4 +plant4order4 +plant5order4 = 31;
capacityplant1: +plant1order1 +plant1order2 +plant1order3 +plant1order4 <= 40;
capacityplant2: +plant2order1 +plant2order2 +plant2order3 +plant2order4 <= 40;
capacityplant3: +plant3order1 +plant3order2 +plant3order3 +plant3order4 <= 40;
capacityplant4: +plant4order1 +plant4order2 +plant4order3 +plant4order4 <= 40;
capacityplant5: +plant5order1 +plant5order2 +plant5order3 +plant5order4 <= 40;

/* Integer definitions */
int plant1order1,plant1order2,plant1order3,plant1order4,plant2order1,plant2order2,plant2order3,plant2order4,plant3order1,plant3order2,plant3order3,plant3order4,plant4order1,plant4order2,plant4order3,plant4order4,plant5order1,plant5order2,plant5order3,plant5order4;
