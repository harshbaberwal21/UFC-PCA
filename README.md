# UFC-PCA

## Intention
Identifying structure in UFC data using PCA. Studying the UFC data can help better understand the game dynamics and to make accurate predictions, because just like boxing, gambling on outcome is big business.

This project was carried out in R and sought out from a statistical analysis perspective. To identify the structure, PCA was used to build components (or factors) and these were further analyzed to identify the latent features in the data.

## Method and Output
In an attempt to find structure across all the features of a UFC fight, as measured in fightmetrics data, PCA digs out a component that explains the most variance and encompasses only some type of hits landed and received. The features loaded onto this component can be seen in the below graph.

![Component 1 Weights](https://github.com/harshbaberwal21/UFC-PCA/blob/ee6b09f7b18d5da3c2ef1e3a9434a13160789805/Comp1%20Weights.png)

*The number of strikes landed on fighter’s leg by opponents in the fighter’s career till that fight combined. Could be taken to indicate the fighter’s ability to take hits on leg and block by the same.*

#### Most Influential Features:
1. Strikes landed  to head and body 
2. Strikes landed  when in clench with the opponent




##### This was part of a group project wherein multiple statistical techniques were applied to UFC dataset to test various hypothesees.
