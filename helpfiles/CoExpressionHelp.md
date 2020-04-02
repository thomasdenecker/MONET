### Help
#### Select columns to calculate co-expression

***

Select the columns that can be used to calculate co-expression. The objective is 
to find connections between proteins / genes. The more they are co-expressed, 
the closer they are to each other, the more likely they are to be connected.

Below is an illustration to explain the strategy.

<p align="center"><img src="img/helpfiles/coexpressionStrategy.PNG" alt="logo"></p>

To calculate the link between two proteins, two methods are available: the 
calculation of an Euclidean distance and a correlation. You can choose between these two approaches.

A selection threshold is calculated in relation to the percentage that you have 
set (between 0 and 100). By default this percentage is 10, which means that we 
will look for the distance that keeps 10% of the links in the network.



