# ANALYSIS
## Observations, remarks, model documentation etc.

### 1. Basic statistics

Assumption it's an eCommerce business based on the transactions.
Right after the first moment it was obvious there is a decreasing trend so there is a room for improvement and the returning customers are valueable.
TODO predictions

All the transactions are really skewed considering the amount and quantity also. This is really typical to eCommerce and we have to handle all the outliers to build a reliable model to predict any CLV related metric.

### 2. High overview predictions

It's obvious the upcoming trend is slightly decreasing so the expected monetary values probably will be lower.
I've created a TBATS model to forecast the sales amount numbers to have a high level estimate.

TODO picture

# 1,087,639

The forecasted sales amount for the upcoming year. For 95% conf there is a range between 19,813,138 and 10,087,812.

### 3. Cohorts

From the cohorts I observed a poor retention rate. Also the new visitors are decreasing so the outlook is not really great.


### 4. Forecast sales amounts

First I tried to fit churn model to predict the retention rate what was observed in cohorts. At a first glance any rf, gbm or deep models feature building seemed really hard and I had a feeling this is not the right way so I thought I need a probabilistic model to get the best results.

I was really impressed by Roberto Medri (Etsy) talk so I tried to move this way.  
http://cdn.oreillystatic.com/en/assets/1/event/85/Case%20Study_%20What_s%20a%20Customer%20Worth_%20Presentation.pdf

The most important take away from the talk what could be used here is the following:
At every moment, customer flips two coins:  
The first coin determines if the customer lives or dies  
(e.g. forgets about your business).   
The second coin determines if she buys or not.   

There are many models out there to use to this problem.  
For the live/die problem there is the Buy Til You Die library in R and the https://github.com/CamDavidsonPilon/lifetimes for Python.

After few quick tests with the MBG/NBD model was the best performing on the training test. The python implementation was a bit different and much easier to use so I moved on with the python package.

The model's implementation with commentary can be found in the CLV notebook.

The final model could be tested in the modelPred notebook with the given function easily.

### 5. Model's summary

Based on the customers historical frequency and recency of buys with the combination of the customers age the future transactions are predictable. Also if we put the sales amount to the model we can estimate the average sales amount of transactions. Putting all together all the future sales amounts are predictable.

BG/NBD model   
In addition to the Pareto/NBD based models it's handling better the non returning customers in the training period. This is really needed to estimate future transactions of customers with recent first buy. The given dataset has a lot of these so it's handy.
The model's based on the followings:
* While active, the number of transactions made by a customer follows a Poisson process
with transaction rate λ. This is equivalent to assuming that the time between transactions
is distributed exponential with transaction rate λ.
*  Heterogeneity in λ follows a gamma distribution
*  After any transaction, a customer becomes inactive with probability p. Therefore the
point at which the customer “drops out” is distributed across transactions according to a
(shifted) geometric distribution
* Heterogeneity in p follows a beta distribution
* The transaction rate λ and the dropout probability p vary independently across customers
* Based on numerical optimization we can estimate the parameters



### 6. Technical part

#### R part

In the R/init.R you can check the required packages and assets for the R/exploration.R. It's also install/update the required packages.

The exploration.R is just a dump of the exploratory analysis part.

#### Python part

For the python part anaconda installation is recommended because it has most of the packages.
If needed you can download it here: https://www.continuum.io/downloads  

Maybe you need some extra packages to install what could be need to run the notebooks. So with pip install -r python/requirements.txt you can install/update the requirements.

The best way to check the files is to use jupyter notebooks. In a command line just hit jupyter notebook and navigate to the python dir in the repository.
