# landscape-stats-analysis

## Working with raster data

- https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/

## Correlation
- https://stats.stackexchange.com/questions/108007/correlations-with-unordered-categorical-variables
- https://stats.stackexchange.com/questions/369783/how-to-do-a-correlation-matrix-with-categorical-ordinal-and-interval-variable

- https://dataviz.hu/2019/04/az-adatvizualizacio-harmadik-hullama/?fbclid=IwAR33z_xlu9asKaDmLGus5pglFtpzsR6Vk6lbF6We0sviAHYBRDHvFepDHbM

- https://medium.com/@outside2SDs/an-overview-of-correlation-measures-between-categorical-and-continuous-variables-4c7f85610365

- http://staskolenikov.net/talks/Gustavo-Stas-PCA-generic.pdf


## Clustering
- https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
- https://stats.stackexchange.com/questions/151942/hierarchical-clustering-of-categorical-variables-in-r-alternative-algorithms
- https://www.quora.com/How-do-I-do-clustering-for-categorical-data
- https://shapeofdata.wordpress.com/2014/03/04/k-modes/

- https://dabblingwithdata.wordpress.com/2016/10/10/clustering-categorical-data-with-r/
- http://www.cs.ust.hk/~qyang/Teaching/537/Papers/huang98extensions.pdf
- https://cran.r-project.org/web/packages/klaR/klaR.pdf

[Kaggle: Clustering of Categorical Data](https://www.kaggle.com/vijjikiran/clustering-of-categorical-data):  
Typical clustering algorithm: `k-means`.  
Issue: _euclidean distance_ is not meaningful for categorical data.  

[Towards Data Science: Hierarchical Clustering on Categorical Data in R](https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995):   
To calculate dissimilarity, we have to chose another distance metric.  
For categorical data, `Gower distance` is the preferred choice.   
Algorithm choice:  
- Agglomerative clustering: 
    1. Start with n clusters.   
    2. The algorithm will try to find most similar data points and group them, so they start forming clusters
    3. Better at discovering small clusters  
    4. Example: AGNES  

- Divisive clustering:  
    1. Assuming all your n data points are one big cluster  
    2. Divide most dissimilar ones into separate groups
    3. Better at discovering large clusters  
    4. Example: DIANA  

Clustering: you are interested in distinctive groups of data points, such that the distance between them within clusters (or compactness) is minimal while the distance between groups (separation) is as large as possible.  

Imbalanced data set - is it a problem??


[The Shape of Data: K-modes](https://shapeofdata.wordpress.com/2014/03/04/k-modes/):  
One solution to the distance problem: convert answers into vector (one-hot encode them).  

- K-means:  
    1. Select a small number of special points called centroids, which are in the data space but not necessarily data points  
    2. Work out which of the centroids each data points is closest to  (using Euclidean distance)  
    3. Replace each centroid with the new centroid/center of mass of the data points that were associated to it  

- K-modes:  
    1. Compute the centroids, we again start by adding up the number of questionnaires that responded with each possible answer to each of the questions. Simply records which answer to each question got the most votes. This is the mode of the responses -the most common answer – which is where the name K-modes comes from.  
    2.  Calculate the “distance” from each data point to each centroid. The most obvious notion of distance is as follows: For each data point and each centroid, we can define the distance to be the number questions they disagree on. As with the Euclidean distance we used in K-means, when they agree on a question, this will make the distance lower, and when they disagree on a question, it will make the distance higher. **Note**, however, that the third point about the Euclidean distance doesn’t hold here: The K-modes centroids don’t keep track of how close the margin was between the answer that that got the most responses and the second most. So, if the data point disagrees with the most popular response among the original data points, it gets the same penalty whether or not the original data points strongly agreed on this answer.  

Questions:
    - K?  
    - handle missing data?  
    - article on K-modes: http://www.irma-international.org/viewtitle/10828/

### K-modes trial
Full data: runs out of memory. OK with 1M sampled rows