# Project with Volvo Cars

### Project description
This project consists of a student groups work with a problem provided
by Volvo Cars. The considered problem is an open set recognition problem for
neural networks (NN). Volvo has a NN that is supposed to get data from two
distributions; In-distribution and Out-of-distribution. The goal is to separate
correctly classified data points from the In-distribution, Inliers from the joint
set of points from Out-of-distribution, (OOD) and Missclassified In-distribution,
Outliers. The predictors for this classification problem is the softmax output
from the NN. The problem is attended with the three methods: Density-based
spatial clustering of applications with noise (DBSCAN), Mixed discriminant
analysis (MDA) and Isolation forest (IF). The findings are that identifying OOD
data points is hard to do with the utilized methods. However, the methods
performs well in separating between Inliers and Missclassified In-distribution.
It is concluded that the latter separation is possible due to well defined limits
in feature space and that the first problem needs to be attended with methods
that does not directly use distance in feature space as means for classification.
It is proposed that further work investigates methods that use deeper layers
from the NN or mappings of the feature space to enable identification of OOD
data points.


#### Link to the [report](https://github.com/calvinnsmith/Volvo-Project/blob/main/volvo_project/report%20and%20presentation/ProjectCourse_VOLVO_report.pdf)

#### Link to the [presentation](https://github.com/calvinnsmith/Volvo-Project/blob/main/volvo_project/report%20and%20presentation/VOLVO2_presentation.pdf)

#### Link to my [Code](https://github.com/calvinnsmith/Volvo-Project/tree/main/volvo_project/code)
