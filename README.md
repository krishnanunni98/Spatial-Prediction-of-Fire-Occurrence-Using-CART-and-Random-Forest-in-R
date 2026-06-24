# Spatial-Prediction-of-Fire-Occurrence-Using-CART-and-Random-Forest-in-R
## Project Overview
This project applies machine learning methods to predict fire occurrence using georeferenced presence/absence data and raster-based environmental predictors. Classification and Regression Trees (CART) and Random Forest (RF) models were developed, validated using independent datasets, and used to generate spatial prediction maps.
The workflow demonstrates how tree-based machine learning algorithms can be applied to environmental risk modelling and spatial prediction.

## Objectives
•	Develop a CART model for fire occurrence prediction. 
•	Develop a Random Forest model using the same predictor variables. 
•	Split the dataset into calibration (75%) and validation (25%) samples. 
•	Evaluate model performance using classification metrics. 
•	Assess variable importance. 
•	Generate spatial prediction maps for the study area. 

## Data
•	fire_logit.csv — fire occurrence presence/absence points. 
•	variables_fires/ — environmental raster predictor layers. 

## Methodology
### Data Preparation
•	Imported fire occurrence data. 
•	Converted coordinates into spatial objects. 
•	Loaded raster predictor layers. 
•	Extracted raster values at sampling locations. 

### Model Development

#### CART Model
•	Built a classification tree using the calibration dataset. 
•	Applied pruning to reduce overfitting. 
•	Evaluated model performance using: 
o	Confusion matrix 
o	Kappa statistic 

#### Random Forest Model
•	Built a Random Forest classifier with 500 trees. 
•	Assessed model performance using: 
o	ROC curve 
o	AUC 
o	Model evaluation statistics 
•	Examined variable importance rankings. 

#### Spatial Prediction
•	Generated spatial prediction maps for: 
o	CART model 
o	Random Forest model 

## Main Outputs
•	Pruned CART model 
•	Random Forest classifier 
•	Confusion matrix 
•	Kappa statistic 
•	ROC curve 
•	AUC value 
•	Variable importance plot 
•	CART prediction map 
•	Random Forest prediction map 

## Skills Demonstrated
•	Machine learning in R 
•	Classification trees 
•	Random Forest modelling 
•	Model pruning 
•	Accuracy assessment 
•	ROC and AUC analysis 
•	Variable importance interpretation 
•	Raster-based spatial prediction 
•	Environmental modelling 
•	Reproducible workflows

