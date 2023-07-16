
# Multivariate Analysis Project

This project contains a detailed multivariate analysis of milk traits data using R. The analysis was originally performed as part of the Multivariate Analysis course. 

## Dataset Background

The dataset used in this project is a part of an extensive research conducted by Sinead McParland, an Adjunct Associate Professor from the Animal and Grassland Research & Innovation at Teagasc. The research explores the use of big data in the Irish Dairy Industry, focusing on how various traits in milk vary among cows and their impact on the quality of milk produced.

The dataset contains information from 622 milk samples from different breeds, ages, different research herds across Ireland, and different stages of lactation. Each of these samples has been analyzed for 14 individual traits including different types of milk proteins (Casein and Whey) and various technological traits such as heat stability, pH, and milk coagulation properties. The data was analyzed using mid-infrared (MIR) spectroscopy technology, which is a routine method of milk analysis globally.

The dataset was used to carry out a partial least squares regression analysis, among other data reduction techniques, to predict various traits from the MIR spectra. The aim was to improve the efficiency and profitability of the dairy industry by identifying the cows that produce the best quality milk and thereby understanding which cows to keep and breed from.

## Overview

The focus of this project is on Principal Component Analysis (PCA), Principal Component Regression (PCR), and handling missing values in the dataset. 

## Files in this Repository

- `Multivariate_Analysis.Rmd`: This is the original R Markdown file that contains the code for the analysis.
- `Multivariate_Analysis.md`: This is the Markdown file generated from the R Markdown file. It contains the code and outputs of the analysis. Click [here](./Multivariate_Analysis.md) to view the detailed analysis.
- `README.md`: This is the file you're currently reading. It provides an overview of the project.

## Running the Code

To run the analysis on your own machine, you will need to have R installed. You can then clone this repository and open the `Multivariate_Analysis.Rmd` file in your R environment. To clone this repository, use the following command in your terminal:

```bash
git clone https://github.com/aditya487/Multivariate-Analysis-Project.git
