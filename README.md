# EDA Tool - R Shiny Application

## Overview
This R Shiny application provides a comprehensive Exploratory Data Analysis (EDA) tool. It is designed to facilitate statistical analysis and data handling, especially focusing on handling missing values and performing various statistical tests.

## Features

### Data Import
- Upload CSV files for analysis.

### Data Viewing
- View the uploaded data in an interactive table format.

### Data Summary
- Generate detailed summaries for numeric and non-numeric data.
- Custom function `summaryNEW` for extended statistical summaries.

### Statistical Tests
- Perform Chi-squared tests, T-tests, and Pearson correlation analysis.
- Custom methods for `edatool` class to handle these tests.

### Handling Missing Values
- Reports on missing values in the dataset.
- Options to handle missing values in numeric and non-numeric columns, including deletion of rows/columns or imputation with mean, median, mode, or constant values.

### Tabulated UI
- Organized user interface with separate tabs for different functionalities.

## Technical Details

### Libraries Used
- shiny
- shinydashboard
- DescTools
- stats
- Rcpp
- installr
- e1071

### Class and Method Extensions
- A new class `edatool` inheriting from `data.frame` is created.
- Generic functions and methods for statistical analysis and data handling are defined for the `edatool` class.

### Installation of Necessary Packages
- Automatic installation of required packages if they are not already installed.

## Usage

1. Run the Shiny application script.
2. Upload a CSV file using the file input control.
3. Explore the various tabs to view data, generate summaries, perform statistical tests, and handle missing values.
