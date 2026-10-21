# DarkPattern Detective 🔍

### ML-Based Detection of Manipulative Patterns in Online Shopping

DarkPattern Detective is a machine learning project built using R that identifies potentially manipulative language used on online shopping websites. It uses a Naive Bayes classifier and an interactive Shiny dashboard to classify shopping text.

## 🚀 Live Demo

[Open DarkPattern Detective](https://falakmunir7.shinyapps.io/DarkPatternDetection/)

## ✨ Features

- Detects potentially manipulative shopping text.
- Classifies text into Potential Dark Pattern or Not Dark Pattern.
- Interactive R Shiny dashboard.
- Visualizes dataset categories and label distribution.
- Uses text preprocessing and machine learning.

## 🛠️ Technologies Used

- R
- Shiny
- Naive Bayes
- tm
- e1071
- ggplot2

## 📊 Dataset
Dataset file: https://github.com/yamanalab/ec-darkpattern/blob/master/dataset/dataset.tsv

The dataset was downloaded in TSV (Tab-Separated Values) format and loaded into R using read.delim() for data preprocessing and machine learning.

The original research dataset contains examples of dark-pattern text collected from e-commerce websites, along with non-dark-pattern text for comparison. The downloaded file used in this project contains 2,356 records and includes the columns page_id, text, label, and Pattern Category.
The dataset contains 2,356 text records with labels and pattern categories. The model's predictions are not guaranteed to identify every dark pattern correctly.

## ▶️ Run Locally

1. Install R and RStudio.
2. Install the required packages: `shiny`, `ggplot2`, `tm`, and `e1071`.
3. Place `app.R` and `dataset.tsv` in the same project folder.
4. Open `app.R` in RStudio.
5. Click **Run App**.

## 👩‍💻 Developer

**Falak Munir Ahmed Tanga**
