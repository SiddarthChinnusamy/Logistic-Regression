# Logistic-Regression
  The objective of the exercise is to build a binary classification model that assigns 
  a probabiltiy of survival to the passengers in Out of Sample dataset based on the training data
  that has labels indicating whether a particular passenger survived the titanic sink.

# Packages Required
  1. ROCR
  2. pROC
  3. WVPlots
  4. vtreat
  5. ggplot2

# Flow
1. Data understanding 
  Exploratory analysis is conducted to understand the dataset.
  Missing values for age are replaced with the median age values

2. Data type conversion
The following variables are converted to the data type (factor) to make them categorical
 - Survived;
  Pclass (Passenger Class);
  Sex (Gender);
  Sibsp (Number of sibling and spouses);
  Parch (Number of Passengers and Children);
  Embarked (Port of embarkment);

3. Feature Selection
  Chi-square test is used to test the significance of categorical variables on the survival of passengers
  T-test is used to test the significance of numeric variables on the survival of passengers
  From the results of the tests the following variables were found to be significant

4. Defining the model
  The model was defined as follows, based on the features selected through hypothesis testing in step 3
  Survived ~ PClass + Sex + Age + Fare

5. Test train split
  K- way cross validation is used with a k value of 3 to ensure maximum utilisation of the available training data

6. Evaluation of the model
  For this use case, the cost of classifying a person who will survive as would not survive is less 
  compared to that of the cost of classifying a person who will not survive as will survive.
  For this reason the threshold value is fixed to achieve a higher specificity value.
  The evaluation metrics calculated for the model is listed in the below section

# Evaluation Metrics
  1. Accuracy = 78.5%
  2. sensitivity = 72.3%
  3. specificity = 82.4%
  4. Area under the curve= 0.8441
  5. Relative Gini index = 0.69

# Interpretation
Based on the model summary, it is found the the variables that have significant impact on the survival of the passengers are
  1. Passenger Class
  -  Passengers who travelled in 2nd and 3rd class are disadvantaged and have less chances of survival compared to passengers who            travelled   in Class 1
  2. Sex
  - Female Passengers are more likely to survive compared to male passengers
  3. Age
  - Younger passengers are more likely to survive compared to older passengers.
