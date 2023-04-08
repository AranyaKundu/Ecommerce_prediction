# Ecommerce_prediction
Prediction of Growth of Sales for a typical Global Super Store. <br>
The data I used contains order details of a Global Superstore. The rows include `Order ID`, `Order Date`, `Ship Date`, `Ship Mode`, `Customer ID`, `Customer Name`, `Customer Segment`, `Postal Code`, `City`, `State`, `Country`, `Market`, `Region`, `Product ID`, `Category`, `Product Name`, `Sub-Category`, `Price`, `Quantity`, `Discount`, `Profit` \& `Ship Mode`. <br>
I have used R to visualize the data using ggplot. This also includes visualization of the data as time series object.<br> 
Then I have fitted Machine learning models (which includes basic models like `linear regression` and `generalized additive model` and also advanced models like `Decision Tree`, `Bootstrap Aggregation` and `XG Boost`).<br>
Finally, I have done time series analysis of the data. Here I tried to fit multiple variants of `linear` and `polynomial` trend models, added seasonality to those models. There are also exponential trend and exponential seasonality models. Following these models I tried fitting smoothing methods like `Seasonal Naive` and `Holt-Winter` method.<br>
## Shiny dashboard
I converted the whole analysis in the form of a shiny dashboard where the user can change different parameters of and visualize the data. They can modify and choose different machine learning models and check the output. <br>
The shiny dashboard can be accessed through this link: <br>
https://aranya-kundu.shinyapps.io/ecommerceanalysis
