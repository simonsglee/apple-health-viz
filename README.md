# Apple Health Visualizations with R Shiny

## Project Description
This is my personal R Shiny dashboard for visualizing health and fitness data gathered through my iPhone and Apple Watch. The data I'm personally interested in monitoring are exercise, weight, mindfulness meditation and sleep. Please feel free to borrow code to create visuals for other metrics you are interested in.

## Demo
You can see a demo of the Shiny Dashboard using fictional data [here](https://simonsglee.shinyapps.io/apple-health-viz/)

## Installation
Here are the steps you need to follow if you want to visualize your own data with this dashboard. 

<b> 1. Install R and R Studio </b>

If you don't have R or R Studio installed, follow the steps outlined [here](https://posit.co/download/rstudio-desktop/).

<b> 2. Download the project from GitHub </b>

On the main Github project page, select "Code". Click the "Download Zip" option and unzip the file. 

<b> 3. Export your health data</b>

Go to the Health app on your iPhone and tap your initial or picture. 

Tap "Export All Health Data" then choose a method for sharing your data to your computer. 

Once you unzip the file, you should see a folder named "apple_health_export". 

Copy this file and navigate to the data folder in the project folder. 

Paste the file into this folder. 

Next time you update the data, replace the existing folder with the new folder.

If you don't want to use your own data but still want to check out the dashboard, there is a sample data file that the Shiny App will load automatically if it doesn't detect any other data.

<b> 4. Install packages </b>

In the project folder, open the file named "app.R" using RStudio.

This project uses the [renv package](https://rstudio.github.io/renv/articles/renv.html) for library management. This means all the packages and the versions required run this project are stored in the lockfile called "renv.lock".

In the command line of RStudio, type `renv::install()` to install all packages from the lockfile.

<b> 5. Run the Shiny App </b>

Click "Run App" in RStuido to run the Shiny app. You should see the Shiny dashboard after a few minutes.