# This project is an Operations Scheduling Project that utilises the 'Just-in-Time' (JIT) Algorithm to maximise the start time of an operation.

### Before running the codes on Rstudio we ensure that there exist:
1. GUROBI installation
We are integrating GUROBI optimizer with R, so R should be able to locate the Gurobi license. 
→ library(gurobi) will only work if the R is connected with Gurobi Optimizer licensing and program. 
https://www.gurobi.com/academia/academic-program-and-licenses/ 
2. R Shiny environment is set. 
We are using using the R Shiny package for our interface, so we must ensure that library(rshiny) is properly installed. 

### Within the zip file called “my-shiny-app”:
1. Holds a file called “app.r”. Please open this in RStudio and please also ensure that the directory is the same as the zip file.
A video called “Tutorial Video” in .mp4 format is also found in the zip file that is the video uploaded to youtube to serve as a tutorial guide for the user on the use of the interface. 

### “WWW” folder is found within the zip file 
1. Within this file, there is our styles.css text script, Icon_styles.css text script 
(both css codes control the animations, fonts size and styles of the texts) that are  applied on the interface. 
2. It also holds various images in .png format in the folder that is applied onto the interface 
No need to open this within Rstudio, running the app.r code is sufficient. 

### Sample data sets are found within the zip file
1. We provide sample data files (xlsx.) to test the scheduling system
“Bicycledata” and “PC_data” are the two different BOM routings that allowed us to test our system. 

### Step-by-step procedure:
Ensure all packages have already installed, open “INSTALL PACKAGES” file and run all the codes in R command
Open app.R in Rstudio
Run: runnApp() on the console
Choose “Open in Browser” option 

