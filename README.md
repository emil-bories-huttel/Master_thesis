This repository contains all code and scripts used for my master’s thesis:  
"Building Beliefs: Are Danes Housing Supply Skeptics And Can Such Beliefs Be Altered? Evidence from an RCT"

Emil Bories Hüttel – Aarhus University, 2025

-------------------------------------------------------------------------------

Overview

The full thesis is available as a PDF in the repository. It broadly examines the measurement of supply skepticism and investigates Bayesian updating behavior related to supply skepticism within the Danish electorate. 

The analysis is implemented in R version 4.2.2, and the scripts are organized into four stages:  
- Data cleaning and preparation  
- Prior and posterior distribution modelling (supply skepticism)
- Model estimation  
- Post-estimation analysis and vizualisation. 

-------------------------------------------------------------------------------

Script Execution Order

Please run the scripts in the following order:

1 cleaning and sample composition/  

2 prior distribution/  

3 posterior distribution/  

4 Bayesian model fitting/ (optional – see below)

5 Likely interval plotting 

6 Learning plots  

Script 4 Note:  
Running the full 4 Bayesian model fitting script is NOT required unless you need to re-estimate certain models.  
All Pre-estimated model objects can be downloaded [here](https://drive.google.com/drive/folders/1t5V34YY1P8jiDxWrskdYXdN9AcxAGRik?usp=sharing)   
Running the entire script takes around 25 minutes on my setup. The models sample very efficiently; the primary bottleneck is the time required for brms to compile the underlying Stan code. 

-------------------------------------------------------------------------------

Miscellaneous Scripts

The Misc, appendix and old scripts/ folder contains:

- Rough or experimental scripts  
- Code for appendices or figures referenced in the thesis  

These scripts may not run as-is and usually require the main pipeline to be executed first unless stated otherwise in the thesis 

-------------------------------------------------------------------------------

Folder Structure

├── Data/   # raw data and processed data 

├── Figures/   # Saved plots

├── Housing market stats/   # Data and code for descriptive analysis (section 2.1)

├── Scripts/   # main scripts

│   ├── Misc, appendix and old scripts/ # Misc scripts 

├── Analysis.Rproj   # R project

├── Thesis.pdf   # Thesis 

└── README.txt

-------------------------------------------------------------------------------

Contact

For questions or feedback, contact:  
emilhuttel@gmail.com
