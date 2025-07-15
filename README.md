Master's Thesis – Building Beliefs: Are Danes Housing Supply Skeptics And Can Such Beliefs Be Altered? Evidence from an RCT

This repository contains all code and scripts used for my master’s thesis:  
"Building Beliefs: Are Danes Housing Supply Skeptics And Can Such Beliefs Be Altered? Evidence from an RCT"
Emil Bories Hüttel – Aarhus University, 2025

-------------------------------------------------------------------------------

Overview

This full thesis is available as a pdf in the repository 

The analysis is implemented in R version 4.2.2, and the scripts are organized into four stages:  
- Data cleaning and preparation  
- Prior and posterior distribution modelling (supply skepticism)
- Model estimation  
- Post-estimation analysis

-------------------------------------------------------------------------------

Script Execution Order

Please run the scripts in the following order:

1. 1 cleaning and sample composition/  
2. 2 prior distribution/  
3. 3 posterior distribution/  
4. 4 Bayesian model fitting/ (optional – see below)
5. 5 Likely interval plotting 
6. 6 Learning plots  

Script 4 Note:  
Running the full 4_post_estimation script is NOT required unless you need to re-estimate certain models.  
All Pre-estimated model objects can be downloaded here: https://drive.google.com/drive/folders/1T9Ql4-0_MRS8L7A32SJ-GAev2vJ9PgT2?usp=drive_link   
Running the entire script takes around 25 minutes on my setup.

-------------------------------------------------------------------------------

Miscellaneous Scripts

The Misc_Appendix_Etc/ folder contains:

- Rough or experimental scripts  
- Code for appendices or figures referenced in the thesis  

These scripts may not run as-is and usually require the main pipeline to be executed first unless stated otherwise in the thesis 

-------------------------------------------------------------------------------

Folder Structure

.
├── Data/
├── Figures/
├── Housing market stats/
├── Scripts/
│   ├── Misc, appendix and old scripts/ 
├── Analysis.Rproj
├── Thesis.pdf
└── README.txt

-------------------------------------------------------------------------------

Contact

For questions or feedback, contact:  
emilhuttel@gmail.com
