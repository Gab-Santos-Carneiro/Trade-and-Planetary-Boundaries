README

OVERVIEW

Here you can find all the codes needed to replicate the analysis carried out in the paper  "Rethinking trade for the ecological transition: Quantifying the trade drivers of planetary boundaries". 
In order to run the codes you need first to download the data from the website: https://ielab.info/resources/gloria and move it to your R project directory folder. 
After you run the code, if you want to generate the exact same images as in the paper you need to use the Flourish free online tool: https://flourish.studio. 
The original code was written to work in the R Studio 2.1.19 version from the Onyxia - SSP Cloud Datalab, and the datasets were stored and retrieved from an online cloud storage service. 
The code here provided is an adaptation of the original code that should run in a local environment. Consequently, the authors were not able to completely test this code (this is particularly true for the section "0 - Loading data and basic matrices"). As they didn't have access to any computer that had the computational requirements needed to run the code locally. 
However, the authors carefully reviewed the code and they believe it should run normally in a local environment given that computational capacity is enough. 

DATA AVAILABILITY 

You can obtain the dataset for free by registering and requesting it in the website: https://ielab.info/resources/gloria. It usually takes some hours to get their approval and it is free. 
The authors employed the version Release 57. Now they are at Release 59 and the Release 57 is not available anymore for download. Nevertheless, there shouldn't be siginificant differences in the results as the 58 and 59 releases were more oriented towards adding for information to the dataset rather than changing values. 
As the dataset is available for free and upon request in the GLORIA official website, the authors believe they should not host the dataset here in this github. They understand that the ones interested in using the GLORIA dataset must obtain it upon request and approval from the dataset owners.

Data Sources: 
Once you get access to GLORIA's repository you need to download the "GLORIA_ReadMe_xxx.xlsx" file and the "GLORIA_MRIOs_59_2021.zip" file which is located inside the folder "GLORIA_MRIO_Loop059_part_I_MRIOdatabase" in the google drive.
Add the xslx file in your R repository folder as well as the content of the unziped folder. The data employed on the paper was downloaded in 2022. 

Statement about Rights: 
I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.
I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package.

INSTRUCTIONS FOR REPLICATORS

1 - Download the datasets and add them to the R project repository. 
2 - Run the code and generate the results and the inputs needed to generate the figures. 
3 - Access the Flourish tool and recreate the figures. Flourish has some sort of "in-built excel sheets" that need to be manually filled in order to generate the figures. Use the output from R as an input in Flourish. 
Important: The authors of the paper originally run the model using a R Studio 2.1.19 version from the Onyxia - SSP Cloud Datalab and a cloud storage service. The authors are unable to run the codes in their personal computers because of computational requirements. Therefore, they were not able to completely test the "0 - Loading data and basic matrices" section of the code. Some changes to the code might be needed in this part in order to run it.

REQUIREMENTS

R and R Studio newest versions. 
The authors don't have the expertise to provide detailed information on memory, runtime and storage requirements. Usually, GLORIA is too heavy to run in a standard PC as matrix operations require significant computational resources. 
Important: The authors of the paper originally run the model using a R Studio 2.1.19 version from the Onyxia - SSP Cloud Datalab. The datasets are uploaded in cloud storage service and uploaded into R from there. Nevertheless, apart from computational and memory requirements, the authors don't see any other reason for the code to not work in the standard downloadable R software. 

CODE DESCRIPTION

There is only one code file. The code first updates the datasets and generate the basic matrices needed for the modeling exercise. Then the code calculates the results. Later, the code structures the results for data visualization. In the end, the code generates the analysis which integrate the Annex A of the Manuscript. 

FOLDER STRUCTURE

README
Script.R
Final Manuscript.pdf
ANNEX A.pdf

Important: All the figures can be found in the "Final Manuscript.pdf". As explained before, the code does not generate the final figures. They are built using the Flourish online tool. 
