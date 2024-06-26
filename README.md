# Method-to-create-a-dataset-of-MNEs-ownership-structures
Instructions to construct a dataset of multinational enterprises and their patent activities. The dataset is based on Orbis data, and the files are not made available here. One needs to have access to Orbis to be able to reproduce the codes.

This repository contains three main R codes (*Code1-Dataset Code - Panel v11_Long Code*, *Code2-Merge_allpatents*, and *Code3-Dataset Code - Additional analysis*) and additional instructions aimed at generating a database of MNE's ownership structure and their related patents for the period from 2011 to 2019. To recreate the dataset, one needs access to Orbis and Orbis IP. Detailed instructions about the files to be downloaded are presented in the document named *Information_raw_files* (available in docx and pdf).

All files generated by the three mentioned R codes are saved into separated folders (namely "files_created_code1", "files_created_code2", and "files_created_code3"). These folders are too big and set to be ignored on GitHub, so anything created within them won't be uploaded or visible to other users.

A description of the method used in these codes will be given in an arxiv paper yet to be publised. This paper will also include general information about the data coverage of Orbis from around the time the data was downloaded and some broad descriptives of the data generated in code 2.

General points:
1. The codes use a lot of memory to run. It is better to run them not at once, but step by step. Codes 1 and 3 don't have "check-points" where you can stop without losing the data (and thus, you have to run them completely without cleaning your environment). Conversely, code 2, which uses more computational power, has a lot of check-points where it saves the important files, clean the environment and starts from scratch. You can identify such check-points for they preclude the command "rm(list=ls())" (which is used to clean the current environment). When you reach one of these, you can close R if you want, and go one whenever you see fit from the check-point on. All you have to do when you start again is to run the libraries, and then continue from where you stopped.
2. There are some errors and warnings in the codes, which are highlighted with a red text in RStudio. The warnings usually are caused when reading Orbis raw excel files, due to the fact that such files don't have a column name for the first column (you will see something like "New names:* `` -> ...1", meaning that R is naming the columns with the name "1"); the errors usually appear when excluding files, because some of the files to be excluded were already excluded in a command before. In general, this errors and warnings were already verified and can be ignored.

## First part: running the code named "Code1-Dataset Code - Panel v11_Long Code"
The generation of this database is to start by running the code named *"Code1-Dataset Code - Panel v11_Long Code"*. This is the code that creates the ownership structures of Global Ultimate Owners (GUOs) and their respective subsidiaries. It creates a folder named *"files_created_code1"*, which contains the following files (generated through the mentioned code):

![image](https://user-images.githubusercontent.com/58182885/131147675-6f736be2-829c-4206-9b01-2753f3dc1d1a.png)

In total, the generated folder occupies a space of 7.99 Gb. The main files are the ones named MNE11, MNE12, ..., MNE19. They contain the ownership information of MNEs and are the input used on the second code (i.e., code named "Code2-Merge_allpatents"). Other files (e.g., Full1, Full2, ..., Patents2011, Patents2012, ..., Stock2011, Stock2012, ...) are used in code 3 to generate panel data with general information about each MNE (named Full.csv). 

Individual inconsistencies in the size of each file generated in your own computer can be generally ignored. They may be caused for several reasons (e.g., you may use "." for decimals, whereas I use "," in my computer, or other configurations like this). At the end, a print of the final result will be presented, so you can visually check if this matches your data despite the small differences.

## Second part: running the code named "Code2-Merge_allpatents"
The second codes generates the ownership structure of all patents registered between 2010 and 2019, and matches them to the ownership structures generated in the previous code. It creates a folder named *"files_created_code2"*, which contains the following files (generated through the mentioned code):

![image](https://user-images.githubusercontent.com/58182885/131331526-b15270f9-5aa7-4002-8f8a-9531f1e99fdd.png)

In total, the generated folder occupies a space of 80.8 Gb. This huge space is partially due to the various different kinds of data that are generated, which can be used for distinct purposes. There is separated data, for example, about patents which passed through a change in ownership during the considered period (file ChangeInOwnership_clean.csv), changes that didn't have any change in ownership (with one owner under No_change_ownership_1owner_Part1.csv and No_change_ownership_1owner_Part2.csv, and with more than one owner in MoreThan1owner.csv), about the IPC codes of every patent (Final_Dataset_IPCcodes.csv), about the 'life' of every patent through every considered year (i.e., to whom the patent belonged in a given year, through the files Patents2011_withPubNos, Patents2012_withPubNos, ..., Patents2019_withPubNos), about the stock of patents considered for the period from 2000 to 2010 (file Stock_2000_2010.csv), etc.

The most important files, which result from the merging of patents with company data, are produced on the SECOND PART of the code, in section 7. Between those, is a file named FinalDataset_allYears_FullGUOs.csv, which includes panel data for every GUO from 2010 (i.e., this 2010 year considers the stock of patents with no change in ownership) to 2019 (some additional descriptives for this file are available in the file Descriptive_per_year.xlsx). The variables from this panel data include:

- TotalCompanies: it counts the total number of GUOs that appear in the data in a given year (even if they don't exist anymore)
- TotalPatents: shows the total number of patents that are linked to our GUOs or subsidiaries in a given year
- TotalPatentsGUOs: shows the total number of patents linked exclusively to our GUOs in a given year
- TotalPatentsSubs: total number of patents linked exclusively to our subsidiaries in a given year (but just the subsidiares that are linked to one of the considered GUOs)
- TotalSubsidiaries: total number of subsidiaries of all GUOs in a given year
- TotalSubsidiariesWithPatents: total number of subsidiaries that have at least one patent in a given year (but just the ones linked to our GUOs are considered)
- Total_AI_Patents: total number of AI patents owned by our considered GUOs and their subsidiaries in a given year
- Total_AI_PatentsOwnedGUOsYear: total number of AI patents owned exclusively by our considered GUOs in a given year
- Total_AI_PatentsOwnedSubsYear total number of AI patents owned exclusively by the subsidiaries (and just the one linked to our GUOs) in a given year
- Total_CompaniesWitOUThAI_PatentsYear: number of GUOs without AI patents in a given year
- Total_CompaniesWithAI_PatentsYear number of GUOs with at least one AI patent in a given year
- Total_GUOs_withPatentsYear: total number of GUOs that have at least one patent (regardless of being about AI). This indicator is important given our data structure: it is through it that we can see which GUOs are 'alive', i.e., GUOs that were not merged with others and therefore still own patents (remember that when GUOs are merged the patents move to the acquiror GUO)
- Total_GUOs_withPatents_throughTheirSubs: total number of GUOs whose subsidiaries have at least one patent; this number is bigger than the last one because apparently there is more often the possibility that GUOs don't create patents themselves but their subsidiaries do;

## Third part: running the code named "Code3-Dataset Code - Additional analysis"
Finally, there is also the option of running code 3, although this code is supplementary. It generates another panel data, which has different variables and considers a distinct classification for AI patents. The file is generated within the folder "files_created_code3", which looks like:

![image](https://user-images.githubusercontent.com/58182885/131343379-2a57e96d-ed11-465c-a029-ebf51f52bc9d.png)

This file has 106 variables. Some of them are described in the code, and some on the file "Data description_MNE_AI".

## Extra: How to work with Git within RStudio
https://inbo.github.io/git-course/course_rstudio.html

