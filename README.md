# DataAnalysis-of-the-EU-Clinical-Trials-Register
Clinical trial registries are important databases for managing the trial enterprise. A government ought to be able to find out which clinical studies have been conducted in the nation. This would make it possible to monitor specific biomedical advancements, including whether they have domestic or foreign origins, the number of trialists in the nation and how active they are individually, the areas in which trials are being conducted, the institutions sponsoring the trials, their locations, and so forth.

In this work we have determined what steps it takes to identify all trials run in India, that are registered with EU CTR.

We used two techniques-
 1) First technique:
    a) It involved downloading the records from the EU CTR registry using a R-script.
    b) Web-scraped all the necessary fields (R programming using functions 'rvest','tidyverse')
    c) Saved the retrieved information in a local SQLite database.
    d) The information retrieved after web-scraping were queried for the keyword 'India' or 'CTRI'
    e) MS-Excel functions were used to filter the data and providing the results.
2) Second technique:
    a) It involved doing a simple basic search for the keyword 'India' in the EU CTR registry.

3) Compared the results of both the techniques.

We found inconsistencies in the outcomes by comparing both the techniques. Although there weren't many discrepant records, it's nevertheless necessary to be aware of the issue and to take it into consideration when conducting meta-studies on trials in the future.

This folder includes R script file for downloading the records and web-scraping the required data.



