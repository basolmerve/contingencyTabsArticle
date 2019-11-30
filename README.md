## GENERAL INFORMATION
This is README.txt of the titled as "The Performance Comparison of Independence Tests in Two-Way Contingency Tables". This is a joint work by  Ebru Ozturk, Merve Basol and Sevilay Karahan. The codes are written by Ebru Ozturk and Merve Basol.

Please do not hesitate to contact if you have any questions, comments or remarks on the codes. The mail addresses for contact are ebru.ozturk3@hacettepe.edu.tr and basolmerve21@gmail.com

## THE DETAILS ON PROGRAMME
The RStudio version 1.1.442. The packages that we used are:
  - "rTableICC" version 1.0.7
  - "stats" version 3.6.1
  - "vcd"version 1.4-4

## EXECUTION of CODES
There are three different files to run codes.
  - simulation_codes_for_5x5table.R file contais the simualtion codes for 5x5 table.
  - simulation_codes_for_5x2table.R file contais the simualtion codes for 5x2 table.
  - real_datasets_and_results.R file contains the tables of real data sets and its results. 

For execution:
  1. Start with simulation_codes_for_5x5table.R file. If you installed three packages above-mentioned, you can select and run all codes freely. The results are stored in two different matrices labelled as "results_power_5x5" and "results_alpha_5x5".
  2. Apply same procedure simulation_codes_for_5x2table.R file. The results are stored in two different matrices labelled as "results_power_5x2" and "results_alpha_5x2".
  3. The real data sets are introduced in "results_real_data.R" file as contingency tables. The names of the contingency tables for real data sets in the "results_real_data.R" are "ex.1" and "ex.2". The results of the real data set are presented om matrix "results_real_data".
