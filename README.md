# Estimating methane emission durations using continuous monitoring systems (CMS)

Repository contains code used to estimate methane emission durations using concentration observations from a network of point-in-space continuous monitoring systems. The main body of code is contained in the "MAIN_1_estimate_durations.R" file found in the "code" directory. The "HELPER" file contains functions used by the "MAIN" script. The "MAIN_2_analyze_controlled_release_results" file makes all of the plots for the accompanying manuscript related to the controlled release evaluations. The "MAIN_3_analyze_case_study_results" file makes all of the plots for the accompanying manuscript related to the real data case study. 

The "MAIN_1_estimate_durations" file takes simulated concentration data and concentration observations as input. The file format must match the output of the "MAIN_1_simulate.R" file from the following repository: https://github.com/wsdaniels/DLQ. Example input files for the case study and METEC evaluation are provided in the "input_data" directory.

The "output_data" directory is where output from the MAIN_1 script is saved. Output files have been pre-generated and are saved in this directory.

The accompanying paper can be found here: https://doi.org/10.1021/acs.estlett.4c00687