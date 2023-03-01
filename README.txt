STATA

1) The "analysis.do" contains all the codes required to reproduce all tables, figures, and statistical tests reported in both the main text and online appendix.

2) Make sure to change the directory at the beginning of the .DO file.

3) Datasets required:
"Data/cleaned-recip.dta"
"Data/cleaned-dict.dta"
"Data/anes_timeseries_2020_stata_20220210"
"Data/survey_responses"

4) STATA Version 17.0


OTREE

1) Folder contains the full oTree program used for i) recipient sessions (Informed-Choice and Uninformed-Choice treatments) and ii) decision maker sessions (Pride vs Non-pride treatments).

2) The informed_consent app requires a pdf of an informed consent form to be displayed. The app will run without it, but you will see an error in the pdf display window. The pdf should be saved in some folder in _static and the consent variable in the relevant session config should be set to '[app_folder]/[filename].pdf' where app_folder is the folder in _static and filename is the name of the consent form pdf file.

3) Version requirements:
- oTree: 2.5.8 (has not been tested extensively with oTree 3.x; likely will not perform well)
- python: 3.7 (otree 2.5.8 will not work with more recent versions of python)
- otreeutils: 0.9.2 (the code requires that otreeutils be installed - run 'pip3 install otreeutils' in the command line)
