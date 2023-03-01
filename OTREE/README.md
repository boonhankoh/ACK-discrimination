# Aksoy, Chadd, and Koh (2023)
## Code for "Sexual Identity, Gender, and Anticipated Discrimination in a Prosocial Domain"
This repo contains the oTree code for i) Recipient sessions (Informed-Choice and Uninformed-Choice treatments) and ii) Dictator sessions (Pride vs Non-pride treatments).

## Notes:
- the informed_consent app requires a pdf of an informed consent form to be displayed. The app will run without it, but you will see an error in the pdf display window. The pdf should be saved in some folder in _static and the consent variable in the relevant session config should be set to '[app_folder]/[filename].pdf' where app_folder is the folder in _static and filename is the name of the consent form pdf file.

## Version requirements
- oTree: 2.5.8 (has not been tested extensively with oTree 3.x; likely will not perform well)
- python: 3.7 (otree 2.5.8 will not work with more recent versions of python)
- otreeutils: 0.9.2 (the code requires that otreeutils be installed - run 'pip3 install otreeutils' in the command line)
