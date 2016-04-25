# MIMIC Patient Viewer

This is a visualization tool that allows users to select and view, in a convenient format, patient information of trauma patients in the MIMIC III database. All code is written in Shiny.

## Getting the Data

There are two dataframes this code relies on. The first is the trPatients.Rdata dataframe that includes basic injury and demographic information from each of the trauma patients in MIMIC. The second is trEvents.Rdata, which includes all the events i.e. measurements for each patient. For now, I will store these files on Dropbox and share them with whoever is working on this project with me. The Shiny code that accesses these dataframes refers to them from the location "~/Dropbox/mimicPatientViewData", so you must store the data at that location on your computer or change the location in the code.


