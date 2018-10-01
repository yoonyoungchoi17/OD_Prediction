This github project is used for research about OD prediction using the PCA and Markov chain method.

Members are Yoon-Young Choi(YY), Yoonseok Oh (YS), Koohong Chung (KH), and Seungmo Kang (SM) from Korea University

Files consist as followings. 
dunsan_trip_38.csv  :  OD table of three days, origin is dunsan-dong in Daejeon
dunsan_38.R  :  code file of R to analyze 'dunsan_trip_38.csv' by PCA method (e.g., projection, approximation)

######################################################
### Data description of "dunsan_trip_38.csv"
### Written by Yoonseok Oh. (ysoh0223@korea.ac.kr)
### Last updated: 2018-09-29
#######################################################
*Explain "aggregated_dunsan_trip_table2.csv"
- Can't read the Korean font.  Can you explain what the columns represent?
- In dusan_od_analysis_180811.r, assume all the files are located in the same folder.
- a.day.list is list of the files in the folder and line 20 of dusan_od_analysis_18011.r is treating it as .rds file.  Can you explain what the purpose of this?



* Column info of the "dunsan_trip_38.csv" file 
1: day - date the data had been recorded, YYMMDD format
2: t.index - 15-minutes time index of a day from 00:00 to 23:59. 
From 3rd column to the end: Amount of trips between Dunsan-dong in Deajeon (ID-30170112) to the other zones in the Deajeon-city area.
                            Headers of the 3rd column to the end mean the destinations ID
                            (Need to add description of zone ID)

* How can we get the "dunsan_trip_38.csv" file?
- It came from the "aggregated_dunsan_trip_table2.csv" file. We can make it by running the "dunsan_od_analysis_180811.r". All of the input-files are included in this repository.
  > input files list.
    * folder named 'dunsan_rds' which contains .rds files (attached)
    * "daejeon_dong_list_180811.csv" file which contains legal-dong information of Daejeon.
    * locating these input files in the working directory of R. 
- After getting the "aggregated_dunsan_trip_table2.csv" file, It is reorganized by eliminating the zero-flow destinations.

* Why it has only 38 destinations of 177 legal dong?
- On 17-01-02 (the initial dates of the file list), the trip from the Dunsan-dong had 38 destinations. (Daejeon has 177 legal-dong)

################## End of data description ############


