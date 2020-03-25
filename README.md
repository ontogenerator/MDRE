# TDRE
Two-dimensional reward evaluation in mice

The folder "analysis/data/raw" contains csv files with raw data from full experimental runs of groups of mice inside a home cage with four water feeders.
The files were produced by control software PhenoSoft Control v.1.0.5163.31723 (PhenoSys, GmbH).
The folder "analysis/data/metadata" contains input csv files ("mastercolnames", "MasterTable", "Conditions", and "vol_measurements") necessary for the R scripts that analyze the raw data as well as the csv files produced as output from other scripts.  
The folder "analysis/R" contains R scripts, including a script for reading and aggregating csv files from PhenSoft Control into a single data frame (load.R) and a script for performing the simulations (simulations.R). The output from these scripts are processed csv files, saved in the folder "analysis/data". The output from load.R is "ChoicesOutput.csv" and the remaining files are outputs from the simulations.R script (simulations.csv and files with sensitivity_xxx.csv naming scheme).   

## 1. Content of raw files

|Column label |Type     |Information |
|-------------|---------|------------|
|DateTime     |-        |The astronomical date and time for each event of the experiment. Missing days due to experimental malfunctions, see below|
|IdRFID       |-        |RFID number of mouse|
|IdLabel			|-        |Short unique mouse identifier|
|unitLabel		|-        |Code of the unit that was activated, with numbers corresponding to the corners of the cage|
|             |LS       |Detections of IR-beam interruptions without a trasnponder number                          detection|
|             |Reader   |Detections of transponder numbers without IR-beam interruptions|
|             |CondMod  |Detections of both a transponder number and an IR-beam                                    interruption (identified nose pokes)|
|             |pumpBeh, Empty, Full| Events relating to states of the syringe and its refilling algorithm|
|             |VersuchCS|Events related to the main program, clarified in **SystemMsg**|
|             |exp      |Events related to the programmed reward schedule, clarified in **SystemMsg**|
|eventDuration|	        |Duration of event in milliseconds|
|sense1duration|        |Total duration of the IR-beam interruption|
|sense1Events |	        |Number of interruptions of IR beam. When such events happen fast enough (less than 200ms apart) these are registered as a single event, but the number of such short interruptions is given here|
|senseRFIDrecords|    	|Number of times the transponder number was detected; irrelevant for this experiment|
|reinforce1value|		    |Reward (in pump step units, delivered by a stepping motor syringe pump filled with water)|
|reinforce1Total|	      |Contents of this column are irrelevant for this experiment|
|reinforce1Account|	    |Contents of this column are irrelevant for this experiment|
|outFuncLabel |  		    |Contents of this column are irrelevant for this experiment|
|outLabel     |         |Contents of this column are irrelevant for this experiment|
|SystemMsg    |         |System state messages, further clarified in the **unitLabel** column and indicating the beginning or end of, e.g. a preprogrammed reward schedule (no rewards delivered before start and after end of schedule) and beginning or end of syringe refill event (no rewards delivered while refilling)|
|MsgValue1    |         |Name of parameter file started with the control software|
|MsgValue2    |      		|Contents of this column are irrelevant for this experiment|
|MsgValue3    |         |Contents of this column are irrelevant for this experiment|


## 2. Content of "ChoicesOutput.csv" file
This file is the output of the load.R script which processes the folder of raw csv files, with further information supplied by "Conditions", "MasterTable", and "mastercolnames" csv files.

|Column label |	Information|
|-------------|------------|
|day          |Experimental day (first true experimental day is 0, negative numbers for training and pre-tests)|
|IdLabel		  |Short unique mouse identifier|
|loc          |Dispenser location, given as corner number clockwise from 1 to 4|
|DateTime	    |Astronomical date and time for each event of the experiment. Missing days due to experimental malfunctions, see below|
|eventDuration|	Duration of event in milliseconds|
|SystemMsg	  |Rewards that were supposed to be delivered but could not be delivered because the system was in a "busy" state are marked as "busy"|
|vol      	  |Volume (in microliters) of delivered reward|
|rewardstatus |1 if a reward was delivered, 0 otherwise|
|prob         |long-term probability that a reward will be delivered for the particular dispenser location and mouse|
|volm	        |Maximal reward volume (rewards can be either this value or 0) for a particular dispenser location and mouse|
|rel		      |1 if rewarding dispenser, 0 otherwise|
|prof         |1 if higher profitability dispenser, 0 for the lower profitability dispenser and NA for the nonrewarding dispensers. For pairs of options of equal profitability in the incongruent (I) condition, 1 denotes the higher volume option and 0, the higher probability option. In the "explore" and "training" conditions all |
|cohort       |Cohort number|
|cond         |Experimental condition. Before the main experiment, mice were in "explore" then "training" condition. In these conditions all four dispensers were rewarding with the same probabilities and volumes, but a **prof** of 1 was randomly assigned to two dispensers and a **prof** of 0 to the other two dispensers|
|experiment   |Experiment number|

## 3. Content of "simulations.csv" file
This file is generated by the simulations.R script and contains the main simulations in which 
different decision models are tested under the same experimental conditions from experiments 1-4.

|Column label |	Information |
|-------------|-------------|
|ind          |ID of the simulated individual|
|experiment   |Experiment number|
|cond         |Experimental condition|
|model        |Abbreviated name of the model|
|performance  |Discrimination performance over 1000 visits, relative choice for higher volume (in **I** condition) or higher profitability option (all other conditions)|

## 4. Content of "sensitivity_xxx" csv files
These files are generated by the simulations.R script and contain the sensitivity analyses data used to select the best free parameter values for each model.

|Column label |	Information |
|-------------|-------------|
|par          |value of other free parameter: threshold in pfirst and vfirst models and theta_v for rnonc model|
|cond         |Experimental condition|
|gamma        |Value of gamma parameter|
|performance  |Discrimination performance over 1000 visits, relative choice for higher probability option|
|pred_perf    |Predicted performance from loess model over all gamma values|
|obs_perf     |Observed performance, median over all experimental mice for the baseline conditions "BPV1" and "BPV2"|
|deviance     |(pred_perf - obs_perf)^2|

## 5. Content of "Conditions.csv" file
This file is user-generated, providing relevant experimental information not present in the raw files.

|Column label|	Information|
|------------|-------------|
|day         |Experimental day (first true experimental day is 0, negative numbers for training and pre-tests)|
|IdLabel		 |Short unique mouse identifier|
|loc         |Dispenser location, given as corner number clockwise from 1 to 4|
|prob        |long-term probability that a reward will be delivered for the particular dispenser location and mouse|
|volm	       |Maximal reward volume (rewards can be either this value or 0) for a particular dispenser location and mouse|
|rel		     |1 if rewarding dispenser, 0 otherwise|
|prof        |1 if higher profitability dispenser, 0 for the lower profitability dispenser and NA for the nonrewarding dispensers. For pairs of options of equal profitability in the incongruent (I) condition, 1 denotes the higher volume option and 0, the higher probability option|
|discard     |1 if data for particular day and mouse is to be discarded from analysis, 0 otherwise|
|cohort      |Cohort number|
|cond        |Experimental condition. Before the main experiment, mice were in "explore" then "training" condition. In these conditions all four dispensers were rewarding with the same probabilities and volumes, but a **prof** of 1 was randomly assigned to two dispensers and a **prof** of 0 to the other two dispensers|
|experiment  |Experiment number|

## 6. Content of "MasterTable.csv" file
This file is user-generated and allows mapping the raw csv files to the respective experimental days.


|Column label|	Information|
|------------|-------------|
|day         |Experimental day (first true experimental day is 0, negative numbers for training and pre-tests)|
|path        |Path of the raw csv file corresponding to the day|
|comments	   |When days are omitted a clarification is given here|

## 7. Content of "vol_measurements.csv" file


|Column label |	Information|
|-------------|-------------|
|cage         |Cage number (1 or 2)|
|loc          |Dispenser location, given as corner number clockwise from 1 to 4|
|steps        |Number of steps that the stepping motor was set to|
|vol          |Measured volume [microLiters] with a graduated pipette (resolution of 1 microLiter)|
|ulstep       |vol/steps, microLiters per step|

For further information contact: vladislav.nachev@gmail.com
