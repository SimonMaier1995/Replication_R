 
global allwhite "plotregion(fcolor(white)) graphregion(color(white)) bgcolor(white)"

/* Dictionary of Variables in raw data set (INPUT): mainData.csv. 
batch:             identifier of each experimental batch
day:      "Initial" Pre-screening survey, "Day 1" 1st day decisions on so on up to "Day 4"
participantid:           random id used to link participant's choices across days
treatment:       Low Debt, High Debt, No Debt
initial endowment:      number of points for each allocation decisions
pointssaving*:			points allocated to each savings account
pointsdebt*:			points allocated to each debt account for treatments with debt
borrowdebt*:			amount of points borrowed from a debt account
pointsthisdecision:		additional points to be allocated in this decision, based on the additional questions
pointsnextdecision:     additional points to be allocated in the next decision, based on the additional questions
additionalpointssaving*:  how many additional points are allocated to each savings accounts
additionalpointsdebt*:  how many additional points are allocated to each debt accounts
points_ia_a*:			points allocated in the initial allocation decisions to each account, numbered 1-4
points_aa_a*:			points allocated in the additional allocation decisions to each account, numbered 1-4
balance_ia_a*:			balance of each account after the initial allocation of points
balance_aa_a*:			balance of each account after the additional allocation of points
orderlottery:			keeps track of the order in which the lottery question was asked in the additional questions
lottery*choice:			answer to the additional lottery question #*, goes from 1-4
time*choice:			answer to the additional time question #*, goes from 1-2
oneshotcontrol_s*:		points allocated to each account in the one-shot no debt questions, goes from 1-4
oneshotdebt_s*:			points allocated to each account in the one-shot low debt questions, goes from 1-4
oneshothigh_s*:			points allocated to each account in the one-shot high debt questions, goes from 1-4
reasoning:				open-ended questions where participants describe their strategy in their own words
end_debtimportance:		answers to how important was debt in making your decisions
end_balanceimportance:	answers to how important were account balances in making your decisions
end_rateimportance:		answers to how important were account interest in making your decisions
end_hindsight:			open-ended questions about what strategy will you use if you were to participate again
demo_age:				subject's age.
demo_sex:            	gender (Female, Male or Prefer not to tell)
demo_state:         	state of residence
demo_race:      		options are Black/Asian/Hispanic/Native/White/Multiple
demo_education:     	Highest level of education achieved. Options are Associate's degree, Bachelor's degree, PhD, High School, Master's, Professional Degree (MD, JD, etc.), Some College.
demo_collegeplus:		if education is college or more, or non college graduate
demo_studentloan:		if participant had a student loan, answers are Yes, No, or Prefer not to answer
demo_holddebt:			if participant currently has any type of debt, answers are Yes, No, or Prefer not to answer
demo_covid:				how was the impact of COVID in your live, answers are None, A moderate amount, a lot, a little, and a great deal
errorbdm*:				mistake dummy for the understanding question #* in the bdm instructions, goes from 1-3
errordebtintro*:		mistake dummy for the understanding question #* in the debt intro instructions, goes from 1-5
errordebtendowment*:    mistake dummy for the understanding question #* in the debt endowment instructions, goes from 1-8
errordebtpayment*:		mistake dummy for the understanding question #* in the debt payment instructions, goes from 1-3
errorend*:				mistake dummy for the understanding question #* in the debt end instructions, goes from 1-2
error_count:			counter of errors in the understanding questions
error_initial:			counter of errors in the understanding intro questions
error_main:				counter of errors in the understanding main questions
error_end:				counter of errors in the understanding end questions
error_end2:				classified open-ended question, where any part of the instructions confusing?
error_main_total:		counter of errors in the understanding main questions over the 4 days
error_count_total:		counter of errors in all the understanding questions over the 4 days
debt1bal:				balance of debt1 account
debt2bal:				balance of debt2 account
debt1percpaid:			percentage of debt1 paid
debt2percpaid:			percentage of debt2 paid
controlsaving*balanceupdated:   balance of account * after first initial allocation in control group
debtsaving*balanceupdated:   	balance of account * after first initial allocation in debt groups
controlsaving*balanceupdated2:   balance of account * after additional allocation in control group
debtsaving*balanceupdated2:   	balance of account * after additional allocation in debt groups
lotterychoice:			choice in the additional lottery question, ranges from 0-1
timechoice:			 	choice in the additional time question, ranges from 0-1
durationinseconds:		length in seconds
controltreatment:		dummy control group
debttreatment:			dummy low debt group
hightreatment:			dummy high debt group
q1967:					open-ended question, where any part of the instructions confusing?
order_oneshotcontrol:	in which order did the oneshot control question appear? Values are either 1,2, or 3
order_oneshotdebt: 		in which order did the oneshot debt question appear? Values are either 1,2, or 3
order_oneshothighdebt:  in which order did the oneshot high debt question appear? Values are either 1,2, or 3
io:						initial endowment + any additional points
perco_orig:				share of initial allocation devoted to account with max. returns
perco_adtl:				share of additonal allocation devoted to account with max. returns
perco_os_c: 			share of points devoted to account with max. returns in oneshot control
perco_os_d: 			share of points devoted to account with max. returns in oneshot debt
perco_os_h:				share of points devoted to account with max. returns in oneshot high debt
ind_optimal_ia:			dummy that equals 1 if share optimal is equal to 1 in initial allocation
ind_optimal_ap:			dummy that equals 1 if share optimal is equal to 1 in additional allocation
ind_optimal_ia_all:		dummy that equals 1 if share optimal is equal to 1 in all initial allocations
ind_optimal_ap_all:     dummy that equals 1 if share optimal is equal to 1 in all additional allocations
share_pointssaving*:    share of points allocated to savings account *, based on initial endowment, goes from 1-4
share_pointsdebt*:		share of points allocated to debt account *, based on initial endowment, goes from 1-2
nz_pointssaving*:       dummy equal to 1 if non-zero points allocated to savings* account, goes from 1-4
nz_pointsdebt*:       	dummy equal to 1 if non-zero points allocated to debt* account, goes from 1-4
type_num_acnt:			number of accounts with non-zero points allocated
debt1paidoff:			dummy equal to 1 if debt1 balance is zero or positive
debt2paidoff:			dummy equal to 1 if debt2 balance is zero or positive
perc_points_ia_a*:		percentage of points allocated to account number * baed on initial endowment
prev_balance_ia_a*:		previous balance in account *, used to keep track of previous decisions in the survey
balance_max:			maximum balance
balance_min:			minimum balance 
		
*/

/* Dictionary of Variables in raw data set (INPUT): BurdenDebt_RobustnessTreatment.xlsx 

durationinseconds:		length in seconds
idturk:             	identifier of each participant
demo_sex:            	gender (Female, Male or Prefer not to tell
demo_age:				subject's age
demo_state:         	state of residence
demo_race:      		options are Black/Asian/Hispanic/Native/White/Multiple
demo_education:     	Highest level of education achieved. Options are Associate's degree, Bachelor's degree, PhD, High School, Master's, Professional Degree (MD, JD, etc.), Some College.
demo_studentloan:		if participant had a student loan, answers are Yes, No, or Prefer not to answer
demo_holddebt:			if participant currently has any type of debt, answers are Yes, No, or Prefer not to answer
demo_covid:				how was the impact of COVID in your live, answers are None, A moderate amount, a lot, a little, and a great deal

initialEndowment*:      number of points for the allocation decision number *, goes from 1-4
debtSaving1Balance:		initial balance of Savings 1 account
debtSaving1BalanceUpdated: 		balance of Savings 1 after first allocation decision
debtSaving1BalanceUpdated2:		balance of Savings 1 after second allocation decision
debtSaving1BalanceUpdated3: 	balance of Savings 1 after third allocation decision
debtSaving1BalanceUpdated4: 	balance of Savings 1 after last allocation decision

debtSaving2Balance:		initial balance of Savings 2 account
debtSaving2BalanceUpdated: 		balance of Savings 2 after first allocation decision
debtSaving2BalanceUpdated2:		balance of Savings 2 after second allocation decision
debtSaving2BalanceUpdated3: 	balance of Savings 2 after third allocation decision
debtSaving2BalanceUpdated4: 	balance of Savings 2 after last allocation decision

debtSaving3Balance:		initial balance of Savings 3 account
debtSaving4Balance:		initial balance of Savings 4 account

debtDebt1Balance:		initial balance of Debt 1 account
debtDebt1BalanceUpdated: 		balance of Debt 1 after first allocation decision
debtDebt1BalanceUpdated2:		balance of Debt 1 after second allocation decision
debtDebt1BalanceUpdated3: 		balance of Debt 1 after third allocation decision
debtDebt1BalanceUpdated4: 		balance of Debt 1 after last allocation decision

debtDebt2Balance:		initial balance of Debt 2 account
debtDebt2BalanceUpdated: 		balance of Debt 2 after first allocation decision
debtDebt2BalanceUpdated2:		balance of Debt 2 after second allocation decision
debtDebt2BalanceUpdated3: 		balance of Debt 2 after third allocation decision
debtDebt2BalanceUpdated4: 		balance of Debt 2 after last allocation decision

counter
errordebtintro*:		mistake dummy for the understanding question #* in the debt intro instructions, goes from 1-5
errordebtendowment*:    mistake dummy for the understanding question #* in the debt endowment instructions, goes from 1-8
errordebtpayment*:		mistake dummy for the understanding question #* in the debt payment instructions, goes from 1-3
errorend*:				mistake dummy for the understanding question #* in the debt end instructions, goes from 1-2
robustnessTreatment:	equals to 1 for all observations in this data set, treatment dummy

pointsSaving1:			points allocated to Savings 1 account in the first allocation decision
pointsSaving2:			points allocated to Savings 2 account in the first allocation decision
pointsDebt1:			points allocated to Debt 1 account in the first allocation decision
pointsDebt2:			points allocated to Debt 2 account in the first allocation decision

*/

/* Dictionary of Variables in raw data set (INPUT): hindsight_main.dta. 
idturk:             	identifier of each participant
perco_orig*:			share of initial allocation devoted to account with max. returns for day*, 4 days in total
treatment:       		Debt, High Debt, Control
end_hindsight:			open-ended questions about what strategy will you use if you were to participate again

*****Participants can be classified in 5 types
beginning				optimal since the beginning
learning				not optimal at the start but they mention the optimal strategy later on
debtthenoptimal			mention strategy of paying first debt and then maximizing returns
suboptimal				describe a suboptimal strategy different from any of the previous
unclassifed				none of the previous categories
sum						auxiliar variable to check that every participant is classified only in one group
debtmentioned			dummy equal to 1 if the word debt is mentioned in the description	
	
batch3:					participant id, for batch3 there was an error mistake and it says batch3 instead
	
*/

/* Dictionary of Variables in raw data set (INPUT): redistributionData.csv. 

idturk:             	identifier of each participant
day:      "0" Pre-screening survey, "1" 1st day decisions on so on up to "4"
treatment:       Low Debt (Redistribution Debt), No Debt (Redistribution No Debt)
batch:             identifier of each experimental batch
durationinseconds:		length in seconds
initialendowment:      		  number of points for each allocation decisions
initialendowmentupdated:      number of points to allocate after redistribution choice
borrowsavings1:				  number of points borrowed from savings 1
borrowsavings2:				  number of points borrowed from savings 2
pointssaving*:			points allocated to each savings account
pointsdebt*:			points allocated to each debt account for treatments with debt
balance_a*:				balance of each account after the initial allocation of points, numbered 1-6
errorbdm*:				mistake dummy for the understanding question #* in the bdm instructions, goes from 1-3
errordebtintro*:		mistake dummy for the understanding question #* in the debt intro instructions, goes from 1-5
errordebtendowment*:    mistake dummy for the understanding question #* in the debt endowment instructions, goes from 1-8
errordebtpayment*:		mistake dummy for the understanding question #* in the debt payment instructions, goes from 1-3
error_count:			counter of errors in the understanding questions
error_initial:			counter of errors in the understanding intro questions
error_main:				counter of errors in the understanding main questions
error_main_total:		counter of errors in the understanding main questions over the 4 days
error_count_total:		counter of errors in all the understanding questions over the 4 days
instruction_confusion:  answer to where any part of the instructions confusing?
demo_sex:            	gender (Female, Male or Prefer not to tell)
demo_age:				subject's age.
demo_state:         	state of residence
demo_race:      		options are Black/Asian/Hispanic/Native/White/Multiple
demo_educ:     	Highest level of education achieved. Options are Associate's degree, Bachelor's degree, PhD, High School, Master's, Professional Degree (MD, JD, etc.), Some College.
demo_collegeplus:		if education is college or more, or non college graduate
demo_studentloan:		if participant had a student loan, answers are Yes, No, or Prefer not to answer
demo_holddebt:			if participant currently has any type of debt, answers are Yes, No, or Prefer not to answer
demo_covid:				how was the impact of COVID in your live, answers are None, A moderate amount, a lot, a little, and a great deal
end_reasoning:			open-ended questions where participants describe their strategy in their own words
end_debtimportance:		answers to how important was debt in making your decisions
end_balanceimportance:	answers to how important were account balances in making your decisions
end_rateimportance:		answers to how important were account interest in making your decisions
end_hindsight:			open-ended questions about what strategy will you use if you were to participate again
orderlottery:			keeps track of the order in which the lottery question was asked in the additional questions
lottery*choice:			answer to the additional lottery question #*, goes from 1-4
time*choice:			answer to the additional time question #*, goes from 1-2
perco_orig:				share of initial allocation devoted to account with max. returns
ind_optimal_ia:			dummy that equals 1 if share optimal is equal to 1 in initial allocation
pointssaving*:			points allocated to each savings account
borrowdebt*:			amount of points borrowed from a debt account
pointsthisdecision:		additional points to be allocated in this decision, based on the additional questions
pointsnextdecision:     additional points to be allocated in the next decision, based on the additional questions
additionalpointssaving*:  how many additional points are allocated to each savings accounts
additionalpointsdebt*:  how many additional points are allocated to each debt accounts
points_ia_a*:			points allocated in the initial allocation decisions to each account, numbered 1-4
points_aa_a*:			points allocated in the additional allocation decisions to each account, numbered 1-4
balance_ia_a*:			balance of each account after the initial allocation of points
balance_aa_a*:			balance of each account after the additional allocation of points
ind_optimal_ia_all:		dummy that equals 1 if share optimal is equal to 1 in all initial allocations
debtsaving1balanceupdated2:  savings 1 balance updated after borrowing decision in debt groups
debtsaving2balanceupdated2:  savings 2 balance updated after borrowing decision in debt groups
debtsaving3balance:			 savings 3 balance in debt groups
debtsaving4balance: 		 savings 4 balance in debt groups
debtdebt1balanceupdated:     balance of debt 1 account after allocation in debt groups
debtdebt2balanceupdated:     balance of debt 2 account after allocation in debt groups
controlsaving1balanceupdated2:   balance of Savings 1 after allocation in control group
controlsaving2balanceupdated2:   balance of Savings 2 after allocation in control group
controlsaving3balanceupdated: 	 balance of Savings 3 after allocation in control group
controlsaving4balanceupdated: 	 balance of Savings 4 after allocation in control group
controlsaving5balance: 			 balance of Savings 5 account in control group
controlsaving6balance: 			 balance of Savings 6 account in control group
i:								 participant identifier
*/

/* Dictionary of Variables in raw data set (INPUT): borrowingData.csv. 

idturk:             	identifier of each participant
day:      "0" Pre-screening survey, "1" 1st day decisions on so on up to "4"
treatment:       Low Debt (Redistribution Debt), No Debt (Redistribution No Debt)
batch:             identifier of each experimental batch
durationinseconds:		length in seconds
initialendowment:      		  number of points for each allocation decisions
initialendowmentupdated:      number of points to allocate after redistribution choice
borrowsavings1:				  number of points borrowed from savings 1
borrowsavings2:				  number of points borrowed from savings 2
pointssaving*:			points allocated to each savings account
pointsdebt*:			points allocated to each debt account for treatments with debt
balance_a*:				balance of each account after the initial allocation of points, numbered 1-6
errorbdm*:				mistake dummy for the understanding question #* in the bdm instructions, goes from 1-3
errordebtintro*:		mistake dummy for the understanding question #* in the debt intro instructions, goes from 1-5
errordebtendowment*:    mistake dummy for the understanding question #* in the debt endowment instructions, goes from 1-8
errordebtpayment*:		mistake dummy for the understanding question #* in the debt payment instructions, goes from 1-3
error_count:			counter of errors in the understanding questions
error_initial:			counter of errors in the understanding intro questions
error_main:				counter of errors in the understanding main questions
error_main_total:		counter of errors in the understanding main questions over the 4 days
error_count_total:		counter of errors in all the understanding questions over the 4 days
instruction_confusion:  answer to where any part of the instructions confusing?
demo_sex:            	gender (Female, Male or Prefer not to tell)
demo_age:				subject's age.
demo_state:         	state of residence
demo_race:      		options are Black/Asian/Hispanic/Native/White/Multiple
demo_educ:     	Highest level of education achieved. Options are Associate's degree, Bachelor's degree, PhD, High School, Master's, Professional Degree (MD, JD, etc.), Some College.
demo_collegeplus:		if education is college or more, or non college graduate
demo_studentloan:		if participant had a student loan, answers are Yes, No, or Prefer not to answer
demo_holddebt:			if participant currently has any type of debt, answers are Yes, No, or Prefer not to answer
demo_covid:				how was the impact of COVID in your live, answers are None, A moderate amount, a lot, a little, and a great deal
end_reasoning:			open-ended questions where participants describe their strategy in their own words
end_debtimportance:		answers to how important was debt in making your decisions
end_balanceimportance:	answers to how important were account balances in making your decisions
end_rateimportance:		answers to how important were account interest in making your decisions
end_hindsight:			open-ended questions about what strategy will you use if you were to participate again
orderlottery:			keeps track of the order in which the lottery question was asked in the additional questions
lottery*choice:			answer to the additional lottery question #*, goes from 1-4
time*choice:			answer to the additional time question #*, goes from 1-2
perco_orig:				share of initial allocation devoted to account with max. returns
ind_optimal_ia:			dummy that equals 1 if share optimal is equal to 1 in initial allocation
ind_optimal_ia_all:		dummy that equals 1 if share optimal is equal to 1 in all allocation
debtsaving1balanceupdated2:  savings 1 balance updated after allocation decision in debt groups
debtsaving2balanceupdated2:  savings 2 balance updated after allocation decision in debt groups
debtsaving3balance:			 savings 3 balance in debt groups
debtsaving4balance: 		 savings 4 balance in debt groups
debtdebt1balanceupdated:     balance of debt 1 account after borrowing decision in debt groups
debtdebt2balanceupdated:     balance of debt 2 account after borrowing decision in debt groups
controlsaving1balanceupdated2:   balance of Savings 1 after allocation in control group
controlsaving2balanceupdated2:   balance of Savings 2 after allocation in control group
controlsaving3balanceupdated: 	 balance of Savings 3 after allocation in control group
controlsaving4balanceupdated: 	 balance of Savings 4 after allocation in control group
controlsaving5balance: 			 balance of Savings 5 account in control group
controlsaving6balance: 			 balance of Savings 6 account in control group
i:								 participant identifier
*/

******************
**# Prepare Data - Main Treatments
******************

{
insheet using "data/mainData.csv", clear

* fix day variable
rename day dayAux
gen day=0
replace day=1 if dayAux=="Day 1"
replace day=2 if dayAux=="Day 2"
replace day=3 if dayAux=="Day 3"
replace day=4 if dayAux=="Day 4"
drop dayAux

* fix treatment variable
rename treatment treatmentAux
gen treatment=0 if treatmentAux=="No Debt"
replace treatment=1 if treatmentAux=="Low Debt"
replace treatment=2 if treatmentAux=="High Debt"
drop treatmentAux
destring treatment, replace

* fix demographic characteristics
gen demo_white = (demo_race == "White")

rename demo_sex demo_sex_t
gen demo_sex = (demo_sex_t == "Male")
drop demo_sex_t

** demographics and exit variables
sum demo_age, detail
gen demo_age_median = .
replace demo_age_median = 1 if demo_age >= r(p50) & demo_age != .
replace demo_age_median = 0 if demo_age < r(p50) & demo_age != .

rename demo_collegeplus demo_collegeplus_t
gen demo_collegeplus = (demo_collegeplus_t == "College or more")
drop demo_collegeplus_t

rename demo_studentloan demo_studentloan_t
gen demo_studentloan = (demo_studentloan_t == "Yes")
drop demo_studentloan_t

rename demo_holddebt demo_holddebt_t
gen demo_holddebt = (demo_holddebt_t == "Yes")
drop demo_holddebt_t

rename demo_covid demo_covid_t
gen demo_covid=0
replace demo_covid=1 if demo_covid_t=="A little"
replace demo_covid=2 if demo_covid_t=="A moderate amount"
replace demo_covid=3 if demo_covid_t=="A lot"
replace demo_covid=5 if demo_covid_t=="A great deal"
drop demo_covid_t

save "data/main_work.dta", replace

}

******************
**# Prepare Data - Redistribution Treatments
******************

{
insheet using "data/redistributionData.csv", clear

* fix treatment variable
rename treatment treatmentAux
gen treatment=0 if treatmentAux=="No Debt"
replace treatment=1 if treatmentAux=="Low Debt"
drop treatmentAux
destring treatment, replace

* fix demographic characteristics
gen demo_white = (demo_race == "White")

rename demo_sex demo_sex_t
gen demo_sex = (demo_sex_t == "Male")
drop demo_sex_t

** demographics and exit variables
sum demo_age, detail
gen demo_age_median = .
replace demo_age_median = 1 if demo_age >= r(p50) & demo_age != .
replace demo_age_median = 0 if demo_age < r(p50) & demo_age != .

rename demo_collegeplus demo_collegeplus_t
gen demo_collegeplus = (demo_collegeplus_t == "College or more")
drop demo_collegeplus_t

rename demo_studentloan demo_studentloan_t
gen demo_studentloan = (demo_studentloan_t == "Yes")
drop demo_studentloan_t

rename demo_holddebt demo_holddebt_t
gen demo_holddebt = (demo_holddebt_t == "Yes")
drop demo_holddebt_t

rename demo_covid demo_covid_t
gen demo_covid=0
replace demo_covid=1 if demo_covid_t=="A little"
replace demo_covid=2 if demo_covid_t=="A moderate amount"
replace demo_covid=3 if demo_covid_t=="A lot"
replace demo_covid=5 if demo_covid_t=="A great deal"
drop demo_covid_t

save "data/redistribution_work.dta", replace


}

******************
**# Prepare Data - Borrowing Treatments
******************

{
insheet using "data/borrowingData.csv", clear

* fix day variable
rename day dayAux
gen day=0
replace day=1 if dayAux=="Day 1"
replace day=2 if dayAux=="Day 2"
replace day=3 if dayAux=="Day 3"
replace day=4 if dayAux=="Day 4"
drop dayAux

* fix treatment variable
rename treatment treatmentAux
gen treatment=0 if treatmentAux=="B Control"
replace treatment=1 if treatmentAux=="B Treatment"
drop treatmentAux
destring treatment, replace

* fix demographic characteristics
gen demo_white = (demo_race == "White")

rename demo_sex demo_sex_t
gen demo_sex = (demo_sex_t == "Male")
drop demo_sex_t

** demographics and exit variables
sum demo_age, detail
gen demo_age_median = .
replace demo_age_median = 1 if demo_age >= r(p50) & demo_age != .
replace demo_age_median = 0 if demo_age < r(p50) & demo_age != .

rename demo_collegeplus demo_collegeplus_t
gen demo_collegeplus = (demo_collegeplus_t == "College or more")
drop demo_collegeplus_t

rename demo_studentloan demo_studentloan_t
gen demo_studentloan = (demo_studentloan_t == "Yes")
drop demo_studentloan_t

rename demo_holddebt demo_holddebt_t
gen demo_holddebt = (demo_holddebt_t == "Yes")
drop demo_holddebt_t

rename demo_covid demo_covid_t
gen demo_covid=0
replace demo_covid=1 if demo_covid_t=="A little"
replace demo_covid=2 if demo_covid_t=="A moderate amount"
replace demo_covid=3 if demo_covid_t=="A lot"
replace demo_covid=5 if demo_covid_t=="A great deal"
drop demo_covid_t

save "data/borrowing_work.dta", replace
}
