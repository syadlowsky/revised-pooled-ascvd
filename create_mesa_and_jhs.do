cd "/home/steve/phd/ASCVD/ASCVD"
set more off

******* EXT VAL *******
******* MESA  *******

use "MESA/MESAe1FinalLabel10102016.dta", clear
keep idno age1c race1c gender1 bmi1c cig1c sbp1c dbp1c glucos1c dm031c htnmed1c htn1c ldl1 hdl1 chol1 trig1 creatin1 lipid1c ascvd1c bpmed1 sttn1c 
g gender = "F" if gender1==0
replace gender = "M" if gender1==1
g racegrp = "B" if race1c==3
replace racegrp= "W" if race1c==1
g glucose = glucos1c
g hdlc = hdl1
g ldlchol = ldl1
g totchol = chol1
g cursmoke = 1 if cig1c == 2
replace cursmoke = 0 if cig1c <2
g prevchf = 0
g prevmi = 0
g prevstrk = 0
g prevproc=0
g prevchd = 0
g prevap = 0
g diabt126 = 1 if dm031c==2 | dm031c==3
replace diabt126 = 0 if dm031c<2
g sysbp = sbp1c
g diabp = dbp1c
g bmi = bmi1c
g cholmed1 = lipid1c
g hyptmdsr = bpmed1
save mesa, replace

use "MESA/MESAEvThru2013_20160308.dta", clear
g str = strk
g chddeath = 1 if dth==1 & dthtype==1 
g strdeath = 1 if dth==1 & dthtype==2

g timetoang = angtt/365.25 if ang==1
g timetomi = (mitt)/365.25 if mi==1
g timetostr = (strktt)/365.25 if str==1
g timetochddeath = dthtt/365.25 if chddeath==1
g timetostrdeath = dthtt/365.25 if strdeath==1
merge 1:1 idno using mesa
drop _merge
save mesa, replace


use mesa, clear
g prevcond = 0

g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr!=1
g rxsbp = sysbp if  hyptmdsr==1

g study = "MESA"


save mesa, replace


use mesa, clear


egen timetoascvd = rowmin(timetomi timetostr timetochddeath timetostrdeath)

g ascvd = 	1 if (mi==1 | str==1 | chddeath==1 | strdeath==1)
replace ascvd = 0 if ascvd!=1

g studytime = timetoascvd if ascvd==1
replace studytime = (dthtt / 365.25) if ascvd==0
replace studytime = 12 if studytime==.

save mesa, replace


******* JHS  *******

import delimited "JHS/analysislong.csv", clear encoding(ISO-8859-1)
drop if visit!=1

g idno = substr(subjid,2,6)
g visitdate2 = date(visitdate,"YMD")
format visitdate2 %td
drop visitdate
rename visitdate2 visitdate
destring, replace force
keep id visitdate age male  bmi currentsmoker sbp dbp fpg diabetes bpmeds htn ldl hdl totchol trigs egfrckdepi statinmeds strokehx mihx cardiacprochx carotidangiohx chdhx cvdhx afib
g gender = "F" if male==0
replace gender = "M" if male==1
g racegrp = "B" 
g glucose = fpg
g hdlc = hdl
g ldlchol = ldl
g cursmoke = 1 if currentsmoker == 1
replace cursmoke = 0 if currentsmoker!=1 & currentsmoker!=.
g prevchf = 0
g prevmi = mihx
g prevstrk = strokehx 
g prevproc=cardiacprochx
g prevchd =chdhx
g prevap =cvdhx
g prevafib = afib
g diabt126 = diabetes
g sysbp = sbp
g diabp = dbp
g cholmed1 = statinmeds
g hyptmdsr = bpmeds
sort idno
save jhs, replace

import delimited "JHS/jhs_inc_by12.csv", clear encoding(ISO-8859-1)
g idno = substr(subjid,2,6)
g midate = date(datemi,"YMD")
format midate %td
g strdate = date(ed12dp,"YMD")
format strdate %td
g chddeathdate = date(enddate,"YMD")
format chddeathdate %td
destring, replace force
merge 1:1 idno using jhs
g str = in12dp
g mi = mi12
g chddeath = fatchd12
g strdeath = .
g timetomi = (midate-visitdate)/365.25
g timetostr = (strdate-visitdate)/365.25
g timetochddeath = (chddeathdate-visitdate)/365.25 
g timetostrdeath = .
g censored = 1 if (mi!=1 & str!=1 & chddeath!=1)
g timecensored = years if censored==1
drop _merge
save jhs, replace

import delimited "JHS/aricjhs.csv", clear
g idno = substr(subjid,2,6)
destring, replace force
merge 1:1 idno using jhs
drop _merge
drop if aric == 1
save jhs, replace

use jhs, clear
g prevcond = 1 if prevmi==1 | prevstrk==1 | prevproc==1 | prevchf==1
replace prevcond = 0 if prevcond != 1

g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr!=1
g rxsbp = sysbp if  hyptmdsr==1

g study = "JHS"

egen timetoascvd = rowmin(timetomi timetostr timetochddeath timetostrdeath)

g ascvd = 	1 if (mi==1 | str==1 | chddeath==1 | strdeath==1)
replace ascvd = 0 if ascvd!=1

g studytime = timetoascvd if ascvd==1
replace studytime = timecensored if ascvd==0
replace studytime = 12 if studytime==.

save jhs, replace
