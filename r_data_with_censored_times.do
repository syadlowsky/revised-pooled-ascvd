use "/home/steve/phd/ASCVD/ASCVD/pooled_data/pooledcohs.dta", clear
set more off

gen nid = _n

drop if grp==.	
egen timetoascvd = rowmin(timetomi timetostr timetochddeath timetostrdeath)

g ascvd = 	1 if (mi==1 | str==1 | chddeath==1 | strdeath==1)
replace ascvd = 0 if ascvd!=1

g prevcond = 1 if (prevmi == 1 | prevstrk == 1 | prevap == 1 | prevchf == 1 | prevdeath == 1 | prevafib == 1)
replace prevcond = 0 if prevcond != 1

g studytime = timetoascvd if ascvd==1
replace studytime = 12 if timetoascvd >= 12 | ascvd==0
replace studytime = timecensored if (censored==1 & ascvd==0)

keep nid study totchol hdlc sysbp hyptmdsr ascvd grp studytime diabt126 cursmoke bmi cholmed1 age prevcond
export delimited using "/home/steve/phd/ASCVD/ASCVD/pooled_data/cohdataforr_censored.csv", replace

