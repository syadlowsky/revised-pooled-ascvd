use "/home/steve/phd/ASCVD/ASCVD/jhs.dta", clear

keep id grp study ascvd studytime age totchol hdlc sysbp hyptmdsr cursmoke diabt126 cholmed1 bmi prevcond

export delimited using "/home/steve/phd/ASCVD/ASCVD/pooled_data/jhsdata.csv", replace

