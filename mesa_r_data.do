use "/home/steve/phd/ASCVD/ASCVD/mesa.dta", clear

keep id grp study ascvd studytime age totchol hdlc sysbp hyptmdsr cursmoke diabt126 cholmed1 bmi prevcond

export delimited using "/home/steve/phd/ASCVD/ASCVD/pooled_data/mesadata.csv", replace

