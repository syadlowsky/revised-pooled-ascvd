cd "/home/steve/phd/ASCVD/ASCVD/"
* ASCVD recal

* ARIC

/*Visit1 */
import delimited "aric/V1/csv/derive13.csv", clear
keep id_c centerid cholmdcode01 cholmdcode02 cursmk01 diabts02 diabts03 gender hdl01 hyptmd01 racegrp cigt01 cigtyr01 glucos01 clvh01 v1age01 enroll_yr prevmi05 prvchd05 prevhf01 forsmk01 fast1202 fast0802 ldl02 bmi01 wsthpr01 tglefh01  momhistorystr dadhistorystr  momhistorychd dadhistorychd  dadhistorydia momhistorydia  rangna01
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/sbpa02.csv", clear
keep id_c sbpa21 sbpa22
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/lipa.csv", clear
keep id_c lipa01 lipa02 lipa08
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/hom.csv", clear
keep id_c hom10d hom10e hom29 hom31 hom32 hom35 hom10a hom54 hom12 hom13 hom14 hom15e hom15d hom16d hom16e hom18d hom18e hom19d hom19e hom20 hom21 hom22 hom23e hom23d hom24d hom24e hom26d hom26e hom27d hom27e hom55 hom56
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/msra.csv", clear
keep id_c msra01 msra02 msra08f
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/anta.csv", clear
keep id_c anta01 anta07a anta04
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/stroke01.csv", varnames(1) clear
keep id_c tia01 stroke01 stia01
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace
import delimited "aric/V1/csv/phea.csv", varnames(1) clear
keep id_c phea06 phea07a phea08 phea09a
sort id_c
merge 1:1 id_c using "pooled_data/aric_v1.dta"
drop _merge
sort id_c
save "pooled_data/aric_v1.dta", replace

g brngmeds = 1 if msra01=="Y"
replace brngmeds = 2 if msra01=="N" & msra02=="T"
replace brngmeds = 3 if msra01=="S"


g cholmed1 = cholmdcode01
g sysbp=sbpa21
g cholmed2=cholmdcode02
g diabp=sbpa22
g age=v1age01
g hyptmdsr=hyptmd01
g hyptmdml=.
g cursmoke=cursmk01
g smokstat=cigt01
g cigpday=hom32 if smokstat==1
replace cigpday=0 if smokstat>=2 & smokstat<=3  
g avgsmoke=hom35 if smokstat>=1 & smokstat<=2 
replace avgsmoke=0 if smokstat==3 
g packyrs=cigtyr01/20
g strtsmk=hom29
g stopsmk=hom31
g bmi=bmi01
g wsthip=wsthpr01
g waist=anta07a
g weight=0.45*anta04
g height=anta01

g fast08=fast0802
g fast12=fast1202
g ldlc=ldl02
g tglefh=tglefh01
g glucose=glucos01
g hdlc=hdl01
g totchol=lipa01 
g trigly=lipa02 
g apolpa=lipa08

g prevmi=prevmi05
g prevchd=prvchd05
g prevstrk=1 if stroke01=="Y" | hom10d=="Y"
replace prevstrk=0 if stroke01!="Y" & hom10d=="N"
g prevchf=prevhf01
g prevap=1 if rangna01==1 
replace prevap=0 if rangna01==4 
replace prevap=. if rangna01>1 & rangna01<4
g prevproc =0  if phea06=="N" 
replace prevproc=1 if phea06=="Y" & (phea07a=="Y" | phea09a=="Y")
replace prevproc=0 if phea06=="Y" & (phea07a=="N" & phea09a=="N")

/* Diabetes Definitions */

g study_dm=diabts03
g diabmed=0 if msra08f=="N" | msra02=="T" 
replace diabmed=1 if msra08f=="Y"
g diabtsr=0 if hom10e=="N"
replace diabtsr=1 if hom10e=="Y"
replace diabtsr=. if hom10e!="N" & hom10e!="Y"

g diabhx=1 if (glucose >=200) | (glucose>=126 & fast08>=1) | diabtsr==1 | diabmed==1
replace diabhx=0 if glucose>0 & glucose<126 & diabtsr==0 & (diabmed==0 | diabmed==.) 

g diabt126=1 if (glucose >=200) | (glucose>=126 & fast08>=1) | diabmed==1 
replace diabt126=0 if glucose>0 & glucose<126 & diabmed!=1
replace diabt126=. if glucose>=126 & glucose<200 & fast08<=0 & diabmed==. 

/* End Diabetes defintion */

g exam=1




 keep id_c  enroll_yr age gender racegrp centerid bmi totchol ldlc tglefh hdlc trigly cholmed1 cholmed2 sysbp diabp hyptmdsr hyptmdml cursmoke smokstat cigpday strtsmk stopsmk packyrs avgsmoke fast08 fast12 glucose diabt126 diabtsr brngmeds diabmed  prevmi prevchd prevstrk wsthip apolpa height exam waist weight  prevchf   study_dm diabhx  prevap prevproc 

save "pooled_data/aric_v1.dta", replace


g aricid_c=id_c
replace smokstat=. if smokstat==4 


label variable height "Standing height in cm" 
label variable weight "Weight in Kg" 	
label variable waist "Waist circumference (cm)" 
label variable study_dm "Study defined Diabetes" 
label variable diabhx "Diabetes history: self-report or exam" 


label variable id_c "Pooling study id_c" 
label variable aricid_c "ARIC orginal id_c" 
label variable age "Participant age (years)" 
label variable gender "Participant sex" 
label variable racegrp "Race group of participant" 
label variable centerid "Baseline exam centerid" 
label variable totchol "Total Chol (mg/dL)" 
label variable ldlc "LDL chol (mg/dL)" 
label variable tglefh "Triglycerid_ces LE 400" 
label variable hdlc "HDL chol (mg/dL)" 
label variable trigly "Triglycerid_ces (mg/dL)" 
label variable cholmed1 "Chol lowering Medications" 
label variable cholmed2 "Meds secondarily lower Cholesterol" 
label variable sysbp "Avg systolic BP" 
label variable diabp "Avg diastolic BP" 
label variable hyptmdsr "High BP+meds or Self report of BP meds" 
label variable hyptmdml "Coded Hypertension meds" 

label variable cursmoke "Current cigarette smoking" 
label variable smokstat "1 cur, 2 For, 3 Never" 
label variable strtsmk "Age started smoking" 
label variable stopsmk "Age stopped smoking" 
label variable packyrs "Pack years of cigarette smoking" 
label variable study "ARIC, CHS, FCOH, FOFF or SHS" 
label variable fast08 "8 hour fast indicator" 
label variable fast12 "12 hour fast indicator" 
label variable glucose "Blood glucose (mg/dL)" 
label variable diabt126 "Diabetes by 126 cutoff" 
label variable prevmi "Prevalent MI by history or ECG" 
label variable prevchd "Prevalent CHD (MI, SMI, Procedures)" 
label variable prevstrk "Prevalent Stroke" 
label variable exam "Clinic exam number" 
label variable bmi "Body Mass Index (kg/M*M)" 
label variable cigpday "Cig per day, Cur Smokers" 
label variable avgsmoke "Avg cigs/day lifetime" 
label variable diabtsr "Diabetes self-report" 
label variable brngmeds "Brought all Meds for past 2 wks" 
label variable diabmed "Meds for Diabetes past 2 wks" 

label variable prevproc "Prevalent Cor Revas procedure"
label variable prevchf "Prevalent Congestive Heart Failure"
label variable prevap "Prevalent Angina (Study)"


replace hyptmdml=0 if hyptmdsr==0
replace hyptmdml=1 if hyptmdsr==1

sort id_c
save "pooled_data/aric.dta", replace

insheet using "aric/cohort_Stroke/csv/cderps10.csv", clear
sort id_c

* From data dictionary: For these reasons, all time-to-event analysis (except time to death) should be censored at last contact as defined by CENSDAT7 and in corresponding variables with the prefix C7_. Events identified after CENSDAT7, including those identified through linkage with registries, should be excluded from analysis.
* unforunately, we dont have CENSDAT7...
g upperdx = upper(finaldx)
* https://www2.cscc.unc.edu/aric/sites/default/files/public/datasets/Data%20Dictionary%20C12DERV1%20Cohort%20Surv%20Stroke%20Derived%20Variables.pdf
g defstroke = 1 if inrange(upperdx, "A", "D")
replace defstroke = 0 if defstroke != 1

collapse (max) defstroke=defstroke, by(id_c)

merge 1:1 id_c using "pooled_data/aric.dta"
drop _merge
sort id_c
save "pooled_data/aric.dta", replace

insheet using "aric/cohort_CHD/csv/cevtps10.csv", clear
sort id_c

* From data dictionary: For these reasons, all time-to-event analysis (except time to death) should be censored at last contact as defined by CENSDAT7 and in corresponding variables with the prefix C7_. Events identified after CENSDAT7, including those identified through linkage with registries, should be excluded from analysis.
* unforunately, we dont have CENSDAT7...
g defmi = 1 if cmidx=="DEFMI"
replace defmi = 0 if defmi != 1

collapse (max) defmi=defmi, by(id_c)

merge 1:1 id_c using "pooled_data/aric.dta"
drop _merge
sort id_c
save "pooled_data/aric.dta", replace

insheet using "aric/cohort_Incident/csv/ichdps10.csv", clear
sort id_c

merge 1:1 id_c using "pooled_data/aric.dta"
drop _merge
sort id_c
save "pooled_data/aric.dta", replace


* mi's/strokes/deaths 
* let's try getting the mi directly from event coding, so that we can only use "definite" ones...
g mi = 1 if mi10==1
*& defmi==1
replace mi = 0 if mi!=1
g str = 1 if indp10 == 1
*& defstroke == 1
replace str = 0 if str != 1
g chddeath = fatchd10
g strdeath = .

g timetomi = fuinc10/365.25 if mi==1
g timetostr = ftdp10/365.25 if str==1
g timetochddeath = fuinc10/365.25 if chddeath==1
g timetostrdeath = .

g censored = 1 if yrdth10<2010
g timecensored = (yrdth10-enroll_yr) if censored==1

save "pooled_data/aric.dta", replace

encode id_c, gen(id)
g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr==0
g rxsbp = sysbp if  hyptmdsr==1
by grp, sort: su id age totchol hdl unrxsbp rxsbp hyptmdsr cursmo diabt126  

save "pooled_data/aric.dta", replace




* CHS

/* BASELINE */

insheet using  "CHS_2015a/BASELINE/basebothfinal.csv", clear
destring, replace force
g gender = "F" if gend01==0
replace gender="M" if gend01==1
drop cohort
g cohort="ORG"
g racegrp = "W" if race01==1
replace racegrp = "B" if race01==2

g  id=idno

g smokstat=3 if smoke==1
replace smokstat=2 if smoke==2
replace smokstat=1 if smoke==3
g cursmoke=1 if smoke==3
replace cursmoke=0 if smoke==1 | smoke==2

g cigpday = amount if smoke==3
replace cigpday=0 if smoke==1 | smoke==2

g avgsmoke=amount

g totchol=choladj
g trigly=trig44
g hdlc=hdl44
g sercreat=cre44
g ldlc=ldladj

g glucose=glu44
g lvh=ecglvh
g age = (age2*2)+63.5
g sysbp=avzmsys
g diabp=avzmdia


g diabetes=diabada
g diab = 1 if diabetes==3 | diabetes==4
replace diab = 0 if diabetes==1 | diabetes==2
drop diabetes
g diabt126 = diab

g cholmed1=1 if sttn06==1 | lipid06==1 | mlpd06==1 
replace cholmed1=0 if (sttn06==0 & lipid06==0 & mlpd06==0)
 
g hyptmdml=1 if htnmed06==1 |  beta06==1 |  betad06==1 |  ccb06==1 |  ace06==1 |  aced06==1 |  vaso06==1 |   vasod06==1 |  diuret06==1 |  loop06==1 |  hctz06==1 |  hctzk06==1 |   ccbir06==1 |  ccbsr06==1 |  alpha06==1 |  alphad06==1 |  ccbt06==1
replace hyptmdml=0 if htnmed06==0 & (  beta06==0 |  betad06==0 |  ccb06==0 |  ace06==0 |  aced06==0 |  vaso06==0 |   vasod06==0 |  diuret06==0 |  loop06==0 |  hctz06==0 |  hctzk06==0 |   ccbir06==0 |  ccbsr06==0 |  alpha06==0 |  alphad06==0 |  ccbt06==0)
    

g hyptmdsr = hyptmdml
* g hyptmdsr = 1 if bp07==1 & hyptmdml==1
* replace hyptmdsr = 0 if bp07==0 | hyptmdml==0

g prevmi=miblmod
g prevchd=1 if prevmi==1 | bpssur==1 | corart==1 
replace prevchd=0 if prevchd!=1
 
g prevchf=chblmod
g prevstrk=stblmod
g prevap=anblmod
g prevafib=afib

g prevproc=1 if bpssur==1 | corart==1 
replace prevproc=0 if bpssur==0 & corart==0

g exam=1

save "pooled_data/chs.dta", replace


/* YEAR 5 */
import delimited "CHS_2015a/YEAR5/yr5newfinal.csv", clear
destring, replace force

g gender = "F" if gend01==0
replace gender="M" if gend01==1

g racegrp = "W" if race01==1
replace racegrp = "B" if race01==2
replace racegrp = "A" if race01==3
replace racegrp = "0" if race01==4 | race01==5

drop cohort
g cohort="NEW"
 
g id=idno

 
g totchol=choladj
g trigly=trig44
g hdlc=hdl44
g sercreat=cre44
g ldlc=ldl44

g glucose=glu44
g lvh=ecglvh
g age = (age2*2)+63.5
g sysbp=avesys
g diabp=avedia


g diabetes=diabada
g diab = 1 if diabetes==3 | diabetes==4
replace diab = 0 if diabetes==1 | diabetes==2
drop diabetes
g diabt126 = diab

g cholmed1=1 if sttn06==1 | lipid06==1 | mlpd06==1 
replace cholmed1=0 if (sttn06==0 & lipid06==0 & mlpd06==0)
 
g hyptmdml=1 if htnmed06==1 |  beta06==1 |  betad06==1 |  ccb06==1 |  ace06==1 |  aced06==1 |  vaso06==1 |   vasod06==1 |  diuret06==1 |  loop06==1 |  hctz06==1 |  hctzk06==1 |   ccbir06==1 |  ccbsr06==1 |  alpha06==1 |  alphad06==1 |  ccbt06==1
replace hyptmdml=0 if htnmed06==0 & (  beta06==0 |  betad06==0 |  ccb06==0 |  ace06==0 |  aced06==0 |  vaso06==0 |   vasod06==0 |  diuret06==0 |  loop06==0 |  hctz06==0 |  hctzk06==0 |   ccbir06==0 |  ccbsr06==0 |  alpha06==0 |  alphad06==0 |  ccbt06==0)
    

g hyptmdsr = 1 if bp57==1 & hyptmdml==1
replace hyptmdsr = 0 if bp57==0 | hyptmdml==0

g prevmi=1 if miblmod==1
replace prevmi=0 if prevmi!=1
g prevchd=1 if prevmi==1
replace prevchd=0 if prevchd!=1
 
g prevchf=1 if chblmod==1
replace prevchf = 0 if prevchf!=1
g prevstrk=1 if stblmod==1
replace prevstrk = 0 if prevstrk!=1
g prevap=anblmod
g prevafib=atrfib21

g prevproc=1 if corart==1 
replace prevproc=0 if corart==0

g exam=5



g smokstat=3 if smoke==1
replace smokstat=2 if smoke==2
replace smokstat=1 if smoke==3
g cursmoke=1 if smoke==3
replace cursmoke=0 if smoke==1 | smoke==2

g cigpday = amount if smoke==3
replace cigpday=0 if smoke==1 | smoke==2

g avgsmoke=amount

merge 1:1 id using "pooled_data/chs.dta"
drop _merge

save "pooled_data/chs.dta", replace


insheet using  "CHS_2015a/EVENTS/eventsfinal.csv", clear
encode perstat, g(perstat1)
drop perstat
rename perstat1 perstat
destring, replace force

g mi = 1 if (evtype==1 | evtype==10) & increc==1
replace mi = 0 if mi!=1
g str= 1 if evtype==3 & increc==1
replace str = 0 if str!=1
g chddeath = 1 if (evtype==11 | evtype==4 | evtype==7 | evtype==8) & fatal==1 & increc!=0
replace chddeath = 0 if chddeath!=1
g strdeath = 1 if evtype==11 & fatal==1 & defprob!=0
replace strdeath = 0 if strdeath!=1

g timetomi = ttoevent/365.25 if mi==1
g timetostr = ttoevent/365.25 if str==1
g timetochddeath = ttoevent/365.25 if chddeath==1
g timetostrdeath = .


g censored = 1 if evtype==9
g timecensored = (ttoevent/365.25) if censored==1



collapse (max) mi str chddeath strdeath censored (min) timetomi timetostr timetochddeath timetostrdeath timecensored, by(id)

sort id

merge 1:1 id using "pooled_data/chs.dta"

drop _merge

save "pooled_data/chs.dta", replace

g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr==0
g rxsbp = sysbp if  hyptmdsr==1
by grp, sort: su id age totchol hdlc unrxsbp rxsbp hyptmdsr cursmoke diabt126  



save "pooled_data/chs.dta", replace




* CARDIA


*******baseline exam;


insheet using "cardia/Y00/DATA/aaf05.csv", clear
keep pid a05fstmn
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf09med.csv", clear
keep if a09mdtyp=="HBP"
keep pid a09mdtyp a09mdnow
sort pid
merge m:1 pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf08v2.csv", clear
keep pid a08hbp a08heart a08diab a08bpmed a08preg
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aalip.csv", clear
keep pid al1chol al1hdl al1ntrig al1ldl
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf02.csv", clear
keep pid a02sbp a02dbp a02pulse
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf10.csv", clear
keep pid a10cigs
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf09tob.csv", clear
keep pid a09smknw a09cgtdy
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaref.csv", clear
keep pid race sex examage a02date a03ed
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf09gen.csv", clear
keep pid a09dibst a09hrtak a09angin a09chf
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf09gen.csv", clear
keep pid a09dibst a09hrtak a09angin
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aachem.csv", clear
keep pid al3_glu al3creat al3album
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aaf20.csv", clear
keep pid a20bmi a20wgt a20wst1
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aains.csv", clear
keep pid al4ins
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace
insheet using "cardia/Y00/DATA/aains.csv", clear
keep pid al4ins
sort pid
merge m:m pid using "pooled_data/cardia.dta"
drop _merge
sort pid
save "pooled_data/cardia.dta", replace

destring, replace force

g gender="M" if sex==1
replace gender="F" if sex==2
g racegrp="B" if race==4
replace racegrp="W" if race==5

g id=int(pid/1000000)
g cardid=pid
g weight=a20wgt*0.45
g waist=a20wst1
g bmi=a20bmi

g pregnant = 1 if a08preg==2 
replace pregnant = 0 if a08preg==1

g smokstat = 3 if a10cigs==1 
replace smokstat = 2 if a10cigs==2 & a09smknw==1 
replace smokstat = 1 if a10cigs==2 & a09smknw==2 
g cursmoke =1 if smokstat==1 
replace cursmoke = 0 if smokstat==2 | smokstat==3 
g cigpday = a09cgtdy if cursmoke==1
replace cigpday = 0 if cursmoke==0 


g hyptmdsr=. if a08bpmed==8
replace hyptmdsr=0 if a08bpmed==1 
replace hyptmdsr=1 if a08bpmed==2 & a09mdnow==2 
replace hyptmdsr=0 if a08bpmed==2 & a09mdnow==1 

g totchol=al1chol
g trigly=al1ntrig
g hdlc=al1hdl

g ldlc=al1ldl
g tglefh=1 if al1ntrig<=400 
replace tglefh=0 if tglefh!=1

g glucose=al3_glu
g sercreat=0.2159+(0.6867*al3creat)
g albumin=al3album
g fast08=0 if a05fstmn/60 <8 
replace fast08=1 if a05fstmn/60 >=8 
g fast12=0  if a05fstmn/60 <12 
replace fast12=1 if a05fstmn/60 >=12 

g age = examage
g exmdate=a02date
g sysbp=a02sbp
g diabp=a02dbp
g finsulin=al4ins
g heartrte=a02pulse*2

/* Diabetes Definitions */

 **history or self report;
 
g diabhx=1 if (a08diab==2 & (a09dibst>=3 | a09dibst<=4)) 
replace diabhx=0 if (a08diab==1 | (a08diab==2 & a09dibst==5)) 

 **status at exam;
g diabt126=1 if (glucose >=200) | (glucose>=126 & fast08>=1) | diabhx==1
replace diabt126=0 if glucose>0 & glucose<126 & (diabhx==. | diabhx==0) 
replace diabt126=. if glucose>=126 & glucose<200 & fast08<=0 & diabhx==. 


/* End Diabetes defintion */


 * Prevalent Disease ;
g prevchd=0 if a09hrtak==1 | a08heart==1 
replace prevchd=1 if a09hrtak==2
replace prevchd=. if a08heart==8 

g prevap=0 if a09angin==1 | a08heart==1 
replace prevap=1 if a09angin==2 
replace prevap=. if a08heart==8 

g prevmi=prevchd

g study="CARD"
g exam=0


save "pooled_data/cardia_yr1.dta", replace

*******year 2 exam;

g diabhx1=diabhx
g diabt1261=diabt126
g prevap1=prevap
g prevchd1=prevchd
g prevmi1=prevmi

 keep pid cardid diabhx1 diabt1261 prevchd1 prevmi1 prevap1

save "pooled_data/cardia_yr2.dta", replace

insheet using "cardia/Y02/DATA/baf05.csv", clear
keep pid b05fstmn
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf09mhb.csv", clear
keep pid b09hbnow
drop if b09hbnow=="1"
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf08v2.csv", clear
keep pid b08hbp b08heart b08diab b08bpmed b08preg 
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf02.csv", clear
keep pid b02avgsy b02avgdi b02pulse
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf10.csv", clear
keep pid b10cigs b10tobac
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf09tob.csv", clear
keep pid b09smknw b09cgtdy
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baref.csv", clear
keep pid race sex ex2_age x2date 
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf09dib.csv", clear
keep pid b09dibst
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace
insheet using "cardia/Y02/DATA/baf20.csv", clear
keep pid b20bmi b20wgt b20wst1
sort pid
merge m:m pid using "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr2.dta", replace

destring, replace force

g idno=pid
g short_id=int(id/10000000)
replace cardid=pid

g gender="M" if sex==1 
replace gender="F" if sex==2 
g racegrp="B" if race==4
replace racegrp="W" if race==5

 
g id=int(pid/1000000)
g weight=b20wgt*0.45
g waist=b20wst1
g bmi=b20bmi

g pregnant = 1 if b08preg==2 
replace pregnant = 0 if b08preg==1

g smokstat = 3 if b10cigs==1
replace smokstat = 2 if b10cigs==2 & b09smknw==1 
replace smokstat = 1 if b10cigs==2 & b09smknw==2
g cursmoke =1 if smokstat==1 
replace cursmoke = 0 if smokstat==2 | smokstat==3
g cigpday = b09cgtdy if cursmoke==1
replace cigpday = 0 if cursmoke==0 


g hyptmdsr=. if b08bpmed==8
replace hyptmdsr=0 if b08bpmed==1 
replace hyptmdsr=1 if b08bpmed==2 & b09hbnow==2 
replace hyptmdsr=0 if b08bpmed==2 & b09hbnow==1 



g fast08=0 if b05fstmn/60 <8 
replace fast08=1 if b05fstmn/60 >=8 
g fast12=0  if b05fstmn/60 <12 
replace fast12=1 if b05fstmn/60 >=12 

g age = ex2_age
g exmdate=x2date
g sysbp=b02avgsy
g diabp=b02avgdi
g heartrte=b02pulse*2


g diabhx=1 if diabhx1==1 | diabt1261==1 
replace diabhx=0 if (diabhx1==0 | diabhx1==.) & (diabt1261==0 | diabt1261==.) 


g study="CARD"
g exam=2

 save "pooled_data/cardia_yr2.dta", replace

 
 
*******year 5 exam;

g diabhx2=diabhx
keep pid cardid diabhx2 diabt1261
sort pid
save "pooled_data/cardia_yr5.dta", replace

insheet using "cardia/Y05/DATA/caf05.csv", clear
keep pid c05fstmn
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caf08.csv", clear
keep pid c08hbp c08hbnow c08diab c08chnow c08preg c08diab
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/calip.csv", clear
keep pid cl1chol cl1hdl cl1ntrig cl1ldl
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caf02.csv", clear
keep pid c02avgsy c02avgdi c02pulse
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caf10.csv", clear
keep pid c10cigs c10tobac
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caf09tob.csv", clear
keep pid c09smknw c09cgtdy
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caref.csv", clear
keep pid  race sex ex3_age c02date c03ed
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caf20.csv", clear
keep pid c20bmi c20wgt c20wst1
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
insheet using "cardia/Y05/DATA/caf11.csv", clear
keep pid c11momlv c11mage c11mdth c11mhage c11dadlv c11fage c11fdth c11fhage c11bdiab c11fdiab c11mdiab c11sdiab c11bstrk c11fstrk c11mstrk c11sstrk
sort pid
merge m:m pid using "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr5.dta", replace
destring, replace force

replace cardid=pid

g gender="M" if sex==1 
replace gender="F" if sex==2 
g racegrp="B" if race==4
replace racegrp="W" if race==5

g id=int(pid/1000000)
g weight=c20wgt*0.45
g waist=c20wst1
g bmi=c20bmi

g pregnant = 1 if c08preg==2 
replace pregnant = 0 if c08preg==1

g smokstat = 3 if c10cigs==1
replace smokstat = 2 if c10cigs==2 & c09smknw==1 
replace smokstat = 1 if c10cigs==2 & c09smknw==2
g cursmoke =1 if smokstat==1 
replace cursmoke = 0 if smokstat==2 | smokstat==3
g cigpday = c09cgtdy if cursmoke==1
replace cigpday = 0 if cursmoke==0 


g hyptmdsr=. if c08hbnow==8
replace hyptmdsr=0 if c08hbnow==1 
replace hyptmdsr=1 if c08hbnow==2 & c08hbp==2 
replace hyptmdsr=0 if c08hbnow==2 & c08hbp==1 


g cholmed1=0 if c08chnow==1
replace cholmed1=1 if c08chnow==2 

g fast08=0 if c05fstmn/60 <8 
replace fast08=1 if c05fstmn/60 >=8 
g fast12=0  if c05fstmn/60 <12 
replace fast12=1 if c05fstmn/60 >=12 

g age = ex3_age
g exmdate=c02date
g sysbp=c02avgsy
g diabp=c02avgdi
g heartrte=c02pulse*2


g diabhx=1 if diabhx2==1 | diabt1261==1 
replace diabhx=0 if (diabhx2==0 | diabhx2==.) & (diabt1261==0 | diabt1261==.) 


g study="CARD"
g exam=5

g totchol=cl1chol
g trigly=cl1ntrig
g hdlc=cl1hdl
g ldlc=cl1ldl

 save "pooled_data/cardia_yr5.dta", replace

 

 g diabhx3=diabhx
 keep pid cardid diabhx3 diabt1261

  save "pooled_data/cardia_yr7.dta", replace

*******year 7 exam;

insheet using "cardia/Y07/DATA/daf05a.csv", clear
keep pid d05fstmn
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daf08.csv", clear
keep pid d08hbp d08hbnow d08diab d08chnow d08preg d08diab
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/dalip.csv", clear
keep pid dl1chol dl1hdl dl1ntrig dl1ldl
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daf02.csv", clear
keep pid d02avgsy d02avgdi d02pulse
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daf10.csv", clear
keep pid d10cigs d10tobac
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daf09tob.csv", clear
keep pid d09smknw d09cgtdy
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daref.csv", clear
keep pid race sex ex4_age d02date  d03ed
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daglu.csv", clear
keep pid dl7glu
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/daf20.csv", clear
keep pid d20bmi d20wgt d20wst1
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace
insheet using "cardia/Y07/DATA/dains.csv", clear
keep pid dl7ins7
sort pid
merge m:m pid using "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr7.dta", replace

destring, replace force

replace cardid=pid

g gender="M" if sex==1 
replace gender="F" if sex==2 
g racegrp="B" if race==4
replace racegrp="W" if race==5

g id=int(pid/1000000)
g weight=d20wgt*0.45
g waist=d20wst1
g bmi=d20bmi

g pregnant = 1 if d08preg==2 
replace pregnant = 0 if d08preg==1

g smokstat = 3 if d10cigs==1
replace smokstat = 2 if d10cigs==2 & d09smknw==1 
replace smokstat = 1 if d10cigs==2 & d09smknw==2
g cursmoke =1 if smokstat==1 
replace cursmoke = 0 if smokstat==2 | smokstat==3
g cigpday = d09cgtdy if cursmoke==1
replace cigpday = 0 if cursmoke==0 


g hyptmdsr=. if d08hbnow==8
replace hyptmdsr=0 if d08hbnow==1 
replace hyptmdsr=1 if d08hbnow==2 & d08hbp==2 
replace hyptmdsr=0 if d08hbnow==2 & d08hbp==1 


g cholmed1=0 if d08chnow==1
replace cholmed1=1 if d08chnow==2 

g fast08=0 if d05fstmn/60 <8 
replace fast08=1 if d05fstmn/60 >=8 
g fast12=0  if d05fstmn/60 <12 
replace fast12=1 if d05fstmn/60 >=12 
g glucose=dl7glu*0.94359772+6.979619035

g age = ex4_age
g exmdate=d02date
g sysbp=d02avgsy
g diabp=d02avgdi
g heartrte=d02pulse*2


g diabt126=1 if (glucose >=200) | (glucose>=126 & fast08>=1) 
replace diabt126=0 if glucose>0 & glucose<126 
replace diabt126=. if glucose>=126 & glucose<200 & fast08<=0 



g diabhx=1 if diabhx3==1 | diabt1261==1 | diabt126==1 
replace diabhx=0 if (diabhx3==0 | diabhx3==.) & (diabt1261==0 | diabt1261==.) & (diabt126==0 | diabt126==.) 


g study="CARD"
g exam=7

g totchol=dl1chol
g trigly=dl1ntrig
g hdlc=dl1hdl
g ldlc=dl1ldl

 save "pooled_data/cardia_yr7.dta", replace



 
*******year 10 exam;

keep pid cardid diabt126 diabhx diabt1261

g  diabt1264=diabt126
g diabhx4=diabhx

drop diabt126 diabhx

save "pooled_data/cardia_yr10.dta", replace

insheet using "cardia/Y10/DATA/eaf05.csv", clear
keep pid e05fstmn e05ogtt 
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eaglu.csv", clear
keep pid el7glu el7glu2h
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eains.csv", clear
keep pid el7ins
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eachem.csv", clear
keep pid el7creat
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eaf08.csv", clear
keep pid e08hbp e08hbnow e08hrtak e08diab e08chnow e08preg e08diab e08heart e08angin e08tia e08ancth
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/ealip.csv", clear
keep pid el1chol el1hdl el1ntrig el1ldl
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eaf02.csv", clear
keep pid e02avgsy e02avgdi e02pulse
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eaf10.csv", clear
keep pid e10cigs e10tobac
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eaf09tob.csv", clear
keep pid e09smknw e09cgtdy
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/earef.csv", clear
keep pid race sex ex5_age e02date e03ed 
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
 insheet using "cardia/Y10/DATA/eaf20.csv", clear
keep pid e20bmi e20wgt e20wst1
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
insheet using "cardia/Y10/DATA/eaf11.csv", clear
keep pid e11momlv e11mage e11mdth e11mhage e11dadlv e11fage e11fdth e11fhage e11bdiab e11fdiab e11mdiab e11sdiab e11bstrk e11fstrk e11mstrk e11sstrk
sort pid
merge m:m pid using "pooled_data/cardia_yr10.dta"
drop _merge
sort pid
save "pooled_data/cardia_yr10.dta", replace
 

destring, replace force

replace cardid=pid

g gender="M" if sex==1 
replace gender="F" if sex==2 
g racegrp="B" if race==4
replace racegrp="W" if race==5

g id=int(pid/1000000)
g weight=e20wgt*0.45
g waist=e20wst1
g bmi=e20bmi

g pregnant = 1 if e08preg==2 
replace pregnant = 0 if e08preg==1

g smokstat = 3 if e10cigs==1 | e10tobac==1
replace smokstat = 2 if e10cigs==2 & e09smknw==1 
replace smokstat = 1 if e10cigs==2 & e09smknw==2
g cursmoke =1 if smokstat==1 
replace cursmoke = 0 if smokstat==2 | smokstat==3
g cigpday = e09cgtdy if cursmoke==1
replace cigpday = 0 if cursmoke==0 


g hyptmdsr=. if e08hbnow==8
replace hyptmdsr=0 if e08hbnow==1 
replace hyptmdsr=1 if e08hbnow==2 & e08hbp==2 
replace hyptmdsr=0 if e08hbnow==2 & e08hbp==1 


g cholmed1=0 if e08chnow==1
replace cholmed1=1 if e08chnow==2 

g fast08=0 if e05fstmn/60 <8 
replace fast08=1 if e05fstmn/60 >=8 
g fast12=0  if e05fstmn/60 <12 
replace fast12=1 if e05fstmn/60 >=12 
g glucose=el7glu*0.94359772+6.979619035
g sercreat=el7creat*0.999190397-0.095038646
g finsulin=el7ins
g glu2hr=el7glu2h

 
g age = ex5_age
g exmdate=e02date
g sysbp=e02avgsy
g diabp=e02avgdi
g heartrte=e02pulse*2



g diabt126=1 if (glucose >=200) | (glucose>=126 & fast08>=1) 
replace diabt126=0 if glucose>0 & glucose<126 
replace diabt126=. if glucose>=126 & glucose<200 & fast08<=0 


g diabogtt=1 if ((glucose >=200) | (glucose>=126 & fast08>=1) | glu2hr>=200)
replace diabogtt=0 if glucose>0 & glucose<126 & glu2hr>0 & glu2hr<200

g diabhx=1 if diabhx4==1 | diabt1264==1 | diabt126==1 | diabogtt==1
replace diabhx=0 if (diabhx4==0 | diabhx4==.) & (diabt1264==0 | diabt1264==.) & (diabt126==0 | diabt126==.)  & (diabogtt==0 | diabogtt==.) 


g study="CARD"
g exam=10

g totchol=el1chol
g trigly=el1ntrig
g hdlc=el1hdl
g ldlc=el1ldl

save "pooled_data/cardia_yr10.dta", replace
sort pid
merge m:m pid using  "pooled_data/cardia_yr7.dta"
drop _merge
sort pid
merge m:m pid using  "pooled_data/cardia_yr5.dta"
drop _merge
sort pid
merge m:m pid using  "pooled_data/cardia_yr2.dta"
drop _merge
sort pid
merge m:m pid using  "pooled_data/cardia_yr1.dta"
 
 
sort pid
quietly by pid: gen dup = cond(_N==1,0,_n)
tab dup
drop if dup>0

drop _merge
save "pooled_data/cardia.dta", replace


insheet using "cardia/DATA/eafuy11.csv", clear
sort pid
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/eafuy12.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/eafuy13.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/eafuy14.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/fafuy16.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/fafuy17.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/fafuy18.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/fafuy19.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/Y15/DATA/faf08.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
destring, replace force
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/Y20/DATA/gaf08.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
destring, replace force
save "pooled_data/cardia_events.dta", replace
insheet using "cardia/Y20/DATA/gaf21.csv", clear
sort pid
merge 1:1 pid using "pooled_data/cardia_events.dta"
drop _merge
destring, replace force
save "pooled_data/cardia_events.dta", replace

insheet using "cardia/DATA/yamorbid.csv", clear
sort pid
merge m:1 pid using "pooled_data/cardia_events.dta"

g mi = 1 if (fy132hak==1 & ytcnt=="FU132")| (fy144hak==1 & ytcnt=="FU144") | (fy156hak==1 & ytcnt=="FU156") | (fy168hak==1 & ytcnt=="FU168") | (fy192hak==1 & ytcnt=="FU192") | (fy204hak==1 & ytcnt=="FU204") | (fy216hak==1 & ytcnt=="FU216") | (fy228hak==1 & ytcnt=="FU228") | f08hrtak==1 | g08hrtak==1 | g21hrtatk==2
replace mi = 0 if mi!=1
g str = 1 if g21stroke==2
replace str = 0 if str!=1
g chddeath = .
g strdeath = .

* this is a date, not a "time until"
* we will subtract off the exam date at a later time.
g timetomi = fm132dat if fy132hak==1
replace timetomi = fm144dat if (fy144hak==1 & ytcnt=="FU144") & timetomi==.
replace timetomi = fm156dat if (fy156hak==1 & ytcnt=="FU156") & timetomi==.
replace timetomi = fm168dat if (fy168hak==1 & ytcnt=="FU168") & timetomi==.
replace timetomi = f08exdat if f08hrtak==1 & timetomi==.
replace timetomi = fm192dat if (fy192hak==1 & ytcnt=="FU192") & timetomi==.
replace timetomi = fm204dat if (fy204hak==1 & ytcnt=="FU204") & timetomi==.
replace timetomi = fm216dat if (fy216hak==1 & ytcnt=="FU216") & timetomi==.
replace timetomi = fm228dat if (fy228hak==1 & ytcnt=="FU228") & timetomi==.
replace timetomi = g08exdat if g08hrtak==1 & timetomi==.
replace timetomi = g21exdat if g21hrtatk==2  & timetomi==.
g timetostr = g21exdat if g21stroke==2

destring, replace force

g timetochddeath = .
g timetostrdeath = .

g censored = . 
g timecensored = 5 if censored==1

collapse (max) mi str chddeath strdeath (min) timetomi timetostr timetochddeath timetostrdeath censored timecensored, by(pid)


sort pid


destring, replace force

merge 1:1 pid using  "pooled_data/cardia.dta"

drop _merge

replace timetomi = (timetomi - e02date)/12 if !mi(timetomi)
replace timetostr = (timetostr - e02date)/12 if !mi(timetostr)
replace prevmi =  e08hrtak
replace prevmi = 0 if prevmi!=1
g prevstrk = e08tia
g prevchf = 1 if a09chf==8
replace prevchf = 0 if prevchf!=1
g prevproc = 1 if e08ancth==2
replace prevproc = 0 if prevproc!=1


save  "cardia.dta", replace


g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr==0
g rxsbp = sysbp if  hyptmdsr==1
by grp, sort: su id age totchol hdlc unrxsbp rxsbp hyptmdsr cursmoke diabt126  



save "pooled_data/cardia.dta", replace




* FRAMINGHAM

insheet using "framcohort/datasets/lex0_15.csv", clear
save "pooled_data/framorig.dta", replace
insheet using "framcohort/datasets/vr_diab_ex28_0_0601d.csv", clear
sort pid
merge 1:1 pid using "pooled_data/framorig.dta"
drop _merge
sort pid
save "pooled_data/framorig.dta", replace
destring, replace force


g age = fh53

g sex = fh54
g gender="M" if sex==1 
replace gender="F" if sex==2 

g racegrp="W" 


g weight=(fh62*5+100)*0.45
g bmi=weight/(fh63*.0254)^2

g cursmoke =1 if fh101==2
replace cursmoke = 0 if fh101!=2
g cigpday = fh102 if fh102<88
replace cigpday = 0 if fh102==88


g hyptmdsr = 1 if (fh86==1 | fh89==1) 
replace hyptmdsr = 0 if (fh86!=1 | fh89!=1)

g diabt126 = 1 if (dmrx15==1 | bg200_hx_diab15==1)
replace diabt126 = 0 if diabt126!=1

g cholmed1=1 if fh91==1 
replace cholmed1=0 if fh91!=1


 
g exmdate=1978-1951
egen sysbp=rowmean(fh68 fh70 fh72)
egen diabp=rowmean(fh69 fh71 fh73)
save "pooled_data/framorig.dta", replace


insheet using "framcohort/datasets/vr_survcvd1_2011_m_0785d.csv", clear


collapse (max) chd=chd chf=chf cvd=cvd stroke=stroke (min) chddate=chddate chfdate=chfdate cvddate=cvddate strokedate=strokedate , by(pid)
sort pid

g str = stroke
replace str = 0 if str!=1

g timetostr = strokedate/365.25-27


g timetochddeath = .
g timetostrdeath = .



merge 1:1 pid using "pooled_data/framorig.dta"
drop _merge

g study="FRAM"
g exam=15

g totchol=fh332 
g trigly=.
g hdlc=fh333
g ldlc=.


g prevstrk = 1 if timetostr<0

save "pooled_data/framorig.dta", replace


insheet using "framcohort/datasets/vr_afcum_2013_a_0026d.csv", clear
keep if idtype==0
g prevafib = 1 if rhythm=="1" & (ecgdate/365.25)<27

collapse (max) prevafib=prevafib , by(pid)
sort pid

merge 1:1 pid using "pooled_data/framorig.dta"
drop _merge
sort pid
save "pooled_data/framorig.dta", replace


insheet using "framcohort/datasets/vr_soe_2013_a_0550d.csv", clear
keep if idtype==0
sort pid
g prevchf = 1 if (event>40) & (date/365.25)<27
g prevproc = .

g mi = 1 if (event >= 1 & event <= 5)
replace mi = 0 if mi != 1
g chddeath = 1 if (event>=21 & event<=24)
replace chddeath = 0 if chddeath!=1
g strdeath = 1 if event==25
replace strdeath = 0 if strdeath!=1
g timetomi = (date/365.25)-27 if mi==1
g timetochddeath = (date/365.25)-27 if chddeath==1
g timetostrdeath = (date/365.25)-27 if strdeath==1


g prevmi=1 if timetomi<=0
replace prevmi=0 if prevmi!=1
g prevdeath = 1 if (timetochddeath<0) | (timetostrdeath<0)

g censored = 1 if event>=27 & event<=29
g timecensored = (date/365.25)-27 if censored==1

collapse (max) prevmi=prevmi prevchf=prevchf prevdeath=prevdeath prevproc=prevproc mi=mi chddeath=chddeath strdeath=strdeath censored=censored (min) timetomi=timetomi timetochddeath=timetochddeath timetostrdeath=timetostrdeath  timecensored=timecensored, by(pid)

merge 1:1 pid using "pooled_data/framorig.dta"
drop _merge
sort pid
save "pooled_data/framorig.dta", replace

* define endpoint

use "pooled_data/framorig.dta", clear

drop if idtype!=0

g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr==0
g rxsbp = sysbp if  hyptmdsr==1
by grp, sort: su id age totchol hdlc unrxsbp rxsbp hyptmdsr cursmoke diabt126  


save "pooled_data/framorig.dta", replace







* FRAMINGHAM OFFSPRING

* merge datasets:  first (1971 to 1975) examination cycle
insheet using "framoffspring/datasets/lex1_1.csv", clear
destring, replace force
sort pid
save "pooled_data/framoff.dta", replace
insheet using "framoffspring/datasets/ldia1_7.csv", clear
destring, replace force
sort pid
merge 1:1 pid using "pooled_data/framoff.dta"
drop _merge
sort pid
save "pooled_data/framoff.dta", replace

g age = age1

g sex = a3
g gender = "M" if sex==1
replace gender = "F" if sex==2

g racegrp = "W"

g weightkg = (5*wgtgp1+97.5)*.453692
g heightm = (hgt1*0.0254)
g bmi = weightkg/(heightm^2)

g diabt126 = diab1

g cursmoke = 1 if a99==1 
replace cursmoke = 0 if cursmoke!=1

g cholmed1 = 1 if a80==1
replace cholmed1 = 0 if a80!=1

g exmdate = 0

egen sysbp=rowmean(a53 a55 a57)
egen diabp=rowmean(a54 a56 a58)

g hyptmdsr = 1 if (a78==4 | a79==7)
replace hyptmdsr = 0 if hyptmdsr!=1

sort pid
save "pooled_data/framoff.dta", replace


insheet using "framoffspring/datasets/vr_survcvd1_2011_m_0785d.csv", clear

collapse (max) chd=chd chf=chf cvd=cvd stroke=stroke (min) chddate=chddate chfdate=chfdate cvddate=cvddate strokedate=strokedate , by(pid)
sort pid

g str = stroke
replace str = 0 if str!=1





g timetostr = strokedate/365.25


g timetochddeath = .
g timetostrdeath = .



merge 1:1 pid using "pooled_data/framoff.dta"
drop _merge
sort pid
save "pooled_data/framoff.dta", replace

g study="FRAMOFF"
g exam=1



g totchol=a9 
g trigly=a13
g hdlc=a10
g ldlc=a12


g prevstrk = 1 if timetostr<exmdate
save "pooled_data/framoff.dta", replace


insheet using "framoffspring/datasets/vr_afcum_2013_a_0026d.csv", clear
keep if idtype==1
g prevafib = 1 if rhythm=="1" & (ecgdate/365.25)<0

collapse (max) prevafib=prevafib , by(pid)
sort pid
merge 1:1 pid using "pooled_data/framoff.dta"



insheet using "framoffspring/datasets/vr_soe_2013_a_0550d.csv", clear
keep if idtype==1
sort pid
g prevchf = 1 if (event>40) & (date/365.25)<=0
g prevproc = .
g prevmi =  1 if (event >= 1 & event <= 5) & (date/365.25)<=0

g mi = 1 if (event >= 1 & event <= 5)
replace mi = 0 if mi!=1
g chddeath = 1 if (event>=21 & event<=24)
replace chddeath = 0 if chddeath!=1
g strdeath = 1 if event==25
replace strdeath = 0 if strdeath!=1
g timetomi = date/365.25 if mi==1
g timetochddeath = (date/365.25) if chddeath==1
g timetostrdeath = (date/365.25) if strdeath==1

g censored =1 if event>=27 & event<=29
g timecensored = date/365.25 if censored == 1

collapse (max) prevchf=prevchf prevproc=prevproc prevmi=prevmi chddeath=chddeath strdeath=strdeath censored=censored mi=mi (min) timetochddeath=timetochddeath timetostrdeath=timetostrdeath  timecensored=timecensored timetomi=timetomi, by(pid)
merge 1:1 pid using "pooled_data/framoff.dta"
drop _merge
sort pid
save "pooled_data/framoff.dta", replace




* define endpoint

use "pooled_data/framoff.dta", clear

g grp = 1 if racegrp=="B" & gender=="F"
replace grp =2 if  racegrp=="W" & gender=="F"
replace grp =3 if  racegrp=="B" & gender=="M"
replace grp =4 if  racegrp=="W" & gender=="M"

g unrxsbp = sysbp if hyptmdsr==0
g rxsbp = sysbp if  hyptmdsr==1

*g study = "FRAMOFF"



by grp, sort: su id age totchol hdlc unrxsbp rxsbp hyptmdsr cursmoke diabt126  


save "pooled_data/framoff.dta", replace




* event rate check

use "pooled_data/aric.dta", clear
g study="ARIC"
save "pooled_data/aric.dta", replace
use "pooled_data/chs.dta", clear
g study="CHS"
append using "pooled_data/aric.dta"
save "pooled_data/pooledcohs.dta", replace

use "pooled_data/cardia.dta", clear


*g grp = 1 if racegrp=="B" & gender=="F"
*replace grp =2 if  racegrp=="W" & gender=="F"
*replace grp =3 if  racegrp=="B" & gender=="M"
*replace grp =4 if  racegrp=="W" & gender=="M"

*g unrxsbp = sysbp if hyptmdsr==0
*g rxsbp = sysbp if  hyptmdsr==1
save "pooled_data/cardia.dta", replace
append using "pooled_data/pooledcohs.dta"

append using "pooled_data/framoff.dta"
append using "pooled_data/framorig.dta"

save "pooled_data/pooledcohs.dta", replace

ta study grp


use "pooled_data/pooledcohs.dta", clear
g studytime = 12

g ascvd = 	1 if (mi==1 | str==1 | chddeath==1 | strdeath==1)
replace ascvd = 0 if ascvd!=1

egen timetoascvd = rowmin(timetomi timetostr timetochddeath timetostrdeath)

replace studytime = timetoascvd if ascvd==1
replace studytime = 0 if ascvd==1 & timetoascvd>10
replace ascvd = 0 if ascvd==1 & timetoascvd>10
stset studytime ascvd
stdescribe

stsum, by(grp study)



