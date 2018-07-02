//Objective: Clean Pith Moromo for to analyze predictors of longitudinal geophagy
//Author: Joshua Miller

//////////////////////////////////////////////
** Sociodemographics
//////////////////////////////////////////////

	*-------------------------------*
	// Participant ID //
	*-------------------------------*
	
codebook pid
drop if pid==.
replace pid=726 if pid==7260
	
	
	*-------------------------------*
	// Site //
	*-------------------------------*

drop site
generate site=.
replace site=1 if (pid>=100) & (pid<=199)
replace site=1 if (pid>=10000 & pid<=10070)
replace site=2 if (pid>=200) & (pid<=299)
replace site=2 if (pid>=20000) & (pid<=20046)
replace site=3 if (pid>=300) & (pid<=399)
replace site=3 if (pid>=30000) & (pid<=30011)
replace site=4 if (pid>=400) & (pid<=499)
replace site=4 if (pid>=40000) & (pid<=40005)
replace site=5 if (pid>=500) & (pid<=599) 
replace site=6 if (pid>=600) & (pid<=699)
replace site=7 if (pid>=700) & (pid<=799)
label define site 1 "KEDH" 2 "Nyahera" 3 "Rongo" 4 "Ongo" 5 "Nyamaraga" 6 "Macalder" 7 "Migori"
label values site site

	*-------------------------------*
	// Visit //
	*-------------------------------*
	
drop if visit==3 | visit==6
recode visit (4=3) (5=4) (7=5) (8=6) (9=7) (10=8) (11=9)
label define visit 1 "Vist 1" 2 "Vist 2" 3 "Vist 3" 4 "Vist 4" 5 "Vist 5" 6 "Vist 6"  7 "Vist 7" 8 "Vist 8" 9 "Vist 9"
label values visit visit

	*-------------------------------*
	// Interview date //
	*-------------------------------*
	
* Visit 1
split idate if visit==1, g(inter) p("/")
forv j = 1/2 {
	replace inter`j' = "0" + inter`j' if length(inter`j') < 2
}
replace inter3 = "20" + inter3
gen tmp = inter1 + inter2 + inter3
gen idate1 = date(tmp, "MDY")
form idate1 %d
drop tmp* inter*

* Visit 2 
split idate if visit==2, g(inter) p("/")
forv j = 1/2 {
	replace inter`j' = "0" + inter`j' if length(inter`j') < 2
}
replace inter3 = "20" + inter3
gen tmp = inter1 + inter2 + inter3
gen idate2 = date(tmp, "MDY")
form idate2 %d
drop tmp* inter*

* Visit 3
split idate if visit==3, g(inter) p("/")
forv j = 1/2 {
	replace inter`j' = "0" + inter`j' if length(inter`j') < 2
}

replace inter3 = "20" + inter3
gen tmp = inter1 + inter2 + inter3
gen idate3 = date(tmp, "MDY")
form idate3 %d
drop tmp* inter*

* Visit 4
split idate if visit==4, g(inter) p("/")
forv j = 1/2 {
	replace inter`j' = "0" + inter`j' if length(inter`j') < 2
}

replace inter3 = "20" + inter3
gen tmp = inter1 + inter2 + inter3
gen idate4 = date(tmp, "MDY")
form idate4 %d
drop tmp* inter*

* Visit 5
split idate if visit==5, g(inter) p("/")
forv j = 1/2 {
	replace inter`j' = "0" + inter`j' if length(inter`j') < 2
}

replace inter3 = "20" + inter3
gen tmp = inter1 + inter2 + inter3
gen idate5 = date(tmp, "MDY")
form idate5 %d
drop tmp* inter*

* Visit 6 
gen idate61=idate if visit==6
replace idate61="2-Jan-16" if idate61=="2-Jan-70"
gen idate6=date(idate, "DMY", 2016)
form idate6 %d
drop idate61
replace idate6=. if idate6<10000

* Visit 7
gen idate7 = date(idate, "MDY", 2016) if visit==7
form idate7 %d

* Visit 8 
gen idatea = subinstr(idate, " 00:00:00 UTC", ",", .) if visit==8
replace idatea=subinword(idatea,"Tue","",1)
replace idatea=subinword(idatea,"Mon","",1)
replace idatea=subinword(idatea,"Wed","",1)
replace idatea=subinword(idatea,"Thu","",1)
replace idatea=subinword(idatea,"Fri","",1)
replace idatea=subinword(idatea,"Sat","",1)
replace idatea=subinword(idatea,"Sun","",1)
replace idatea="." if idatea=="Jan 02, 1970"
gen idate8=date(idatea, "MDY", 2016)
form idate8 %d
drop idatea 
replace idate8=. if idate8<10000

* Visit 9
gen idatea = subinstr(idate, " 00:00:00 UTC", ",", .) if visit==9
replace idatea=subinword(idatea,"Tue","",1)
replace idatea=subinword(idatea,"Mon","",1)
replace idatea=subinword(idatea,"Wed","",1)
replace idatea=subinword(idatea,"Thu","",1)
replace idatea=subinword(idatea,"Fri","",1)
replace idatea=subinword(idatea,"Sat","",1)
replace idatea=subinword(idatea,"Sun","",1)
replace idatea="." if idatea=="Jan 02, 1970"
gen idate9=date(idatea, "MDY", 2016)
form idate9 %d
drop idatea

* Merge into one variable
drop idate
gen idate=.
foreach x in 1 2 3 4 5 6 7 8 9 {
replace idate=idate`x' if visit==`x'
}
form idate %d


	*-------------------------------*
	// Tribe //
	*-------------------------------*

bysort pid: egen tribe=max(sd1a)
recode tribe (3=2) (4=2) (5=2) (6=2) (7=2) (8=2) (9=2) (10=2) (11=2) (12=2)
label define tribe 1 "Luo" 2 "Minority tribe"
label values tribe tribe


	*-------------------------------*
	// Religion //
	*-------------------------------*
	
bysort pid: egen religion=max(sd2a)
recode religion (6=4) (7=4) (8=3) (9=4) (10=4) (11=4) (12=4) (13=4) (14=4) (15=4) (16=4) (17=4) (18=4)
label define relig 1 "Catholic" 2 "Seventh Day Adventist" 3 "Protestant" 4 "Other - Christian" 5 "African Traditional"
label values religion relig 


	*-------------------------------*
	// Relationship status //
	*-------------------------------*

bysort pid: egen relatstat=max(sd7)
replace relatstat=atd1 if relatstat!=atd1 & atd1!=. 
recode relatstat (2=1) (3=1) (4=2) (5=2)
label define relat 1 "Single" 2 "Married" 
label values relatstat relat


	*-------------------------------*
	// Maternal age //
	*-------------------------------*
	
bysort pid: egen matage1=max(matage)
drop matage 
rename matage1 matage 


	*-------------------------------*
	// Household location  //
	*-------------------------------*
	
gen urban = .
replace urban = 1 if (pid>=100 & pid<=199) & pid!=.
replace urban = 1 if (pid>=10000 & pid<=19999) & pid!=.
replace urban = 0 if (pid>=200 & pid<=10000) & pid!=.
replace urban = 0 if pid>=19999 & pid!=.
label variable urban "Urban residence"
gen periurban = .
replace periurban = 1 if pid>=700 & pid<=799 & pid!=.
replace periurban = 1 if pid>=70000 & pid<=79999 & pid!=.
replace periurban = 1 if pid>=200 & pid<=399 & pid!=.
replace periurban = 1 if pid>=20000 & pid<=39999 & pid!=.
replace periurban = 0 if pid>=400 & pid<=699 & pid!=.
replace periurban = 0 if pid>=40000 & pid<=69999 & pid!=.
replace periurban = 0 if pid<=199 & pid!=.
replace periurban = 0 if pid>=10000 & pid<=19999 & pid!=.
label variable periurban "Peri-urban residence"
gen rural = .
replace rural = 1 if pid>=400 & pid<=699 & pid!=.
replace rural = 1 if pid>=40000 & pid<=69999 & pid!=.
replace rural = 0 if pid<=399 & pid!=.
replace rural = 0 if pid>=700 & pid<=39999 & pid!=.
replace rural = 0 if pid>=70000 & pid!=.
label variable rural "Rural residence"
gen hhloc=.
replace hhloc=0 if rural==1
replace hhloc=1 if periurban==1
replace hhloc=2 if urban==1
label define hhloc1 0"Rural" 1"Urban" 2"Per-Urban"
label values hhloc hhloc1
label variable hhloc "Urban, rural or peri-urban residence"


	*-------------------------------*
	// HIV status  //
	*-------------------------------*

codebook posneg
codebook status
	
	
	*-------------------------------*
	// Asset/wealth index  //
	*-------------------------------*

* STEP 2: define 0/1 variables for housing characteristics and each type of livestock owned

** LIGHT SOURCE
tab hha16, missing
gen light_improved = .
replace light_improved = 1 if (hha16 == 2 | hha16 == 1) & hha16 != . 
replace light_improved = 0 if hha16 != 2 & hha16!= 1 & hha16 != .


** FLOOR TYPE
tab hha17
* only 0.27% has cement --> going to define as "manmade" (cement/carpet) v. other (earth/dung)
gen floor_improved = .
replace floor_improved = 1 if (hha17==7 | hha17==8) & hha17!=.
replace floor_improved = 0 if hha17==1 & hha17!=.


** ROOF TYPE
tab hha18

gen roof_natural = . 
replace roof_natural = 1 if (hha18 == 1 | hha18 == 2) & hha18 != .
replace roof_natural = 0 if hha18 != 1 & hha18 != 2 & hha18 != .

gen roof_metal = .
replace roof_metal = 1 if (hha18 == 3 | hha18 == 5) & hha18 != .
replace roof_metal = 0 if hha18 != 3 & hha18 != 5 & hha18 != .

gen roof_improved = .
replace roof_improved = 1 if (roof_metal==1) & roof_metal!=. 
replace roof_improved = 0 if roof_natural==1 & roof_natural!=.


** COOKING FUEL
tab hha20
	* gas and keronsene are 1.85% and 3.33%, respectively --> going to lump together
gen fuel_gas = .
replace fuel_gas = 1 if (hha20 == 2 | hha20 == 4) & hha20 != .
replace fuel_gas = 0 if hha20 != 2 & hha20 != 4 & hha20 != .

gen fuel_charcoal = .
replace fuel_charcoal = 1 if hha20 == 6 & hha20 != .
replace fuel_charcoal = 0 if hha20 != 6 & hha20 != .

gen fuel_wood = .
replace fuel_wood = 1 if hha20 == 7 & hha20 != .
replace fuel_wood = 0 if hha20 != 7 & hha20 != .

gen cook_improved = 0
replace cook_improved = 1 if (fuel_gas==1 | hha20==1)


* Define variables to include 

global xlist hha1 hha2 hha3 hha4 hha6 hha7 hha8 hha9 hha10 hha11 hha13 hha14 hhdvd hhclock hhgener hhhouse hhothbldg hhland hhcar hha21 light_improved floor_improved roof_improved cook_improved
global id pid

* Examine data

describe $xlist
summarize $xlist
corr $xlist

* Look at covariance, eliminating cases with missing values --> assumes unidimensional scale

alpha $xlist if visit==5, item casewise
	* overall alpha: 0.7370
	* hha7, hha8, hha10, hha11, hhgener, hhothbldg, hhcar, roof_improved are low
	
global xlist_reduced hha1 hha2 hha3 hha4 hha6 hha9 hha13 hha14 hhdvd hhclock hhhouse hhland hha21 light_improved floor_improved cook_improved

alpha $xlist_reduced if visit==5, item casewise
	* overall alpha: 0.7687

	
* Principal component analysis (PCA)
pca $xlist_reduced if visit==5

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)
	* examine as 2, 3, and 5 factors (arguments for each based on scree plot) 

* PCA based on screeplot interpretation 
pca $xlist_reduced if visit==5, mineigen(1)
pca $xlist_reduced if visit==5, factor(2)
pca $xlist_reduced if visit==5, factor(3)
pca $xlist_reduced if visit==5, factor(3) blanks(.3)
	* three factors appears to be sufficient 

* Component rotations
rotate, varimax
rotate, varimax blanks(.3)
rotate, clear

rotate, promax
rotate, promax blanks(.3)
rotate, clear

* Scatter plots of the loadings and score variables
loadingplot
scoreplot
scoreplot, mlabel($id)

* Create asset index variable 
predict asset_index if visit==5

* Create wealth tertiles (3 = wealthiest)
xtile wealth = asset_index, nq(3)


* Create asset index for only consumer goods (to potentially improve robustness since flooring, lighting, land, etc. may depend on location (i.e. rural vs. urban)) 

global xlist2 hha1 hha2 hha3 hha4 hha6 hha9 hha13 hha14 hhdvd hhclock


pca $xlist2  if visit==5

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

pca $xlist2  if visit==5, mineigen(1)
	* greater cumulative variance accounted for (0.453 for xlist_reduced vs. 0.502)

* Create second asset index variable 

predict asset_index2  if visit==5

xtile wealth2 = asset_index2, nq(3)

* Higher value means greater wealth

drop asset_index wealth
bysort pid: egen asset_index=max(asset_index2)
bysort pid: egen wealth=max(wealth2)
drop asset_index2 wealth2


	*-------------------------------*
	// Seasonality //
	*-------------------------------*

gen month=month(idate)
format month %ty
gen day=day(idate)
format day %ty

	* 1. "Rough" metric based solely on month interview and reported wet/dry seasons

gen season=.
replace season=0 if (month==1 | month==2 | month==3 | month==12) & month!=.
replace season=1 if (month==6 | month==7 | month==9 | month==10 | month==11) & month!=.
replace season=2 if (month==4 | month==5 | month==8) & month!=. 
label define season 0 "Dry" 1 "Not wet nor dry" 2 "Wet" 
lab values season season 

	* 2. "Rough" metric based solely on month interview and meterological criteria for dry season (<60mm rainfaill)

gen season_drywet=.
replace season_drywet=0 if (month==1 | month==2 | month==6 | month==7 | month==8 | month==9 | month==12) & month!=.
replace season_drywet=1 if  season_drywet==. & month!=.

	* 3. Lagged based on annual rainfall averages & meterological criteria (<60mm is dry season); continuous scale accounting for amount of time in each month
	
gen month_prev = month-1
replace month_prev=12 if month_prev==0


gen season_curr=.
replace season_curr=0 if (month==1 | month==2 | month==6 | month==7 | month==8 | month==9 | month==12) & month!=.
replace season_curr=1 if  season_curr==. & month!=.

gen season_prev=. 
replace season_prev=0 if (month_prev==1 | month_prev==2 | month_prev==6 | month_prev==7 | month_prev==8 | month_prev==9 | month_prev==12) & month_prev!=.
replace season_prev=1 if  season_prev==. & month_prev!=.

gen ratio=. 
replace ratio=day/28 if day!=.

gen season_score=.
replace season_score=(ratio*season_curr)+((1-ratio)*season_prev) if ratio!=. & ratio<1
replace season_score=season_curr if ratio>=1 & ratio!=.


	
//////////////////////////////////////////////
** Pregnancy
//////////////////////////////////////////////

	*-------------------------------*
	// Current pregnancy //
	*-------------------------------*
	
gen preg=.
replace preg=1 if gestation!=0 & gestation!=. & visit==1
replace preg=1 if gestation!=0 & gestation!=. & visit==2
replace preg=0 if gestation==. & visit==2
replace preg=0 if visit==3
replace preg=0 if visit==4
replace preg=0 if visit==5
replace preg=1 if (pid==102 | pid==108 | pid==114) & visit==5

gen preg6=mp1 if visit==6
replace preg6=0 if preg6==999

replace preg=preg6 if visit==6

gen preg7=mpm1 if visit==7
replace preg7=0 if preg7==999
replace preg7=1 if pid==307 | pid==528 | pid==558| pid==645 | pid==10038 | pid==10065 | pid==40003 | pid==471

replace preg=preg7 if visit==7


gen preg8=mpm1 if visit==8
replace preg8=0 if preg8==999
replace preg8=1 if pid==393 | pid==528 | pid==558 | pid==654 | pid==10038 | pid==40003 | pid==103 | pid==307 | pid==501 | pid==524 | pid==528 | pid==558 | pid==603 | pid==713 | pid==10038

replace preg=preg8 if visit==8

gen preg9=mpm1 if visit==9
replace preg9=0 if preg9==999
replace preg9=1 if pid==501 | pid==558 | pid==10065 | pid==30008 | pid==216

replace preg=preg9 if visit==9


	*-------------------------------*
	// Gestational age //
	*-------------------------------*
	
codebook gestation
* clean

	*-------------------------------*
	// Gravidity  //
	*-------------------------------*

bysort pid: egen gravidity=max(oh9)


	*-------------------------------*
	// Parity  //
	*-------------------------------*
	
bysort pid: egen parity=max(oh10a)

	
	*-------------------------------*
	// Number of miscarriages  //
	*-------------------------------*
	
bysort pid: egen miscarriage=max(oh10c)

	


//////////////////////////////////////////////
** Dietary Recall
//////////////////////////////////////////////

	*-------------------------------*
	// Iron-rich foods  //
	*-------------------------------*
	
gen organ=.
replace organ=1 if dd11==1 | dd12==1
replace organ=0 if dd11==0 & dd12==0
gen meat=.
replace meat=1 if dd14==1 | dd15==1 | dd16==1 | dd17==1 | dd18==1
replace meat=0 if dd14==0 & dd15==0 & dd16==0 & dd17==0 & dd18==0
rename dd20 egg
gen milk=.
replace milk=1 if dd21==1 | dd22==1 | dd23==1
replace milk=0 if dd21==0 & dd22==0 & dd23==0
gen fefood=(organ+meat)
gen fefood_yn=.
replace fefood_yn=0 if fefood==0
replace fefood_yn=1 if fefood>=1 & fefood!=.


	*-------------------------------*
	// Iron supplement  //
	*-------------------------------*

replace hsb5=hsb5o if hsb5==. & hsb5o!=.
replace hsb5y=hsb5t if hsb5y==. & hsb5t!=.
rename hsb5y fepill
replace fepill=0 if hsb5==0 | hsb5==999
replace fepill=0 if fepill==. & hsb5==1



//////////////////////////////////////////////
** Psychosocial Health
//////////////////////////////////////////////

	*-------------------------------*
	// Social status  //
	*-------------------------------*

* community
rename sss1 socialstat_comm

* Kenya
rename sss2 socialstat_country


	*-------------------------------*
	// Depression  //
	*-------------------------------*

gen depscore=.
foreach x in 1 3 5 7 9 {
gen mdep4a=. if visit==`x'
replace mdep4a=0 if mdep4==3 & mdep4!=.
replace mdep4a=1 if mdep4==2 & mdep4!=.
replace mdep4a=2 if mdep4==1 & mdep4!=.
replace mdep4a=3 if mdep4==0 & mdep4!=.
gen mdep8a=. if visit==`x'
replace mdep8a=0 if mdep8==3 & mdep8!=.
replace mdep8a=1 if mdep8==2 & mdep8!=.
replace mdep8a=2 if mdep8==1 & mdep8!=.
replace mdep8a=3 if mdep8==0 & mdep8!=.
gen mdep12a=. if visit==`x'
replace mdep12a=0 if mdep12==3 & mdep12!=.
replace mdep12a=1 if mdep12==2 & mdep12!=.
replace mdep12a=2 if mdep12==1 & mdep12!=.
replace mdep12a=3 if mdep12==0 & mdep12!=.
gen mdep16a=. if visit==`x'
replace mdep16a=0 if mdep16==3 & mdep16!=.
replace mdep16a=1 if mdep16==2 & mdep16!=.
replace mdep16a=2 if mdep16==1 & mdep16!=.
replace mdep16a=3 if mdep16==0 & mdep16!=.
replace depscore=(mdep1 + mdep2 + mdep3 + mdep4a + mdep5 + mdep6 + mdep7 + mdep8a + mdep9 + mdep10 + mdep11 + mdep12a + mdep13 + mdep14 + mdep15 + mdep16a + mdep17 + mdep18 + mdep19 + mdep20) if mdep1!=. & mdep2!=. & mdep3!=. & mdep4a!=. & mdep5!=. & mdep6!=. & mdep7!=. & mdep8a!=. & mdep9!=. & mdep10!=. & mdep11!=. & mdep12a!=. & mdep13!=. & mdep14!=. & mdep15!=. & mdep16a!=. & mdep17!=. & mdep18!=. & mdep19!=. & mdep20!=.
drop mdep4a mdep8a mdep12a mdep16a
}
gen dep_cat=.
replace dep_cat=0 if depscore<=16 & depscore!=.
replace dep_cat=1 if depscore>16 & depscore!=.
label define dep 0 "No depression" 1 "Likely depression" 
label values dep_cat dep


	*-------------------------------*
	// Stress  //
	*-------------------------------*
	
recode mstr4 mstr5 mstr7 mstr8 (0=4) (1=3) (2=2) (3=1) (4=0), generate(mstr4a mstr5a mstr7a mstr8a)
gen stress =.
replace stress=(mstr1+mstr2+mstr3+mstr4a+mstr5a+mstr6+mstr7a+mstr8a+mstr9+mstr10) if mstr1!=. & mstr2!=. & mstr3!=. & mstr4a!=. & mstr5a!=. & mstr6!=. & mstr7a!=. & mstr8a!=. & mstr9!=. & mstr10!=. 


	*-------------------------------*
	// Social support  //
	*-------------------------------*

gen socialsupp=.
replace socialsupp=ss1+ss2+ss3+ss4+ss5+ss6+ss7+ss8+ss9+ss10



//////////////////////////////////////////////
** Water Quality
//////////////////////////////////////////////

	*-------------------------------*
	// Primary water source  //
	*-------------------------------*

rename wat1 primary_wat

gen wat_improve=.
replace wat_improve=1 if primary_wat==1 | primary_wat==2 | primary_wat==3 | primary_wat==4 | primary_wat==5 | primary_wat==7 | primary_wat==9
replace wat_improve=0 if primary_wat==6 | primary_wat==8 | primary_wat==10 | primary_wat==11 | primary_wat==12 | primary_wat==13 | primary_wat==14 | primary_wat==15 | primary_wat==16


	*-------------------------------*
	// Water treatment  //
	*-------------------------------*
	
rename wattx wat_tx
replace wat_tx=. if wat_tx==999



//////////////////////////////////////////////
** Pica
//////////////////////////////////////////////

	*-------------------------------*
	// Past 24 hours  //
	*-------------------------------*
	
rename dd70 geo
rename dd71 other
rename dd72 amylo
gen pica_yn=.
replace pica_yn=1 if geo==1 | amylo==1 | other==1
replace pica_yn=0 if geo==0 & amylo==0 & other==0


	*-------------------------------*
	// Childhood recall  //
	*-------------------------------*

bysort pid: egen geo_child=max(pica1)
replace geo_child=. if geo_child==999
bysort pid: egen other_child=max(pica2)
replace other_child=. if other_child==999
bysort pid: egen amylo_child=max(pica3)
replace amylo_child=. if amylo_child==999
gen pica_child=.
replace pica_child=1 if amylo_child==1 | geo_child==1 | other_child==1
replace pica_child=0 if amylo_child==0 & geo_child==0 & other_child==0


//////////////////////////////////////////////
** Food Insecurity
//////////////////////////////////////////////

	*-------------------------------*
	// FIAS & HHS  //
	*-------------------------------*

* create FIAS score
foreach x in fi2 fi3 fi4 fi5 fi6 fi8 fi9 fi10 fi11 {
recode `x' (9=.)
}
gen fias=.
replace fias=(fi2 + fi3 +fi4 + fi5 + fi6 + fi8 + fi9 + fi10 + fi11) if  fi2~=. & fi3~=. & fi4~=. & fi5~=. & fi6~=. & fi8~=. & fi9~=. & fi10~=. & fi11~=. 

* create tertiles
foreach x in 1 2 3 4 5 6 7 8 9 {
xtile fi_score`x'=fias if visit==`x', n(3)
}

gen fi_score=. 
foreach x in 1 2 3 4 5 6 7 8 9 {
replace fi_score=fi_score`x' if visit==`x'
}

foreach x in 1 2 3 4 5 6 7 8 9 {
drop fi_score`x'
}
rename fi_score fi_tert
	
* HHS
gen fi9a=.
replace fi9a=0 if fi9==0
replace fi9a=1 if fi9==1 | fi9==2
replace fi9a=2 if fi9==3
lab define hhsb 0"Never" 1"rarely or sometimes" 2"Often" 
lab values fi9a hhsb
lab variable fi9a "Recode of FI9 for HHS"
gen fi10a=.
replace fi10a=0 if fi10==0
replace fi10a=1 if fi10==1 | fi10==2
replace fi10a=2 if fi10==3
lab values fi10a hhsb
lab variable fi10a "Recode of FI10 for HHS"
gen fi11a=.
replace fi11a=0 if fi11==0 
replace fi11a=1 if fi11==1 | fi11==2
replace fi11a=2 if fi11==3
lab values fi11a hhsb
lab variable fi11a "Recode of FI11 for HHS"
bysort pid: gen hhs= (fi9a + fi10a + fi11a)
lab variable hhs "Household hunger scale (0-6)"



//////////////////////////////////////////////
** Physical Health
//////////////////////////////////////////////

	*-------------------------------*
	// Hemoglobin  //
	*-------------------------------*
	
* Visit 2
rename mm16_649 hemo2
replace hemo2=. if hemo2==0 | hemo2==999
rename mm16_650 hemo_da
split hemo_da, g(hem) p("/")
forv j = 1/2 {
	replace hem`j' = "0" + hem`j' if length(hem`j') < 2
}
replace hem3 = "20" + hem3
drop hemo_da
gen tmp = hem1 + hem2 + hem3
gen hemo_date2 = date(tmp, "MDY")
form hemo_date2 %d
drop tmp* hem1 hem2 hem3

* Visit 5
gen hemo5=mm16a if visit==5
replace hemo5=. if hemo5==0
gen hemo_da=mm16b if visit==5
split hemo_da,g(hem) p("/")
forv j = 1/2 {
	replace hem`j' = "0" + hem`j' if length(hem`j') < 2
}
replace hem3 = "20" + hem3
drop hemo_da
gen tmp = hem1 + hem2 + hem3
gen hemo_date5 = date(tmp, "MDY")
form hemo_date5 %d
drop tmp* hem1 hem2 hem3

* Visit 6
gen hemo6=mm16c if visit==6
replace hemo6=. if hemo6==0
gen hemo_da=mm16b if visit==6
gen hemo_date6=date(hemo_da, "DMY", 2016)
form hemo_date6 %d
split hemo_da, g(hem) p("-")
	* year 2020 means date was unknown
replace hemo_date6=. if hem3=="20"
drop hemo_da hem1 hem2 hem3

* Visit 7
gen hemo7=mm16c if visit==7
replace hemo7=mm16f if (pid==718 | pid==10038) & visit==7
replace hemo7=. if hemo7==999 | hemo7==0
gen hemo_da=mm16b if visit==7
gen hemo_date7=date(hemo_da, "MDY", 2016)
form hemo_date7 %d
split hemo_da, g(hem) p(",")
	* year 2020 means date was unknown
destring hem2, replace
replace hemo_date7=. if hem2==2020 | hem2==2019
drop hemo_da hem1 hem2

* To compare values
bysort pid: egen hemo55=max(hemo5)
bysort pid: egen hemo22=max(hemo2)

replace hemo7=hb_mother if visit==7 & hemo7==hemo55 & hb_mother!=.
replace hemo_date7=idate7 if hemo7==hb_mother

* Visit 8
gen hemo8=hbtestmoth if visit==8
replace hemo8=. if hemo8==0
gen hemo_date8=idate8 if hemo8!=.
replace hemo8=mm16f if pid==114 & visit==8

* Visit 9
gen hemo9=hbtestmoth if visit==9
replace hemo9=. if hemo9==0
gen hemo_date9=idate9 if hemo9!=.

* Clean up, remove duplicate measurements
bysort pid: egen hemo_date55=max(hemo_date5)
bysort pid: egen hemo_date22=max(hemo_date2)
form hemo_date55 hemo_date22 %d

list pid if hemo_date55==hemo_date22 & visit==5
replace hemo5=. if hemo_date55==hemo_date22 
list pid if hemo55==hemo22 & visit==5 & hemo55!=. & hemo22!=. 
replace hemo5=. if hemo55==hemo22 

drop hemo55
bysort pid: egen hemo55=max(hemo5)

bysort pid: egen hemo66=max(hemo6)
bysort pid: egen hemo77=max(hemo7)
bysort pid: egen hemo88=max(hemo8)
bysort pid: egen hemo99=max(hemo9)


list pid if hemo66==hemo55 & visit==6 & hemo66!=. 
* permissible 
list pid if hemo77==hemo55 & visit==7 & hemo77!=. 
* permissible 
list pid if hemo77==hemo88 & visit==8 & hemo77!=.
* permissible
list pid if hemo99==hemo88 & visit==9 & hemo99!=.
* permissible 

gen hemo=. 
foreach x in 2 5 6 7 8 9 {
replace hemo=hemo`x' if visit==`x'
}
tab pid visit if hemo<2
replace hemo=13.1 if pid==334 & visit==8
replace hemo=14.1 if pid==333 & visit==8

* Anemia
gen anemic=.
replace anemic=1 if hemo<11 & hemo!=. & preg==1
replace anemic=0 if hemo>=11 & hemo!=. & preg==1
replace anemic=1 if hemo<12 & hemo!=. & preg==0
replace anemic=0 if hemo>=12 & hemo!=. & preg==0


	*-------------------------------*
	// GI distress  //
	*-------------------------------*
	
rename msx3 nausea
replace nausea=. if nausea==999 
rename msx4 diar
replace diar=. if diar==999 
gen gidis_yn=.
replace gidis_yn=0 if nausea==0 & diar==0
replace gidis_yn=1 if nausea==1 | diar==1


//////////////////////////////////////////////
** Keep Relevant Variables
//////////////////////////////////////////////

keep pid idate visit site relig relatstat tribe matage hhloc posneg status asset_index wealth season* preg gestation gravidity parity miscarriage fefood* fepill socialstat* dep* stress socialsupp primary_wat wat_improve wat_tx geo* other* amylo* pica_yn pica_child fias fi_tert hhs hemo anemic nausea diar gidis_yn
order pid visit idate season* site hhloc posneg status matage gestation preg gravidity parity miscarriage relatstat tribe relig asset_index wealth fepill fefood* socialstat* dep* stress socialsupp primary_wat wat_improve wat_tx geo amylo other pica_yn geo_child amylo_child other_child pica_child fias fi_tert hhs hemo anemic nausea diar gidis_yn
drop season_curr season_prev
sort pid visit

replace depscore=. if visit==2 | visit==4 | visit==6 | visit==8
replace dep_cat=. if visit==2 | visit==4 | visit==6 | visit==8
replace stress=. if visit==2 | visit==4 | visit==6 | visit==8
replace socialsupp=. if visit==2 | visit==4 | visit==6 | visit==8

drop if pid==614 | pid==740

list pid if pica_child!=. & geo_child==. & amylo_child==. & other_child==.



//////////////////////////////////////////////
** Participants - Withdrew
//////////////////////////////////////////////

	*-------------------------------*
	// KEDH  //
	*-------------------------------*
	
gen withdrew=.

foreach x in 120 137 {
replace withdrew=2 if visit==1 & pid==`x'
}

foreach x in 140 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 115 119 123 124 {
replace withdrew=4 if visit==3 & pid==`x'
}

foreach x in 105 133 137 142 10002 10067 {
replace withdrew=5 if visit==4 & pid==`x'
}

foreach x in 109 {
replace withdrew=6 if visit==5 & pid==`x'
}

foreach x in 126 177 181 {
replace withdrew=7 if visit==6 & pid==`x'
}


	*-------------------------------*
	// Nyahera  //
	*-------------------------------*

foreach x in 200 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 211 {
replace withdrew=4 if visit==3 & pid==`x'
}

foreach x in 244 260 277 20020 20027 {
replace withdrew=5 if visit==4 & pid==`x'
}

foreach x in 227 20025 {
replace withdrew=6 if visit==5 & pid==`x'
}

foreach x in 226 239 {
replace withdrew=7 if visit==6 & pid==`x'
}

	*-------------------------------*
	// Rongo  //
	*-------------------------------*

foreach x in 312 {
replace withdrew=2 if visit==1 & pid==`x'
}

foreach x in 302 313 314 346 364 30008 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 30010 {
replace withdrew=4 if visit==3 & pid==`x'
}

foreach x in 360 386 387 {
replace withdrew=5 if visit==4 & pid==`x'
}

foreach x in 312 362 {
replace withdrew=6 if visit==5 & pid==`x'
}

foreach x in 30000 {
replace withdrew=7 if visit==6 & pid==`x'
}


	*-------------------------------*
	// Ongo  //
	*-------------------------------*

foreach x in 403 407 458 472 498 {
replace withdrew=2 if visit==1 & pid==`x'
}

foreach x in 451 495 40000 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 417 453 484 {
replace withdrew=5 if visit==4 & pid==`x'
}


	*-------------------------------*
	// Nyamaraga  //
	*-------------------------------*

foreach x in 554 {
replace withdrew=2 if visit==1 & pid==`x'
}

foreach x in 553 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 509 {
replace withdrew=4 if visit==3 & pid==`x'
}

foreach x in 506 {
replace withdrew=5 if visit==4 & pid==`x'
}

foreach x in 500 {
replace withdrew=6 if visit==5 & pid==`x'
}


	*-------------------------------*
	// Macalder  //
	*-------------------------------*

foreach x in 620 {
replace withdrew=2 if visit==1 & pid==`x'
}

foreach x in 624 638 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 611 {
replace withdrew=4 if visit==3 & pid==`x'
}

foreach x in 617 621 622 {
replace withdrew=5 if visit==4 & pid==`x'
}

foreach x in 631 {
replace withdrew=6 if visit==5 & pid==`x'
}


	*-------------------------------*
	// Migori  //
	*-------------------------------*

foreach x in 715 716 724 733 750 779 786 787 {
replace withdrew=2 if visit==1 & pid==`x'
}

foreach x in 722 725 {
replace withdrew=3 if visit==2 & pid==`x'
}

foreach x in 719 732 754 {
replace withdrew=5 if visit==4 & pid==`x'
}

label values withdrew visit


//////////////////////////////////////////////
** Remove Duplicates
//////////////////////////////////////////////

duplicates drop pid visit idate, force
duplicates list pid visit
drop if pid==270 & season==0 & visit==9
drop if pid==404 & season==0 & visit==9
drop if pid==485 & season==1 & visit==7
drop if pid==601 & wat_tx==0 & visit==8
drop if pid==623 & season==2 & visit==7
drop if pid==20027 & season==0 & visit==3
duplicates list pid visit



//////////////////////////////////////////////
** Remove PIDs with no data
//////////////////////////////////////////////

drop if pid==20030 | pid==445



