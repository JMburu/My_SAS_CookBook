My SAS CookBook.
================
James Wanja.
Last compiled on: April 27, 2020.











<hr>

## 

<hr>

## *Check if macro variable is blank;*

``` r
===============================================          ======  */
  
Check if macro variable is blank;
  
===============================================          ======  */  

%let cond=;
%put %sysevalf(%superq(cond)=,boolean);# 1- blank, 0 - not blank;
```

<hr>

<hr>

## *Is a dataset empty?;*

``` r
===============================================          ======  */
  
Is a dataset empty?
  
===============================================          ======  */  

%macro jw_empty(din=, lib=);
 proc sql noprint;
    select count(distinct subjectid) into :noobs
    from &lib..&din. ;
  quit;
  
%if &noobs eq 0 %then %do;
  data _null_;
    file print;
    put "NOTE: No Observation in the Dataset";
  run;
  %end;
%mend jw_empty;
%jw_empty(din=, lib=);
```

<hr>

<hr>

## *Checking for the existence of a Dataset;*

``` r
===============================================          ======  */
  
Checking for the existence of a Dataset;
  
===============================================          ======  */  

%macro jw_exist(domain =,lib=rdb);
    %if %sysfunc(exist(&lib..&domain.)) %then 
      %do;
        data _null_;
          file print;/*directs PUT to write to the SAS print file*/
          put "THE DATASET &domain EXISTS";
        run;
      %end;
    %else %do;
      data _null_;
        file print;
        put "THE DATASET &domain. DOES NOT EXIST";;
      run;
      %end;
%mend jw_exist;

%jw_exist(domain =,lib= );
```

<hr>

<hr>

## *Macro Getting the BIG-N’s\[Denominators\] through a tabulation*.

``` r
===============================================          ======  */
  
Macro Getting the BIG-Ns[Denominators] through a tabulation
  
===============================================          ======  */    

%macro jw_bigN(where=, cond=,data=,var= );
  %if &where = 'Y' %then 
    %do;
      proc freq data = &data noprint;
        tables &var. /out=_bign(drop=percent rename=(count=bigN)) ;
        where &cond.;
      run;
    %end;
  %else %do;
         proc freq data = &data noprint;
           tables &var./out=_bign(drop=percent rename=(count=bigN)) ;
         run;
        %end;
   proc sort data = _bign;
     by &var.;
   run;
%mend jw_bigN;

%jw_bigN(data=,cond=,where=,var=);
```

<hr>

<hr>

## *Calculating AGE*.

``` r
===============================================          ======  */
- Computing INTERVALS between dates : INTCK, INTNX function

The 'CONTINUOUS' option in the INTCK function enables you to count the number of anniversaries 
of one date that occur prior to a second date. For example, the statement
Years = intck('year', '30APR1789'd, '04MAR1797'd, 'continuous');

INTNX ( interval, from, n < , alignment > ) ; 
- The INTCK function counts the number of interval boundaries between two date values 
  or between two datetime values
 ===============================================          ======  */

age = floor((intck("month", numbirth, numconsent) - (day(numconsent) < day(numbirth))) / 12);

years    = intck('year','01jan2009'd,'01jan2010'd);
SEMIYEAR = intck('SEMIYEAR','01jan2009'd,'01jan2010'd);
quarters = intck('qtr','01jan2009'd,'01jan2010'd);
months   = intck('month','01jan2009'd,'01jan2010'd);
weeks    = intck('week','01jan2009'd,'01jan2010'd);
days     = intck('day','01jan2009'd,'01jan2010'd);

/*DateTime*/
hours    = intck('hour','01jan2009:00:00:00'dt,'01jan2010:00:00:00'dt);
minutes  = intck('minute','01jan2009:00:00:00'dt,'01jan2010:00:00:00'dt);
seconds  = intck('second','01jan2009:00:00:00'dt,'01jan2010:00:00:00'dt);

/*Time*/
hours   = intck('hour','00:00:00't,'12:00:00't);
minutes = intck('minute','00:00:00't,'12:00:00't);
seconds = intck('second','00:00:00't,'12:00:00't);

*Important notice! The INTCK functions does not count the number of complete intervals between two values.;
years=intck('year','31dec2009'd,'01jan2010'd); /* This will give 1*/

* So USE!!!!!
The 'day365' paremeter tells to the INTCK function to multiply day by 365 times
;
days365=intck('day365','31dec2009'd,'01jan2010'd); /* This will give 0*/
```

<hr>

<hr>

## *Categorical Variables Macro: TWO-WAY Tabulation*.

``` r
* ================================ =============
  Categorical Variables Macro: TWO-WAY Tabulation;
   Assuming You have ran the macro above %jw_bigN().
   to give you _bign  
* ================================ =============
  
%macro jw_cat2way(colvar=, rowvar= ,data=, where=, cond=,);

%* The Tabulation;
%if &where. = 'Y' %then %do;
  proc freq data = &data. noprint;
    tables &rowvar.*&colvar./ out=&data.1(drop=percent);
    where &cond.;
  run;
%end;
%else %do;
  proc freq data = &data. noprint;
    tables &rowvar.*&colvar./ out=&data.1(drop=percent);
  run;
%end;
%* sort;
  proc sort data= &data.1;
    by &rowvar.;
  run;

%* Get total... Cumulative sum;
data &data.2(drop=total);
set &data.1;
by &rowvar.;
retain total;
if first.&rowvar. then total=0;
total+count;
output;
if last.&rowvar. then 
do;
count     = total;
&colvar.  = 'Total';
output;
end;
%* Sort by the colvar which will help merge with BigN;
proc sort;
by &colvar.;
run;

%* Merge with bigN to get %;
data &data.3(drop = count perc);
merge &data.2(in=a) _bign;
by &colvar.;
%* Some formating ;
percent  = (count/bign)*100;
perc     = put(round(percent,0.1),5.1);
countper = cat(put(count,4.0),' ','(',perc,')');
drop percent bign;
%* sort by colvar to help in ransposing;
if a;
proc sort;
by &rowvar.;
run;

%* Transposing ;
proc transpose data=&data.3 out=&data.4(drop=_name_);
by &rowvar.;
var countper;
id &colvar.;
run;

%* delete some ;
proc datasets library=work nolist nodetails;
delete &data.1  &data.2  &data.3 ;
run;
quit;


%mend jw_cat2way;


%jw_cat2way(colvar=, rowvar= ,data=, where=, cond=,)
```

<hr>

<hr>

## *Continuous variables Macro - Calculating the stats - 2 variables*.

``` r
*===================================================
  Continuous variables Macro - Calculating the stats 
1 by/class variable
2 Variables 
====================================================
  ;
;
%macro jw_contvar2(var1=,libr=,splitvar=,data=, var2=);

* Calculating the Numeric Stats ;
proc means data= &libr..&data. noprint nway mean;
class &splitvar.;
var &var1. &var2.;
output out = &libr.._stats1(drop =  _type_ _freq_) 
n     = n&var1.1 n&var2.1
mean  = mean&var1.1 mean&var2.1
std   = SD&var1.1 SD&var2.1
median= median&var1.1 median&var2.1
min   = min&var1.1 min&var2.1
max   = max&var1.1 max&var2.1
q1    = q1&var1.1  q1&var2.1
q3    = q3&var1.1  q3&var2.1
;
run;

* formating the values ;
data &libr.._stats2;
set &libr.._stats1;

array n{*}    n&var1.1 n&var2.1;
array n1{*} $13 n&var1. n&var2.;
array av{*}   mean&var1.1 mean&var2.1;
array av1{*} $13 mean&var1. mean&var2.;
array std{*}  SD&var1.1 SD&var2.1;
array std1{*} $13 SD&var1. SD&var2.;
array med{*}  median&var1.1 median&var2.1;
array med1{*} $13 median&var1. median&var2.;
array min{*}  min&var1.1 min&var2.1;
array min1{*} $13 min&var1. min&var2.;
array max{*}  max&var1.1 max&var2.1;
array max1{*} $13 max&var1. max&var2.;
array q1{*}  q1&var1.1 q1&var2.1;
array q11{*} $13 q1&var1. q1&var2.;
array q3{*}  q3&var1.1 q3&var2.1;
array q31{*} $13 q3&var1. q3&var2.;

do i = 1 to dim(n);
if n{i} ne .   then n1{i}  = put(round(n{i},1),4.0);
if av{i} ne .  then av1{i} = put(round(av{i},0.1),6.1);
if std{i} ne . then std1{i}= put(round(std{i},0.01),7.2);
if med{i} ne . then med1{i}= put(round(med{i},0.1),6.1);
if min{i} ne . then min1{i}= put(round(min{i},1),4.0);
if max{i} ne . then max1{i}= put(round(max{i},1),4.0);
if q1{i}  ne . then q11{i} = put(round(q1{i},0.1),6.1);
if q3{i}  ne . then q31{i} = put(round(q3{i},0.1),6.1);
end;
keep &splitvar. n&var1.--q3&var2.;
run;

* Transpose ;
proc transpose data=&libr.._stats2 out=&libr.._stats3(rename=(_name_=stat) drop=_label_);
var _character_;
id &splitvar.;
run;

* some further formating:...checking cases ;
data &libr.._stats4;
length grp $ 15.;
set &libr.._stats3;

if upcase(substr(stat,1,1))='N' then grp ='n';
if upcase(substr(stat,1,2))='SD' then grp ='SD';
if upcase(substr(stat,1,6))='MEDIAN' then grp ='Median';
if upcase(substr(stat,1,4))='MEAN' then grp ='Mean';
if upcase(substr(stat,1,3)) in ('MIN','MAX') then grp =propcase(substr(stat,1,3));
if upcase(substr(stat,1,2))='Q1' then grp ='Q1';
if upcase(substr(stat,1,2))='Q3' then grp ='Q3';

stat =lowcase(stat);
if find(stat,"&var1.")>1 then desc = "&var1." ;
if find(stat,"&var2.")>1 then desc = "&var2." ;
%* A sorting variable ;
if upcase(grp) = 'N' then order =1;
else if upcase(grp) = 'MEAN'   then order =2;
else if upcase(grp) = 'SD'     then order =3;
else if upcase(grp) = 'MIN'    then order =4;
else if upcase(grp) = 'Q1'     then order =5;
else if upcase(grp) = 'MEDIAN' then order =6;
else if upcase(grp) = 'Q3'     then order =7;
else if upcase(grp) = 'MAX'    then order =8;
drop stat;
* Sorting by the grouping;
proc sort;
by desc order;
run;
%* delete some ;
proc datasets library=work nolist nodetails;
delete &libr.._stats1 &libr.._stats2 &libr.._stats3;
run;
quit;

%mend jw_contvar2;


%jw_contvar2(var1=,var2= ,libr=,splitvar = ,data=);
```

<hr>

<hr>

## *Continuous variables Macro - Calculating the stats- 1 variable *.

``` r
*===================================================
  Continuous variables Macro - Calculating the stats 
1 by/class variable
1 Variables 
====================================================
  ;
;
%macro jw_contvar1(var1=,libr=,splitvar=,data=);

* Calculating the Numeric Stats ;
proc means data= &libr..&data. noprint nway mean;
class &splitvar.;
var &var1. ;
output out = &libr.._stats1(drop =  _type_ _freq_) 
n     = n&var1.1 
mean  = mean&var1.1 
std   = SD&var1.1 
median= median&var1.1 
min   = min&var1.1 
max   = max&var1.1 
;
run;

* formating the values ;
data &libr.._stats2;
set &libr.._stats1;

array n{*}    n&var1.1;
array n1{*} $13 n&var1.;
array av{*}   mean&var1.1 ;
array av1{*} $13 mean&var1. ;
array std{*}  SD&var1.1 ;
array std1{*} $13 SD&var1.;
array med{*}  median&var1.1;
array med1{*} $13 median&var1.;
array min{*}  min&var1.1;
array min1{*} $13 min&var1.;
array max{*}  max&var1.1 ;
array max1{*} $13 max&var1.;

do i = 1 to dim(n);
if n{i} ne .   then n1{i}  = put(round(n{i},1),4.0);
if av{i} ne .  then av1{i} = put(round(av{i},0.1),6.1);
if std{i} ne . then std1{i}= put(round(std{i},0.01),7.2);
if med{i} ne . then med1{i}= put(round(med{i},0.1),6.1);
if min{i} ne . then min1{i}= put(round(min{i},1),4.0);
if max{i} ne . then max1{i}= put(round(max{i},1),4.0);
end;
keep &splitvar. n&var1.--max&var1.;
run;

* Transpose ;
proc transpose data=&libr.._stats2 out=&libr.._stats3(rename=(_name_=stat) drop=_label_);
var _character_;
id &splitvar.;
run;

* some further formating:...checking cases ;
data &libr.._stats4;
length grp $ 15.;
set &libr.._stats3;

if upcase(substr(stat,1,1))='N' then grp ='n';
if upcase(substr(stat,1,2))='SD' then grp ='SD';
if upcase(substr(stat,1,6))='MEDIAN' then grp ='Median';
if upcase(substr(stat,1,4))='MEAN' then grp ='Mean';
if upcase(substr(stat,1,3)) in ('MIN','MAX') then grp =propcase(substr(stat,1,3));

stat =lowcase(stat);
if find(stat,"&var1.")>1 then desc = "&var1." ;
%* A sorting variable ;
if upcase(grp) = 'N' then order =1;
else if upcase(grp) = 'MEAN'   then order =2;
else if upcase(grp) = 'SD'     then order =3;
else if upcase(grp) = 'MEDIAN' then order =4;
else if upcase(grp) = 'MIN'    then order =5;
else if upcase(grp) = 'MAX'    then order =6;

drop stat;
* Sorting by the grouping;
proc sort;
by desc order;
run;
%* delete some ;
proc datasets library=work nolist nodetails;
delete &libr.._stats1 &libr.._stats2 &libr.._stats3;
run;
quit;

%mend jw_contvar1;


%jw_contvar1(var1=,libr=,splitvar = ,data=);
```

## *Add aditional rows*.

<hr>

<hr>

``` r
* =========================================================
  Add aditional rows 

Add on to the  contvar1 outputs
* =========================================================;

data _stats5;
length grp $30 ;
set _stats4;
by desc;
if missing(grp) then delete;
grp = cat("  ",grp);
retain desc;
array dd{*} grp--total;
output;
if last.desc then 
do;
call missing(of dd{*});
order=.;
if (desc="cmdose" & missing(grp)) then grp="Fraction dose (Gy)";
if (desc="numfra" & missing(grp)) then grp="Number of fraction doses";
output;
end;
* Sorting by the grouping;
proc sort;
by desc order;
run;
```

<hr>

<hr>

### *Alternatively* .

``` r
creating a dataset with regimen='0' and regimen='Unknown';

data rg;
input regimen $20.;
datalines;
0
Unknown;
run;
```

<hr>

<hr>

## *Epoch*.

``` r
* ===================================================
  * Epoch;
* ===================================================
  
  *====== start macro epoch === ;
%macro jw_epoch(sedata = ,
                datevar = ,
                datanew=);
data  &datanew.1;
set &sedata.(keep = usubjid epoch sestdtc seendtc) 
&datanew.(keep=usubjid &datevar. rename=(&datevar.=sestdtc) in=a);
if a then flag=1;
proc sort;
by usubjid sestdtc flag;
run;
%*If there is a missing value of epoch use the previous observation;
data &datanew.2(keep=usubjid epoch &datevar.);
set &datanew.1;
by usubjid;
retain epoch1 end;
if first.usubjid then epoch1= epoch;
if not missing(epoch) then 
do;
epoch1= epoch;
sest  = input(sestdtc,anydtdte11.);
end   = input(seendtc,anydtdte11.);
end;
else epoch = epoch1; 
%* - Ensure last out of range dates are not populated
- Not expected though;
if (last.usubjid & flag=1 & sest>end) then epoch = '';
rename sestdtc = &datevar.;
if flag;
%*sort;
proc sort;
by usubjid &datevar.;
run;

%mend jw_epoch;

*====== end macro epoch === ;

%jw_epoch(sedata  = sdtm., datevar = , datanew = );
```

<hr>

<hr>

## *NO REPORT* .

``` r
* =========================================================
  NO rEPoRt...                                                                   
* =========================================================;

%macro noreport(dsin=RESULTS,out=yes,dsout=&output.);
data RESULTS;
length col1 $ 40;
col1 = '';
output;
col1 = 'No Data to Report';
output;
col1 = '';
output;
run;
%if %upcase(&out) = YES %then %do;
data output.&dsout.;
set results;
run;
%end;
proc report data=&dsin. nowindows headline headskip split='|' missing;
column col1;
define col1 / " " style(column)=[cellwidth=80% just=c];
run;
%mend noreport;


/*Example*/
  %macro ds100(din=);
%* If there are NO observations;
proc sql noprint;
select count(distinct usubjid) into :noobs
from &din. ;
quit;
%if &noobs eq 0 %then %do;
%rstart;
%noreport;
%rstop;
%end;
%else %do;
%rstart;
proc report data=&din.  split='~' headline headskip center missing nowindows style(column)={asis=on};
column col1 col2 col3 ;
define col1       / display "Subject ID" style={cellwidth=15% paddingright=1cm 
just=left asis=on};
define col2       / display "Analysis Population"  style={cellwidth=25% paddingright=1cm
just=left asis=on};
define col3       / display "Reason for Exclusion"  style={cellwidth=50% paddingright=1cm
just=left asis=on};
run;
%rstop;
%end;
%mend ds100;
```

<hr>

<hr>

## *Proc Report*.

``` r
%rstart;

proc report data=output.cm201 nowd headline headskip split='~' style(report)=[fontsize=10pt] nocenter;
column  regimen ('Number (%) of patients' a b c) order;
define order / order noprint;
define regimen / display 'Number of regimens' left style(header)=[just=left] 
style(column)=[cellwidth=4in cellspacing=10pt
               just=left asis=on];
define a / display "&arm_A~(N=&armcd_A)" left style(header)=[just=left] 
style(column)=[cellwidth=1.5in cellspacing=10pt
               just=left asis=on];
define b / display "&arm_B~(N=&armcd_B)" left style(header)=[just=left]
style(column)=[cellwidth=1.5in cellspacing=10pt
               just=left asis=on];
define c / display "&arm_C~(N=&armcd_C)" left style(header)=[just=left]
style(column)=[cellwidth=1.5in cellspacing=10pt
               just=left asis=on];
compute after order;
line '';
endcomp;
run;



ods escapechar='^';

proc report data=output.ex201 nowd headline headskip split='~' style(report)=[fontsize=12pt] nocenter missing;
column  duration name stat order;
define order / order noprint;
define duration / display 'Treatment duration' style(header)=[just=left]
style(column)=[cellwidth=4in asis=on ] flow;
define name / display '' left style(header)=[just=left] style(column)=[cellwidth=3.5in 
                                                                       just=left asis=on] flow;
define stat / display "&cross_C.~(N=&n_c.)" left style(header)=[just=left]
style(column)=[cellwidth=1.5in  just=left asis=on] flow;
compute after order;
line '';
endcomp;
run;

/*
  Padding before or after or both in a column;
--------------------------------------------
  style(column)= {just=l leftmargin= 0.5in rightmargin= 0.5in} 
OR
style(column)= {just=l pretext="   " posttext="    "} 

*/
  
  %rstop;
```

<hr>

<hr>

## *COMBINING SUPP+Main dataset*.

``` r
* ============================================= 
  
  COMBINING SUPP+Main dataset
;

*==== Macro Start ====;
* Transpose supp to wide and merge with main dataset;
%macro transpw(lib =,supp=,main= ,id =, id2=,);
%* sort supp and main;
proc sort data=&lib..&supp. out=_&supp.;
by usubjid &id.;
run;
proc sort data=&lib..&main. out=_&main.;
by usubjid &id2.;
run;
%* Transpose supp;
proc transpose data= _&supp. out=_&supp.1(drop=_label_ _name_);
by usubjid &id.;
var qval; 
id qnam;
idlabel qlabel;
run;
%* Make --seq numeric;
data _&supp.2(drop=idvarval);
set _&supp.1;
if not missing(&id.) then &id2. = input(&id.,8.);
proc sort;
by usubjid &id2.;
run;
%* Merge the two;
data _&main.1;
merge _&main.  _&supp.2;
by usubjid &id2.;
*rename &id2. seq;
run;
%* delete some ;
proc datasets library=work nolist nodetails;
delete _&main. _&supp. _&supp.1 _&supp.2;
run;
quit;
%mend transpw;
*==== Macro End ====;

* ==============
  Merge ds+suppds
;
%transpw(lib=sdtm,supp=suppds,main=ds,id=idvarval,id2=dsseq);
```

<hr>

<hr>

## .

<hr>

<hr>

## *List of all datsets in a Lib*.

``` r
proc contents data=raw._all_  nods;
run;
proc contents data=raw._all_  out=_sets(keep=name memname); 
run;


proc datasets lib=raw out=; 
*contents data=_all_; 
quit; 
run; 
```

<hr>

<hr>

## *Compress*.

``` r
* ============================================= ;
/* COmpress */
  * ============================================= ;
%macro jw_compress(din=,dout=);
data &dout.(keep=col:);
set &din.;
array coln{*} $200 col: ;
do i = 1 to dim(coln);
coln{i} = compress(tranwrd(coln{i},"*n",""));
end;
run;
%mend jw_compress;

%jw_compress(din=_eff101c,dout=_eff101c1);
```

<hr>

<hr>

## *MAcro Variables*.

``` r
* ============================================= ;

/*   MAcro Variable */
  * ============================================= ;

proc sql noprint;
select avisitn
into: tt separated by ' '
from tt;
quit;
```

<hr>

<hr>

## *Check if a Variable is in the dataset *.

``` r
* ============================================= ;

Check if a Variable is in the dataset 

VARNUM - returns the number of a variables position in a SAS data set, or 0 if the variable is not in the SAS data set. 
OPEN function - opens a SAS data set

* ============================================= *
  
  data test;
input fruit $ count;
datalines;
apple 12
banana 4
coconut 5
date 7
eggs 9
fig 5
;
run;

data _null_;
datasetid = open('test');
check     = varnum(datasetid,'count');
check2    = varnum(datasetid,'missingVar');

if check = 0 then put 'Variable does not exist';
else put 'Variable does exists';

if check2 = 0 then put 'Variable does NOT exist';
else put 'Variable does exists';

run;
```

<hr>

<hr>

## *Check d.p/s from raw variable*.

``` r
*===================================================
  Check d.p/s from raw variable

====================================================;

options mprint symbolgen;

%macro jw_dps(din= , var=, cat="");

/* Check if Variable is Character.If Not.Convert it*/
data vartype(keep=v);
  set &din. ;
  v = vtype(&var.);
  proc sort nodupkey;
  by v;
run;
/*Store this is a macro var*/
  proc sql noprint;
    select unique(v)  into :vtype  from vartype;
run;
%put &vtype.;

/* If Var is already a character */
%if &vtype. = C %then %do;
data &din.1(keep = dp: pre: &var. L: dcount);
  set &din.;
  dp    = scan(&var.,2,"."); /*get the numbers after the decimal*/
  if dp = "" then dcount = 0;
  else dcount = length(dp); /* get the length of the numbers after decimal point*/
  
/* d.p and precision for mean/median */
  dpmean = (dcount+1);
  precm  = 1/(10**(dcount+1));
/* d.p and precision for sd/median */
  dpsd  = dcount+2;
  presd  = 1/(10**(dcount+2));
/* d.p and precision for max/min */
  dpmin   = dcount;
  precmin  = 1/(10**(dcount));

/*To help with put widths:- use put(x,length.dp)*/
  L  = lengthn(&var.);
  L2 = L+1;
run;
%end;
%else %do;
data &din.1(keep = dp: pre: var dcount L: fmt:);
  set &din.;
  var   = strip(put(&var.,best.));
  dp    = scan(var,2,"."); /*get the numbers after the decimal*/
  if dp = "" then dcount = 0;
  else dcount = length(dp); /* get the length of the numbers after decimal point*/
  
  
/* d.p and precision for mean/median */
  dpmean   = (dcount+1);
  precmean = 1/(10**(dcount+1));
/* d.p and precision for sd/median */
  dpsd  = dcount+2;
  presd  = 1/(10**(dcount+2));
/* d.p and precision for max/min */
  dpmin   = dcount;
  precmin  = 1/(10**(dcount));
/* to help with put widths:- use put(x,length.dp)*/
  L  = lengthn(var)+1;
  L2 = L+1;
/* formats */
  fmtmean = (L2 + dpmean) +(dpmean/10);
  fmtsd   = (L2 + dpsd) + (dpsd/10);

if dpmin = 0 then do;
  fmtmin  = (L + dpmin) + (dpmin/10);
  fmtminc = strip(put(fmtmin,4.0)) || "." ||"0";
  end;
else do;
  fmtmin  = (L2 + dpmin) + (dpmin/10);
  fmtminc = strip(put(fmtmin,5.1));
end;
run;
%end;

/*Get the max number of dp and put theat in a macro variable
Make these Macro Variables global.
*/
  
proc sql noprint;
  %global dpmin dpsd dpmean presd precmean precmin length fmtmean fmtsd fmtmin;
  select max(dpmin)    into :dpmin    from &din.1;
  select max(dpsd)     into :dpsd     from &din.1;
  select max(dpmean)   into :dpmean   from &din.1;
  select min(presd)    into :presd    from &din.1;
  select min(precmin)  into :precmin  from &din.1;
  select min(precmean) into :precmean from &din.1;
  select max(L)        into :length   from &din.1;

/*infromats...To be used within put(,informat)*/
  select max(fmtmean) into :fmtmean from &din.1;
  select max(fmtsd)   into :fmtsd   from &din.1;
  select max(fmtminc)  into :fmtmin  from &din.1;
quit;

/* =================================================== */
  /* Calculating the Numeric Stats */
  
proc means data= &din. noprint nway mean;
  var &var. ;
  output out = _stats1(drop =  _type_ _freq_) 
  n     = n&var.1 
  mean  = mean&var.1 
  std   = SD&var.1 
  median= median&var.1 
  min   = min&var.1 
  max   = max&var.1 
  ;
run;

/** formating the values  */
data _stats2;
  set _stats1;
  
  array n{*}    n&var.1;
  array n1{*} $13 n&var.;
  array av{*}   mean&var.1 ;
  array av1{*} $13 mean&var. ;
  array std{*}  SD&var.1 ;
  array std1{*} $13 SD&var.;
  array med{*}  median&var.1;
  array med1{*} $13 median&var.;
  array min{*}  min&var.1;
  array min1{*} $13 min&var.;
  array max{*}  max&var.1 ;
  array max1{*} $13 max&var.;

  do i = 1 to dim(n);
    if n{i} ne .   then n1{i}  = put(round(n{i},1),4.0);
    if av{i} ne .  then av1{i} = put(round(av{i},&precmean.),&fmtmean.);
    if std{i} ne . then std1{i}= put(round(std{i},&presd.),&fmtsd.);
    if med{i} ne . then med1{i}= put(round(med{i},&precmean.),&fmtmean.);
    if min{i} ne . then min1{i}= put(round(min{i},&precmin.),&fmtmin.);
    if max{i} ne . then max1{i}= put(round(max{i},&precmin.),&fmtmin.);
  end;
  %*keep &splitvar. n&var1.--max&var1.;
run;



%mend jw_dps;
```

<hr>

<hr>

## Using -NOT IN- in a macro.

You need to use the options minoperator mindelimiter = “,”

``` r
/* =================================================== */
  
  /* Using NOT IN in a macro */
  
/* =================================================== */
options minoperator mindelimiter = ",";
%macro dummy();
%if not %eval(&listofvisits  in 122,124) %then %do;
  data _dummyMonths;
    col1 = "  Month 12"; col2 = " 0";  ord2 = 3; ord1= 1;  output;
  run;
  %end;
%else %do;
%end;
%mend;

/* =================================================== */
```

<hr>

<hr>

## *EXACT - Logistic reg*.

``` r
/* =================================================== 
  EXACT
/* =================================================== */
  
proc genmod data=_adeff1;
  class trtp/*(ref="Sunitinib QD") strat1 strat2*// param=glm;
  model aval(event='1')= trtp /*strat1 strat2*// LRCI ITPRINT type3 
  dist=bin link=logit;
  exact trtp;
  lsmeans trtp/ diff exp ilink cl ;%*for each trtp, estimate of the log odds of objective response;
  estimate 'trt_effect' trtp 1 -1/exp ;
  /*ods output ExactOddsRatio = _OR;
  ods output ExactParmEst   = _paramEst;*/
    ods output ExactTests     = _midpval(keep= midpvalue test where=(test="Score"));
  /*ods output Estimates      = _Est(keep= label lbetaestimate --probchisq where=(label="Exp(trt_effect)")); %*Estimates of contrasts ;
  ods output Diffs     =_lsdiff;
  ods output LSMeans   =_lsmean;*/
run;

* ============================================= ;
```

<hr>

<hr>

## *Read the NEXT OBServation*.

``` r
/* =================================================== 
  Read the NEXT OBServation.
/* =================================================== */
  
  data have;
input ID $ Index Measure;
cards;
A 1 11
A 2 12
A 3 13
B 1 21
B 2 22
;run;
data _nextObs;
  set have;
  by ID;
  set have ( firstobs = 2 keep = Measure rename = (Measure = Next_Measure) )/*read in from line number 2*/
    have (      obs = 1 drop = _all_                                     );/*stop reading at line 1*/
    Next_Measure = ifn(last.ID, (.), Next_Measure );
  /*IFN(logical-expression, value-returned-when-true, value-returned-when-false <,value-returned-when-missing>) 
  similar to R ifelse
*/
run;
```

<hr>

<hr>

## Sequence.

``` r
* Get a sequence to label the yaxis. Limit this to 2dp;
data seq;
  do i = &min1. to  &max1. by &by1.;
    x = round(i,0.01); 
    output;
  end;
run;

proc sql noprint;
  %global ylist;
  select x  into: ylist separated by " " from seq;
quit;
```

<hr>

<hr>

## Macro to subset data by given row numbers.

``` r
%macro Jw_rows(din=,rows=,dout=);
 data &dout.;
    length rows 8.;
    set &din.;
    if _n_ in(&rows.);
    rows = _n_;
 run;
%mend Jw_rows;
```

<hr>

<hr>

## Iterative process using -do until-.

``` r
%macro mac_until();
proc sql;
  select distinct trtgrp
  into :trt_list separated by '!'
  from pop_demo
  where pnitt = 'Y';
  %let num_trt = &sqlobs;
quit;
%put &=trt_list &=num_trt;
%let i = 1;
%do %until (%scan(&trt_list, &i, !) eq   );
        %let trt_name = %scan(&trt_list, &i, !);
        %put &=trt_name;
        proc means data = train.demo;
              where trtgrp = "&trt_name";
              var age;
        run;
    %let i = %eval(&i + 1);
%end;
%mend;
%mac_until()
```

## Iterative process using -do-.

<hr>

<hr>

``` r
proc sort nodupkeys;
by memname;
run;

proc sql noprint;
  select count(distinct(memname)), memname as vars
  into: nobs,
      : vars separated by '/'
  from cont;
quit;

  
%macro uns;
%do i = 1 %to &nobs;
%let d = %scan(%superq(vars),&i.,/);
 data &d._ ;
set &d. ;
where visitid in (850);
run;
%end;
```

<hr>

<hr>

## List alphabet in lowcase (a-z).

### Use function collate(96+num, 96+num);

``` r
....
```

<hr>

<hr>

## Get A list of all files is a certain directory/library.

``` r
filename pp pipe 'dir "P:\PH19KTR\Kenya Assess 2020\Data\Analysis"';

data pp;
  length listing $400;
  infile pp length = reclen;
  input listing $varying400. reclen;
  re = prxparse("/\s?(\w+.sas7bdat)/");
  if prxmatch(re,listing) then do;
    list  = prxposn(re,1,listing);
  end;
run;
```

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## .

<hr>

<hr>

## *Delete Sets*.

``` r
proc datasets library=work nolist nodetails;
  delete _:;
run;
quit;
```

<hr>

<hr>
