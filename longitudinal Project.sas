*****************************************************************;
*Created by: Yonghua Zhuang
*Created on: DEC 8, 2013
*Purpose: Project for Longitudinal Data Analysis.		
*****************************************************************;
data CD4;
infile "c:\SAS\CD4.txt" firstobs=2;
input ID Treatment Age  Gender  Week logCD4;
run;
data baseline postbase;
set CD4;
if week =0
then output baseline;
else output postbase;
run;
data baseline1;
set baseline; 
rename logCD4=baseline;
run;
data CD4b;
merge CD4 (in=inp) baseline1 (in=inb);
by id;
percentage= logCD4/baseline*100;
run; 
Proc univariate data=CD4b;
var logcd4 week; 
run;
******Create a new class variable for time********;
Data CD4C;
Set CD4b;
If week <= 4 then time =0;
else if week > 4 and week <= 12 then time = 8;
else if week > 12 and week <= 20 then time = 16;
else if week > 20 and week <= 28 then time = 24;
else if week > 28 and week <= 36 then time = 32;
else time=40;
run;
proc means data = CD4C;
where week=0;
var age logCD4;
run; 
proc means data = CD4C;
where week=0;
var age logCD4;
class treatment;
run; 
Proc freq data=CD4C;
where week=0;
tables Gender*treatment;
run;
******check drop-out rate********;
Proc freq data=CD4C;
tables time*treatment;
run;

******Non-parametric regression to identify trend********;
proc loess data=CD4b;
where treatment=1;
model LogCD4=week  /smooth=0.4;
  ods output OutputStatistics=logout1;
  run;
proc sort data=logout1;
  by  week;
run;
title 'Placebo + ZDV';
axis1 order =(0 to 40 by 5);
axis2 order=(0 to 7 by 1) label=(r=0 a=90);
symbol1 c = black i=none v =star h=0.8;
symbol2 c=black i=join v=none h=0.8 line=1 WIDTH=3;
proc gplot data=logout1;
  format DepVar f4.0 income f8.0;
  plot DepVar*week=1 Pred*week=2 /overlay haxis=axis1 vaxis=axis2 hminor=0 vminor=0;
  label Week='Week';
  label DepVar='Log of CD4 Count';
run;

proc loess data=CD4b;
where treatment=2;
model LogCD4=week  /smooth=0.4;
  ods output OutputStatistics=logout2;
  run;
proc sort data=logout2;
  by  week;
run;
title 'ZDV + DDI';
axis1 order =(0 to 40 by 5);
axis2 order=(0 to 7 by 1) label=(r=0 a=90);
symbol1 c = black i=none v =star h=0.8;
symbol2 c=red i=join v=none h=0.8 line=1 WIDTH=3;
proc gplot data=logout2;
  format DepVar f4.0 income f8.0;
  plot DepVar*week=1 Pred*week=2 /overlay haxis=axis1 vaxis=axis2 hminor=0 vminor=0;
  label Week='Week';
  label DepVar='Log of CD4 count';
run;
proc loess data=CD4b;
where treatment=3;
model LogCD4=week  /smooth=0.4;
  ods output OutputStatistics=logout3;
  run;
proc sort data=logout3;
  by  week;
run;
title 'ZDV + DDC';
axis1 order =(0 to 40 by 5);
axis2 order=(0 to 6.5 by 1) label=(r=0 a=90);
symbol1 c = black i=none v =star h=0.8;
symbol2 c=blue i=join v=none h=0.8 line=1 WIDTH=3;
proc gplot data=logout3;
  format DepVar f4.0 income f8.0;
  plot DepVar*week=1 Pred*week=2 /overlay haxis=axis1 vaxis=axis2 hminor=0 vminor=0;
  label Week='Week';
  label DepVar='Log of CD4 count';
run;
proc loess data=CD4b;
where treatment=4;
model LogCD4=week  /smooth=0.4;
  ods output OutputStatistics=logout4;
  run;
proc sort data=logout4;
  by  week;
run;
title 'ZDV + DDI + DDC';
axis1 order =(0 to 40 by 5);
axis2 order=(0 to 7 by 1) label=(r=0 a=90);
symbol1 c = black i=none v =star h=0.8;
symbol2 c=green i=join v=none h=0.8 line=1 WIDTH=3;
proc gplot data=logout4;
  format DepVar f4.0 income f8.0;
  plot DepVar*week=1 Pred*week=2 /overlay haxis=axis1 vaxis=axis2 hminor=0 vminor=0;
  label Week='Week';
  label DepVar='Log of CD4 count';
run;

Data logout11;
set logout1;
Treatment=1;
run;
Data logout21;
set logout2;
Treatment=2;
run;
Data logout31;
set logout3;
Treatment=3;
run;
Data logout41;
set logout4;
Treatment=4;
run;
data logout;
merge logout11 logout21 logout31 logout41;
by treatment;
run;
Proc print data=logout (obs=100);
run;

title 'Log-transformed CD4 counts by 4 treatments';
axis1 order =(0 to 40 by 5);
axis2 order=(2.2 to 3.6 by 0.2) label=(a=90);
symbol1 c=black i=join v=none h=0.8 line=1 WIDTH=2;
symbol2 c=red i=join v=none h=0.8 line=2 WIDTH=2;
symbol3 c=blue i=join v=none h=0.8 line=2 WIDTH=2;
symbol4 c=green i=join v=none h=0.8 line=3 WIDTH=2;
legend label=none value=(h=2 font=swiss 'Placebo + DDI' 
'ZDV + DDI' 'ZDV + DDC' 'ZDV + DDI + DDC')
position=(top right inside) mode=share cborder=black;
proc gplot data=logout;
  plot  Pred*week=treatment /overlay haxis=axis1 vaxis=axis2 legend = legend1;
  label Week='Week';
  label Pred='Log of CD4 count';
run;

****mixed regresion with class variable****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id  r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id type=CS r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id type=AR(1) r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id type=SP(pow)(time) r rcorr;
Run;
proc mixed data=CD4C;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id type=TOEP r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id type=UN r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Repeated /subject=id type=UN(1) r rcorr;
Run;

proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Random intercept  / subject =ID ; 
Repeated /subject=id type=CS r rcorr;
Run;

proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Random intercept  / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
Run;

proc mixed data=CD4C;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Random intercept  / subject =ID ; 
Repeated /subject=id type=UN r rcorr;
Run;

proc mixed data=CD4C;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution;
Random intercept  / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
Contrast 'Test difference between treat 3 and treat 4 over time' 
	time*treatment 0 0 0 0 0 0	   0 0 0 0 0 0	    1 -1 0	0 0	 0   -1 1 0	0 0 0,
	time*treatment 0 0 0 0 0 0	   0 0 0 0 0 0	    1 0 -1	0 0	 0   -1 0 1	0 0 0,
	time*treatment 0 0 0 0 0 0	   0 0 0 0 0 0	    1 0  0 -1 0	 0   -1 0 0	1 0 0,
	time*treatment 0 0 0 0 0 0	   0 0 0 0 0 0	    1 0  0 0 -1 0   -1 0 0	0 1 0,
	time*treatment 0 0 0 0 0 0	   0 0 0 0 0 0	    1 0  0 0 0 -1   -1 0 0	0 0 1;
Contrast 'Test difference between treat 2 and treat 4 over time' 
		time*treatment 0 0 0 0 0 0	   1 -1 0	0 0	 0   0 0 0 0 0 0  -1 1 0	0 0 0,
		time*treatment 0 0 0 0 0 0	   1 0 -1	0 0	 0   0 0 0 0 0 0  -1 0 1	0 0 0,
		time*treatment 0 0 0 0 0 0	   1 0  0 -1 0	 0   0 0 0 0 0 0  -1 0 0	1 0 0,
		time*treatment 0 0 0 0 0 0	   1 0  0 0 -1  0   0 0 0 0 0 0  -1 0 0	0 1 0,
		time*treatment 0 0 0 0 0 0	   1 0  0 0 0  -1    0 0 0 0 0 0  -1 0 0	0 0 1;
Estimate '8 week-BL, treat 4 vs. treat 1'
	time*treatment 1 -1 0 0 0 0	   0 0 0 0 0 0	    0 0 0	0 0	 0   -1 1 0	0 0 0 /cl;
Estimate '8 week-BL, treat 4 vs. treat 2'
	time*treatment 0 0 0 0 0 0	   1 -1 0 0 0 0	    0 0 0	0 0	 0   -1 1 0	0 0 0 /cl;
Estimate '8 week-BL, treat 4 vs. treat 3'
	time*treatment 0 0 0 0 0 0	   0 0 0 0 0 0	    1 -1 0	0 0	 0   -1 1 0	0 0 0 /cl;
Run;

proc mixed data=CD4C;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution outp= pred;
Random intercept  / subject =ID ;
Repeated /subject=id type=AR(1) r rcorr;
lsmeans time;
Run;
proc mixed data=CD4C;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution outp= pred;
Random intercept  / subject =ID ;
Repeated /subject=id type=CS r rcorr;
lsmeans treatment age gender time time*treatment;
contrast 'linear' time -5 -3 -1 1  3 5;
contrast 'quadartic' time 5 -1 -4 -4  -1 5;
contrast 'cubic' time -5 7 4 -4  -7 5;
contrast 'lxl' 	time*treatment -5 -3 -1 1  3 5   5 3 1 -1 -3 -5  0 0 0 0 0 0  0 0 0 0 0 0,
			 	time*treatment -5 -3 -1 1  3 5   0 0 0 0 0 0     5 3 1 -1 -3 -5  0 0 0 0 0 0,
				time*treatment -5 -3 -1 1  3 5   0 0 0 0 0 0     0 0 0 0 0 0     5 3 1 -1 -3 -5;
contrast 'qxq' 	time*treatment 5 -1 -4 -4 -1 5  -5 1  4 4 1 -5   0 0 0 0 0 0   0 0 0 0 0 0,
			 	time*treatment 5 -1 -4 -4 -1 5  0 0 0 0 0 0     -5 1  4 4 1 -5  0 0 0 0 0 0,	
				time*treatment 5 -1 -4 -4 -1 5  0 0 0 0 0 0    0 0 0 0 0 0   -5 1  4 4 1 -5;	
Run;

proc sort data = pred;
by treatment time;
run;
proc summary data = pred;
by treatment time;
var pred;
output out = means (drop = _:) mean = mean ;
run; 

Proc print data=means;
run; 

title1 'Predicted logCD4 among 4 treatments';
title2 'Time: class variable';
axis1 order =(0 to 40 by 5);
axis2 order=(2.2 to 3.6 by 0.05) label=(a=90);
symbol1 c=black i=j v=none h=0.8 line=1 WIDTH=2;
symbol2 c=red i=j v=none h=0.8 line=2 WIDTH=2;
symbol3 c=blue i=j v=none h=0.8 line=2 WIDTH=2;
symbol4 c=green i=j v=none h=0.8 line=3 WIDTH=2;
legend label=none value=(h=2 font=swiss 'Placebo + DDI' 
'ZDV + DDI' 'ZDV + DDC' 'ZDV + DDI + DDC')
position=(top right inside) mode=share cborder=black;
proc gplot data=means;
  plot  mean*time=treatment /overlay haxis=axis1 vaxis=axis2 legend = legend1;
  label Week='Week';
  label mean='Log of CD4 count';
run;

proc mixed data=CD4C;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution outp= pred;
Random intercept  / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
run;

proc mixed data=CD4C method=ml;
Class ID Treatment Gender time;
Model logCD4 = treatment age gender time time*treatment  / solution outp= pred;
Random intercept  / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
run;


****mixed regresion: time as a continuous variable ****;
****Linear & random intercept****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment  / solution;
Random intercept /subject =ID ; 
Repeated /subject=id  type=AR(1) r rcorr;
Run;

****Quardratic & random intercept ****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
Run;

****Cubic & random intercept****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment week*week*week  week*week*week*treatment / solution;
Random intercept / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
Run;

****Quartic & random intercept****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment week*week*week  week*week*week*treatment
week*week*week*week  week*week*week*week*treatment/ solution;
Random intercept / subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
Run;


****Linear, random intercept & slope****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment  / solution;
Random intercept week/subject =ID ; 
Repeated /subject=id  type=AR(1) r rcorr;
Run;

****Quardratic & random intercept & slope ****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept week/ subject =ID ; 
Repeated /subject=id type=AR(1) r rcorr;
Run;

****Cubic & random intercept & slope****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment week*week*week  week*week*week*treatment / solution;
Random intercept week/ subject =ID ; 
Repeated /subject=id type=AR(1)  r rcorr;
Run;
****Quartic & random intercept & slope****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment week*week*week  week*week*week*treatment 
week*week*week*week  week*week*week*week*treatment/ solution;
Random intercept week/ subject =ID ; 
Repeated /subject=id type=AR(1)  r rcorr;
Run;
****Linear, random intercept & slope with UN for G****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment  / solution;
Random intercept week/Type=UN subject =ID ; 
Repeated /subject=id  type=AR(1) r rcorr;
Run;

****quardratic with different structure for G matrix****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept  week / Type=UN subject =ID v g; 
Repeated /subject=id type=AR(1) r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment
week*week*week  week*week*week*treatment/ solution;
Random intercept  week / Type=UN subject =ID v g; 
Repeated /subject=id type=AR(1) r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment
week*week*week  week*week*week*treatment
week*week*week*week  week*week*week*week*treatment
/ solution;
Random intercept  week / Type=UN subject =ID v g; 
Repeated /subject=id type=AR(1) r rcorr;
Run;
****quardratic with different structure for errors****;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept  week /type= UN subject =ID v g; 
Repeated /subject=id  Type=UN r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept  week /type= UN subject =ID v g; 
Repeated /subject=id  Type=CS r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept  week /type= UN subject =ID v g; 
Repeated /subject=id  type=AR(1) r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender;
Model logCD4 = treatment age gender week week*treatment week*week  week*week*treatment / solution;
Random intercept  week /type= UN subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;
Run;
****Mixed regression with spline terms******;
data CD4C;
set CD4C;
s1=max (0, (week-8));
s2=max (0, (week-16));
s3=max (0, (week-24));
run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender week s1 week*treatment s1*treatment  / solution;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=Ar(1) r rcorr;
Run;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender week s1 week*treatment s1*treatment  / solution;
Random intercept  week /type= VC subject =ID v g; 
Repeated /subject=id  type=Ar(1) r rcorr;
Run;
****Mixed regression with spline terms, quadratic ******;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender week s1 week*treatment s1*treatment week*week s1*s1 s1*s1*treatment/ solution;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=Ar(1) r rcorr;
Run;
****Mixed regression with spline terms******;
proc mixed data=CD4C method=ml;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender week s1 week*treatment s1*treatment  / solution ;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=Ar(1) r rcorr;
Run;

data CD4d;
set CD4c;
if treatment=4 then treatment4=1;
else treatment4=0;
run;
proc mixed data=CD4C_new method=ml;
Class ID Treatment time Gender ;
Model logCD4 = treatment age gender time time*treatment / solution outp= result2 ;
Random intercept week/ subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;
run;
****Regression with 2 knots ******;
proc mixed data=CD4d;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender week s1 week*treatment s1*treatment s2 s2*treatment/ solution outp= result;
Random intercept  week s1 s2/type=un subject =ID v g; 
Repeated /subject=id  type=Ar(1) r rcorr;
Run;

***Predict value by mixed model and check model fit with visual inspection**********;
***Assume  patients based on the baseline means of variable by treatments**********;
data new;
 input ID Treatment Age  Gender  Week logCD4 ;
 datalines;
2001	1	37.83	0	0	2.98
2001	1	37.83	0	1	.
2001	1	37.83	0	2	.
2001	1	37.83	0	3	.
2001	1	37.83	0	4	.
2001	1	37.83	0	5	.
2001	1	37.83	0	6	.
2001	1	37.83	0	7	.
2001	1	37.83	0	8	.
2001	1	37.83	0	9	.
2001	1	37.83	0	10	.
2001	1	37.83	0	11	.
2001	1	37.83	0	12	.
2001	1	37.83	0	13	.
2001	1	37.83	0	14	.
2001	1	37.83	0	15	.
2001	1	37.83	0	16	.
2001	1	37.83	0	17	.
2001	1	37.83	0	18	.
2001	1	37.83	0	19	.
2001	1	37.83	0	20	.
2001	1	37.83	0	21	.
2001	1	37.83	0	22	.
2001	1	37.83	0	23	.
2001	1	37.83	0	24	.
2001	1	37.83	0	25	.
2001	1	37.83	0	26	.
2001	1	37.83	0	27	.
2001	1	37.83	0	28	.
2001	1	37.83	0	29	.
2001	1	37.83	0	30	.
2001	1	37.83	0	31	.
2001	1	37.83	0	32	.
2001	1	37.83	0	33	.
2001	1	37.83	0	34	.
2001	1	37.83	0	35	.
2001	1	37.83	0	36	.
2001	1	37.83	0	37	.
2001	1	37.83	0	38	.
2001	1	37.83	0	39	.
2001	1	37.83	0	40	.

2002	2	37.73	0	0	2.93
2002	2	37.83	0	1	.
2002	2	37.83	0	2	.
2002	2	37.83	0	3	.
2002	2	37.83	0	4	.
2002	2	37.83	0	5	.
2002	2	37.83	0	6	.
2002	2	37.83	0	7	.
2002	2	37.83	0	8	.
2002	2	37.83	0	9	.
2002	2	37.83	0	10	.
2002	2	37.83	0	11	.
2002	2	37.83	0	12	.
2002	2	37.83	0	13	.
2002	2	37.83	0	14	.
2002	2	37.83	0	15	.
2002	2	37.83	0	16	.
2002	2	37.83	0	17	.
2002	2	37.83	0	18	.
2002	2	37.83	0	19	.
2002	2	37.83	0	20	.
2002	2	37.83	0	21	.
2002	2	37.83	0	22	.
2002	2	37.83	0	23	.
2002	2	37.83	0	24	.
2002	2	37.83	0	25	.
2002	2	37.83	0	26	.
2002	2	37.83	0	27	.
2002	2	37.83	0	28	.
2002	2	37.83	0	29	.
2002	2	37.83	0	30	.
2002	2	37.83	0	31	.
2002	2	37.83	0	32	.
2002	2	37.83	0	33	.
2002	2	37.83	0	34	.
2002	2	37.83	0	35	.
2002	2	37.83	0	36	.
2002	2	37.83	0	37	.
2002	2	37.83	0	38	.
2002	2	37.83	0	39	.
2002	2	37.83	0	40	.

2003	3	37.47	0	0	2.91
2003	3	37.47	0	1	.
2003	3	37.47	0	2	.
2003	3	37.47	0	3	.
2003	3	37.47	0	4	.
2003	3	37.47	0	5	.
2003	3	37.47	0	6	.
2003	3	37.47	0	7	.
2003	3	37.47	0	8	.
2003	3	37.47	0	9	.
2003	3	37.47	0	10	.
2003	3	37.47	0	11	.
2003	3	37.47	0	12	.
2003	3	37.47	0	13	.
2003	3	37.47	0	14	.
2003	3	37.47	0	15	.
2003	3	37.47	0	16	.
2003	3	37.47	0	17	.
2003	3	37.47	0	18	.
2003	3	37.47	0	19	.
2003	3	37.47	0	20	.
2003	3	37.47	0	21	.
2003	3	37.47	0	22	.
2003	3	37.47	0	23	.
2003	3	37.47	0	24	.
2003	3	37.47	0	25	.
2003	3	37.47	0	26	.
2003	3	37.47	0	27	.
2003	3	37.47	0	28	.
2003	3	37.47	0	29	.
2003	3	37.47	0	30	.
2003	3	37.47	0	31	.
2003	3	37.47	0	32	.
2003	3	37.47	0	33	.
2003	3	37.47	0	34	.
2003	3	37.47	0	35	.
2003	3	37.47	0	36	.
2003	3	37.47	0	37	.
2003	3	37.47	0	38	.
2003	3	37.47	0	39	.
2003	3	37.47	0	40	.
2004	4	37.89	0	0	2.84
2004	4	37.89	0	1	.
2004	4	37.89	0	2	.
2004	4	37.89	0	3	.
2004	4	37.89	0	4	.
2004	4	37.89	0	5	.
2004	4	37.89	0	6	.
2004	4	37.89	0	7	.
2004	4	37.89	0	8	.
2004	4	37.89	0	9	.
2004	4	37.89	0	10	.
2004	4	37.89	0	11	.
2004	4	37.89	0	12	.
2004	4	37.89	0	13	.
2004	4	37.89	0	14	.
2004	4	37.89	0	15	.
2004	4	37.89	0	16	.
2004	4	37.89	0	17	.
2004	4	37.89	0	18	.
2004	4	37.89	0	19	.
2004	4	37.89	0	20	.
2004	4	37.89	0	21	.
2004	4	37.89	0	22	.
2004	4	37.89	0	23	.
2004	4	37.89	0	24	.
2004	4	37.89	0	25	.
2004	4	37.89	0	26	.
2004	4	37.89	0	27	.
2004	4	37.89	0	28	.
2004	4	37.89	0	29	.
2004	4	37.89	0	30	.
2004	4	37.89	0	31	.
2004	4	37.89	0	32	.
2004	4	37.89	0	33	.
2004	4	37.89	0	34	.
2004	4	37.89	0	35	.
2004	4	37.89	0	36	.
2004	4	37.89	0	37	.
2004	4	37.89	0	38	.
2004	4	37.89	0	39	.
2004	4	37.89	0	40	.
;
 run;

data new;
set new;
s1=max (0, (week-8));
run;
data CD4C_new;
 set CD4C new;
 run;
*****Treat therapies sepearatly in regression*******;
Data CD4C_new1;
set CD4C_new;
If treatment=1 then treatment1=1;else treatment1=0;
If treatment=2 then treatment2=1;else treatment2=0;
If treatment=3 then treatment3=1; else treatment3=0;
If treatment=4 then treatment4=4; else treatment4=0;
run;

proc mixed data=CD4C_new1 method=ml;
Class ID Treatment Gender ;
Model logCD4 = treatment1 treatment2 treatment3 treatment4 age gender week 
week*treatment1 week*treatment2 week*treatment3 week*treatment4 
 s1*treatment2 s1*treatment3 
week*week*treatment4  week*week*week*treatment4 week*week*week*week*treatment4 
/ solution outp= result001 ;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;
run;
***Predict value by the mixed model with lowest AIC **********;
Data pred001;
set result001;
if ID <2000 then pred=.;
time=week;
run;
title 'Log-transformed CD4 counts by 4 treatments';
axis1 order =(0 to 40 by 5);
axis2 order=(2 to 3.6 by 0.2) label=(a=90);
symbol1 c=black i=j v=none h=0.8 line=1 WIDTH=2;
symbol2 c=red i=j v=none h=0.8 line=2 WIDTH=2;
symbol3 c=blue i=j v=none h=0.8 line=2 WIDTH=2;
symbol4 c=green i=j v=none h=0.8 line=3 WIDTH=2;
legend label=none value=(h=2 font=swiss 'Placebo + DDI' 
'ZDV + DDI' 'ZDV + DDC' 'ZDV + DDI + DDC')
position=(top right inside) mode=share cborder=black;
proc gplot data=pred001;
  plot  Pred*week=treatment /overlay haxis=axis1 vaxis=axis2 legend = legend1;
  label Week='Week';
  label Pred='Log of CD4 count';
run;

**********continue to explore models since model with lowest AIC does not match well the trend shown on scatter plot********;
proc mixed data=CD4C_new1 method=ml;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender 
week week*week week*week*week  week*week*week*week
week*treatment week*week*treatment  week*week*week*treatment  week*week*week*week*treatment  
/ solution;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;
run;

proc mixed data=CD4C_new1;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender 
week week*week week*week*week  week*week*week*week
week*treatment week*week*treatment  week*week*week*treatment  week*week*week*week*treatment  
/ solution outp= result002 ;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;
run;

Data pred002;
set result002;
if ID <2000 then pred=.;
time=week;
run;

title 'Log-transformed CD4 counts by 4 treatments';
axis1 order =(0 to 40 by 5);
axis2 order=(2 to 3.6 by 0.2) label=(a=90);
symbol1 c=black i=j v=none h=0.8 line=1 WIDTH=2;
symbol2 c=red i=j v=none h=0.8 line=2 WIDTH=2;
symbol3 c=blue i=j v=none h=0.8 line=2 WIDTH=2;
symbol4 c=green i=j v=none h=0.8 line=3 WIDTH=2;
legend label=none value=(h=2 font=swiss 'Placebo + DDI' 
'ZDV + DDI' 'ZDV + DDC' 'ZDV + DDI + DDC')
position=(top right inside) mode=share cborder=black;
proc gplot data=pred002;
  plot  Pred*week=treatment /overlay haxis=axis1 vaxis=axis2 legend = legend1;
  label Week='Week';
  label Pred='Log of CD4 count';
run;

title 'Predicted log-transformed CD4 counts by 4 treatments';
axis1 order =(0 to 40 by 5);
axis2 order=(2.2 to 3.6 by 0.2) label=(a=90);
symbol1 c=black i=j v=none h=0.8 line=1 WIDTH=2;
symbol2 c=red i=j v=none h=0.8 line=2 WIDTH=2;
symbol3 c=blue i=j v=none h=0.8 line=2 WIDTH=2;
symbol4 c=green i=j v=none h=0.8 line=3 WIDTH=2;
legend label=none value=(h=2 font=swiss 'Placebo + DDI' 
'ZDV + DDI' 'ZDV + DDC' 'ZDV + DDI + DDC')
position=(top right inside) mode=share cborder=black;
proc gplot data=pred002;
  plot  Pred*week=treatment /overlay haxis=axis1 vaxis=axis2 legend = legend1;
  label Week='Week';
  label Pred='Log of CD4 count';
run;
**********The above model match well the trend shown in scatter plot********;
**********Estimate with overall best fit model********;		
proc mixed data=CD4C_new1;
Class ID Treatment Gender ;
Model logCD4 = treatment age gender 
week week*week week*week*week  week*week*week*week
week*treatment week*week*treatment  week*week*week*treatment  week*week*week*week*treatment  
/ solution;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;
		
ESTIMATE "Treat1: 8 week vs 0 week " week 8 week*week 64 week*week*week 512 week*week*week*week 4096
week*treatment 8 0 0 0 week*week*treatment 64 0 0 0 week*week*week*treatment 512 0 0 0  week*week*week*week*treatment 4096 0 0 0 /cl;
ESTIMATE "Treat2: 8 week vs 0 week " week 8 week*week 64 week*week*week 512 week*week*week*week 4096
week*treatment 0 8 0 0 week*week*treatment 0 64  0 0 week*week*week*treatment 0 512  0 0  week*week*week*week*treatment 0 4096 0 0 /cl;
ESTIMATE "Treat3: 8 week vs 0 week" week 8 week*week 64 week*week*week 512 week*week*week*week 4096
week*treatment 0 0 8 0 week*week*treatment 0 0 64 0 week*week*week*treatment 0 0 512 0  week*week*week*week*treatment 0 0 4096 0 /cl;

ESTIMATE "Treat1: 16 week vs 0 week " week 16 week*week 256 week*week*week 4096 week*week*week*week 65536
week*treatment 16 0 0 0 week*week*treatment 256 0 0 0 week*week*week*treatment 4096 0 0 0  week*week*week*week*treatment 65536 0 0 0 /cl;
ESTIMATE "Treat2: 16 week vs 0 week " week 16 week*week 256 week*week*week 4096 week*week*week*week 65536
week*treatment 0 16 0 0 week*week*treatment 0 256  0 0 week*week*week*treatment 0 4096  0 0  week*week*week*week*treatment 0 65536 0 0 /cl;
ESTIMATE "Treat3: 16 week vs 0 week" week 16 week*week 256 week*week*week 4096 week*week*week*week 65536
week*treatment 0 0 16 0 week*week*treatment 0 0 256 0 week*week*week*treatment 0 0 4096 0  week*week*week*week*treatment 0 0 65536 0 /cl;

ESTIMATE "Treat1: 24 week vs 0 week " week 24 week*week 576 week*week*week 13824 week*week*week*week 331776
week*treatment 24 0 0 0 week*week*treatment 576 0 0 0 week*week*week*treatment 13824 0 0 0  week*week*week*week*treatment 331776 0 0 0 /cl;
ESTIMATE "Treat2: 24 week vs 0 week " week 24 week*week 576 week*week*week 13824 week*week*week*week 331776
week*treatment 0 24 0 0 week*week*treatment 0 576  0 0 week*week*week*treatment 0 13824 0 0  week*week*week*week*treatment 0 331776 0 0 /cl;
ESTIMATE "Treat3: 24 week vs 0 week" week 24 week*week 576 week*week*week 13824 week*week*week*week 331776
week*treatment 0 0 24 0 week*week*treatment 0 0 576 0 week*week*week*treatment 0 0 13824 0  week*week*week*week*treatment 0 0 331776 0 /cl;

ESTIMATE "Treat1: 32 week vs 0 week " week 32 week*week 1024 week*week*week 32768 week*week*week*week 1048576
week*treatment 32 0 0 0 week*week*treatment 1024 0 0 0 week*week*week*treatment 32768 0 0 0  week*week*week*week*treatment 1048576 0 0 0 /cl;
ESTIMATE "Treat2: 32 week vs 0 week " week 32 week*week 1024 week*week*week 32768 week*week*week*week 1048576
week*treatment 0 32 0 0 week*week*treatment 0 1024  0 0 week*week*week*treatment 0 32768 0 0  week*week*week*week*treatment 0 1048576 0 0 /cl;
ESTIMATE "Treat3: 32 week vs 0 week" week 32 week*week 1024 week*week*week 32768 week*week*week*week 1048576
week*treatment 0 0 32 0 week*week*treatment 0 0 1024 0 week*week*week*treatment 0 0 32768 0  week*week*week*week*treatment 0 0 1048576 0 /cl;

ESTIMATE "Treat1: 40 week vs 0 week " week 40 week*week 1600 week*week*week 64000 week*week*week*week 2560000
week*treatment 40 0 0 0 week*week*treatment 1600 0 0 0 week*week*week*treatment 64000 0 0 0  week*week*week*week*treatment 256000 0 0 0 /cl;
ESTIMATE "Treat2: 40 week vs 0 week " week 40 week*week 1600 week*week*week 64000 week*week*week*week 2560000
week*treatment 0 40 0 0 week*week*treatment 0 1600  0 0 week*week*week*treatment 0 64000 0 0  week*week*week*week*treatment 0 2560000 0 0 /cl;
ESTIMATE "Treat3: 40 week vs 0 week" week 40 week*week 1600 week*week*week 64000 week*week*week*week 2560000
week*treatment 0 0 40 0 week*week*treatment 0 0 1600 0 week*week*week*treatment 0 0 64000 0  week*week*week*week*treatment 0 0 2560000 0 /cl;

run;
**********Since SAS could not estimate treatment 4 in the above model, the reference group as changed to treatment 1********;
Data CD4C_new2;
set CD4C_new1;
If treatment =1 then group=4;
If treatment =2 then group=3;
If treatment =3 then group=2;
If treatment =4 then group=1;
run;

proc mixed data=CD4C_new2;
Class ID group Gender ;
Model logCD4 = group age gender 
week week*week week*week*week  week*week*week*week
week*group week*week*group  week*week*week*group  week*week*week*week*group  
/ solution;
Random intercept  week s1/type= UN subject =ID v g; 
Repeated /subject=id  type=sp(pow)(week) r rcorr;

ESTIMATE "Treat4: 8 week vs 0 week " week 8 week*week 64 week*week*week 512 week*week*week*week 4096
week*group 8 0 0 0 week*week*group 64 0 0 0 week*week*week*group 512 0 0 0  week*week*week*week*group 4096 0 0 0 /cl;
ESTIMATE "Treat4: 16 week vs 0 week " week 16 week*week 256 week*week*week 4096 week*week*week*week 65536
week*group 16 0 0 0 week*week*group 256 0 0 0 week*week*week*group 4096 0 0 0  week*week*week*week*group 65536 0 0 0 /cl;
ESTIMATE "Treat4: 24 week vs 0 week " week 24 week*week 576 week*week*week 13824 week*week*week*week 331776
week*group 24 0 0 0 week*week*group 576 0 0 0 week*week*week*group 13824 0 0 0  week*week*week*week*group 331776 0 0 0 /cl;
ESTIMATE "Treat4: 32 week vs 0 week " week 32 week*week 1024 week*week*week 32768 week*week*week*week 1048576
week*group 32 0 0 0 week*week*group 1024 0 0 0 week*week*week*group 32768 0 0 0  week*week*week*week*group 1048576 0 0 0 /cl;

ESTIMATE "Treat4: 40 week vs 0 week " week 40 week*week 1600 week*week*week 64000 week*week*week*week 2560000
week*group 40 0 0 0 week*week*group 1600 0 0 0 week*week*week*group 64000 0 0 0  week*week*week*week*group 256000 0 0 0 /cl;

Contrast "8 week: Treat 4 Vs 2" group -1 0 1 0
week*group -8 0 8  0 week*week*group -64 0 64 0 week*week*week*group -512  0 512 0  week*week*week*week*group -4096 0 4096 0;
Contrast "8 week: Treat 4 Vs 3" group -1 1 0 0
week*group -8 8  0 0 week*week*group -64  64 0 0 week*week*week*group -512 512 0 0  week*week*week*week*group -4096  4096 0 0;

Contrast "40 week: Treat 4 Vs 2" group -1 0 1 0
week*group -40 0 40  0 week*week*group -1600 0 1600 0 week*week*week*group -6400  0 6400 0  week*week*week*week*group -256000 0 256000 0;
Contrast "40 week: Treat 4 Vs 3" group -1 1 0 0
week*group -40 40  0 0 week*week*group -1600 1600 0 0 week*week*week*group -6400 6400 0 0  week*week*week*week*group -256000  256000 0 0;
run;




