data qs;
infile "C:\Users\User\Desktop\newqssc49_Change.csv" dlm="," firstobs=2;
input RANK Rank_Change LAGRANK INSTITUTION $ Country $ IF_GB AR_SCORE AR_change	
ER_SCORE ER_change FS_SCORE	FS_change CPF_SCORE	CPF_change IF_SCORE	IF_change
IS_SCORE IS_change Overall_Score Year TIMES_ranking Times_overall Times_teaching Times_research
Times_citations	Times_Industryincome Times_international_outlook Number_FTE	student_staff_ratio	
international_students female_male_ratio Unemployment Unemployment_growth gdp gdpgrowth_lag 
gdpgrowth_lag2 RegionGDP_growth RegionGDP_growth_lag2 ARWU_ranking total_score alumni award 
hici ns	pub	pcp	year_ARWU;
if Rank_change>0 then good=0;
else good=1;
run;
proc print data=qs;
run;

data qs09;
infile "C:\Users\User\Desktop\newqssc49_Change_09.csv" dlm="," firstobs=2;
input RANK Rank_Change LAGRANK INSTITUTION $ Country $ IF_GB AR_SCORE
ER_SCORE FS_SCORE CPF_SCORE IF_SCORE IS_SCORE Overall_Score Year Unemployment gdp gdpgrowth_lag 
gdpgrowth_lag2 RegionGDP_growth RegionGDP_growth_lag2 ARWU_ranking total_score alumni award 
hici ns	pub	pcp;
if Rank_change>0 then good=0;
else good=1;
run;
proc print data=qs09;
run;

data top10;
infile "C:\Users\User\Desktop\Top10.csv" dlm="," firstobs=2;
input RANK Rank_Change LAGRANK INSTITUTION $ Country $ IF_GB AR_SCORE
ER_SCORE FS_SCORE CPF_SCORE IF_SCORE IS_SCORE Overall_Score Year Unemployment gdp gdpgrowth_lag 
gdpgrowth_lag2 RegionGDP_growth RegionGDP_growth_lag2 ARWU_ranking total_score alumni award 
hici ns	pub	pcp;
if Rank_change>0 then good=0;
else good=1;
run;
proc print data=top10;
run;

