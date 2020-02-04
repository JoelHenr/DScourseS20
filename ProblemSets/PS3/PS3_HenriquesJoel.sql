CREATE TABLE JOEL(
"policyID" INTEGER,
"statecode" CHAR,
"county" CHAR,
"eq_site_limit" CHAR,
"hu_site_limit" INTEGER,
"fl_site_limit" INTEGER,
"fr_site_limit" INTEGER,
"tiv_2011" INTEGER,
"tiv_2012" INTEGER,
"eq_site_deductible" REAL,
"hu_site_deductible" INTEGER,
"fl_site_deductible" REAL,
"fr_site_deductible" INTEGER,
"point_latitude" INTEGER,
"point_longitude" REAL,
"line" REAL,
"construction" CHAR,
"point_granularity" CHAR
);
.mode csv
.import FL_insurance_sample.csv JOEL
SELECT*FROM JOEL LIMIT 10;
SELECT County, COUNT(*) FROM JOEL GROUP BY County;
SELECT AVG(tiv_2012-tiv_2011) FROM JOEL;
SELECT Construction, COUNT(*) FROM JOEL GROUP BY Construction;
