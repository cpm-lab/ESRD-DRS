# Extract comoribidities using ICD code lists

library(RODBC)
library(tidyverse)
library(tictoc)

# Run from parent directory
source("config_mnt.R")

# DIM ICD9 ####
# Store ICD9 comorbidities codelists in a temporary table
sqlQuery(
  db_dbProj,
  paste0(
    "Drop Table If Exists #ICD9;
      Drop Table If Exists #ICD10;

      SELECT ICD9SID, Sta3n, ICD9Code
      INTO #ICD9
      FROM CDWWork.Dim.ICD9 AS b
      WHERE
      ICD9Code LIKE '398.91%' OR ICD9Code LIKE '428.[0-9]%'
      OR ICD9Code LIKE '093.2%'
      OR ICD9Code LIKE '39[4-6].%'
      OR ICD9Code LIKE '397.0%'
      OR ICD9Code LIKE '397.1%'
      OR ICD9Code LIKE '397.9%'
      OR ICD9Code LIKE '424.%'
      OR ICD9Code LIKE '746.[3-6]%'
      OR ICD9Code LIKE 'V42.2%'
      OR ICD9Code LIKE 'V43.3%'
      OR ICD9code LIKE '415.1[1-9]%'
      OR ICD9code LIKE '416.%'
      OR ICD9code LIKE '417.9%'
      OR ICD9Code like '440.%'
      OR ICD9Code like '441.%'
      OR ICD9Code like '443.1%'
      OR ICD9Code like '443.[8-9]%'
      OR ICD9Code like '447.1%'
      OR ICD9Code like '557.1%'
      OR ICD9Code like '557.9%'
      OR ICD9Code like 'V43.4%'
      OR ICD9Code LIKE '401.%'
      OR ICD9Code like '40[2-5].%'
      OR ICD9Code like '343.%'
      OR ICD9Code like '344.%'
      OR ICD9Code like '438.[2-4]'
      OR ICD9code like '438.5[0-3]%'
      OR ICD9Code like '780.72%'
      OR ICD9Code like '331.9%'
      OR ICD9Code like '332.0%'
      OR ICD9Code like '332.1%'
      OR ICD9Code like '333.4%'
      OR ICD9Code like '333.5%'
      OR ICD9Code like '333.92%'
      OR ICD9Code like '33[4-5].%'
      OR ICD9Code like '336.2%'
      OR ICD9Code like '340.%'
      OR ICD9Code like '341.%'
      OR ICD9Code like '345.%'
      OR ICD9Code like '348.1%'
      OR ICD9Code like '348.3%'
      OR ICD9Code like '780.3%'
      OR ICD9Code like '784.3%'
      OR ICD9Code like '416.8%'
      OR ICD9Code like '416.9%'
      OR ICD9Code like '49[0-9].%'
      OR ICD9Code like '50[0-5].%'
      OR ICD9Code like '506.4%'
      OR ICD9Code like '508.1%'
      OR ICD9Code like '508.8%'
      OR ICD9Code like '250.%'
      OR ICD9code like '240.9%'
      OR ICD9code like '24[3-4].%'
      OR ICD9code like '246.1%'
      OR ICD9code like '246.8%'
      OR ICD9Code like '585.3%'
      OR ICD9Code like '585.9%'
      OR ICD9Code like '586.%'
      OR ICD9Code like '585.[4-6]%'
      OR ICD9Code like 'V42.0%'
      OR ICD9Code like 'V45.1[1-2]%'
      OR ICD9Code like 'V56.[0-6]%'
      OR ICD9Code like 'V56.8%'
      OR ICD9code like '070.[2-3][2-3]%'
      OR ICD9code like '070.44%'
      OR ICD9code like '070.54%'
      OR ICD9code like '070.6%'
      OR ICD9code like '070.9%'
      OR ICD9code like '456.[0-2]%'
      OR ICD9code like '57[0-1].%'
      OR ICD9code like '572.[2-8]%'
      OR ICD9code like '573.[3-4]%'
      OR ICD9code like '573.[8-9]%'
      OR ICD9code like 'V42.7%'
      OR ICD9code like '53[1-4].7%'
      OR ICD9code like '53[1-4].9%'
      OR ICD9code like ' 04[2-4].%'
      OR ICD9code like '20[0-2].%'
      OR ICD9code like '203.0%'
      OR ICD9code like '238.6%'
      OR ICD9code like '19[6-9].%'
      OR ICD9code like '14[0-9].%'
      OR ICD9code like '15[0-9].%'
      OR ICD9code like '16[0-9].%'
      OR ICD9code like '17[0-2].%'
      OR ICD9code like '17[4-9].%'
      OR ICD9code like '18[0-9].%'
      OR ICD9code like '19[0-5].%'
      OR ICD9Code like '446.%'
      OR ICD9Code like '701.0%'
      OR ICD9Code like '710.[0-4]%'
      OR ICD9Code like '710.[8-9]%'
      OR ICD9Code like '711.2%'
      OR ICD9Code like '714.%'
      OR ICD9Code like '719.3%'
      OR ICD9Code like '720.%'
      OR ICD9Code like '725.%'
      OR ICD9Code like '728.5%'
      OR ICD9Code like '728.89%'
      OR ICD9Code like '729.30%'
      OR ICD9Code like '286.%'
      OR ICD9Code like '287.1%'
      OR ICD9Code like '287.[3-5]%'
      OR ICD9Code like '278.0%'
      OR ICD9code like '26[0-3].%'
      OR ICD9code like '783.2%'
      OR ICD9code like '799.4%'
      OR ICD9Code like '253.6%'
      OR ICD9Code like '276.%'
      OR ICD9code like '280.0%'
      OR ICD9Code like '280.[1-9]%'
      OR ICD9Code like '281.%'
      OR ICD9Code like '265.2%'
      OR ICD9Code like '291.[1-3]%'
      OR ICD9Code like '291.[5-9]%'
      OR ICD9Code like '303.0%'
      OR ICD9Code like '303.9%'
      OR ICD9Code like '305.0%'
      OR ICD9Code like '357.5%'
      OR ICD9Code like '425.5%'
      OR ICD9Code like '535.3%'
      OR ICD9Code like '571.[0-3]%'
      OR ICD9Code like '980.%'
      OR ICD9Code like 'V11.3%'
      OR ICD9Code like '292.%'
      OR ICD9Code like '304.%'
      OR ICD9Code like '305.[2-9]%'
      OR ICD9Code like 'V65.42%'
      OR ICD9Code like '293.8%'
      OR ICD9Code like '295.%'
      OR ICD9Code like '296.04%'
      OR ICD9Code like '296.14%'
      OR ICD9Code like '296.44%'
      OR ICD9Code like '296.54%'
      OR ICD9Code like '297.%'
      OR ICD9Code like '298.%'
      OR ICD9Code like '300.4%'
      OR ICD9Code like '301.12%'
      OR ICD9Code like '309.[0-1]%'
      OR ICD9Code like '311%'"
  )
)

# DIM ICD10 ####
# Store ICD10 comorbidities codelist in a temporary table
sqlQuery(db_dbProj, "
      SELECT ICD10SID, Sta3n, ICD10Code
      INTO #ICD10
      FROM CDWWork.Dim.ICD10
      WHERE
      ICD10Code LIKE 'I09.981%' OR ICD10Code LIKE 'I50.%'
      OR ICD10Code LIKE 'I51.81%' OR ICD10Code LIKE 'I97.713[0-1]%'
      OR ICD10Code LIKE 'O29.12%' OR ICD10Code LIKE 'R57.0%'
      OR ICD10Code LIKE 'Z95.81[1-2]%'
      OR ICD10Code LIKE 'A18.84%' OR ICD10Code LIKE 'A32.82%'
      OR ICD10Code LIKE 'A39.51'
      OR ICD10Code LIKE 'A52.03%' OR ICD10Code LIKE 'B33.21%'
      OR ICD10Code LIKE 'B37.6%'
      OR ICD10Code LIKE 'I01.1%' OR ICD10Code LIKE 'I01.[8-9]%'
      OR ICD10Code LIKE 'I02.0%'
      OR ICD10Code LIKE 'I0[5-8].%' OR ICD10Code LIKE 'I09.1%'
      OR ICD10Code LIKE 'I09.89%'
      OR ICD10Code LIKE 'I3[3-9].%' OR ICD10Code LIKE 'M32.11%'
      OR ICD10Code LIKE 'Q2[2-3].%'
      OR ICD10Code LIKE 'Z95.[2-4]%'
      OR ICD10code LIKE 'I27.%' OR ICD10code LIKE 'I28.[0-1]%'
      OR ICD10code LIKE 'I28.[8-9]%'
      OR ICD10Code like 'A52.0%' OR ICD10Code like 'I70.[0-1]'
      OR ICD10Code like 'I70.[2-9]%' OR ICD10Code like 'I71.0[0-3]%'
      OR ICD10Code like 'I71.[1-9]%' OR ICD10code like 'I72.%'
      OR ICD10code like 'I73.01%' OR ICD10code like 'I73.1%'
      OR ICD10Code like 'I73.[8-9]%' OR ICD10Code like 'I74.01%'
      OR ICD10Code like 'I74.09%' OR ICD10Code like 'I74.1%'
      OR ICD10Code like 'I74.[2-4]%' OR ICD10Code like 'I75.0[1-2]%'
      OR ICD10Code like 'I75.8%' OR ICD10Code like 'I77.[0-9]%'
      OR ICD10Code like 'I78.%' OR ICD10Code like 'I79.%'
      OR ICD10Code like 'I99.[8-9]%' OR ICD10Code like 'K31.81%'
      OR ICD10Code like 'K55.1%' OR ICD10Code like 'K55.8%'
      OR ICD10Code like 'K55.9%' OR ICD10Code like 'Z95.82%'
      OR ICD10Code like 'I10.%'
      OR ICD10Code like 'I16.0%'
      OR ICD10Code like 'I16.9%'
      OR ICD10Code like 'O10.01%'
      OR ICD10Code like 'O10.0[2-3]%'
      OR ICD10Code like 'I1[1-3].%'
      OR ICD10Code like 'I15.%'
      OR ICD10Code like 'I16.1%'
      OR ICD10Code like 'O10.[1-4]1_%'
      OR ICD10Code like 'O10.[1-4][2-3]%'
      OR ICD10Code like 'O10.91_%'
      OR ICD10Code like 'O10.9[2-3]%'
      OR ICD10Code like 'O11.%'
      OR ICD10Code like 'O16.%'
      OR ICD10code like 'G04.1%' OR ICD10Code like 'G80.0%'
      OR ICD10Code like 'G81.%'
      OR ICD10Code like 'G82.%' OR ICD10Code like 'G83.[0-5]%'
      OR ICD10Code like 'G83.[8-9]%'
      OR ICD10Code like 'I69.0[3-6]%' OR ICD10Code like 'I69.1[3-6]'
      OR ICD10Code like 'I69.2[3-6]'
      OR ICD10Code like 'R53.2%'
      OR ICD10code LIKE 'G1[0-3].%' OR ICD10code LIKE 'G2[0-2].%'
      OR ICD10code like 'G25.4%' OR ICD10Code like 'G25.5%'
      OR ICD10Code like 'G31.2%'
      OR ICD10Code like 'G31.8%' OR ICD10Code like 'G31.9%'
      OR ICD10Code like 'G32.%' OR ICD10Code like 'G3[5-7].%'
      OR ICD10Code like 'G40.%'
      OR ICD10Code like 'G41.%' OR ICD10Code like 'G93.1%'
      OR ICD10Code like 'G93.4%' OR ICD10Code like 'R47.0%'
      OR ICD10Code like 'R56.%'
      OR ICD10code like 'J4[1-4].%' OR ICD10Code like 'J45.[2-5][0-2]%'
      OR ICD10Code like 'J45.90%' OR ICD10Code like 'J45.99%'
      OR ICD10Code like 'J47.1%' OR ICD10Code like 'J47.9%'
      OR ICD10Code like 'J6[0-1].%' OR ICD10Code like 'J62.0%'
      OR ICD10Code like 'J62.8%' OR ICD10Code like 'J63.%'
      OR ICD10Code like 'J6[4-7].%'OR ICD10Code like 'J68.4%'
      OR ICD10Code like 'J70.1%' OR ICD10Code like 'J70.3%'
      OR ICD10code like 'E10.%'
      OR ICD10code like 'E11.%'
      OR ICD10code like 'E13.%'
      OR ICD10code like 'E14.%'
      OR ICD10Code like 'O24.%'
      OR ICD10Code like 'E0[0-3].%' OR ICD10Code like 'E89.0%'
      OR ICD10Code like 'E0[5-6].%' OR ICD10Code like 'O90.5%'
      OR ICD10Code like 'N18.3%' OR ICD10Code like 'N19.%'
      OR ICD10Code like 'N18.[4-6]%' OR ICD10Code like 'Z49.[0-2]%'
      OR ICD10Code like 'Z49.3[1-2]%' OR ICD10Code like 'Z94.0%'
      OR ICD10Code like 'Z99.2%'
      OR ICD10Code like 'A51.45%' OR ICD10Code like 'A52.74%'
      OR ICD10Code like 'B18.%' OR ICD10Code like 'B19.[1-2]0%'
      OR ICD10Code like 'B19.9%' OR ICD10Code like 'B25.1%'
      OR ICD10Code like 'B58.1%' OR ICD10Code like 'K70.[0-1]%'
      OR ICD10Code like 'K70.2%' OR ICD10Code like 'K70.3[0-1]'
      OR ICD10Code like 'K70.9%'
      OR ICD10code like 'K71.[3-8]%' OR ICD10code like 'K7[3-7].%'
      OR ICD10code like 'B19.0%' OR ICD10code like 'B19.[1-2]1%'
      OR ICD10Code like 'I85.0[0-1]%' OR ICD10Code like 'I85.11%'
      OR ICD10code like 'I86.4%' OR ICD10Code like 'K70.4[0-1]%'
      OR ICD10code like 'K72.[0-1]%' OR ICD10code like 'K72.9[0-1]%'
      OR ICD10code like 'K76.[5-7]%' OR ICD10code like 'K91.82%'
      OR ICD10code like 'Z94.4%'
      OR ICD10code like 'K2[5-8].%'
      OR ICD10code like 'B20%' OR ICD10Code like 'Z21%'
      OR ICD10code like 'O98.71[1-3]%'
      OR ICD10code like 'O98.719%' OR ICD10code like 'O98.7[2-3]%'
      OR ICD10Code like 'C8[1-5].%' OR ICD10Code like 'C88.%'
      OR ICD10Code like 'C96.%'
      OR ICD10code like 'C90.0%' OR ICD10Code like 'C90.2%'
      OR ICD10code like 'C7[7-9].%' OR ICD10code like 'C80.%'
      OR ICD10code like 'C0[0-1].%' OR ICD10code like 'C02.[0-6]%'
      OR ICD10code like 'C3[0-4].%' OR ICD10code like 'C3[7-9].%'
      OR ICD10code like 'C4[0-1].%'
      OR ICD10code like 'C43.%'
      OR ICD10code like 'C4[5-9].%' OR ICD10code like 'C5[0-8].%'
      OR ICD10code like 'C6[0-9].%' OR ICD10code like 'C7[0-6].%'
      OR ICD10code like 'C97.%'
      OR ICD10code like 'A18.0[1-2]%' OR ICD10code like 'A39.84%'
      OR ICD10code like 'A54.4[1-2]%'
      OR ICD10code like 'L94.0%' OR ICD10Code like 'L94.1%'
      OR ICD10Code like 'L94.3%' OR ICD10Code like 'M01.X%'
      OR ICD10Code like 'M02.%'OR ICD10Code like 'M05.%'
      OR ICD10Code like 'M06.%' OR ICD10Code like 'M07.6%'
      OR ICD10Code like 'M08.%' OR ICD10Code like 'M12.0%'
      OR ICD10Code like 'M30.%' OR ICD10Code like 'M31.[0-3]%'
      OR ICD10Code like 'M3[2-5].%' OR ICD10Code like 'M45.%'
      OR ICD10Code like 'M46.[0-1]%' OR ICD10Code like 'M46.8%'
      OR ICD10Code like 'M46.9%'OR ICD10Code like 'M49.8[0-9]%'
      OR ICD10Code like 'D61.%' OR ICD10Code like 'D6[5-8].%'
      OR ICD10code like 'D69.1%'
      OR ICD10Code like 'D69.[3-6]%' OR ICD10Code like 'D69.[8-9]%'
      OR ICD10Code like 'D75.82%'
      OR ICD10Code like 'O99.11%' OR ICD10Code like 'O99.1[2-3]%'
      OR ICD10Code like 'E66.%' OR ICD10Code like 'O99.21%'
      OR ICD10Code like 'R93.9%'
      OR ICD10Code like 'Z68.[3-4]%' OR ICD10Code like 'Z68.54%'
      OR ICD10Code like 'E4[0-6].%' OR ICD10code like 'R63.4%'
      OR ICD10Code like 'R64.%'
      OR ICD10Code like 'O25.%'
      OR ICD10code like 'E22.2%' OR ICD10Code like 'E8[6-7].%'
      OR ICD10code like 'D50.0%' OR ICD10code like 'O90.81%'
      OR ICD10code like 'O99.0[2-3]%'
      OR ICD10code like 'D50.1%' OR ICD10code like 'D50.[8-9]%'
      OR ICD10Code like 'D5[1-3].%'
      OR ICD10Code like 'D63.%' OR ICD10Code like 'D64.9%'
      OR ICD10Code like 'O99.01[1-3].%' OR ICD10Code like 'O99.019.%'
      OR ICD10code like 'F10%' OR ICD10Code like 'G62.1%'
      OR ICD10Code like 'I42.6%' OR ICD10Code like 'K29.2[0-1]%'
      OR ICD10Code like 'O99.31[0-5]%' OR ICD10Code like 'K70.1[0-1]%'
      OR ICD10Code like 'F1[1-6].%' OR ICD10Code like 'F1[8-9].%'
      OR ICD10code LIKE 'O99.32%'
      OR ICD10Code like 'F19.159%' OR ICD10Code like 'F19.25%'
      OR ICD10Code like 'F19.95%'
      OR ICD10Code like 'F2[0-5].%' OR ICD10Code like 'F2[8-9].%'
      OR ICD10Code like 'F3[0-1].%'
      OR ICD10Code like 'F32.[4-5].%' OR ICD10Code like 'F33.4%'
      OR ICD10Code like 'F34.%'
      OR ICD10Code like 'F39.%' OR ICD10Code like 'F44.89%'
      OR ICD10Code like 'F84.3.%'
      OR ICD10code LIKE 'F06.3[1-2]%' OR ICD10code LIKE 'F06.34%'
      OR ICD10Code like 'F32.[0-3]%' OR ICD10Code like 'F32.8%'
      OR ICD10Code like 'F32.9%' OR ICD10Code like 'F33.[0-3]%'
      OR ICD10Code like 'F33.[8-9].%'OR ICD10Code like 'F34.1%'
      ")


# Combine Dims ####
# Join ICD9, ICD10 into one codelist in order to extract ICDCode from RDS files
comorbidities_dim <- bind_rows(
  sqlQuery(db_dbProj, "SELECT * FROM #ICD9"),
  sqlQuery(db_dbProj, "SELECT * FROM #ICD10")
)
sqlDrop(db_dbProj, "ICD_COMORBID_DIM")
sqlSave(db_dbProj, data.frame(comorbidities_dim), "ICD_COMORBID_DIM")

# Define conditions ####
# Using the codelist, define which ICD codes belong to which comorbidity
# Some are overlapping to multiple phenotypes.
sqlQuery(
  db_dbProj,
  "ALTER TABLE ICD_COMORBID_DIM ADD
          CHF int, VD int, PCD int, PVD int, HT int, Paralysis int,
          Neuro int, CPD int, DM int, DMcomp int, DMany int, Thyroid int,
          RF int, LD int, PUD int, AIDS int, Lymph int, MC int, Tumor int,
          RA int, Coag int, Obese int, WtLoss int, FED int, BLAnemia int,
          DAnemia int, Alcohol int, Drug int, Psychoses int, Depression int"
)


sqlQuery(
  db_dbProj,
  "UPDATE ICD_COMORBID_DIM SET CHF=1
        WHERE
        ICD10Code LIKE 'I09.981%' OR ICD10Code LIKE 'I50.%'
        OR ICD10Code LIKE 'I51.81%' OR ICD10Code LIKE 'I97.713[0-1]%'
        OR ICD10Code LIKE 'O29.12%' OR ICD10Code LIKE 'R57.0%'
        OR ICD10Code LIKE 'Z95.81[1-2]%'
        OR ICD9Code LIKE '398.91%' OR ICD9Code LIKE '428.[0-9]%'

        UPDATE ICD_COMORBID_DIM SET VD=1
        WHERE
        ICD10Code LIKE 'A18.84%'
        OR ICD10Code LIKE 'A32.82%'
        OR ICD10Code LIKE 'A39.51'
        OR ICD10Code LIKE 'A52.03%'
        OR ICD10Code LIKE 'B33.21%'
        OR ICD10Code LIKE 'B37.6%'
        OR ICD10Code LIKE 'I01.1%'
        OR ICD10Code LIKE 'I01.[8-9]%'
        OR ICD10Code LIKE 'I02.0%'
        OR ICD10Code LIKE 'I0[5-8].%'
        OR ICD10Code LIKE 'I09.1%'
        OR ICD10Code LIKE 'I09.89%'
        OR ICD10Code LIKE 'I3[3-9].%'
        OR ICD10Code LIKE 'M32.11%'
        OR ICD10Code LIKE 'Q2[2-3].%'
        OR ICD10Code LIKE 'Z95.[2-4]%'
        OR ICD9Code LIKE '093.2%'
        OR ICD9Code LIKE '39[4-6].%'
        OR ICD9Code LIKE '397.0%'
        OR ICD9Code LIKE '397.1%'
        OR ICD9Code LIKE '397.9%'
        OR ICD9Code LIKE '424.%'
        OR ICD9Code LIKE '746.[3-6]%'
        OR ICD9Code LIKE 'V42.2%'
        OR ICD9Code LIKE 'V43.3%';
        UPDATE ICD_COMORBID_DIM SET PCD=1
        WHERE
        ICD10code LIKE 'I27.%' OR ICD10code LIKE 'I28.[0-1]%'
        OR ICD10code LIKE 'I28.[8-9]%'
        OR ICD9code LIKE '415.1[1-9]%'
        OR ICD9code LIKE '416.%'
        OR ICD9code LIKE '417.9%';

        UPDATE ICD_COMORBID_DIM SET PVD=1
        WHERE
        ICD10Code like 'A52.0%' OR ICD10Code like 'I70.[0-1]'
        OR ICD10Code like 'I70.[2-9]%' OR ICD10Code like 'I71.0[0-3]%'
        OR ICD10Code like 'I71.[1-9]%' OR ICD10code like 'I72.%'
        OR ICD10code like 'I73.01%' OR ICD10code like 'I73.1%'
        OR ICD10Code like 'I73.[8-9]%' OR ICD10Code like 'I74.01%'
        OR ICD10Code like 'I74.09%' OR ICD10Code like 'I74.1%'
        OR ICD10Code like 'I74.[2-4]%' OR ICD10Code like 'I75.0[1-2]%'
        OR ICD10Code like 'I75.8%' OR ICD10Code like 'I77.[0-9]%'
        OR ICD10Code like 'I78.%' OR ICD10Code like 'I79.%'
        OR ICD10Code like 'I99.[8-9]%' OR ICD10Code like 'K31.81%'
        OR ICD10Code like 'K55.1%' OR ICD10Code like 'K55.8%'
        OR ICD10Code like 'K55.9%' OR ICD10Code like 'Z95.82%'
        OR ICD9Code like '440.%'
        OR ICD9Code like '441.%'
        OR ICD9Code like '443.1%'
        OR ICD9Code like '443.[8-9]%'
        OR ICD9Code like '447.1%'
        OR ICD9Code like '557.1%'
        OR ICD9Code like '557.9%'
        OR ICD9Code like 'V43.4%';

        UPDATE ICD_COMORBID_DIM SET HT=1
        WHERE
        ICD10Code like 'I10.%'
        OR ICD10Code like 'I16.0%'
        OR ICD10Code like 'I16.9%'
        OR ICD10Code like 'O10.01%'
        OR ICD10Code like 'O10.0[2-3]%'
        OR ICD10Code like 'I1[1-3].%'
        OR ICD10Code like 'I15.%'
        OR ICD10Code like 'I16.1%'
        OR ICD10Code like 'O10.[1-4]1_%'
        OR ICD10Code like 'O10.[1-4][2-3]%'
        OR ICD10Code like 'O10.91_%'
        OR ICD10Code like 'O10.9[2-3]%'
        OR ICD10Code like 'O11.%'
        OR ICD10Code like 'O16.%'
        OR ICD9Code LIKE '401.%'
        OR ICD9Code like '40[2-5].%';

        UPDATE ICD_COMORBID_DIM SET Paralysis=1
        WHERE
        ICD10code like 'G04.1%' OR ICD10Code like 'G80.0%'
        OR ICD10Code like 'G81.%' OR ICD10Code like 'G82.%'
        OR ICD10Code like 'G83.[0-5]%' OR ICD10Code like 'G83.[8-9]%'
        OR ICD10Code like 'I69.0[3-6]%' OR ICD10Code like 'I69.1[3-6]'
        OR ICD10Code like 'I69.2[3-6]'
        OR ICD10Code like 'R53.2%'
        OR ICD9Code like '343.%'
        OR ICD9Code like '344.%'
        OR ICD9Code like '438.[2-4]'
        OR ICD9code like '438.5[0-3]%'
        OR ICD9Code like '780.72%';

        UPDATE ICD_COMORBID_DIM SET Neuro=1
        WHERE
        ICD10code LIKE 'G1[0-3].%' OR ICD10code LIKE 'G2[0-2].%'
        OR ICD10code like 'G25.4%' OR ICD10Code like 'G25.5%'
        OR ICD10Code like 'G31.2%' OR ICD10Code like 'G31.8%'
        OR ICD10Code like 'G31.9%' OR ICD10Code like 'G32.%'
        OR ICD10Code like 'G3[5-7].%' OR ICD10Code like 'G40.%'
        OR ICD10Code like 'G41.%' OR ICD10Code like 'G93.1%'
        OR ICD10Code like 'G93.4%' OR ICD10Code like 'R47.0%'
        OR ICD10Code like 'R56.%'
        OR ICD9Code like '331.9%'
        OR ICD9Code like '332.0%'
        OR ICD9Code like '332.1%'
        OR ICD9Code like '333.4%'
        OR ICD9Code like '333.5%'
        OR ICD9Code like '333.92%'
        OR ICD9Code like '33[4-5].%'
        OR ICD9Code like '336.2%'
        OR ICD9Code like '340.%'
        OR ICD9Code like '341.%'
        OR ICD9Code like '345.%'
        OR ICD9Code like '348.1%'
        OR ICD9Code like '348.3%'
        OR ICD9Code like '780.3%'
        OR ICD9Code like '784.3%';

        UPDATE ICD_COMORBID_DIM SET CPD=1
        WHERE
        ICD10code like 'J4[1-4].%' OR ICD10Code like 'J45.[2-5][0-2]%'
        OR ICD10Code like 'J45.90%' OR ICD10Code like 'J45.99%'
        OR ICD10Code like 'J47.1%' OR ICD10Code like 'J47.9%'
        OR ICD10Code like 'J6[0-1].%' OR ICD10Code like 'J62.0%'
        OR ICD10Code like 'J62.8%' OR ICD10Code like 'J63.%'
        OR ICD10Code like 'J6[4-7].%'OR ICD10Code like 'J68.4%'
        OR ICD10Code like 'J70.1%' OR ICD10Code like 'J70.3%'
        OR ICD9Code like '416.8%'
        OR ICD9Code like '416.9%'
        OR ICD9Code like '49[0-9].%'
        OR ICD9Code like '50[0-5].%'
        OR ICD9Code like '506.4%'
        OR ICD9Code like '508.1%'
        OR ICD9Code like '508.8%';

        UPDATE ICD_COMORBID_DIM SET DM=1
        WHERE
        ICD10code like 'E10.0%'
        OR ICD10code like 'E10.1%'
        OR ICD10code like 'E10.9%'
        OR ICD10code like 'E11.0%'
        OR ICD10code like 'E11.1%'
        OR ICD10code like 'E11.9%'
        OR ICD10code like 'E12.0%'
        OR ICD10code like 'E12.1%'
        OR ICD10code like 'E12.9%'
        OR ICD10code like 'E13.0%'
        OR ICD10code like 'E13.1%'
        OR ICD10code like 'E13.9%'
        OR ICD10code like 'E14.0%'
        OR ICD10code like 'E14.1%'
        OR ICD10code like 'E14.9%'
        OR ICD10Code like 'O24.%'
        OR ICD9Code like '250.[0-3]%';

        UPDATE ICD_COMORBID_DIM SET DMcomp=1
        WHERE
        ICD10code like 'E10.[2-8]%'
        OR ICD10code like 'E11.[2-8]%'
        OR ICD10code like 'E12.[2-8]%'
        OR ICD10code like 'E13.[2-8]%'
        OR ICD10code like 'E14.[2-8]%'
        OR ICD9code like '250.[4-9]%';

        UPDATE ICD_COMORBID_DIM SET DMany=1
        WHERE
        ICD10code like 'E10.%'
        OR ICD10code like 'E11.%'
        OR ICD10code like 'E13.%'
        OR ICD10code like 'E14.%'
        OR ICD10Code like 'O24.%'
        OR ICD9Code like '250.%';

        UPDATE ICD_COMORBID_DIM SET Thyroid=1
        WHERE
        ICD10Code like 'E0[0-3].%' OR ICD10Code like 'E89.0%'
        OR ICD10Code like 'E0[5-6].%' OR ICD10Code like 'O90.5%'
        OR ICD9code like '240.9%'
        OR ICD9code like '24[3-4].%'
        OR ICD9code like '246.1%'
        OR ICD9code like '246.8%';

        UPDATE ICD_COMORBID_DIM SET RF=1
        WHERE
        ICD10Code like 'N18.3%' OR ICD10Code like 'N19.%'
        OR ICD10Code like 'N18.[4-6]%' OR ICD10Code like 'Z49.[0-2]%'
        OR ICD10Code like 'Z49.3[1-2]%' OR ICD10Code like 'Z94.0%'
        OR ICD10Code like 'Z99.2%'
        OR ICD9Code like '585.3%'
        OR ICD9Code like '585.9%'
        OR ICD9Code like '586.%'
        OR ICD9Code like '585.[4-6]%'
        OR ICD9Code like 'V42.0%'
        OR ICD9Code like 'V45.1[1-2]%'
        OR ICD9Code like 'V56.[0-6]%'
        OR ICD9Code like 'V56.8%';

        UPDATE ICD_COMORBID_DIM SET LD=1
        WHERE
        ICD10Code like 'A51.45%' OR ICD10Code like 'A52.74%'
        OR ICD10Code like 'B18.%' OR ICD10Code like 'B19.[1-2]0%'
        OR ICD10Code like 'B19.9%' OR ICD10Code like 'B25.1%'
        OR ICD10Code like 'B58.1%' OR ICD10Code like 'K70.[0-1]%'
        OR ICD10Code like 'K70.2%' OR ICD10Code like 'K70.3[0-1]'
        OR ICD10Code like 'K70.9%'
        OR ICD10code like 'K71.[3-8]%' OR ICD10code like 'K7[3-7].%'
        OR ICD10code like 'B19.0%' OR ICD10code like 'B19.[1-2]1%'
        OR ICD10Code like 'I85.0[0-1]%' OR ICD10Code like 'I85.11%'
        OR ICD10code like 'I86.4%' OR ICD10Code like 'K70.4[0-1]%'
        OR ICD10code like 'K72.[0-1]%' OR ICD10code like 'K72.9[0-1]%'
        OR ICD10code like 'K76.[5-7]%' OR ICD10code like 'K91.82%'
        OR ICD10code like 'Z94.4%'
        OR ICD9code like '070.[2-3][2-3]%'
        OR ICD9code like '070.44%'
        OR ICD9code like '070.54%'
        OR ICD9code like '070.6%'
        OR ICD9code like '070.9%'
        OR ICD9code like '456.[0-2]%'
        OR ICD9code like '57[0-1].%'
        OR ICD9code like '572.[2-8]%'
        OR ICD9code like '573.[3-4]%'
        OR ICD9code like '573.[8-9]%'
        OR ICD9code like 'V42.7%';

        UPDATE ICD_COMORBID_DIM SET PUD=1
        WHERE
        ICD10code like 'K2[5-8].%'
        OR ICD9code like '53[1-4].7%'
        OR ICD9code like '53[1-4].9%';

        UPDATE ICD_COMORBID_DIM SET AIDS=1
        WHERE
        ICD10code like 'B20%' OR ICD10Code like 'Z21%'
        OR ICD10code like 'O98.71[1-3]%'
        OR ICD10code like 'O98.719%' OR ICD10code like 'O98.7[2-3]%'
        OR ICD9code like ' 04[2-4].%';

        UPDATE ICD_COMORBID_DIM SET Lymph=1
        WHERE
        ICD10Code like 'C8[1-5].%' OR ICD10Code like 'C88.%'
        OR ICD10Code like 'C96.%'
        OR ICD10code like 'C90.0%'
        OR ICD10Code like 'C90.2%'
        OR ICD9code like '20[0-2].%'
        OR ICD9code like '203.0%'
        OR ICD9code like '238.6%';

        UPDATE ICD_COMORBID_DIM SET MC=1
        WHERE
        ICD10code like 'C7[7-9].%' OR ICD10code like 'C80.%'
        OR ICD9code like '19[6-9].%';

        UPDATE ICD_COMORBID_DIM SET Tumor=1
        WHERE
        ICD10code like 'C0[0-1].%'
        OR ICD10code like 'C2[0-6].%'
        OR ICD10code like 'C3[0-4].%'
        OR ICD10code like 'C3[7-9].%'
        OR ICD10code like 'C4[0-1].%'
        OR ICD10code like 'C43.%'
        OR ICD10code like 'C4[5-9].%'
        OR ICD10code like 'C5[0-8].%'
        OR ICD10code like 'C6[0-9].%'
        OR ICD10code like 'C7[0-6].%'
        OR ICD10code like 'C97.%'
        OR ICD9code like '14[0-9].%'
        OR ICD9code like '15[0-9].%'
        OR ICD9code like '16[0-9].%'
        OR ICD9code like '17[0-2].%'
        OR ICD9code like '1[74-95].%'
        OR ICD9code like '1[74-95].%'
        OR ICD9code like '1[74-95].%';

        UPDATE ICD_COMORBID_DIM SET RA=1
        WHERE
        ICD10code like 'A18.0[1-2]%' OR ICD10code like 'A39.84%'
        OR ICD10code like 'A54.4[1-2]%' OR ICD10code like 'L94.0%'
        OR ICD10Code like 'L94.1%' OR ICD10Code like 'L94.3%'
        OR ICD10Code like 'M01.X%' OR ICD10Code like 'M02.%'
        OR ICD10Code like 'M05.%' OR ICD10Code like 'M06.%'
        OR ICD10Code like 'M07.6%' OR ICD10Code like 'M08.%'
        OR ICD10Code like 'M12.0%' OR ICD10Code like 'M30.%'
        OR ICD10Code like 'M31.[0-3]%' OR ICD10Code like 'M3[2-5].%'
        OR ICD10Code like 'M45.%' OR ICD10Code like 'M46.[0-1]%'
        OR ICD10Code like 'M46.8%'  OR ICD10Code like 'M46.9%'
        OR ICD10Code like 'M49.8[0-9]%'
        OR ICD9Code like '446.%'
        OR ICD9Code like '701.0%'
        OR ICD9Code like '710.[0-4]%'
        OR ICD9Code like '710.[8-9]%'
        OR ICD9Code like '711.2%'
        OR ICD9Code like '714.%'
        OR ICD9Code like '719.3%'
        OR ICD9Code like '720.%'
        OR ICD9Code like '725.%'
        OR ICD9Code like '728.5%'
        OR ICD9Code like '728.89%'
        OR ICD9Code like '729.30%';

        UPDATE ICD_COMORBID_DIM SET Coag=1
        WHERE
        ICD10Code like 'D61.%' OR ICD10Code like 'D6[5-8].%'
        OR ICD10code like 'D69.1%' OR ICD10Code like 'D69.[3-6]%'
        OR ICD10Code like 'D69.[8-9]%' OR ICD10Code like 'D75.82%'
        OR ICD10Code like 'O99.11%' OR ICD10Code like 'O99.1[2-3]%'
        OR ICD9Code like '286.%'
        OR ICD9Code like '287.1%'
        OR ICD9Code like '287.[3-5]%';

        UPDATE ICD_COMORBID_DIM SET Obese=1
        WHERE
        ICD10Code like 'E66.%' OR ICD10Code like 'O99.21%'
        OR ICD10Code like 'R93.9%'
        OR ICD10Code like 'Z68.[3-4]%'
        OR ICD10Code like 'Z68.54%'
        OR ICD9Code like '278.0%';

        UPDATE ICD_COMORBID_DIM SET WtLoss=1
        WHERE
        ICD10Code like 'E4[0-6].%' OR ICD10code like 'R63.4%'
        OR ICD10Code like 'R64.%'
        OR ICD10Code like 'O25.%'
        OR ICD9code like '26[0-3].%'
        OR ICD9code like '783.2%'
        OR ICD9code like '799.4%';

        UPDATE ICD_COMORBID_DIM SET FED=1
        WHERE
        ICD10code like 'E22.2%' OR ICD10Code like 'E8[6-7].%'
        OR ICD9Code like '253.6%'
        OR ICD9Code like '276.%';

        UPDATE ICD_COMORBID_DIM SET BLAnemia=1
        WHERE
        ICD10code like 'D50.0%' OR ICD10code like 'O90.81%'
        OR ICD10code like 'O99.0[2-3]%'
        OR ICD9code like '280.0%';

        UPDATE ICD_COMORBID_DIM SET DAnemia=1
        WHERE
        ICD10code like 'D50.1%' OR ICD10code like 'D50.[8-9]%'
        OR ICD10Code like 'D5[1-3].%'
        OR ICD10Code like 'D63.%' OR ICD10Code like 'D64.9%'
        OR ICD10Code like 'O99.01[1-3].%' OR ICD10Code like 'O99.019.%'
        OR ICD9Code like '280.[1-9]%'
        OR ICD9Code like '281.%';

        UPDATE ICD_COMORBID_DIM SET Alcohol=1
        WHERE
        ICD10code like 'F10%' OR ICD10Code like 'G62.1%'
        OR ICD10Code like 'I42.6%' OR ICD10Code like 'K29.2[0-1]%'
        OR ICD10Code like 'O99.31[0-5]%' OR ICD10Code like 'K70.1[0-1]%'
        OR ICD9Code like '265.2%'
        OR ICD9Code like '291.[1-3]%'
        OR ICD9Code like '291.[5-9]%'
        OR ICD9Code like '303.0%'
        OR ICD9Code like '303.9%'
        OR ICD9Code like '305.0%'
        OR ICD9Code like '357.5%'
        OR ICD9Code like '425.5%'
        OR ICD9Code like '535.3%'
        OR ICD9Code like '571.[0-3]%'
        OR ICD9Code like '980.%'
        OR ICD9Code like 'V11.3%';

        UPDATE ICD_COMORBID_DIM SET Drug=1
        WHERE
        ICD10Code like 'F1[1-6].%' OR ICD10Code like 'F1[8-9].%'
        OR ICD10code LIKE 'O99.32%'
        OR ICD9Code like '292.%'
        OR ICD9Code like '304.%'
        OR ICD9Code like '305.[2-9]%'
        OR ICD9Code like 'V65.42%';

        UPDATE ICD_COMORBID_DIM SET Psychoses=1
        WHERE
        ICD10Code like 'F19.159%' OR ICD10Code like 'F19.25%'
        OR ICD10Code like 'F19.95%' OR ICD10Code like 'F2[0-5].%'
        OR ICD10Code like 'F2[8-9].%' OR ICD10Code like 'F3[0-1].%'
        OR ICD10Code like 'F32.[4-5].%' OR ICD10Code like 'F33.4%'
        OR ICD10Code like 'F34.%' OR ICD10Code like 'F39.%'
        OR ICD10Code like 'F44.89%' OR ICD10Code like 'F84.3.%'
        OR ICD9Code like '293.8%'
        OR ICD9Code like '295.%'
        OR ICD9Code like '296.04%'
        OR ICD9Code like '296.14%'
        OR ICD9Code like '296.44%'
        OR ICD9Code like '296.54%'
        OR ICD9Code like '297.%'
        OR ICD9Code like '298.%';

        UPDATE ICD_COMORBID_DIM SET Depression=1
        WHERE
        ICD10code LIKE 'F06.3[1-2]%' OR ICD10code LIKE 'F06.34%'
        OR ICD10Code like 'F32.[0-3]%' OR ICD10Code like 'F32.8%'
        OR ICD10Code like 'F32.9%' OR ICD10Code like 'F33.[0-3]%'
        OR ICD10Code like 'F33.[8-9].%'OR ICD10Code like 'F34.1%'
        OR ICD9Code like '300.4%'
        OR ICD9Code like '301.12%'
        OR ICD9Code like '309.[0-1]%'
        OR ICD9Code like '311%';"
)

comor <- sqlFetch(db_dbProj, "ICD_COMORBID_DIM")
fwrite(comor, paste0(results_dir_dim, "ICD_COMORBID_DIM.txt"),
  sep = "\t", row.names = FALSE
)
