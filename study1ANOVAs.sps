* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.

EXAMINE VARIABLES=sur_rate hap_rate ang_rate amb_rate pos_rate neg_rate amw_rate pow_rate new_rate
  /PLOT NPPLOT
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

GLM hap_rate pos_rate pow_rate ang_rate neg_rate new_rate sur_rate amb_rate amw_rate
  /WSFACTOR=Valence 3 Polynomial Stim 3 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(Valence) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(Stim) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(Valence*Stim) COMPARE (Valence) ADJ(BONFERRONI)
  /EMMEANS=TABLES(Valence*Stim) COMPARE (Stim) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ OPOWER 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Valence Stim Valence*Stim.



