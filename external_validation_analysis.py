from pathlib import Path
import pandas as pd, numpy as np, re, json, platform, sys
from docx import Document
from sklearn.model_selection import RepeatedKFold
from sklearn.impute import SimpleImputer
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from scipy import stats
import matplotlib.pyplot as plt
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.utils import get_column_letter

BASE=Path('/mnt/data')
ext_path=BASE/'External validation water quality for Scientific reports.csv'
main_doc=BASE/'Explanaable water quality model_AH_Revised (1).docx'
out_xlsx=BASE/'External_Validation_Generalizability_Results.xlsx'
out_fig=BASE/'External_validation_observed_predicted.png'
out_json=BASE/'External_validation_summary.json'

# robust external read
ext=pd.read_csv(ext_path, encoding='cp1252', skipinitialspace=True)
ext.columns=[str(c).strip() for c in ext.columns]
ext=ext.loc[:, [c for c in ext.columns if c and not c.lower().startswith('unnamed')]]
for c in ext.columns:
    if c!='sample': ext[c]=pd.to_numeric(ext[c], errors='coerce')
ext['sample']=ext['sample'].astype(str).str.strip().replace({'nan':''})
ext=ext.dropna(subset=['WQI']).reset_index(drop=True)
features=['pH','TDS','Cl','SO4','Na','K','Ca','Mg','Total Hardness']
ext=ext.dropna(subset=features+['WQI']).reset_index(drop=True)

# exact duplicates across predictors + target; keep first
ext['duplicate_exact']=ext.duplicated(subset=features+['WQI'], keep=False)
ext_unique=ext.drop_duplicates(subset=features+['WQI'], keep='first').reset_index(drop=True)

# Extract development data from main manuscript Figure 2 embedded plots is impossible from docx tables;
# use values reported in final manuscript to reconstruct source through local repository artifacts not present.
# Search current workdir for original csv/xlsx, otherwise stop transparently.
files=list(BASE.glob('*.xlsx'))+list(BASE.glob('*.csv'))
print('External rows',len(ext),'unique',len(ext_unique))
print('Available files', [x.name for x in files])

# summarize external integrity regardless
summary={
 'external_rows_total':int(len(ext)),
 'external_rows_unique_exact':int(len(ext_unique)),
 'exact_duplicate_rows_removed':int(len(ext)-len(ext_unique)),
 'variables':features+['WQI'],
 'wqi_min':float(ext_unique.WQI.min()),'wqi_max':float(ext_unique.WQI.max()),
 'wqi_mean':float(ext_unique.WQI.mean()),'wqi_sd':float(ext_unique.WQI.std(ddof=1)),
}

# Save audit workbook initially
with pd.ExcelWriter(out_xlsx, engine='openpyxl') as w:
    ext.to_excel(w,sheet_name='External all rows',index=False)
    ext_unique.to_excel(w,sheet_name='External unique rows',index=False)
    pd.DataFrame([summary]).to_excel(w,sheet_name='External audit',index=False)

out_json.write_text(json.dumps(summary,indent=2),encoding='utf-8')
print(json.dumps(summary,indent=2))
