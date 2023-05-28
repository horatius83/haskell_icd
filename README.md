# haskell_icd
ICD-10 lookup service written in Haskell

I wanted to explore Haskell again so I created an ICD-10 lookup program. ICD-10 codes are codes defined by CMS to describe injuries to a patient. There are a lot of them, and some of them are pretty bizarre. The core loop is to just query a SQLite database with various values.
Example:

```powershell
PS C:\Users\horat\Documents\github\haskell_icd\icd10-service> stack run
Checking if icd10Codes.db exists...
Enter ICD 10 code to lookup (or quit):
jet engine
Looking up jet engine
"V9733 Sucked into jet engine"
"V9733XA Sucked into jet engine, initial encounter"
"V9733XD Sucked into jet engine, subsequent encounter"
"V9733XS Sucked into jet engine, sequela"
Enter ICD 10 code to lookup (or quit):
quit
PS C:\Users\horat\Documents\github\haskell_icd\icd10-service>
```

The program will:
1. Get any override values for the database path or CMS text file path
2. Check if the database exists
    1. If the database does not exist
    2. Load the CMS text file
    3. Parse all the ICD-10 codes out of it
    4. Insert all of those codes into the database
3. Ask the user to enter in a description to search for or quit
    1. If quit, terminate program
4. Look for a value similar to the description
5. Display results
6. Go back to step #3

The zip file provided by CMS can be found [here](https://www.cms.gov/files/zip/2023-code-descriptions-tabular-order-updated-01/11/2023.zip)