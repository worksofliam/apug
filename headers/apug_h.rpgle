
        Dcl-Pr APUG_Init EXTPROC;
        End-Pr;

        Dcl-Pr APUG_Execute Pointer EXTPROC;
          Path Char(128) Const;
        End-Pr;
        
        Dcl-Pr APUG_SetDelims EXTPROC;
          pDelims Char(13) Const;
        End-Pr;
        
        Dcl-Pr APUG_AddVar EXTPROC;
          pKey   Pointer Value Options(*String);
          pValue Pointer Value Options(*String);
        End-Pr;