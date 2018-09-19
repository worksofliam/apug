
        Dcl-Pr APUG_Init Pointer EXTPROC;
        End-Pr;

        Dcl-Pr APUG_Execute Pointer EXTPROC;
          pEngine Pointer;
          Path    Char(128) Const;
        End-Pr;
        
        Dcl-Pr APUG_SetDelims EXTPROC;
          pDelims Char(13) Const;
        End-Pr;
        
        Dcl-Pr APUG_AddVar EXTPROC;
          pEngine Pointer;
          pKey    Pointer Value Options(*String);
          pValue  Pointer Value Options(*String);
        End-Pr;