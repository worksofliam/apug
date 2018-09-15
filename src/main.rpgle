
        Dcl-Pr APUG_Init EXTPROC;
        End-Pr;

        Dcl-Pr APUG Pointer EXTPROC;
          Path Char(128) Const;
        End-Pr;
        
        Dcl-Pr APUG_AddVar EXTPROC;
          pKey   Pointer Value Options(*String);
          pValue Pointer Value Options(*String);
        End-Pr;
        
        Dcl-S Ptr    Pointer;
        Dcl-S Result Char(256);
        
        APUG_Init();
        APUG_AddVar('hello':'This is awesome');
        Ptr = APUG('./pug/test1.pug') + 2;
        
        Result = %Str(Ptr);
        
        Return;