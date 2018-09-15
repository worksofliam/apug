
        Dcl-Pr APUG Pointer EXTPROC;
          Path Char(128) Const;
        End-Pr;
        
        Dcl-S Ptr    Pointer;
        Dcl-S Result Char(256);
        
        Ptr = APUG('/home/liam/apug/test.pug') + 2;
        
        Result = %Str(Ptr);
        
        Return;