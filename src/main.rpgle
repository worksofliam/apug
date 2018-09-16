
        /copy 'headers/apug_h.rpgle'
        
        Dcl-S Ptr    Pointer;
        Dcl-S Result Char(256);
        
        APUG_Init();
        APUG_AddVar('hello':'This is awesome');
        Ptr = APUG('./pug/test1.pug');
        
        Result = %Str(Ptr);
        
        Return;