
        /copy 'headers/apug_h.rpgle'
        
        Dcl-S Ptr    Pointer;
        Dcl-S Result Char(256);
        Dcl-S JobUser Char(10) Inz(*USER);
        
        APUG_Init();
        APUG_AddVar('user':%Trim(JobUser));
        Ptr = APUG('./pug/test1.pug');
        
        Result = %Str(Ptr);
        
        Return;