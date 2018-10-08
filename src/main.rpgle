
        /copy 'headers/apug_h.rpgle'
        
        Dcl-S Engine  Pointer;
        Dcl-S ResPtr  Pointer;
        Dcl-S Result  Char(256);
        Dcl-S JobUser Char(10) Inz(*USER);
        
        Result = '';
        
        Engine = APUG_Init();
        APUG_AddVar(Engine:'user':%Trim(JobUser));
        ResPtr = APUG_Execute(Engine:'./pug/test1.pug');
        
        Result = %Str(ResPtr);
        
        APUG_Dealloc(Engine);
        
        Return;