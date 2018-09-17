        
        Ctl-Opt NoMain;
        
        Dcl-Ds C Qualified;
          LT Char(1) Inz('<');
          MT Char(1) Inz('>');
          FS Char(1) Inz('/');
          BS Char(1) Inz('\');
          EQ Char(1) Inz('=');
          SM Char(1) Inz('"');
          QT Char(1) Inz('''');
          CM Char(1) Inz(',');
          OB Char(1) Inz('(');
          CB Char(1) Inz(')');
          DT Char(1) Inz('.');
          P  Char(1) Inz(x'BB'); //Pipe
          HS Char(1) Inz(x'4A'); //hash/pound
        End-Ds;
        
        Dcl-C LINE_LEN 512;

        Dcl-C KEY_LEN   128;
        Dcl-C VALUE_LEN 1024;
        
        Dcl-C VAL_LEN  256;
        Dcl-C TAG_LEN  10;
        Dcl-C TAG_LVLS 20;
        
        Dcl-C MODE_TAG  1;
        Dcl-C MODE_PROP 2;
        DCL-C MODE_VAL  3;
        
        Dcl-C MODE_PROP_KEY   4;
        Dcl-C MODE_PROP_VALUE 5;
        
        Dcl-C MODE_VAL_CONST 6;
        Dcl-C MODE_VAL_VAR   7;

        Dcl-Ds Property_T Qualified Template;
          Name  Varchar(10)  Inz('');
          Value Varchar(256) Inz('');
        End-Ds;
        
        //----------------------------------------------
        
        Dcl-Ds EachLoop Qualified;
          AfterSpaces Int(5) Inz(0);  //What index is inside the each loop
          Count       Int(5) Inz(0);  //How many times to loop
          Line        Int(5) Inz(-1); //Restart from here
          
          ArrayName  Varchar(KEY_LEN) Inz(''); //Original array name
          ItemName   Varchar(KEY_LEN) Inz(''); //Temp item name
          CurrentInx Int(5)           Inz(-1);  //Current index
        End-Ds;
        
        Dcl-S BlockStart   Int(5);

        Dcl-S pugSource   Pointer;
        Dcl-S CurrentLine Int(5);
        
        Dcl-S ClosingIndx Int(3) Inz(0);
        Dcl-DS ClosingTags Qualified Dim(TAG_LVLS);
          Tag   Varchar(TAG_LEN) Inz('');
          Space Int(3);
        End-Ds;
        
        Dcl-S  APUG_VarsList Pointer;
        Dcl-S  APUG_VarPtr   Pointer;      
        Dcl-Ds APUG_Variable Qualified Based(APUG_VarPtr);
          Key   Varchar(KEY_LEN);
          Value Varchar(VALUE_LEN);
        End-Ds;
        
        Dcl-S OUTPUT Varchar(8192) Inz('');
        
        //----------------------------------------------
        
        /copy 'headers/arraylist_h.rpgle'
        
        Dcl-Ds File_Temp Qualified Template;
          PathFile char(128);
          RtvData  char(LINE_LEN);
          OpenMode char(5);
          FilePtr  pointer inz;
        End-ds;
        
        dcl-pr OpenFile pointer extproc('_C_IFS_fopen');
          *n pointer value;  //File name
          *n pointer value;  //File mode
        end-pr;
        
        dcl-pr ReadFile pointer extproc('_C_IFS_fgets');
          *n pointer value;  //Retrieved data
          *n int(10) value;  //Data size
          *n pointer value;  //Misc pointer
        end-pr;
        
        dcl-pr WriteFile pointer extproc('_C_IFS_fwrite');
          *n pointer value;  //Write data
          *n int(10) value;  //Data size
          *n int(10) value;  //Block size
          *n pointer value;  //Misc pointer
        end-pr;
        
        dcl-pr CloseFile extproc('_C_IFS_fclose');
          *n pointer value;  //Misc pointer
        end-pr;
        
        //----------------------------------------------
        
        Dcl-Proc APUG_SetDelims Export;
          Dcl-Pi *N;
            pDelims Char(13) Const;
          End-Pi;
          
          C = pDelims;
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc APUG_Init Export;
          OUTPUT = '';
        
          ClosingIndx = 0;
          Clear ClosingTags;
          
          APUG_VarsList = arraylist_create();
          pugSource     = arraylist_create();
          
          BlockStart  = 0;
          
          Reset EachLoop;
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc APUG_AddVar Export;
          Dcl-Pi *N;
            pKey   Pointer Value Options(*String);
            pValue Pointer Value Options(*String);
          End-Pi;
          
          APUG_VarPtr = %Alloc(%Size(APUG_Variable));
          
          APUG_Variable.Key   = %Str(pKey:KEY_LEN);
          APUG_Variable.Value = %Str(pValue:VALUE_LEN);
          
          arraylist_add(APUG_VarsList:
                        %Addr(APUG_Variable):
                        %Size(APUG_Variable));
                        
          Dealloc(NE) APUG_VarPtr;
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc APUG_Execute Export;
          Dcl-Pi *N Pointer;
            pPath Char(128) Const;
          End-Pi;
          
          Dcl-S lLine  Char(LINE_LEN);
          Dcl-S lIndex Int(3);

          ProcessFile(pPath:0);

          //Now process all lines
          For CurrentLine = 0 to arraylist_getSize(pugSource) - 1;
            lLine = %Str(arraylist_get(pugSource : CurrentLine):LINE_LEN);
            ProcessLine(lLine);
          Endfor;
          
          //Add the unclosed tags!
          For lIndex = ClosingIndx downto 1;
            OUTPUT += C.LT + C.FS + ClosingTags(lIndex).Tag + C.MT;
          Endfor;
          
          arraylist_dispose(APUG_VarsList);
          arraylist_dispose(pugSource);
          
          Output += x'00';
          Return %Addr(Output) + 2;
        
        End-Proc;
        
        //----------------------------------------------

        Dcl-Proc ProcessFile;
          Dcl-Pi *N;
            pPath   Char(128) Const;
            pSpaces Int(3)    Const;
          End-Pi;

          Dcl-Ds pugFile LikeDS(File_Temp);
          Dcl-S  lSpaces Int(5);
        
          pugFile.PathFile = %TrimR(pPath) + x'00';
          pugFile.OpenMode = 'r' + x'00';
          pugFile.FilePtr  = OpenFile(%addr(pugFile.PathFile)
                                        :%addr(pugFile.OpenMode));
        
          If (pugFile.FilePtr = *null);
            OUTPUT = 'Failed to read file: ' + %TrimR(pPath);
          EndIf;
        
          Dow  (ReadFile(%addr(pugFile.RtvData)
                        :%Len(pugFile.RtvData)
                        :pugFile.FilePtr) <> *null);
        
            //End of record null
            //Line feed (LF)
            //Carriage return (CR)
            //Tab
            pugFile.RtvData = SpacePad(pSpaces) 
                            + %xlate(x'00250D05':'    ':pugFile.RtvData);
        
            //include keyword check
            lSpaces = SpaceCount(pugFile.RtvData);
            If (%Subst(pugFile.RtvData:lSpaces+1:7) = 'include');
              ProcessFile(%Subst(pugFile.RtvData:lSpaces+9):lSpaces);
            Else;
              arraylist_add(pugSource:
                            %Addr(pugFile.RtvData):
                            %Len(%TrimR(pugFile.RtvData)));
            Endif;

            pugFile.RtvData = ' ';
          Enddo;
        
          CloseFile(pugFile.FilePtr);
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc ProcessLine;
          Dcl-Pi *N;
            pLine Char(LINE_LEN) Value;
          End-Pi;
        
          Dcl-S lMode     Int(3);
          Dcl-S lLen      Int(5);
          Dcl-S lIndex    Int(5);
          Dcl-S lChar     Char(1);
          Dcl-S lSpaces   Int(5);
          Dcl-S lIsCond   Ind;
        
          Dcl-S lPropIdx  Int(3);
          Dcl-S lIsString Ind;
          Dcl-S lPropMode Int(3);
          
          Dcl-S lVarIndx  Int(5);
          Dcl-S lEvalMode Int(3);
        
          Dcl-Ds CurrentElement Qualified;
            Tag        Varchar(TAG_LEN)   Inz('');
            Properties LikeDS(Property_T) Inz Dim(5);
            Value      Varchar(VAL_LEN) Inz('');
          End-Ds;
          
          //Default value for some variables
          
          lLen  = %Len(%TrimR(pLine));
          lMode = MODE_TAG;
          lIsString = *Off;
          lPropIdx  = 1;
          lPropMode = MODE_PROP_KEY;
          lIsCond   = *Off;
          
          lEvalMode  = MODE_VAL_CONST;
          
          // Processing starts below
          
          //No point running the line if it's blank!
          If (pLine = *Blank);
            Return;
          Endif;
        
          lSpaces = SpaceCount(pLine);
          
          //Check if end of each block and go back if possible
          If (EachLoop.AfterSpaces >= lSpaces);
            If (EachLoop.CurrentInx < EachLoop.Count-1);
              //Update temp variable
              EachLoop.CurrentInx += 1;
              CurrentLine = EachLoop.Line;
              Return;
                
            Else;
              //Delete temp variable
              EachLoop.AfterSpaces = 0;
              Reset EachLoop;
            Endif;
          Else;
            If (EachLoop.CurrentInx >= 0);
              pLine = %ScanRpl(EachLoop.ItemName:
                               EachLoop.ArrayName + 
                               '.' + %Char(EachLoop.CurrentInx):
                               pLine:1:lLen + %Len(EachLoop.ArrayName));
              lLen = %Len(%TrimR(pLine));
            Endif;
          Endif;
          
          //Check if we need to close any existing tags.
          For lIndex = ClosingIndx downto 1;
            If (ClosingTags(ClosingIndx).Space >= lSpaces);
              OUTPUT += C.LT + C.FS + ClosingTags(ClosingIndx).Tag + C.MT;
              ClosingIndx -= 1;
            Endif;
          Endfor;
          
          //Check if inside block that cannot run (if statement)
          If (BlockStart <> 0);
            If (lSpaces > BlockStart);
              Return;
            Else;
              BlockStart = 0;
            Endif;
          Endif;
        
          //Conditional checking
          Select;
            When (%Subst(pLine:lSpaces+1:1) = C.DT); //Dot for class
              CurrentElement.Tag = 'div';
              CurrentElement.Properties(lPropIdx).Name = 'class';
              lMode     = MODE_PROP;
              lPropMode = MODE_PROP_VALUE;
              
              lIsCond   = *On; 
              lSpaces  += 1;
              
            When (%Subst(pLine:lSpaces+1:1) = C.HS); //Hash for ID
              CurrentElement.Tag = 'div';
              CurrentElement.Properties(lPropIdx).Name = 'id';
              lMode     = MODE_PROP;
              lPropMode = MODE_PROP_VALUE;
              
              lIsCond   = *On; 
              lSpaces  += 1;
              
            When (%Subst(pLine:lSpaces+1:1) = C.P); //Pipe
              lChar = %Subst(pLine:lSpaces+2:1);
              
              If (lChar = C.EQ);
                OUTPUT += GetVarByIndex(%TrimR(%Subst(pLine:lSpaces+3)));
              Else;
                OUTPUT += %TrimR(%Subst(pLine:lSpaces+2));
              Endif;
              
              Return;
            
            When (%Subst(pLine:lSpaces+1:2) = C.FS + C.FS);
              Return;
          Endsl;
        
          //Now time to process the line
          For lIndex = (lSpaces+1) to lLen;
            lChar = %Subst(pLine:lIndex:1); //Current character
        
            Select;
              When (lMode = MODE_TAG);
                Select;
                  When (lChar = C.OB); //User is adding properties
                    lMode     = MODE_PROP;
                    lPropMode = MODE_PROP_KEY;
        
                  When (lChar = ' '); //Usually means no properties and just a const value!
                    //Check if it's a special keyword
                    If (IsConditionalStatement(CurrentElement.Tag));
                      If (ProcessCondition(CurrentElement.Tag
                                          :%TrimR(%Subst(pLine:lIndex+1))));
                        Select; //Can run block
                          When (CurrentElement.Tag = 'if');
                            BlockStart = 0;
                          When (CurrentElement.Tag = 'each');
                            EachLoop.AfterSpaces = lSpaces;
                            //Create temp var
                        Endsl;
                        
                      Else;
                        Select; //Cannot run block
                          When (CurrentElement.Tag = 'if');
                            BlockStart = lSpaces;
                          When (CurrentElement.Tag = 'each');
                            EachLoop.AfterSpaces = lSpaces;
                        Endsl;
                      Endif;
                      
                      Return;
                    Endif;
                    
                    lMode = MODE_VAL;
                    lEvalMode = MODE_VAL_CONST;
                    
                  When (lChar = C.EQ); //Usually means no properties and just a variable!
                    lMode = MODE_VAL;
                    lEvalMode = MODE_VAL_VAR;
        
                  Other;
                    CurrentElement.Tag += lChar; //Append to the tag name
                Endsl;
        
              When (lMode = MODE_PROP); //We're parsing the properties now!
                Select;
                  When (lChar = C.BS); //Check if the user is added a quote mark
                    If (%Subst(pLine:lIndex+1:1) = C.QT);
                      lChar = '';
                    Endif;
        
                  When (lChar = C.QT); //Check if it's the end of a string or the user is adding a quote mark
                    If (%Subst(pLine:lIndex-1:1) <> C.BS);
                      lIsString = NOT lIsString;
                      lChar = '';
                    Endif;
        
                  When (lChar = C.CB); //Could be the end of the properties
                    If (lIsString = *Off);
                      lMode = MODE_VAL;
                      
                      lChar = %Subst(pLine:lIndex+1:1);
                      If (lChar = C.EQ); //It's a variable next!
                        lEvalMode =  MODE_VAL_VAR;
                      Else;
                        lEvalMode =  MODE_VAL_CONST;
                      Endif;
                      
                      lIndex += 1;
                      lChar = ''; //Add nothing, it's the end!
                    Endif;
        
                  When (lChar = C.EQ); //Next is the value to the key!
                    If (lIsString = *Off);
                      lPropMode = MODE_PROP_VALUE;
                      lChar = '';
                    Endif;
        
                  When (lChar = C.CM); //Next prop!
                    If (lIsString = *Off);
                      lPropIdx += 1;
                      lPropMode = MODE_PROP_KEY;
                      lChar = '';
                    Endif;
                Endsl;
        
                If (lChar <> *BLANK); //If the character is not blank, append to correct prop variable
                  Select;
                    When (lPropMode = MODE_PROP_KEY);
                      CurrentElement.Properties(lPropIdx).Name += lChar;
                    When (lPropMode = MODE_PROP_VALUE);
                      CurrentElement.Properties(lPropIdx).Value += lChar;
                  Endsl;
                Endif;
        
              When (lMode = MODE_VAL); //Now we're just appending the value!
                CurrentElement.Value += lChar;
            Endsl;
          Endfor;
          
          If (lIsCond);
            lSpaces -= 1;
          Endif;
          
          //Time to generate the output

          OUTPUT += C.LT + CurrentElement.Tag;
          
          //Append proerties if any
          For lIndex = 1 to %Elem(CurrentElement.Properties);
            If (CurrentElement.Properties(lIndex).Name <> *BLANK);
              OUTPUT += ' ' + CurrentElement.Properties(lIndex).Name;
              If (CurrentElement.Properties(lIndex).Value <> *BLANK);
                OUTPUT += C.EQ + C.SM
                       + CurrentElement.Properties(lIndex).Value + C.SM;
              Endif;
            Else;
              Leave;
            Endif;
          Endfor;
      
          If (CurrentElement.Value = *BLANK);
            //Will close in the future.
            ClosingIndx += 1;
            ClosingTags(ClosingIndx).Tag   = CurrentElement.Tag;
            ClosingTags(ClosingIndx).Space = lSpaces; 
            OUTPUT += C.MT;
          Else;
            //Write close tag
            If (lEvalMode = MODE_VAL_CONST);
              OUTPUT += C.MT + %Trim(CurrentElement.Value) + C.LT + C.FS
                             + CurrentElement.Tag + C.MT;
            Else;
                OUTPUT += C.MT
                       + GetVarByIndex(%Trim(CurrentElement.Value))
                       + C.LT + C.FS
                       + CurrentElement.Tag + C.MT;
            Endif;
          Endif;
        
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc VarExists;
          Dcl-Pi *N Int(10);
            pKey Pointer Value Options(*String);
          End-Pi;
          
          Dcl-S lIndex Int(10);
          
          If (arraylist_getSize(APUG_VarsList) = 0);
            Return -1;
            
          Else;
            For lIndex = 0 to arraylist_getSize(APUG_VarsList) - 1;
              APUG_VarPtr = arraylist_get(APUG_VarsList : lIndex);
                If (APUG_Variable.Key = %Str(pKey:KEY_LEN));
                  Return lIndex;
                Endif;
            endfor;
          Endif;
          
          Return -1;
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc GetVarByIndex; 
          Dcl-Pi *N Like(APUG_Variable.Value);
            pKey Pointer Value Options(*String);
          End-Pi;
          
          Dcl-S lIndex Uns(10);
          
          If (arraylist_getSize(APUG_VarsList) = 0);
            Return '';
            
          Else;
            For lIndex = 0 to arraylist_getSize(APUG_VarsList) - 1;
              APUG_VarPtr = arraylist_get(APUG_VarsList : lIndex);
                If (APUG_Variable.Key = %Str(pKey:KEY_LEN));
                  Return APUG_Variable.Value;
                Endif;
            endfor;
          Endif;
          
          Return '';
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc VarArrayCount;
          Dcl-Pi *N Int(5);
            pKey Pointer Value Options(*String);
          End-Pi;
          
          Dcl-S lLen   Int(3);
          Dcl-S lKey   Varchar(128);
          
          Dcl-S lCount Int(5);
          Dcl-S lIndex Uns(10);
          
          lCount = 0;
          
          lKey = %Str(pKey:KEY_LEN) + '.';
          lLen = %Len(lKey);
          
          If (arraylist_getSize(APUG_VarsList) = 0);
            Return 0;
            
          Else;
            For lIndex = 0 to arraylist_getSize(APUG_VarsList) - 1;
              APUG_VarPtr = arraylist_get(APUG_VarsList : lIndex);
                If (%Len(APUG_Variable.Key) >= lLen);
                  If (%Subst(APUG_Variable.Key:1:lLen) = lKey);
                    lCount += 1;
                  Endif;
                Endif;
            endfor;
          Endif;
          
          Return lCount;
          
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc IsConditionalStatement;
          Dcl-Pi *N Ind;
            pCondition  Char(TAG_LEN) Const;
          End-Pi;
          
          Return (pCondition = 'if' OR
                  pCondition = 'each'
                 );
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc ProcessCondition;
          Dcl-Pi *N Ind;
            pCondition  Char(TAG_LEN) Const;
            pExpression Pointer Value Options(*String);
          End-Pi;
          
          Dcl-S lCount Int(5);
          
          Select;
            When (pCondition = 'if');
              Return (VarExists(pExpression) >= 0);
              
            When (pCondition = 'each');
              //PARSE STUFF
              
              Reset EachLoop;
              ParseEach(pExpression);
              
              lCount = VarArrayCount(EachLoop.ArrayName);
              EachLoop.Count = lCount;
              
              If (lCount > 0);
                EachLoop.Line       = CurrentLine;
                EachLoop.CurrentInx = 0;
              Endif;
              
              Return (lCount > 0);
          Endsl;
          
          Return *Off;
        End-Proc;
        
        //----------------------------------------------
        
        //each Item in Items
        Dcl-Proc ParseEach;
          Dcl-Pi *N;
            pExpression Pointer;
          End-Pi;
          
          Dcl-S lPart  Int(3);
          Dcl-S lIndex Int(3);
          Dcl-S lChar  Char(1);
          Dcl-S lParts Varchar(KEY_LEN) Dim(3);
          Dcl-S lValue Varchar(KEY_LEN);
          
          lValue = %Str(pExpression:KEY_LEN);
          lPart = 1;
          
          For lIndex = 1 to %Len(lValue);
            lChar = %Subst(lValue:lIndex:1);
            
            If (lPart >= 4);
              Leave;
            Endif;
            
            If (lChar = ' ');
              lPart += 1;
            Else;
              lParts(lPart) += lChar;
            Endif;
          Endfor;
          
          EachLoop.ItemName  = lParts(1);
          EachLoop.ArrayName = lParts(3);
          
        End-Proc;
        
        //----------------------------------------------

        Dcl-Proc SpaceCount;
          Dcl-Pi *N Int(5);
            pLine Char(LINE_LEN);
          End-Pi;

          Dcl-S lIndex Int(5);
          Dcl-S lLen   Int(5);
          Dcl-S lChar  Char(1);

          lLen = %Len(%TrimR(pLine));

          For lIndex = 1 to lLen;
            lChar = %Subst(pLine:lIndex:1); //Current character
            If (lChar <> ' ');
              Return lIndex-1;
            Endif;
          Endfor;

          Return 0;
        End-Proc;
        
        //----------------------------------------------

        Dcl-Proc SpacePad;
          Dcl-Pi *N Varchar(LINE_LEN);
            pLength Int(3) Const;
          End-Pi;

          Dcl-S lResult Varchar(LINE_LEN);
          Dcl-S lIndex  Int(3);

          lResult = '';
          For lIndex = 1 to pLength;
            lResult += ' ';
          Endfor;

          Return lResult;
        End-Proc;