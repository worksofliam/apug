        
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
        End-Ds;
        
        Dcl-C LINE_LEN 512;
        
        Dcl-C TAG_LEN  10;
        Dcl-C TAG_LVLS 20;
        
        Dcl-C MODE_TAG  1;
        Dcl-C MODE_PROP 2;
        DCL-C MODE_VAL  3;
        
        Dcl-C MODE_PROP_KEY   4;
        Dcl-C MODE_PROP_VALUE 5;
        
        //----------------------------------------------
        
        Dcl-S ClosingIndx Int(3) Inz(0);
        Dcl-DS ClosingTags Qualified Dim(TAG_LVLS);
          Tag   Varchar(TAG_LEN) Inz('');
          Space Int(3);
        End-Ds;
        
        Dcl-Ds Property_T Qualified Template;
          Name  Varchar(10)  Inz('');
          Value Varchar(256) Inz('');
        End-Ds;
        
        Dcl-S OUTPUT Varchar(8192) Inz('');
        
        //----------------------------------------------
        
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
            pDelims Char(10);
          End-Pi;
          
          C = pDelims;
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc APUG Export;
          Dcl-Pi *N Pointer;
            pPath Char(128) Const;
          End-Pi;
          
          Dcl-Ds pugSource LikeDS(File_Temp);
          Dcl-S  lIndex Int(3);
        
          OUTPUT = '';
        
          ClosingIndx = 0;
          Clear ClosingTags;
        
          ////pugSource.PathFile = %TrimL(pPath) + x'00';
          pugSource.PathFile = %TrimR(pPath) + x'00';
          pugSource.OpenMode = 'r' + x'00';
          pugSource.FilePtr  = OpenFile(%addr(pugSource.PathFile)
                                        :%addr(pugSource.OpenMode));
        
          If (pugSource.FilePtr = *null);
            OUTPUT = 'Failed to read file: ' + %TrimR(pPath);
          EndIf;
        
          Dow  (ReadFile(%addr(pugSource.RtvData)
                        :%Len(pugSource.RtvData)
                        :pugSource.FilePtr) <> *null);
        
            //End of record null
            //Line feed (LF)
            //Carriage return (CR)
            //Tab
            pugSource.RtvData = %xlate(x'00'+x'25'+x'0D'+x'05'
                                       :'    ':pugSource.RtvData);
        
            ProcessLine(pugSource.RtvData);
            pugSource.RtvData = ' ';
          Enddo;
        
          CloseFile(pugSource.FilePtr);
          
          //Add the unclosed tags!
          For lIndex = ClosingIndx downto 1;
            OUTPUT += C.LT + C.FS + ClosingTags(lIndex).Tag + C.MT;
          Endfor;
          
          Output += x'00';
          
          Return %Addr(Output);
        
        End-Proc;
        
        //----------------------------------------------
        
        Dcl-Proc ProcessLine;
          Dcl-Pi *N;
            pLine Char(LINE_LEN) Const;
          End-Pi;
        
          Dcl-S lMode     Int(3);
          Dcl-S lLen      Int(5);
          Dcl-S lIndex    Int(5);
          Dcl-S lChar     Char(1);
          Dcl-S lSpaces   Int(3);
          Dcl-S lIsCond   Ind;
        
          Dcl-S lPropIdx  Int(3);
          Dcl-S lIsString Ind;
          Dcl-S lPropMode Int(3);
        
          Dcl-Ds CurrentElement Qualified;
            Tag        Varchar(TAG_LEN)   Inz('');
            Properties LikeDS(Property_T) Inz Dim(5);
            Value      Varchar(256) Inz('');
          End-Ds;
          
          //Default value for some variables
          
          lLen  = %Len(%TrimR(pLine));
          lMode = MODE_TAG;
          lIsString = *Off;
          lPropIdx  = 1;
          lPropMode = MODE_PROP_KEY;
          lIsCond   = *Off;
          
          // Processing starts beloww
          
          If (pLine = *Blank);
            Return;
          Endif;
        
          //Check if we need to close any existing tags.
          lSpaces = 0;
          
          For lIndex = 1 to lLen;
            lChar = %Subst(pLine:lIndex:1); //Current character
            If (lChar <> ' ');
              lSpaces = lIndex-1;
              Leave;
            Endif;
          Endfor;
          
          For lIndex = ClosingIndx downto 1;
            If (ClosingTags(ClosingIndx).Space >= lSpaces);
              OUTPUT += C.LT + C.FS + ClosingTags(ClosingIndx).Tag + C.MT;
              ClosingIndx -= 1;
            Endif;
          Endfor;
        
          //Conditional checking
          lChar = %Subst(pLine:lSpaces+1:1);
          If (lChar = C.DT);
            CurrentElement.Tag = 'div';
            CurrentElement.Properties(lPropIdx).Name = 'id';
            lMode     = MODE_PROP;
            lPropMode = MODE_PROP_VALUE;
            
            lIsCond   = *On; 
            lSpaces  += 1;
          Endif;
        
          For lIndex = (lSpaces+1) to lLen;
            lChar = %Subst(pLine:lIndex:1); //Current character
        
            Select;
              When (lMode = MODE_TAG);
                Select;
                  When (lChar = C.OB); //User is adding properties
                    lMode     = MODE_PROP;
                    lPropMode = MODE_PROP_KEY;
        
                  When (lChar = ' ' OR lChar = C.EQ); //Usually means no properties and just a value!
                    lMode = MODE_VAL;
        
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
                      lChar = ''; //Add nothing, it's the end!
                      lMode = MODE_VAL;
                      lIndex += 1;
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
          
          OUTPUT += '<' + CurrentElement.Tag;
          
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
            OUTPUT += C.MT + %Trim(CurrentElement.Value) + C.LT + C.FS
                           + CurrentElement.Tag + C.MT;
          Endif;
        
        End-Proc;