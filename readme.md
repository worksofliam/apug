## apug

apug is a version of the pugjs templating engine for ILE applications. It's primary purpose is for CGI applications.

#### Example

```
body
  title Hello world
  
  div(id='yo')
    p(id='hi') Hello world
    b This is a test!!!

  div(id='hi')
    p(id='hi') Hello world
    b This is a test!!!
```

```
Dcl-Pr APUG Pointer EXTPROC;
  Path Char(128) Const;
End-Pr;

Dcl-S Ptr    Pointer;
Dcl-S Result Char(256);

Ptr = APUG('/home/liam/apug/test.pug') + 2;

Result = %Str(Ptr);

Return;
```

```
> EVAL Result                                                            
  RESULT =                                                               
            ....5...10...15...20...25...30...35...40...45...50...55...60 
       1   '<body><title>Hello world</title><div id="yo"><p id="hi">Hell'
      61   'o world</p><b>This is a test!!!</b></div><div id="hi"><p id='
     121   '"hi">Hello world</p><b>This is a test!!!</b></div></body>   '
```