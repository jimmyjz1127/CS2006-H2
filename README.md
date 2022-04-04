# CS2006-H2

Files:

    src:  contains all haskell .hs files, compile files, and executables

    app:  contains main loop

         - Main.hs : main loop which gets built together with the other game files by cabal

    test: contains Spec.hs file
    
         - Spec.hs : Arbitrary Genereator, and test cases implemented in QuickCheck


Commands and Expressions in our scripting language:

   Commands       Use in our scripting language

    set          : <variable> = <value>

    print        : print <variable/value type input>

    read         : read "<path/filename>"

    write        : write "<path/filename>" "<input>"

    if-then      : if(<condition>) then (<conditional statement>)

    if-then-else : if(<consition>) then (<conditional statement1>) else (<conditional statement2>)

    quit         : quit


   Expressions    Use in our scripting language
   
    add          : <variable/numercial value> + <variable/numercial value>

    substract    : <variable/numercial value> - <variable/numercial value>

    multiply     : <variable/numercial value> * <variable/numercial value>

    divide       : <variable/numercial value> / <variable/numercial value>

    modulus      : <variable/numercial value> % <variable/numercial value>

    ABS          : abs(<variable/numercial value>)

    power        : <variable/numercial value> ^ <variable/numercial value>

    circle Area  : cirA(<variable/numercial value>)

    swap         : swap(<variable/numercial value>)

    toString     : toString(<variable/numercial value>)

    toFloat      : toFloat(<variable/String value>)

    toInt        : toInt(<variable/String value>)