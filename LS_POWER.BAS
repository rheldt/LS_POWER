DECLARE SUB Banner (Row%, Fcolor%, Bcolor%, Text$)
DECLARE SUB Box (Row1%, Col1%, Row2%, Col2%, Fcolor%, Bcolor%, Style%)
DECLARE SUB GetInput (Row%, Col%, MaxCharacters%, Fcolor%, DefaultText$, Password%, Bleep%)
DECLARE SUB Menu (Top%, Bottom%, Col%, Fcolor%, Bcolor%, InverseFColor%, InverseBColor%)
DECLARE SUB MultiSelect (Row%, Col%, NumberAnswers%, Question$, QColor%, AColor%)
DECLARE SUB press ()
DECLARE SUB ScreenSwap (Action AS INTEGER)
DECLARE SUB UserInterface ()
DECLARE SUB WallPaper (Fcolor%, Bcolor%, ASCIIChr%)
DECLARE SUB Center (Row%, Text$)
DECLARE FUNCTION Bold$ (Text$)
DECLARE FUNCTION Underline$ (Text$)
DECLARE FUNCTION StrikeThrough$ (Text$)
DECLARE FUNCTION Encrypt$ (Text$, EncryptKey%)
DEFINT A-Z
CONST True = -1
CONST False = 0
DIM SHARED InputString$
DIM SHARED MenuItems$(1 TO 10)
DIM SHARED ScrollText$(1 TO 10)
DIM SHARED ItemSelected

SUB Banner (Row, Fcolor, Bcolor, Text$)
COLOR Fcolor, Bcolor
LOCATE Row, 1
PRINT STRING$(80, " ");
Center Row, Text$
END SUB

FUNCTION Bold$ (Text$)
BackSpace$ = STRING$(LEN(Text$), CHR$(8))
Text$ = Text$ + BackSpace$ + Text$
Bold$ = Text$
END FUNCTION

SUB Box (Row1, Col1, Row2, Col2, Fcolor, Bcolor, Style)

SELECT CASE Style
    CASE 1
        Horizontal$ = "Ä"
        Verticle$ = "³"
        UpperLeft$ = "Ú"
        LowerLeft$ = "À"
        UpperRight$ = "¿"
        LowerRight$ = "Ù"
    CASE 2
        Horizontal$ = "Í"
        Verticle$ = "º"
        UpperLeft$ = "É"
        LowerLeft$ = "È"
        UpperRight$ = "»"
        LowerRight$ = "¼"
    CASE 3
        Horizontal$ = "Û"
        Verticle$ = "Û"
        UpperLeft$ = "Û"
        LowerLeft$ = "Û"
        UpperRight$ = "Û"
        LowerRight$ = "Û"
    CASE ELSE
        EXIT SUB
END SELECT
COLOR Fcolor, Bcolor
BW = Col2 - Col1 + 1
LOCATE Row1, Col1
PRINT UpperLeft$; STRING$(BW - 2, Horizontal$); UpperRight$;
FOR A = Row1 + 1 TO Row2 - 1
    LOCATE A, Col1
    PRINT Verticle$; SPACE$(BW - 2); Verticle$;
NEXT A
LOCATE Row2, Col1
PRINT LowerLeft$; STRING$(BW - 2, Horizontal$); LowerRight$;

END SUB

SUB Center (Row, Text$)
    LOCATE Row, 41 - LEN(Text$) / 2
    PRINT Text$;
END SUB

SUB Delay (Seconds AS LONG)

'=============================================================================
'Delay (Seconds)
'=============================================================================
'Seconds            Number of seconds Delay should pause
'=============================================================================
  DIM TimeNow(0) AS LONG
  DIM DestinationTime(0) AS LONG
 
  TimeNow(0) = TIMER
  DestinationTime(0) = TimeNow(0) + Seconds
  DO
  LOOP UNTIL DestinationTime(0) < TIMER
END SUB

FUNCTION Encrypt$ (Text$, EncryptKey)
Temp$ = ""
FOR RR = 1 TO LEN(Text$)
    Temp$ = Temp$ + CHR$(ASC(MID$(Text$, RR, 1)) XOR EncryptKey)
NEXT RR
Encrypt$ = Temp$
END FUNCTION

SUB GetInput (Row, Col, MaxCharacters, Fcolor, DefaultText$, Password, Bleep)

'=============================================================================
'GetInput (Row, Col, MaxCharacters, FColor, DefaultText$, Password, Bleep)
'=============================================================================
'Row                Screen row where input cursor should be placed
'Col                Screen column where input cursor should be placed
'MaxCharacters      Max number of characters allowed in string
'FColor             Foreground color of text
'DefaultText$       A text string that is to be edited
'Password           If True, displays all text as a series of *'s
'Bleep              If True, a beep is sounded on an input error
'=============================================================================

WHILE INKEY$ <> "": WEND
InputString$ = DefaultText$

  LOCATE Row, Col
  COLOR 23: PRINT "_"
  LOCATE Row, Col
  COLOR Fcolor
  GOTO ShowString

StartInput:
 
  DO 'This DO loop continues until the user presses the <ENTER> key
    DO 'This one uses INKEY to check for a key presses
      Character$ = INKEY$
    LOOP WHILE Character$ = "" 'If no key has been pressed, continue checking
ShowString:
    LOCATE Row, Col
    PRINT STRING$(LEN(InputString$), " ");
    LOCATE Row, Col
    PRINT InputString$;
   
    IF Character$ = CHR$(8) THEN
      Chrs = LEN(InputString$)
      IF Chrs = 0 THEN
        IF Bleep = True THEN BEEP
        GOTO StartInput
      END IF
      DelChrs = Chrs - 1
      InputString$ = LEFT$(InputString$, DelChrs)
      LOCATE Row, Col + LEN(InputString$) + 1: PRINT " ";
      LOCATE Row, Col: PRINT STRING$(LEN(InputString$) + 1, " ")
      LOCATE Row, Col: PRINT InputString$
      LOCATE Row, Col + LEN(InputString$)
      GOSUB ShowStars
      COLOR 23: PRINT "_": COLOR Fcolor
      GOTO StartInput
    END IF
    
    FOR RR = 0 TO 31
      IF RR = 13 THEN RR = 14
      IF Character$ = CHR$(RR) THEN GOTO StartInput
    NEXT RR
    IF Character$ = CHR$(13) THEN GOTO RelayChrs
    IF LEN(InputString$) = MaxCharacters THEN
      IF Bleep = True THEN BEEP
      GOSUB ShowStars
      GOTO StartInput
    END IF
    IF LEN(Character$) = 2 THEN GOTO StartInput
    InputString$ = InputString$ + Character$
RelayChrs:
    IF Character$ <> CHR$(13) THEN
      PRINT Character$;
      COLOR 23
      PRINT "_"
      COLOR Fcolor
    END IF
    GOSUB ShowStars
  LOOP UNTIL Character$ = CHR$(13) '<ENTER> Key - abort loop
 
  LOCATE Row, Col + LEN(InputString$): PRINT " ";

  EXIT SUB

ShowStars:
    IF Password = True THEN
      LOCATE Row, Col: PRINT STRING$(LEN(InputString$), "*");
    END IF
    RETURN
END SUB

SUB Menu (Top, Bottom, Col, Fcolor, Bcolor, InverseFColor, InverseBColor)

'=============================================================================
'Menu (Top, Bottom, Col, Fcolor, BColor, InverseFColor, InverseBColor)
'=============================================================================
'Top                Top screen row where the first menu item is placed
'Bottom             Bottom screen row where the first menu item is placed
'Col                Screen column where the menu should be put
'FColor             Foreground color of text
'BColor             Background color of text
'InverseFColor      Foreground color selected text
'InverseBColor      Background color selected text
'=============================================================================

WHILE INKEY$ <> "": WEND
CONST HomeKey = 71, EndKey = 79, UpKey = 72, DownKey = 80, EnterKey = 13
Null$ = CHR$(0)
CurrentItem = Top
TextTrack = 1
GOSUB DisplayItems
GOSUB GoTop

StartMenu:

DO
  MenuKey$ = INKEY$
LOOP WHILE MenuKey$ = ""

IF MenuKey$ = CHR$(13) THEN GOTO ReturnHit
IF MenuKey$ = Null$ + CHR$(UpKey) THEN GOSUB GoUp
IF MenuKey$ = Null$ + CHR$(DownKey) THEN GOSUB GoDown
IF MenuKey$ = Null$ + CHR$(HomeKey) THEN GOSUB GoTop
IF MenuKey$ = Null$ + CHR$(EndKey) THEN GOSUB GoBottom

GOTO StartMenu

GoUp:
  CurrentItem = CurrentItem - 1
  IF CurrentItem < Top THEN GOTO GoBottom
  TextTrack = TextTrack - 1
  GOSUB DisplayPointer
RETURN

GoDown:
  CurrentItem = CurrentItem + 1
  IF CurrentItem > Bottom THEN GOTO GoTop
  TextTrack = TextTrack + 1
  GOSUB DisplayPointer
RETURN

GoTop:
  TextTrack = 1
  CurrentItem = Top
  GOSUB DisplayPointer
RETURN

GoBottom:
  TextTrack = (Bottom - Top) + 1
  CurrentItem = Bottom
  GOSUB DisplayPointer
RETURN

ReturnHit:
  COLOR InverseFColor, InverseBColor
  LOCATE CurrentItem, Col
  PRINT "  "; MenuItems$(TextTrack); "  "
  ItemSelected = TextTrack
  COLOR Fcolor, Bcolor
EXIT SUB

DisplayPointer:
  GOSUB DisplayItems
  COLOR InverseFColor, InverseBColor
  LOCATE CurrentItem, Col
  PRINT "  "; MenuItems$(TextTrack); "  "
RETURN

DisplayItems:
  CurrentRow = Top
  COLOR Fcolor, Bcolor
  FOR RR = 1 TO ((Bottom - Top) + 1)
    LOCATE CurrentRow, Col - 1
    PRINT "   "; MenuItems$(RR); "   "
    CurrentRow = CurrentRow + 1
  NEXT RR
RETURN
END SUB

SUB MultiSelect (Row, Col, NumberAnswers, Question$, QColor, AColor)

'=============================================================================
'MultiSelect (Row, Col, NumberAnswers, Question$, QColor, AColor)
'=============================================================================
'Row                Screen row where input cursor should be placed
'Col                Screen column where input cursor should be placed
'NumberAnswers      The max number of questions to be displayed
'Question$          The question to be displayed
'QColor             Color of the Question
'AColor             Color of the Answers
'=============================================================================

  WHILE INKEY$ <> "": WEND
  AnswerLength = LEN(MenuItems$(1))
  QuestionLength = LEN(Question$) + Col + 2
  LOCATE Row, Col
  COLOR QColor
  PRINT Question$; " ";
  CurrentAnswer = 0
  GOSUB SwitchAnswer

StartSelect:
  DO
    MenuKey$ = INKEY$
  LOOP WHILE MenuKey$ = ""
    IF MenuKey$ = CHR$(13) THEN GOTO SelectedItem
    IF MenuKey$ = CHR$(32) THEN GOSUB SwitchAnswer
GOTO StartSelect

SwitchAnswer:
  COLOR AColor
  CurrentAnswer = CurrentAnswer + 1
  IF CurrentAnswer > NumberAnswers THEN CurrentAnswer = 1
  LOCATE Row, QuestionLength
  PRINT STRING$(AnswerLength, " ")
  LOCATE Row, QuestionLength
  PRINT MenuItems$(CurrentAnswer)
  AnswerLength = LEN(MenuItems$(CurrentAnswer))
RETURN
SelectedItem:
  ItemSelected = CurrentAnswer
END SUB

SUB press
  WHILE INKEY$ <> "": WEND
  WHILE INKEY$ = "": WEND
END SUB

SUB ScreenSwap (Action AS INTEGER)
'=============================================================================
'ScreenSwap (Action)
'=============================================================================
'Action         1 to capture, 2 to restore
'=============================================================================


SELECT CASE Action
    CASE 1
        PCOPY 0, 1
        SCREEN 0, , 1, 1
    CASE 2
        SCREEN 0, , 0, 0
END SELECT
END SUB

FUNCTION StrikeThrough$ (Text$)
BackSpace$ = STRING$(LEN(Text$), CHR$(8))
Strike$ = STRING$(LEN(Text$), "-")
Text$ = Text$ + BackSpace$ + Strike$
StrikeThrough$ = Text$
END FUNCTION

FUNCTION Underline$ (Text$)
BackSpace$ = STRING$(LEN(Text$), CHR$(8))
ULine$ = STRING$(LEN(Text$), "_")
Text$ = Text$ + BackSpace$ + ULine$
Underline$ = Text$
END FUNCTION

SUB UserInterface
'=============================================================================
'UserInterface
'=============================================================================
'Put all of your code here to display your common user interface
'=============================================================================
WallPaper 7, 1, 177
Banner 1, 15, 4, "Legend Software Power Pack Demo"
Banner 25, 7, 1, "Copyright 1994-1995 Legend Software.  All rights reserved."
END SUB

SUB WallPaper (Fcolor, Bcolor, ASCIIChr)
'=============================================================================
'WallPaper (Fcolor, BColor, ASCIIChr)
'=============================================================================
'FColor             Foreground color of text
'BColor             Background color of text
'ASCIIChr           ASCII Code for Wallpaper design, can try 176, 177, or 178
'=============================================================================
COLOR Fcolor, Bcolor
FOR RR = 1 TO 25
    LOCATE RR, 1
    PRINT STRING$(80, ASCIIChr);
NEXT RR
END SUB

