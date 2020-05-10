unit dialogunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video, Keyboard, Mouse, Math, EventUnit;

const
  URCorner = #191;
  LLCorner = #192;
  ULCorner = #218;
  LRCorner = #217;
  HLine = #196;
  VLine = #179;

  RBorder = #195;
  LBorder = #180;

  ProgressEmpty = #176;
  ProgressHalf = #221;
  ProgressFull = #219;

type

  { TBaseDialog }

  TBaseDialog = class
  protected
    Mid: TPoint;
    WindowRect: TRect;
    InnerRect: TRect;
  protected
    function PollNextKey: TKeyEvent; virtual;
    procedure DrawButtons; virtual; abstract;
    procedure DrawWindowBorder; virtual;
    procedure Paint; virtual; // Draw the window
  public
    function Execute: Integer; virtual; abstract; // returns the number of the Button pressed
  end;

  { TShowMessage }

  TShowMessage = class(TBaseDialog)
  protected
    procedure DrawButtons; override;
    procedure Paint; override; // Draw the window
  public
    Text: string; // set before Execute!
    function Execute: Integer; override; // returns the number of the Button pressed
  end;

  { TNonWaitMessage }

  TNonWaitMessage = class(TShowMessage)
  protected
    procedure DrawButtons; override;
  public
    function Execute: Integer; override;
  end;

  { TAskQuestion }

  TAskQuestion = class(TShowMessage)
  protected
    YesActive: Boolean;
    procedure DrawButtons; override;
  public
    function Execute: Integer; override; // returns the number of the Button pressed
  end;

  { TAskForName }

  TAskForName = class(TAskQuestion)
  private
     TxtL, TxtR: LongInt;
  protected
    procedure Paint; override; // Draw the window
  public
    NewName: string;
    AsName: Boolean;
    function Execute: Integer; override; // returns the number of the Button pressed
  end;

  { TSingleProgress }

  TSingleProgress = class(TBaseDialog)
  protected
    LastCall: LongWord;
    CurValue: LongWord;
    Pup: LongInt;
    PGL: LongInt;
    PGR: LongInt;
  protected
    procedure DrawButtons; override;
    procedure Paint; override;
  public
    MaxValue: LongWord;
    Text: string;
    function Execute: Integer; override;
    function UpdateValue(AValue: LongWord; NText: string = ''): Boolean; virtual;
  end;

  { TDoubleProgress }

  TDoubleProgress = class(TSingleProgress)
  protected
    CurValue2: LongWord;
    procedure Paint; override;
  public
    MaxValue2: LongWord;
    Text2: string;
    function Execute: Integer; override;
    function UpdateValue(AValue: LongWord; NText: string = ''): Boolean; override;
    function UpdateValue2(AValue1: LongWord; NText1: string = ''; AValue2: LongWord = 0; NText2: string = ''): Boolean;
  end;




function AskQuestion(AText: string): Boolean; // yes = true
function AskForName(AText: string; var ANewName: string; UseAsName: Boolean = True): Boolean;

procedure ShowHelp;
procedure ShowViewHelp;
procedure ShowMessage(AText: string);
procedure NonWaitMessage(AText: string);

implementation

uses
  FileListUnit;

const       //.........1.........2.........3.........4.........5........6.........7
  HelpText = '        MyCommander Amiga Version 0.2       '#13#10 +
             '        =============================        '#13#10 +
             ' F1  - Help              Ins  - Select File'#13#10 +
             ' F3  - View              +    - Select by pattern'#13#10 +
             ' F4  - Edit              -    - Deselect by pattern'#13#10 +
             ' F5  - Copy              TAB  - Switch Focus'#13#10 +
             ' F6  - Move              Alt F1/F2 - Show Drives'#13#10 +
             ' F7  - Create Directory  Backspace - Parent'#13#10 +
             ' F8  - Delete Files      Shift + Enter - Open Dir in other side'#13#10 +
             ' F10/ESC - Quit Program'#13#10 +
             ' Ctrl + R - Rescan Directory'#13#10 +
             ' Ctrl + O - Set Destination Directory to Source Directory'#13#10 +
             ' Ctrl + S - type to find entry in current directory'#13#10 +
             '';

  const       //.........1.........2.........3.........4.........5........6.........7
  HelpViewText = '       Editor Help       '#13#10 +
                 ' F1  - Help'#13#10 +
                 ' F4  - Toggle ASCII and Hex View '#13#10 +
                 ' F10/ESC - Leave Viewer'#13#10 +
                 ' Cursor Keys(4,6,8,2) Pg Up(9), Pg Down(3),'#13#10 +
                 '    Home(7), End(1) - navigate in Text'#13#10 +
                 ' '#13#10 + '';

procedure ShowHelp;
begin
  with TShowMessage.Create do
  begin
    Text := HelpText;
    Execute;
    Free;
  end;
end;

procedure ShowViewHelp;
begin
  with TShowMessage.Create do
  begin
    Text := HelpViewText;
    Execute;
    Free;
  end;
end;

procedure ShowMessage(AText: string);
begin
  with TShowMessage.Create do
  begin
    Text := AText;
    Execute;
    Free;
  end;
end;

procedure NonWaitMessage(AText: string);
begin
  with TNonWaitMessage.Create do
  begin
    Text := AText;
    Execute;
    Free;
  end;
end;

function AskQuestion(AText: string): Boolean;
begin
  with TAskQuestion.Create do
  begin
    Text := AText;
    Result := Execute = 0;
    Free;
  end;
end;


function AskForName(AText: String; var ANewname: String; UseAsName: Boolean = True): Boolean;
begin
  with TAskForName.Create do
  begin
    Text := AText;
    NewName := ANewName;
    AsName := UseAsName;
    Result := Execute = 0;
    if Result then
      ANewName := NewName;
    Free;
  end;
end;

{ TNonWaitMessage }

procedure TNonWaitMessage.DrawButtons;
begin
  // No button here ;)
end;

function TNonWaitMessage.Execute: Integer;
begin
  Result := 0;
  Paint;
end;

{ TDoubleProgress }

procedure TDoubleProgress.Paint;
var
  x: Integer;
begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;
  //
  WindowRect.Left := Max(2, mid.x - 40);
  WindowRect.Top := Max(2, mid.y - 3);
  WindowRect.Bottom := Min(ScreenHeight - 3, mid.y + 3);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + 40);
  BGPen := LightGray;
  FGPen := Black;
  DrawWindowBorder;

  SetText(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  PGL := WindowRect.Left + 2;
  PGR := WindowRect.Right - 2;
  Pup := Mid.y;

  BGPen := LightGray;
  FGPen := Black;
  for x := PGL to PGR do
  begin
    SetChar(x, Pup - 1, ProgressEmpty);
    SetChar(x, Pup + 1, ProgressEmpty);
  end;
  if MaxValue <= 0 then
    MaxValue := 1;
  if MaxValue2 <= 0 then
    MaxValue2 := 1;
  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
end;

function TDoubleProgress.Execute: Integer;
begin
  Result := inherited Execute;
end;

function TDoubleProgress.UpdateValue(AValue: LongWord; NText: string): Boolean;
begin
  Result := UpdateValue2(AValue, NText, CurValue, Text2);
end;

function TDoubleProgress.UpdateValue2(AValue1: LongWord; NText1: string; AValue2: LongWord; NText2: string): Boolean;
var
  w,p,x: LongInt;
  t1: LongWord;
  Key: TKeyEvent;
begin
  CurValue := AValue1;
  Text := NText1;
  CurValue2 := AValue2;
  Text2 := NText2;

  Result := True;
  {$WARNINGS OFF}
  t1 := GetTickCount;
  {$WARNINGS ON}
  if t1 - LastCall > 100 then
  begin
    LastCall := t1;
    BGPen := LightGray;
    FGPen := Black;
    w := PGR - PGL;
    p := Round(CurValue/MaxValue * w);
    //writeln('num1: ', num1 , ' maxNum ', MaxNum,' w ', w, ' p ', P, ' PGL ', PGL, ' PGR ', PGR );
    for x := PGL to PGL + P do
    begin
      SetChar(x, Pup - 1, ProgressFull);
    end;
    p := Round(CurValue2/MaxValue2 * w);
    for x := PGL to PGL + P do
    begin
      SetChar(x, Pup + 1, ProgressFull);
    end;
    if Text <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup - 2, ' ');
      end;
      Text := LimitName(Text, w - 10, False);
      p := w div 2 - Length(Text) div 2 + 5;
      SetText(p, Pup - 2, Text);
    end;
    if Text2 <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup, ' ');
      end;
      Text2 := LimitName(Text2, w - 10, False);
      p := (PGR - 2) - Length(Text2);
      SetText(p, Pup, Text2);
    end;
    UpdateScreen(False);
  end;
  Key := PollNextKey;
  // Break on Enter -> Cancel
  if (Key and $FFFF) = $1C0D then
    Result := False;

end;

{ TSingleProgress }

procedure TSingleProgress.DrawButtons;
begin
  BGPen := Cyan;
  FGPen := Black;
  SetText(Mid.x - 4, WindowRect.Bottom, LBorder + 'Cancel' + RBorder);
  BGPen := LightGray;
end;

procedure TSingleProgress.Paint;
var
  x: LongInt;
begin
  inherited;
  WindowRect.Left := Max(2, mid.x - 40);
  WindowRect.Top := Max(2, mid.y - 2);
  WindowRect.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + 40);
  BGPen := LightGray;
  FGPen := Black;
  DrawWindowBorder;

  SetText(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  PGL := WindowRect.Left + 2;
  PGR := WindowRect.Right - 2;
  Pup := Mid.y;

  BGPen := LightGray;
  FGPen := Black;
  for x := PGL to PGR do
  begin
    SetChar(x, Mid.y, ProgressEmpty);
  end;

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
end;

function TSingleProgress.Execute: Integer;
begin
  Result := 1;
  {$WARNINGS OFF}
  LastCall := GetTickCount;
  {$WARNINGS ON}
  CurValue := 0;
  if MaxValue = 0 then
    MaxValue := 1;

  Paint;
end;

function TSingleProgress.UpdateValue(AValue: LongWord; NText: string = ''): boolean;
var
  x, w, p: LongInt;
  t1: LongWord;
  Key: TKeyEvent;
begin
  CurValue := AValue;
  if NText <> '' then
    Text := NText;
  Result := True;
  {$WARNINGS OFF}
  t1 := GetTickCount;
  {$WARNINGS ON}
  if t1 - LastCall > 100 then
  begin
    LastCall := t1;
    BGPen := LightGray;
    FGPen := Black;
    w := PGR - PGL;
    p := Round(CurValue/MaxValue * w);
    for x := PGL to P do
    begin
      SetChar(x, Pup, ProgressFull);
    end;
    if Text <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup - 1, ' ');
      end;
      Text := LimitName(Text, w - 10, False);
      p := w div 2 - Length(Text) div 2;
      SetText(p, Pup - 1, Text);
    end;
    UpdateScreen(False);
  end;
  Key := PollNextKey;
  // Break on Enter -> Cancel
  if (Key and $FFFF) = $1C0D then
    Result := False;
end;

{ TAskForName }

procedure TAskForName.Paint;
var
  x: LongInt;
begin
  inherited;
  WindowRect.Left := Max(2, mid.x - 20);
  WindowRect.Top := Max(2, mid.y - 2);
  WindowRect.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + 20);
  BGPen := LightGray;
  FGPen := Black;
  DrawWindowBorder;

  SetText(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  TxtL := WindowRect.Left + 2;
  TxtR := WindowRect.Right - 2;

  BGPen := Black;
  FGPen := LightGray;
  for x := TxtL to TxtR do
  begin
    SetChar(x, Mid.y, ' ');
  end;

  SetCursorType(crUnderline);
  SetCursorPos(TxtL + Length(NewName), Mid.Y);
  SetText(TxtL, Mid.y, Newname);

  // draw Buttons
  FGPen := Black;
  BGPen := LightGray;
  DrawButtons;
  UpdateScreen(False);
end;

function TAskForName.Execute: Integer;
var
  Key: TKeyEvent;
  c: Char;
  p: LongInt;
begin
  YesActive := True;
  Paint;
  repeat
    Key := PollNextKey;
    case (Key and $FFFF) of
      $4B00: begin // cursor left
        if CursorX > TxtL then
          SetCursorPos(CursorX - 1, CursorY);
      end;
      $4D00: begin // cursor right
        if CursorX < TxtL + Length(NewName) then
          SetCursorPos(CursorX + 1, CursorY);
      end;
      $1C0D: begin
        Result := ifthen(YesActive, 0, 1);
        Break;
      end;
      $011B: begin
        Result := 1;
        Break;
      end;
      $0F09: begin
        YesActive := not YesActive;
        FGPen := Black;
        BGPen := LightGray;
        DrawButtons;
        UpdateScreen(False);
      end
      else
      begin
        c := GetKeyEventChar(Key);
        case c of
          'a'..'z','A'..'Z','-','.','_','0'..'9',' ','*': begin
            if (not AsName) or (c <> '*') then
            begin
              p := CursorX - TxtL;
              if (p >= 0) and (Length(NewName) < 30) then
              begin
                Insert(c, NewName, p + 1);
                BGPen := Black;
                FGPen := LightGray;
                SetText(TxtL, Mid.y, Newname);
                SetCursorPos(CursorX + 1, Mid.Y);
                UpdateScreen(False);
              end;
            end;
          end;
          #8: begin
            p := CursorX - TxtL;
            if p > 0 then
            begin
              Delete(NewName, p, 1);
              BGPen := Black;
              FGPen := LightGray;
              SetText(TxtL, Mid.y, Newname);
              SetChar(TxtL + Length(NewName), Mid.y, ' ');
              SetCursorPos(CursorX - 1, Mid.Y);
              UpdateScreen(False);
            end;
          end;
        end
      end;
    end;
    Sleep(25);
  until False;
  SetCursorType(crHidden);
end;

{ TAskQuestion }

procedure TAskQuestion.DrawButtons;
begin
  FGPen := Black;
  if YesActive then
    BGPen := Cyan
  else
    BGPen := LightGray;
  SetText(Mid.x - 6, WindowRect.Bottom, LBorder + 'Yes' + RBorder);
  if not YesActive then
    BGPen := Cyan
  else
    BGPen := LightGray;
  SetText(Mid.x + 2, WindowRect.Bottom, LBorder + 'No ' + RBorder);
  BGPen := LightGray;
end;

function TAskQuestion.Execute: Integer;
var
  Key: TKeyEvent;
begin
  YesActive := True;
  Result := 0;
  Paint;
  repeat
    Key := PollNextKey;
    case (Key and $FFFF) of
      $1C0D, $000D: begin
        if YesActive then
          Result := 0
        else
          Result := 1;
        Break;
      end;
      $0F09: begin
        YesActive := not YesActive;
        DrawButtons;
        UpdateScreen(False);
      end;
      $4B00, $34: begin // cursor left
        if not YesActive then
        begin
          YesActive := True;
          DrawButtons;
          UpdateScreen(False);
        end;
      end;
      $4D00, $36: begin // cursor right
        if YesActive then
        begin
          YesActive := False;
          DrawButtons;
          UpdateScreen(False);
        end;
      end;
      $011B: begin
        Result := 1;
        Exit;
      end;
    end;
    Sleep(25);
  until False;
end;

{ TBaseDialog }

function TBaseDialog.PollNextKey: TKeyEvent;
begin
  Result := GetNextKeyEvent;
  if Result = ResizeKey then
    Paint;
end;

procedure TBaseDialog.DrawWindowBorder;
var
  x,y: Integer;
begin
  for y := WindowRect.Top to WindowRect.Bottom do
  begin
    for x := WindowRect.Left to WindowRect.Right do
    begin
      if (y = WindowRect.Top) or (y = WindowRect.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = WindowRect.Left) or (x = WindowRect.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(WindowRect.Left, WindowRect.Top, ULCorner);
  SetChar(WindowRect.Left, WindowRect.Bottom, LLCorner);
  SetChar(WindowRect.Right, WindowRect.Bottom, LRCorner);
  SetChar(WindowRect.Right, WindowRect.Top, URCorner);

  InnerRect := WindowRect;
  InnerRect.Inflate(-1,-1);
end;

procedure TBaseDialog.Paint;
begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;
end;

{ TShowMessage }

procedure TShowMessage.DrawButtons;
begin
  BGPen := Cyan;
  FGPen := Black;
  SetText(Mid.x - 1, WindowRect.Bottom, LBorder + 'OK' + RBorder);
  BGPen := LightGray;
end;

procedure TShowMessage.Paint;
var
  i: Integer;
  SL: TStringList;
  MaxX, MaxY: Integer;
  s,s1: string;
begin
  inherited;
  SL := TStringList.Create;
  SL.Text := Text;

  MaxX := 1;
  i := 0;
  while i < SL.Count do
  begin
    if Length(SL[i]) > ScreenWidth - 6 then
    begin
      s := SL[i];
      s1 := Copy(s, 1, ScreenWidth - 6);
      SL[i] := s1;
      Delete(s, 1, ScreenWidth - 6);
      if Length(SL[i]) > MaxX then
        MaxX := Length(SL[i]);
      SL.Insert(i + 1, s1);
    end
    else
    begin
      if Length(SL[i]) > MAxX then
        MaxX := Length(SL[i]);
      i := i + 1;
    end;
  end;
  MaxX := MaxX + 4;
  MaxY := SL.Count + 4;

  WindowRect.Left := Mid.X - MaxX div 2;
  WindowRect.Top := Mid.Y - MaxY div 2;
  WindowRect.Right :=  Mid.X + MaxX div 2;
  WindowRect.Bottom := Mid.Y + MaxY div 2;

  BGPen := LightGray;
  FGPen := Black;

  DrawWindowBorder;


  for i := 0 to SL.Count - 1 do
    SetText(InnerRect.left + 1, InnerRect.Top + 1 + i, SL[i]);
  SL.Free;

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
end;

function TShowMessage.Execute: Integer;
var
  Key: TKeyEvent;
begin
  Result := 0;
  Paint;
  repeat
    Key := PollNextKey;
    case (Key and $FFFF) of
      $1C0D: Break;
    end;
    Sleep(25);
  until (Key and $ff00) = $0100;
end;

end.

