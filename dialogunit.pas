unit dialogunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video, Keyboard, Mouse, Math;

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

function AskQuestion(Text: string): Boolean; // yes = true
function AskForName(Text: string; var NewName: string; AsName: Boolean = True): Boolean;

procedure ShowMessage(Text: string);

procedure StartProgress(Text: string; AMaxNum: LongInt);
function UpdateProgress(Num: LongInt; Text: string = ''): Boolean;

procedure StartProgress2(Text: string; AMaxNumUpper, AMaxNumLower: LongInt);
function UpdateProgress2(Num1, Num2: LongInt; Text1: string = ''; Text2: string = ''): Boolean;

procedure ShowHelp;

implementation

uses
  FileListUnit;

var
  Pup: LongInt;
  PGL: LongInt;
  PGR: LongInt;
  MaxNum: LongInt;
  MaxNum2: LongInt;

const       //.........1.........2.........3.........4.........5........6.........7
  HelpText = '        MyCommander Amiga Version 0.1       '#13#10 +
             '        =============================        '#13#10 +
             '  ### Keys: ###  '#13#10 +
             ' F1      - This Help Text'#13#10 +
             ' F5      - Copy selected Files'#13#10 +
             ' F6      - Move selected Files'#13#10 +
             ' F7      - Create a new Directory'#13#10 +
             ' F8      - Delete selected Files'#13#10 +
             ' F10     - Quit Program'#13#10 +
             ' TAB     - Set Focus to other Side'#13#10 +
             ' '#13#10 +
             ' Ctrl + R - Rescan Directory'#13#10 +
             ' Ctrl + O - Set Destination Directory to Source Directory'#13#10 +
             '';


procedure ShowHelp;
var
  Mid: TPoint;
  Wind: TRect;
  x, y, i: Integer;
  Key: TKeyEvent;
  SL: TStringList;

  procedure DrawButtons;
  begin
    BGPen := Cyan;
    SetText(Mid.x - 1, Wind.Bottom + 1, LBorder + 'OK' + RBorder);
    BGPen := LightGray;
  end;

begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;

  Wind.Left := 3;
  Wind.Top := 3;
  Wind.Bottom := ScreenHeight - 3;
  Wind.Right :=  ScreenWidth - 3;
  BGPen := LightGray;
  FGPen := Black;
  for y := Wind.Top to Wind.Bottom do
  begin
    for x := Wind.Left to Wind.Right do
    begin
      if (y = Wind.Top) or (y = Wind.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = Wind.Left) or (x = Wind.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(Wind.Left, Wind.Top, ULCorner);
  SetChar(Wind.Left, Wind.Bottom, LLCorner);
  SetChar(Wind.Right, Wind.Bottom, LRCorner);
  SetChar(Wind.Right, Wind.Top, URCorner);

  Wind.Inflate(-1,-1);

  SL := TStringList.Create;
  SL.Text := HelpText;

  for i := 0 to SL.Count - 1 do
    SetText(Wind.left, Wind.Top + i, SL[i]);

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
  repeat
    Key := GetKeyEvent;
    case (Key and $FFFF) of
      $1C0D: Break;
    end;
  until (Key and $ff00) = $0100;
end;


function AskQuestion(Text: string): Boolean;
var
  Mid: TPoint;
  Wind: TRect;
  x, y: Integer;
  Key: TKeyEvent;
  YesActive: Boolean;

  procedure DrawButtons;
  begin
    if YesActive then
      BGPen := Cyan
    else
      BGPen := LightGray;
    SetText(Mid.x - 6, Wind.Bottom, LBorder + 'Yes' + RBorder);
    if not YesActive then
      BGPen := Cyan
    else
      BGPen := LightGray;
    SetText(Mid.x + 2, Wind.Bottom, LBorder + 'No ' + RBorder);
    BGPen := LightGray;
  end;

begin
  Result := False;
  YesActive := True;
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;

  Wind.Left := Max(2, mid.x - 20);
  Wind.Top := Max(2, mid.y - 2);
  Wind.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  Wind.Right :=  Min(ScreenWidth - 3, mid.x + 20);
  BGPen := LightGray;
  FGPen := Black;
  for y := Wind.Top to Wind.Bottom do
  begin
    for x := Wind.Left to Wind.Right do
    begin
      if (y = Wind.Top) or (y = Wind.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = Wind.Left) or (x = Wind.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(Wind.Left, Wind.Top, ULCorner);
  SetChar(Wind.Left, Wind.Bottom, LLCorner);
  SetChar(Wind.Right, Wind.Bottom, LRCorner);
  SetChar(Wind.Right, Wind.Top, URCorner);

  SetText(Mid.X - Length(Text) div 2, Mid.Y, Text);

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
  repeat
    Key := GetKeyEvent;
    case (Key and $FFFF) of
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
      $1C0D: begin
        Result := YesActive;
        Break;
      end;
    end;
  until (Key and $ff00) = $0100;
end;

function AskForName(Text: String; var Newname: String; AsName: Boolean = True): Boolean;
var
  Mid: TPoint;
  Wind: TRect;
  x, y: Integer;
  Key: TKeyEvent;
  YesActive: Boolean;
  Txtl, Txtr: Longint;
  C: Char;

  procedure DrawButtons;
  begin
    FGPen := Black;
    if YesActive then
      BGPen := Cyan
    else
      BGPen := LightGray;
    SetText(Mid.x - 7, Wind.Bottom, LBorder + 'Ok' + RBorder);
    if not YesActive then
      BGPen := Cyan
    else
      BGPen := LightGray;
    SetText(Mid.x + 2, Wind.Bottom, LBorder + 'Cancel' + RBorder);
    BGPen := LightGray;
  end;

begin
  Result := False;
  YesActive := True;
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;

  Wind.Left := Max(2, mid.x - 20);
  Wind.Top := Max(2, mid.y - 2);
  Wind.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  Wind.Right :=  Min(ScreenWidth - 3, mid.x + 20);
  BGPen := LightGray;
  FGPen := Black;
  for y := Wind.Top to Wind.Bottom do
  begin
    for x := Wind.Left to Wind.Right do
    begin
      if (y = Wind.Top) or (y = Wind.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = Wind.Left) or (x = Wind.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(Wind.Left, Wind.Top, ULCorner);
  SetChar(Wind.Left, Wind.Bottom, LLCorner);
  SetChar(Wind.Right, Wind.Bottom, LRCorner);
  SetChar(Wind.Right, Wind.Top, URCorner);

  SetText(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  TxtL := Wind.Left + 2;
  TxtR := Wind.Right - 2;

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
  DrawButtons;
  UpdateScreen(False);
  repeat
    Key := GetKeyEvent;
    case (Key and $FFFF) of
      $4B00: begin // cursor left
        if not YesActive then
        begin
          YesActive := True;
          DrawButtons;
          UpdateScreen(False);
        end;
      end;
      $4D00: begin // cursor right
        if YesActive then
        begin
          YesActive := False;
          DrawButtons;
          UpdateScreen(False);
        end;
      end;
      $1C0D: begin
        Result := YesActive;
        Break;
      end;
      else
      begin
        c := GetKEyEventChar(Key);
        case c of
          'a'..'z','A'..'Z','-','.','_','0'..'9','*': begin
            if (not AsName) or (c <> '*') then
            begin
              if Length(NewName) < 30 then
                NewName := NewName + c;
              BGPen := Black;
              FGPen := LightGray;
              SetCursorPos(TxtL + Length(NewName), Mid.Y);
              SetText(TxtL, Mid.y, Newname);
              UpdateScreen(False);
            end;
          end;
          #8: begin
            if Length(NewName) > 0 then
              NewName := Copy(NewName, 1, Length(NewName) - 1);
            BGPen := Black;
            FGPen := LightGray;
            SetChar(TxtL + Length(NewName), Mid.y, ' ');
            SetCursorPos(TxtL + Length(NewName), Mid.Y);
            UpdateScreen(False);
          end;
        end
      end;
    end;
  until (Key and $ff00) = $0100;
  SetCursorType(crHidden);
end;

procedure ShowMessage(Text: string);
var
  Mid: TPoint;
  Wind: TRect;
  x, y: Integer;
  Key: TKeyEvent;

  procedure DrawButtons;
  begin
    BGPen := Cyan;
    SetText(Mid.x - 1, Wind.Bottom, LBorder + 'OK' + RBorder);
    BGPen := LightGray;
  end;

begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;

  Wind.Left := Max(2, mid.x - 20);
  Wind.Top := Max(2, mid.y - 2);
  Wind.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  Wind.Right :=  Min(ScreenWidth - 3, mid.x + 20);
  BGPen := LightGray;
  FGPen := Black;
  for y := Wind.Top to Wind.Bottom do
  begin
    for x := Wind.Left to Wind.Right do
    begin
      if (y = Wind.Top) or (y = Wind.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = Wind.Left) or (x = Wind.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(Wind.Left, Wind.Top, ULCorner);
  SetChar(Wind.Left, Wind.Bottom, LLCorner);
  SetChar(Wind.Right, Wind.Bottom, LRCorner);
  SetChar(Wind.Right, Wind.Top, URCorner);

  SetText(Mid.X - Length(Text) div 2, Mid.Y, Text);

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
  repeat
    Key := GetKeyEvent;
    case (Key and $FFFF) of
      $1C0D: Break;
    end;
  until (Key and $ff00) = $0100;
end;

var
  LastCall: LongWord = 0;

procedure StartProgress(Text: string; AMaxNum: LongInt);
var
  Mid: TPoint;
  Wind: TRect;
  x, y: Integer;

  procedure DrawButtons;
  begin
    BGPen := Cyan;
    SetText(Mid.x - 3, Wind.Bottom, LBorder + 'Cancel' + RBorder);
    BGPen := LightGray;
  end;

begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;

  Wind.Left := Max(2, mid.x - 40);
  Wind.Top := Max(2, mid.y - 2);
  Wind.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  Wind.Right :=  Min(ScreenWidth - 3, mid.x + 40);
  BGPen := LightGray;
  FGPen := Black;
  for y := Wind.Top to Wind.Bottom do
  begin
    for x := Wind.Left to Wind.Right do
    begin
      if (y = Wind.Top) or (y = Wind.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = Wind.Left) or (x = Wind.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(Wind.Left, Wind.Top, ULCorner);
  SetChar(Wind.Left, Wind.Bottom, LLCorner);
  SetChar(Wind.Right, Wind.Bottom, LRCorner);
  SetChar(Wind.Right, Wind.Top, URCorner);

  SetText(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  PGL := Wind.Left + 2;
  PGR := Wind.Right - 2;
  Pup := Mid.y;

  BGPen := LightGray;
  FGPen := Black;
  for x := PGL to PGR do
  begin
    SetChar(x, Mid.y, ProgressEmpty);
  end;
  MaxNum := AMaxNum;
  if MaxNum <= 0 then
    MaxNum := 1;

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
  {$WARNINGS OFF}
  LastCall := GetTickCount;
  {$WARNINGS ON}
end;

function UpdateProgress(Num: LongInt; Text: string = ''): Boolean;
var
  w,P,x: LongInt;
  Key: TKeyEvent;
  t1: Int64;
begin
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
    p := Round(Num/MaxNum * w);
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
  Key := PollKeyEvent;
  // Break on Enter -> Cancel
  if (Key and $FFFF) = $1C0D then
    Result := False;
end;

procedure StartProgress2(Text: string; AMaxNumUpper, AMaxNumLower: LongInt);
var
  Mid: TPoint;
  Wind: TRect;
  x, y: Integer;

  procedure DrawButtons;
  begin
    BGPen := Cyan;
    SetText(Mid.x - 3, Wind.Bottom, LBorder + 'Cancel' + RBorder);
    BGPen := LightGray;
  end;

begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;

  Wind.Left := Max(2, mid.x - 40);
  Wind.Top := Max(2, mid.y - 3);
  Wind.Bottom := Min(ScreenHeight - 3, mid.y + 3);
  Wind.Right :=  Min(ScreenWidth - 3, mid.x + 40);
  BGPen := LightGray;
  FGPen := Black;
  for y := Wind.Top to Wind.Bottom do
  begin
    for x := Wind.Left to Wind.Right do
    begin
      if (y = Wind.Top) or (y = Wind.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = Wind.Left) or (x = Wind.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(Wind.Left, Wind.Top, ULCorner);
  SetChar(Wind.Left, Wind.Bottom, LLCorner);
  SetChar(Wind.Right, Wind.Bottom, LRCorner);
  SetChar(Wind.Right, Wind.Top, URCorner);

  SetText(Mid.X - Length(Text) div 2, Mid.Y - 2, Text);

  PGL := Wind.Left + 2;
  PGR := Wind.Right - 2;
  Pup := Mid.y;

  BGPen := LightGray;
  FGPen := Black;
  for x := PGL to PGR do
  begin
    SetChar(x, Pup - 1, ProgressEmpty);
    SetChar(x, Pup + 1, ProgressEmpty);
  end;
  MaxNum := AMaxNumUpper;
  MaxNum2 := AMaxNumLower;
  if MaxNum <= 0 then
    MaxNum := 1;
  if MaxNum2 <= 0 then
    MaxNum2 := 1;

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
  {$WARNINGS OFF}
  LastCall := GetTickCount;
  {$WARNINGS ON}
end;

function UpdateProgress2(Num1, Num2: LongInt; Text1: string = ''; Text2: string = ''): Boolean;
var
  w,P,x: LongInt;
  Key: TKeyEvent;
  t1: Int64;
begin
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
    p := Round(Num1/MaxNum * w);
    writeln('num1: ', num1 , ' maxNum ', MaxNum,' w ', w, ' p ', P, ' PGL ', PGL, ' PGR ', PGR );
    for x := PGL to PGL + P do
    begin
      SetChar(x, Pup - 1, ProgressFull);
    end;
    p := Round(Num2/MaxNum2 * w);
    for x := PGL to PGL + P do
    begin
      SetChar(x, Pup + 1, ProgressFull);
    end;
    if Text1 <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup - 2, ' ');
      end;
      Text1 := LimitName(Text1, w - 10, False);
      p := w div 2 - Length(Text1) div 2 + 5;
      SetText(p, Pup - 2, Text1);
    end;
    if Text2 <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup, ' ');
      end;
      Text2 := LimitName(Text2, w - 10, False);
      p := w div 2 - Length(Text2) div 2;
      SetText(p, Pup, Text2);
    end;
    UpdateScreen(False);
  end;
  Key := PollKeyEvent;
  // Break on Enter -> Cancel
  if (Key and $FFFF) = $1C0D then
    Result := False;
end;

end.

