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

implementation

uses
  FileListUnit;

var
  Pup: LongInt;
  PGL: LongInt;
  PGR: LongInt;
  MaxNum: LongInt;

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

end.

