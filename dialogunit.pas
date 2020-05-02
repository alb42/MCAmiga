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

function AskQuestion(Text: string): Boolean; // yes = true

implementation

uses
  FileListUnit;

function AskQuestion(Text: string): Boolean;
var
  Mid: TPoint;
  Wind: TRect;
  x, y: Integer;
  Key: TKeyEvent;
  OKr: TRect;
  Cancelr: TRect;
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
  YesActive := False;
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

end.

