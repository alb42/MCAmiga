unit EventUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video, Keyboard, Mouse;

type
  TOnKeyPress = procedure(Key: TKeyEvent);
  TOnResize = procedure(NewWidth, NewHeight: Integer);

const
  ResizeKey = $FFFF0000;

function GetNextKeyEvent: TKeyEvent;


procedure RunApp;
procedure Terminate;

var
  OnKeyPress: TOnKeyPress = nil;
  OnResize: TONResize = nil;
  OnIdle: TProcedure = nil;

implementation

var
  Terminated: Boolean = False;


function GetNextKeyEvent: TKeyEvent;
var
  nw, nh: Integer;
begin
  Result := 0;
  nw := 0;
  nh := 0;
  if PollKeyEvent <> 0 then
    Result := GetKeyEvent;
  // check for Resize Messages
  {$ifdef HASAMIGA}
  if HasResizeWindow(nw, nh) then
  begin
    try
      if Assigned(OnResize) then
        OnResize(Nw, Nh);
      Result := ResizeKey;
      Exit;
    except
    end;
  end;
  {$endif}
  // we got killed
  if Terminated then
    Exit;
end;

procedure ProcessMessages;
var
  ev: TKeyEvent;
begin
  ev := GetNextKeyEvent;
  if (ev <> 0) and Assigned(OnKeyPress) then
  begin
    try
      OnKeyPress(ev);
    except
      on e: Exception do
      begin
        writeln('Exception: ', e.message);
      end;
    end;
  end;
end;

procedure RunApp;
var
  LastCall: LongWord;
  CurCall: LongWord;
begin
  {$WARNINGS OFF}
  LastCall := GetTickCount;
  {$WARNINGS ON}
  Terminated := False;
  repeat
    ProcessMessages;
    if Terminated then
      Exit;
    Sleep(25);
    {$WARNINGS OFF}
    CurCall := GetTickCount;
    {$WARNINGS ON}
    if Assigned(OnIdle) and (CurCall - LastCall >= 1000) then
    begin
      OnIdle;
      LastCall := CurCall;
    end;
  until Terminated;
end;

procedure Terminate;
begin
  Terminated := True;
end;

end.

