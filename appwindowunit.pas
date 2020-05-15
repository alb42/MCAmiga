unit appwindowunit;

{$mode objfpc}{$H+}
interface

uses
   Utility, Types, Exec, amigados, workbench, Video;

const
  ID = 1;

type
  TOnDropItem = procedure(Path: string; Name: string; DropPos: TPoint);

procedure MakeAppWindow;
procedure CheckForAppMsgs;
procedure DestroyAppWindow;

var
  OnDropItem: TOnDropItem = nil;

implementation

var
  AppWindow: PAppWindow;
  AppMsgPort: PMsgPort;

procedure MakeAppWindow;
begin
  AppMsgPort := CreateMsgPort();
  if not Assigned(AppMsgPort) then
	  Exit;
  AppWindow := AddAppWindowA(id, 0, VideoWindow, AppMsgPort, nil);
	if not Assigned(AppWindow) then
	  exit;
end;

var
  Buffer: array[0..256] of Char;

procedure CheckForAppMsgs;
var
  AppMsg: PAppMessage;
	x,y: LongInt;
	ArgList: PWBArgList;
  Path, Name: string;
begin
	if not Assigned(AppWindow) or not Assigned(AppWindow) or not Assigned(OnDropItem) then
	  Exit;
	
  AppMsg := PAppMessage(GetMsg(AppMsgPort));
	if Assigned(AppMsg) then
	begin
	  // get mouse position
    x := 0;
    y := 0;
	  TranslateToCharXY(AppMsg^.am_MouseX, AppMsg^.am_MouseY, x, y);
		//
		if AppMsg^.am_NumArgs >= 1 then
		begin
		  ArgList := AppMsg^.am_ArgList;
			FillChar(Buffer[0], Length(Buffer), 0);
      if NameFromLock(ArgList^[1].wa_Lock, @Buffer[0], 255) then
      begin
        Path := Buffer;
        Name := string(ArgList^[1].wa_Name);
			  OnDropItem(Path, Name, Point(x, y));
      end;
    end;
		ReplyMsg(PMessage(AppMsg));
	end;
end;


procedure DestroyAppWindow;
begin
  if Assigned(AppWindow) then
	begin
	  RemoveAppWindow(AppWindow);
		AppWindow := nil;
	end;
	//
	if Assigned(AppMsgPort) then
	begin
	  DeleteMsgPort(AppMsgPort);
		AppMsgPort := nil;
	end;
end;

end.
