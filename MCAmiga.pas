program MCAmiga;
{$mode objfpc}{$H+}
uses
  {$ifdef HASAMIGA}
  Exec, workbench, icon, AppWindowUnit,
  {$endif}
  {$ifdef RELEASE}
  Versioncheck,
  {$endif}
  Types, SysUtils, Video, mouse, keyboard, FileListUnit, dialogunit, EventUnit, archiveunit,
  {$if defined(Amiga68k) or defined(MorphOS) or defined(AROS)}
  xad, xadarchive,
  {$endif}
  toolsunit;

var
  Src: TFileList;
  Dest: TFileList;
  Left, Right: TFileList;

  ViewerLink: string = '';
  AltViewerLink: string = '';
  EditLink: string = '';
  AltEditLink: string = '';

  LeftDefaultPath: string = '';
  RightDefaultPath: string = '';



procedure SwapSrcDest;
var
  Temp: TFileList;
begin
  Temp := Dest;
  Dest := Src;
  Src := Temp;
  Src.IsActive := True;
  Dest.IsActive := False;
end;

procedure MouseEvent(Me: TMouseEvent);
var
  P: TPoint;
begin
  if me.Action = MouseActionDown then
  begin
    P := Point(Me.x, Me.y);
    if Left.PanelRect.Contains(P) then
    begin
      if Right.IsActive then
        SwapSrcDest;
      Left.MouseEvent(Me);
    end
    else
      if Right.PanelRect.Contains(P) then
      begin
        if Left.IsActive then
          SwapSrcDest;
        Right.MouseEvent(Me);
      end;
  end
  else
    Src.MouseEvent(Me);
end;

procedure KeyEvent(Ev: TKeyEvent);
var
  st: Byte;
begin
  st := GetKeyEventShiftState(Ev);
  case TranslateKeyEvent(Ev) and $ffff of
    $0F09: SwapSrcDest;                                        // TAB -> change Focus to other window
    $0008: Src.GoToParent;                                     // Backspace -> Parent
    $1C0D, $000D: Src.EnterPressed(st and kbShift <> 0);      // return -> Enter Dir/Assign/Drive
    kbdUp, $38: Src.ActiveElement := Src.ActiveElement - 1;    // cursor up -> Move around
    kbdDown, $32: Src.ActiveElement := Src.ActiveElement + 1;  // cursor down -> Move around
    kbdPgUp, $39: Src.ActiveElement := Src.ActiveElement - 10; // pg up -> Move around
    kbdPgDn, $33: Src.ActiveElement := Src.ActiveElement + 10; // pg down -> Move around
    kbdHome, $37: Src.ActiveElement := 0;                      // Home -> Move around
    kbdEnd, $31: Src.ActiveElement := MaxInt;                  // end -> Move around
    $1312, $1300: Src.Update(True);                    // Ctrl + R Alt + R -> Reload
    $180F, $1800: Dest.CurrentPath := Src.CurrentPath; // Ctrl + O Alt + O -> copy path to dest
    $2004, $2000: Src.CurrentPath := '';               // Ctrl + D Alt + D -> back to drives/Assign
    $1F13: Src.SearchList;                             // Crtl + s  -> jump mode
    kbdInsert, $23, $30, $20: begin
      if ((st and kbShift) <> 0) and (TranslateKeyEvent(Ev) and $ffff = $20) then
      begin
        Src.ScanSize;
        Left.Update(False);
        Right.Update(False);
      end
      else
        Src.SelectActiveEntry;   // Insert, #, 0, Space -> Select file
    end;
    $002B: Src.SelectByPattern(True);                       // + -> Select files by pattern
    $002D: Src.SelectByPattern(False);                      // - -> Deselect files by pattern
    kbdF10, $011B: begin                               // F10, ESC -> Quit
      if AskQuestion('Quit Program') then
      begin
        Terminate;
        Exit;
      end;
      Left.Update(False);
      Right.Update(False);
    end;
    kbdF3: begin                                      // F3 -> View
      if st and kbShift <> 0 then
        Src.ViewFile(AltViewerLink)
      else
        Src.ViewFile(ViewerLink);
    end;
    kbdF4: begin                                      // F4 -> Edit
      if st and kbShift <> 0 then
        Src.EditFile(AltEditLink)
      else
        Src.EditFile(EditLink);
    end;
    kbdF5: Src.CopyFiles;                            // F5 -> Copy/CopyAs
    kbdF6: begin                                     // F6 -> Move/Rename
      if st and kbShift <> 0 then
        Src.Rename()
      else
        Src.MoveFiles;
    end;
    kbdF7:  Src.MakeDir();                           // F7 -> MakeDir
    kbdF8, kbdDelete: Src.DeleteSelected();          // F8 -> Delete
    kbdF2: begin                                     // F2
      if ((st and kbAlt) <> 0) or ((st and kbCtrl) <> 0) then
      begin
        Right.CurrentPath := '';
      end
      else
      begin
        ShowTools(Src, Dest);
        Left.Update(False);
        Right.Update(False);
      end;
    end;
    kbdF1: begin                                      // F1 -> Help
      if ((st and kbAlt) <> 0) or ((st and kbCtrl) <> 0) then
      begin
        Left.CurrentPath := '';
      end
      else
      begin
        ShowHelp;
        Left.Update(False);
        Right.Update(False);
      end;
    end
    //else
    //  if (ev and $FFFF) <> 0 then writeln('Key: $' + HexStr(TranslateKeyEvent(Ev), 4));
  end;
end;

var
  MySize: TSize;

procedure ResizeEvent(NewWidth, NewHeight: Integer);
var
  Mode: TVideoMode;
begin
  if (NewWidth > 0) and (NewHeight > 0) and ((NewWidth <> MySize.cx) or (NewHeight <> MySize.cy)) then
  begin
    Mode.Col := 0;
    Video.GetVideoMode(Mode);
    Mode.Col := NewWidth;
    Mode.Row := NewHeight;
    Video.SetVideoMode(Mode);
    MySize.cx := NewWidth;
    MySize.cy := NewHeight;
    ClearScreen;
    Left.Resize(Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1));
    Right.Resize(Rect((ScreenWidth div 2), 0, ScreenWidth - 1, ScreenHeight - 1));
  end;
end;

procedure IdleEvent;
begin
  Src.IdleEvent;
end;

procedure DropEvent(Path, Name: string; MousePos: TPoint);
begin
  if Path = '' then
    Exit;
  if PtInRect(Left.PanelRect, MousePos) then
  begin
    Left.CurrentPath := Path;
    if Name <> '' then
      Left.ActivateFile(Name);
  end else
  if PtInRect(Right.PanelRect, MousePos) then
  begin
    Right.CurrentPath := Path;
    if Name <> '' then
      Right.ActivateFile(Name);
  end;
end;

{$ifdef HASAMIGA}
function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
  {$ifdef AROS}
  TT: PPchar;
  s: string;
  {$endif}
begin
  Result := Default;
  if not assigned(Dobj) then
    Exit;
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  {$ifdef AROS}
  TT := Dobj^.do_Tooltypes;
  while Assigned(TT^) do
  begin
    s := TT^;
    if (Pos('=', s) > 0) and (Trim(Copy(s, 1, Pos('=', s) - 1)) = Entry) then
    begin
      Result := copy(s, Pos('=', s) + 1, Length(s));
      Break;
    end;
    Inc(TT);
  end;
  {$else}
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  if Assigned(Res) then
    Result := Res;
  {$endif}
end;
{$endif}


procedure GetSettings;
{$ifdef HASAMIGA}
var
  DObj: PDiskObject;
{$endif}
begin
  {$ifdef HASAMIGA}
  DObj := GetDiskObject(PChar(ParamStr(0)));
  if Assigned(DObj) then
  begin
    // Viewer
    ViewerLink := GetStrToolType(DObj, 'VIEWER', ViewerLink);
    AltViewerLink := GetStrToolType(DObj, 'VIEWER2', AltViewerLink);
    // Editor
    EditLink := GetStrToolType(DObj, 'EDITOR', EditLink);
    AltEditLink := GetStrToolType(DObj, 'EDITOR2', AltEditLink);
    // Defaults                                          but with th
    LeftDefaultPath := GetStrToolType(DObj, 'LEFT', LeftDefaultPath);
    RightDefaultPath := GetStrToolType(DObj, 'RIGHT', RightDefaultPath);
    FreeDiskObject(DObj);
  end;
  {$endif}
end;

procedure StartMe;
var
  Mode: TVideoMode;
begin
  LockScreenUpdate;
  {$ifdef HASAMIGA}
  LeftDefaultPath := 'sys:';
  RightDefaultPath := 'ram:';
  {$endif}
  {$ifdef LINUX}
  LeftDefaultPath := '/';
  RightDefaultPath := '/usr/bin';
  {$endif}
  GetSettings;

  OnKeyPress := @KeyEvent;
  OnMouseEvent := @MouseEvent;
  OnResize := @ResizeEvent;
  OnIdle := @IdleEvent;
  {$ifdef HASAMIGA}
  OnDropItem := @DropEvent;
  {$endif}

  Left := TFileList.Create(Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1));
  Right := TFileList.Create(Rect((ScreenWidth div 2), 0, ScreenWidth - 1, ScreenHeight - 1));
  Left.OtherSide := Right;
  Right.OtherSide := Left;

  Src := Left;
  Dest := Right;

  Mode.Col := 0;
  Video.GetVideoMode(Mode);
  Video.SetVideoMode(Mode);

  Left.CurrentPath := LeftDefaultPath;
  Right.CurrentPath := RightDefaultPath;

  UnlockScreenUpdate;
  UpdateScreen(True);

  Right.ActiveElement := 0;
  Left.IsActive := True;

  {$ifdef RELEASE}
  CreateVersion;
  {$endif}
  {$ifdef HASAMIGA}
  MakeAppWindow;
  {$endif}
  RunApp;
  {$ifdef HASAMIGA}
  DestroyAppWindow;
  {$endif}
  Left.Free;
  Right.Free;
end;

begin
  {$ifdef RELEASE}
  DoVersionInformation;
  {$endif}
  InitVideo;
  InitMouse;
  InitKeyboard;
  {$ifdef HASAMIGA}
  Video.SetWindowTitle('MyCommander Amiga ' + NumVERSION, Copy(VERSION, 6, 12));
  {$endif}

  StartMe;

  DoneKeyboard;
  DoneMouse;
  DoneVideo;
end.

