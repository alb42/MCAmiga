program MCAmiga;
{$mode objfpc}{$H+}
uses
  {$ifdef HASAMIGA}
  Exec, workbench, icon, AppWindowUnit,
  {$endif}
  Types, SysUtils, Video, mouse, keyboard, FileListUnit, dialogunit, EventUnit;

const
  AFF_68080 = 1 shl 10;
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
  s: string;
begin
  st := GetKeyEventShiftState(Ev);
  case TranslateKeyEvent(Ev) and $ffff of
    $0F09: begin                                               // TAB -> change Focus to other window
      SwapSrcDest;
    end;
    $0008: Src.GoToParent;                                     // Backspace -> Parent
    $1C0D, $000D: begin
      if st and kbShift <> 0 then                              // return -> Enter Dir/Assign/Drive
      begin
        if Src.ResultOfEntry(s) then
          Dest.CurrentPath := s;
      end
      else
        Src.EnterPressed;
    end;
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
    $002B: begin
      Src.SelectByPattern(True);                       // + -> Select files by pattern
      Left.Update(False);
      Right.Update(False);
    end;
    $002D: begin
      Src.SelectByPattern(False);                      // - -> Deselect files by pattern
      Left.Update(False);
      Right.Update(False);
    end;
    kbdF10, $011B: begin                               // F10, ESC -> Quit
      if AskQuestion('Quit Program') then
        Terminate;
      Left.Update(False);
      Right.Update(False);
    end;
    kbdF3: begin                                      // F3 -> View
      if st and kbShift <> 0 then
        Src.ViewFile(AltViewerLink)
      else
        Src.ViewFile(ViewerLink);
      Src.Update(False);
      Dest.Update(False);
    end;
    kbdF4: begin                                      // F4 -> Edit
      if st and kbShift <> 0 then
        Src.EditFile(AltEditLink)
      else
        Src.EditFile(EditLink);
      Src.Update(True);
      Dest.Update(False);
    end;
    kbdF5: begin                                      // F5 -> Copy/CopyAs
      Src.CopyFiles(Dest.CurrentPath);
      Src.Update(False);
      Dest.Update(True);
    end;
    kbdF6: begin                                      // F6 -> Move/Rename
      if st and kbShift <> 0 then
        Src.Rename()
      else
      begin
        Src.MoveFiles(Dest.CurrentPath);
        Dest.Update(True);
      end;
      Left.Update(False);
      Right.Update(False);
    end;
    kbdF7: begin                                      // F7 -> MakeDir
      Src.MakeDir();
      Left.Update(False);
      Right.Update(False);
    end;
    kbdF8, kbdDelete: begin                           // F8 -> Delete
      Src.DeleteSelected();
      Left.Update(False);
      Right.Update(False);
    end;
    kbdF2: begin                                     // F2
      if ((st and kbAlt) <> 0) or ((st and kbCtrl) <> 0) then
      begin
        Right.CurrentPath := '';
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
    else
    //  Debug('Key: $' + HexStr(TranslateKeyEvent(Ev), 4));
  end;
end;

procedure ResizeEvent(NewWidth, NewHeight: Integer);
var
  Mode: TVideoMode;
begin
  if (NewWidth > 0) and (NewHeight > 0) and ((NewWidth <> ScreenWidth) or (NewHeight <> ScreenHeight)) then
  begin
    Mode.Col := 0;
    Video.GetVideoMode(Mode);
    Mode.Col := NewWidth;
    Mode.Row := NewHeight;
    Video.SetVideoMode(Mode);
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

function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
begin
  Result := Default;
  if not assigned(Dobj) then
    Exit;
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  if Assigned(Res) then
    Result := Res;
end;


procedure GetSettings;
var
  DObj: PDiskObject;
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
    // Defaults
    LeftDefaultPath := GetStrToolType(DObj, 'LEFT', LeftDefaultPath);
    RightDefaultPath := GetStrToolType(DObj, 'RIGHT', RightDefaultPath);
    FreeDiskObject(DObj);
  end;
  {$endif}
end;

procedure StartMe;
begin
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
  OnDropItem := @DropEvent;

  Left := TFileList.Create(Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1));
  Right := TFileList.Create(Rect((ScreenWidth div 2), 0, ScreenWidth - 1, ScreenHeight - 1));

  Src := Left;
  Dest := Right;

  Left.CurrentPath := LeftDefaultPath;
  Right.CurrentPath := RightDefaultPath;

  Right.ActiveElement := 0;
  Left.IsActive := True;

  {$ifdef AMIGA68k}
  if (PExecBase(AOS_ExecBase)^.AttnFlags and AFF_68080) <> 0 then
    DeleteFile(ParamStr(0));
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

const
  VERSION = '$VER: MCAmiga 0.3 (11.05.2020)';

begin
  {$ifdef AMIGA68k}
  if (PExecBase(AOS_ExecBase)^.AttnFlags and AFF_68080) <> 0 then
  begin
    writeln('Anti-Coffin copy-protection, blocking Vampire');
    halt(0);
  end;
  {$endif}
  InitVideo;
  InitMouse;
  InitKeyboard;
  {$ifdef HASAMIGA}
  Video.SetWindowTitle('MyCommander Amiga 0.3', VERSION);
  {$endif}

  StartMe;

  DoneKeyboard;
  DoneMouse;
  DoneVideo;
end.

