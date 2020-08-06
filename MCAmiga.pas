program MCAmiga;
{$mode objfpc}{$H+}
uses
  ATHreads,
  workbench, icon, AppWindowUnit, Intuition,
  Types, SysUtils, Video, mouse, keyboard, Classes,
  {$if defined(Amiga68k) or defined(MorphOS) or defined(AROS)}
  xad, xadarchive,
  {$endif}
  FileListUnit, dialogunit, EventUnit, archiveunit, ViewerUnit, diffviewerunit;

var
  Src: TFileList;
  Dest: TFileList;
  Left, Right: TFileList;

  LeftDefaultPath: string = '';
  RightDefaultPath: string = '';
  WindowSize: string = '';
  DefWinSize: TRect;

// Swap Destination and Source -> change the Focus
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

// Mouse Event, handling all Mouse Events of main application
// connected to EventUnit OnMouseEvent
procedure MouseEvent(Me: TMouseEvent);
var
  P: TPoint;
  Len: LongInt;
begin
  // react on Mouse down, Panel will check for right/left
  if me.Action = MouseActionDown then
  begin
    P := Point(Me.x, Me.y);
    // check if left panel is hit by mouse
    if Left.PanelRect.Contains(P) then
    begin
      // click to panel also activate the panel
      if Right.IsActive then
        SwapSrcDest;
      Left.MouseEvent(Me);
      Exit;
    end;
    // check if right panel is hit by mouse
    if Right.PanelRect.Contains(P) then
    begin
      if Left.IsActive then
        SwapSrcDest;
      Right.MouseEvent(Me);
      Exit;
    end;
    // special handling of the Bottom F Key Menu, if visible
    if (Me.y = ScreenHeight - 1) and DefShowMenu then
    begin
      if ShowClock then
        Len := ScreenWidth div 11
      else
        Len := ScreenWidth div 10;
      case Me.x div Len of
        0:begin //F1
            ShowHelp;
            Left.Update(False);
            Right.Update(False);
          end;
        1:begin // F2
          ShowTools(Src, Dest);
          Left.Update(False);
          Right.Update(False);
        end;
        2:begin //F3
          if me.buttons = MouseLeftButton then
            Src.ViewFile(ViewerLink);
          if me.buttons = MouseRightButton then
            Src.ViewFile(AltViewerLink);
        end;
        3:begin //F4
          if me.buttons = MouseLeftButton then
            Src.EditFile(EditLink);
          if me.buttons = MouseRightButton then
            Src.EditFile(AltEditLink);
        end;
        4:begin //F5
          Src.CopyFiles;
        end;
        5:begin //F6
          if me.buttons = MouseLeftButton then
            Src.MoveFiles;
          if me.buttons = MouseRightButton then
            Src.Rename();
        end;
        6:begin //F7
          Src.MakeDir();
        end;
        7:begin //F8
          Src.DeleteSelected();
        end;
        9: begin // F10
          if AskQuestion('Quit Program') then
          begin
            Terminate;
            Exit;
          end;
          Left.Update(False);
          Right.Update(False);
        end;
      end;
    end;
  end
  else
  begin
    // all other events just gibve to src panel -> needed
    // for click and move to selece
    Src.MouseEvent(Me);
  end;
end;

procedure ParseWinSize;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.create;
  try
    DefWinSize := Rect(0, 0 , 0 , 0);
    ExtractStrings(['/'], [], PChar(WindowSize), SL);
    if SL.Count = 4 then
    begin
      if TryStrToInt(SL[0], i) then
        DefWinSize.Left := i
      else
        Exit;
      if TryStrToInt(SL[1], i) then
        DefWinSize.Top := i
      else
        Exit;
      if TryStrToInt(SL[2], i) then
        DefWinSize.Width := i
      else
        Exit;
      if TryStrToInt(SL[3], i) then
        DefWinSize.Height := i
      else
        Exit;
    end;
  finally
    SL.Free;
  end;
end;

// Key Event, handling all key Events of main application
// connected to EventUnit OnKeyEvent
procedure KeyEvent(Ev: TKeyEvent);
var
  st: Byte;
begin
  st := GetKeyEventShiftState(Ev);
  case TranslateKeyEvent(Ev) and $ffff of
    $0F09, $0F00: SwapSrcDest;                                  // (Shift) TAB -> change Focus to other window
    kbdLeft, $0008, $0034: Src.GoToParent;                       // Backspace, Left -> Parent
    $1C0D, $000D: Src.EnterPressed(st and kbShift <> 0, True);  // return -> Enter Dir/Assign/Drive
    kbdRight, $0036: Src.EnterPressed(False, False);  // return -> Enter Dir/Assign/Drive
    kbdUp, $38: begin if (st and kbShift) <> 0 then Src.SelectActiveEntry(False); Src.ActiveElement := Src.ActiveElement - 1; end;   // cursor up -> Move around
    kbdDown, $32: if (st and kbShift) <> 0 then Src.SelectActiveEntry else Src.ActiveElement := Src.ActiveElement + 1;  // cursor down -> Move around
    kbdPgUp, $39, $8D00: Src.ActiveElement := Src.ActiveElement - 10; // pg up -> Move around
    kbdPgDn, $33, $9100: Src.ActiveElement := Src.ActiveElement + 10; // pg down -> Move around
    kbdHome, $37, $7300: Src.ActiveElement := 0;                      // Home -> Move around
    kbdEnd, $31, $7400: Src.ActiveElement := MaxInt;                  // end -> Move around
    $1312, $1300: Src.Update(True);                    // Ctrl + R Alt + R -> Reload
    $180F, $1800: Dest.CurrentPath := Src.CurrentPath; // Ctrl + O Alt + O -> copy path to dest
    $2004, $2000: Src.CurrentPath := '';               // Ctrl + D Alt + D -> back to drives/Assign
    $1F13: Src.SearchList;                             // Crtl + S  -> jump mode
    $2106: begin                                       // Ctrl + F -> toggle visibility of bottom menu
      DefShowMenu := not DefShowMenu;
      LockScreenUpdate;
      ClearScreen;
      Left.Resize(Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1));
      Right.Resize(Rect((ScreenWidth div 2), 0, ScreenWidth - 1, ScreenHeight - 1));
      UnLockScreenUpdate;
      UpdateScreen(False);
    end;
    kbdInsert, $23, $30, $20: begin                    // Insert, #, 0, Space -> Select file, scan dir size
      if ((st and kbShift) <> 0) and (TranslateKeyEvent(Ev) and $ffff = $20) then
      begin
        Src.ScanSize;
        Left.Update(False);
        Right.Update(False);
      end
      else
        Src.SelectActiveEntry;
    end;
    $002B: Src.SelectByPattern(True);                  // + -> Select files by pattern
    $002D: Src.SelectByPattern(False);                 // - -> Deselect files by pattern
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
    kbdF1, kbdF20: begin                               // F1,Help -> Help
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
    {$ifndef RELEASE}
    else
      if (ev and $FFFF) <> 0 then writeln('Key: $' + HexStr(TranslateKeyEvent(Ev), 4), ' Shiftstate ', st);
    {$endif}
  end;
end;

var
  MySize: TSize; // prevent endless looping of resize

// Resize of Window, is not supported by the original video unit, so we have to fake that
// by changing the video mode
procedure ResizeEvent(NewWidth, NewHeight: Integer);
var
  Mode: TVideoMode;
begin
  // prevent looping
  if (NewWidth > 0) and (NewHeight > 0) and ((NewWidth <> MySize.cx) or (NewHeight <> MySize.cy)) then
  begin
    Mode.Col := 0;
    // get video mode, it still has the old sizes, set the new sizes, and set again!
    Video.GetVideoMode(Mode);
    Mode.Col := NewWidth;
    Mode.Row := NewHeight;
    // careful at this point a resize event will occur again
    Video.SetVideoMode(Mode);
    MySize.cx := NewWidth;
    MySize.cy := NewHeight;
    // we have a new FAST clearscreen, there for we can do that here
    ClearScreen;
    // let the panels resize
    Left.Resize(Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1));
    Right.Resize(Rect((ScreenWidth div 2), 0, ScreenWidth - 1, ScreenHeight - 1));

    if Assigned(ClassToPaint) then
      ClassToPaint.Paint
    else
    begin
      Left.Update(False);
      Right.Update(False);
    end;
  end;
end;

// idle event, sent to src panel -> used to scroll too long filenames
procedure IdleEvent;
begin
  Src.IdleEvent;
end;

// Event when a item is dropped on the app window
procedure DropEvent(Path, Name: string; MousePos: TPoint);
begin
  // not a path -> nothing to do
  if Path = '' then
    Exit;
  // Which panel is hit
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

// Get a ToolType as String
// Entry = name of ToolType
// Default = if ToolType Entry not found what to return
// Returns the Value begin the '=' of the toolType if it's only a keyword
// it will return an empty string (then better to have Default <> '')
function GetStrToolType(DObj: PDiskObject; Entry: string; Default: string): string;
var
  Res: PChar;
  {$ifdef AROS}
  TT: PPchar;
  s: string;
  {$endif}
begin
  Result := Default;
  // just to be sure
  if not assigned(Dobj) then
    Exit;
  if not Assigned(Dobj^.do_Tooltypes) then
    Exit;
  // aros does not have this call until now, we have to parse for ourself
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
  // get the ToolType
  Res := FindToolType(Dobj^.do_Tooltypes, PChar(Entry));
  if Assigned(Res) then
    Result := Res;
  {$endif}
end;

// Get the settings from the icon
procedure GetSettings;
var
  DObj: PDiskObject;
begin
  // Load icon propertied
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
    // WithDevices
    WithDevices := GetStrToolType(DObj, 'WITHDEVICES', '0') <> '0';
    // ShowMenu
    DefShowMenu := GetStrToolType(DObj, 'SHOWMENU', '0') = '';
    // Own Screen
    FullScreen := GetStrToolType(DObj, 'FULLSCREEN', '0') = '';
    // Default Shell settings (CON:....)
    DefaultShell := GetStrToolType(DObj, 'DEFAULTSHELL', '');
    //
    AutoInfo := GetStrToolType(DObj, 'INFOFILESAUTOHANDLING', '0') = '';
    //
    AutoCreateInfo := GetStrToolType(DObj, 'CREATEDIRICON', '0') = '';
    //
    ShowClock := GetStrToolType(DObj, 'SHOWCLOCK', '0') = '';
    //
    WindowSize := GetStrToolType(DObj, 'WINDOW', '');
    ParseWinSize;
    //
    FreeDiskObject(DObj);
  end;
end;

// Main procedure of MCAmiga
procedure StartMe;
var
  Mode: TVideoMode;
  VideoFontHeight, x,y: LongInt;
  Diffmode: Boolean;
  ViewMode: Boolean;
begin
  Diffmode := False;
  ViewMode := False;
  if ParamCount > 0 then
  begin
    if ParamStr(1) = 'diff' then
    begin
      Diffmode := True;
    end else
    if ParamStr(1) = 'view' then
    begin
      Viewmode := True;
    end;
  end;

  LockScreenUpdate;
  // defaults are available on every amiga style system
  LeftDefaultPath := 'sys:';
  RightDefaultPath := 'ram:';
  // get prefs from tooltypes
  GetSettings;
  // connect Events
  OnKeyPress := @KeyEvent;
  OnMouseEvent := @MouseEvent;
  OnResize := @ResizeEvent;
  OnIdle := @IdleEvent;
  OnDropItem := @DropEvent;
  // Check Video size, set fullscreen if needed
  Mode.Col := 0;
  Video.GetVideoMode(Mode);
  if FullScreen then
    Mode.color := False;
  Video.SetVideoMode(Mode);
  // create Main Panels, and link them together
  Left := TFileList.Create(Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1));
  Right := TFileList.Create(Rect((ScreenWidth div 2), 0, ScreenWidth - 1, ScreenHeight - 1));
  Left.OtherSide := Right;
  Right.OtherSide := Left;
  // src and dest for easier handling
  Src := Left;
  Dest := Right;
  // set the path for both panels -> both will load these and redraw
  Left.CurrentPath := LeftDefaultPath;
  Right.CurrentPath := RightDefaultPath;

  // nasty hack to make FullScreen work
  if FullScreen then
  begin
    // I don't know how big the char ist
    // luckyly there is a video call translating the real to video coords
    // so we can claculate back
    x := 1;
    y := 1;
    VideoFontHeight := 16;
    TranslateToCharXY(100, 100, x,y);
    if y > 0 then
      VideoFontHeight := 100 div y;
    // start resize to the new calculated screen size
    ResizeEvent(VideoWindow^.GZZWidth div 8, VideoWindow^.GZZHeight div VideoFontHeight);
    // strangely the opened video screen is not active
    ActivateWindow(VideoWindow);
  end;

  if not DefWinSize.IsEmpty then
    Intuition.ChangeWindowBox(VideoWindow, DefWinSize.Left, DefWinSize.Top, DefWinSize.Width, DefWinSize.Height);

  if Diffmode then
  begin
    if (ParamCount = 3) and FileExists(ParamStr(2)) and FileExists(ParamStr(3)) then
    begin
      UnlockScreenUpdate;
      with TDiffViewer.Create do
      begin
        Execute(ParamStr(2), PAramStr(3));
        Free;
      end;
    end
    else
    begin
      writeln('MCAmiga diff viewer usage: ' + ExtractFileName(ParamStr(0)) + ' diff <file1> <file2>');
    end;
  end
  else
  if Viewmode then
  begin
    if (ParamCount = 2) and FileExists(ParamStr(2)) then
    begin
      UnlockScreenUpdate;
      FileViewer(ParamStr(2));
    end
    else
    begin
      writeln('MCAmiga viewer usage: ' + ExtractFileName(ParamStr(0)) + ' view <file1>');
    end;
  end
  else
  begin

    // all done lets draw
    UnlockScreenUpdate;
    UpdateScreen(True);

    Right.ActiveElement := 0;
    Left.IsActive := True;

    // Activate the App Window
    MakeAppWindow;
    // run the main event cycle
    RunApp;
    // Application is closed, remove everything
    DestroyAppWindow;
  end;
  //
  Left.Free;
  Right.Free;
end;

begin
  InitVideo;
  InitMouse;
  InitKeyboard;

  Video.SetWindowTitle('MyCommander Amiga ' + NumVERSION, Copy(VERSION, 6, 12));

  StartMe;

  DoneKeyboard;
  DoneMouse;
  DoneVideo;
end.

