program MCAmiga;
uses
  {$ifdef AMIGA68k}
  Exec,
  {$endif}
  Types, SysUtils, Video, mouse, keyboard, FileListUnit, dialogunit, EventUnit;

procedure Debug(AMsg: string);
begin
  {$ifdef HASAMIGA}
  writeln(AMsg);
  {$endif}
end;

var
  Src: TFileList;
  Dest: TFileList;
  Left, Right: TFileList;
  rightSide, leftSide: TRect;

procedure SwapSrcDest;
var
  Temp: TFileList;
begin
  Temp := Dest;
  Dest := Src;
  Src := Temp;
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
      Src.IsActive := True;
      Dest.IsActive := False;
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
    kbdInsert, $23, $30, $20: Src.SelectActiveEntry;   // Insert, #, 0, Space -> Select file
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
    kbdF5: begin                                      // F5 -> Copy/CopyAs
      Src.CopyFiles(Dest.CurrentPath);
      Src.Update(False);
      Dest.Update(True);
    end;
    kbdF6: begin                                      // F6 -> Move/Rename
      if st and kbShift <> 0 then
        Src.Rename();
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
    kbdF2: begin
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
      Debug('Key: $' + HexStr(TranslateKeyEvent(Ev), 4));
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
    LeftSide := Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1);
    RightSide := Rect(LeftSide.Right + 1, 0, ScreenWidth - 1, ScreenHeight - 1);
    Left.Resize(LeftSide);
    Right.Resize(rightSide);
  end;

end;

procedure StartMe;
begin
  OnKeyPress := @KeyEvent;
  OnResize := @ResizeEvent;

  LeftSide := Rect(0, 0, (ScreenWidth div 2) - 1, ScreenHeight - 1);
  RightSide := Rect(LeftSide.Right + 1, 0, ScreenWidth - 1, ScreenHeight - 1);



  Left := TFileList.Create(LeftSide);
  Right := TFileList.Create(RightSide);

  Src := Left;
  Dest := Right;

  {$ifdef HASAMIGA}
  Left.CurrentPath := 'sys:';
  Right.CurrentPath := 'ram:';
  {$endif}
  {$ifdef LINUX}
  Left.CurrentPath := '/';
  Right.CurrentPath := '/usr/bin';
  {$endif}

  Right.ActiveElement := 0;
  Left.IsActive := True;

  RunApp;
  Left.Free;
  Right.Free;
end;

const
  AFF_68080 = 1 shl 10;

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
  Video.SetWindowTitle('MyCommander Amiga 0.1', 'My Commander Amiga');
  {$endif}

  StartMe;

  DoneKeyboard;
  DoneMouse;
  DoneVideo;
end.

