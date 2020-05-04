program MCAmiga;
uses
  {$ifdef AMIGA68k}
  Exec,
  {$endif}
  Types, SysUtils, Video, mouse, keyboard, FileListUnit, dialogunit;

procedure Debug(AMsg: string);
begin
  {$ifdef HASAMIGA}
  writeln(AMsg);
  {$endif}
end;

var
  Src: TFileList;
  Dest: TFileList;


procedure SwapSrcDest;
var
  Temp: TFileList;
begin
  Temp := Dest;
  Dest := Src;
  Src := Temp;
end;

procedure StartMe;
var
  rightSide, leftSide: TRect;
  Left, Right: TFileList;
  Ev: TKeyEvent;
begin
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

  repeat
    Ev := GetKeyEvent;
    try
      case TranslateKeyEvent(Ev) and $ffff of
        $0F09: begin // TAB
          SwapSrcDest;
          Src.IsActive := True;
          Dest.IsActive := False;
        end;
        $0008: Src.GoToParent;                                     // Backspace
        $1C0D, $000D: Src.EnterPressed;                            // return
        kbdUp, $38: Src.ActiveElement := Src.ActiveElement - 1;    // cursor up
        kbdDown, $32: Src.ActiveElement := Src.ActiveElement + 1;  // cursor down
        kbdPgUp, $39: Src.ActiveElement := Src.ActiveElement - 10; // pg up
        kbdPgDn, $33: Src.ActiveElement := Src.ActiveElement + 10; // pg down
        kbdHome, $37: Src.ActiveElement := 0; // Home
        kbdEnd, $31: Src.ActiveElement := MaxInt; // end
        $1312, $1300: Src.Update(True);                    // Ctrl + R Alt + R
        $180F, $1800: Dest.CurrentPath := Src.CurrentPath; // Ctrl + O   Alt + O
        $2004, $2000: Src.CurrentPath := '';               // Ctrl + D Alt + D
        kbdInsert, $0023, $0030: Src.SelectActiveEntry;    // Insert, #, 0
        $002B: begin
          Src.SelectByPattern(True);                       // +
          Left.Update(False);
          Right.Update(False);
        end;
        $002D: begin
          Src.SelectByPattern(False);                      // -
          Left.Update(False);
          Right.Update(False);
        end;
        kbdF10, $011B: begin                               // F10, ESC
          if AskQuestion('Quit Program') then
            Break;
          Left.Update(False);
          Right.Update(False);
        end;
        kbdF5: begin
          Src.CopyFiles(Dest.CurrentPath);
          Src.Update(False);
          Dest.Update(True);
        end;
        kbdF7: begin
          Src.MakeDir();
          Left.Update(False);
          Right.Update(False);
        end;
        kbdF8, kbdDelete: begin                           // Delete
          Src.DeleteSelected();
          Left.Update(False);
          Right.Update(False);
        end;
        kbdF1: begin
          ShowHelp;
          Left.Update(False);
          Right.Update(False);
        end;
        else
          Debug('Key: $' + HexStr(TranslateKeyEvent(Ev), 4));
      end;
    except
      Left.Update(False);
      Right.Update(False);
    end;
  until False; //(ev and $ff00) = $0100;
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

