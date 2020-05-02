program MCAmiga;
uses
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
  ClearScreen;
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

    case TranslateKeyEvent(Ev) and $ffff of
      $0F09: begin // TAB
        SwapSrcDest;
        Src.IsActive := True;
        Dest.IsActive := False;
      end;
      $1C0D, $000D: Src.EnterPressed; // return
      kbdUp, $38: Src.ActiveElement := Src.ActiveElement - 1; // cursor up
      kbdDown, $32: Src.ActiveElement := Src.ActiveElement + 1; // cursor down
      kbdPgUp, $39: Src.ActiveElement := Src.ActiveElement - 10; // pg up
      kbdPgDn, $33: Src.ActiveElement := Src.ActiveElement + 10; // pg down
      kbdHome, $37: Src.ActiveElement := 0; // Home
      kbdEnd, $31: Src.ActiveElement := MaxInt; // end
      kbdF10:begin // F10
        if AskQuestion('Quit Program') then
          Break;
        Left.Update(False);
        Right.Update(False);
      end
      else
        Debug('Key: $' + HexStr(TranslateKeyEvent(Ev), 4));
    end;
  until False; //(ev and $ff00) = $0100;
  Left.Free;
  Right.Free;
end;

begin
  InitVideo;
  InitMouse;
  InitKeyboard;

  StartMe;

  DoneKeyboard;
  DoneMouse;
  DoneVideo;
end.

