unit diffviewerunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Diff, keyboard, Math;

type


  TCompRec = record
    Kind: TChangeKind;
    chr1, chr2 : Char;
  end;

  TCompareLine = record
    Compares: array of TCompRec;
    Len: Integer;
    Changed: Boolean;
  end;

  { TDiffViewer }

  TDiffViewer = class
  private
    XOffset: Integer;
    File1, File2: string;
    Terminated: Boolean;
    Diff: TDiff;
    Text1, Text2: PChar;
    Len1, Len2: LongInt;
    LeftRect, RightRect: TRect;
    ViewLines: array of TCompareLine;
    StartLine: LongInt;
    FSelectedLine: LongInt;
    procedure DrawCurrentLine;
    procedure SetLineIndicator(Enable: Boolean);
    procedure FormatText;
    procedure Paint;
    function PollNextKey: TKeyEvent;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute(AFilename1, AFilename2: string);
  end;

implementation
uses
  Video, FileListUnit, eventunit;

const
  HLine = #196;
  VLine = #179;

{ TDiffViewer }

procedure TDiffViewer.DrawCurrentLine;
var
  Y1, Y2: LongInt;
  i, X, j: Integer;
  c1, c2: Char;
begin
  X := 0;
  Y2 := ScreenHeight - 1;
  Y1 := ScreenHeight - 2;
  if InRange(FSelectedLine, 0, High(ViewLines)) then
  begin
    //Y := (FSelectedLine - StartLine) + LeftRect.Top;

    XOffset := Min(XOffset, Max(0, ViewLines[FSelectedLine].Len - (ScreenWidth - 1)));

    for i := XOffset to ViewLines[FSelectedLine].Len - 1 do
    begin
      c1 := ViewLines[FSelectedLine].Compares[i].chr1;
      c2 := ViewLines[FSelectedLine].Compares[i].chr2;
      FGPen := White;
      case ViewLines[FSelectedLine].Compares[i].Kind of
        ckAdd: begin
          BGPen := LightGray;
          SetChar(i - XOffset, y1, c1);
          BGPen := Green;
          SetChar(i - XOffset, y2, c2);
        end;
        ckDelete:begin
          BGPen := Red;
          SetChar(i - XOffset, y1, c1);
          BGPen := LightGray;
          SetChar(i - XOffset, y2, c2);
        end;
        ckModify: begin
          BGPen := Brown;
          SetChar(i - XOffset, y1, c1);
          SetChar(i - XOffset, y2, c2);
        end;
        ckNone: begin
          BGPen := Blue;
          SetChar(i - XOffset, y1, c1);
          SetChar(i - XOffset, y2, c2);
        end;
      end;
      x := i - XOffset;
      if x >= ScreenWidth - 1 then
        Break;
      //if CurLine >= LeftRect.Height then
      //  Break;
    end;
    for j := x to ScreenWidth - 1 do
    begin
      BGPen := Blue;
      SetChar(j, y1, ' ');
      SetChar(j, y2, ' ');
    end;
  end;
end;

procedure TDiffViewer.SetLineIndicator(Enable: Boolean);
var
  f1, f2: string;
begin
  if InRange(FSelectedLine, StartLine, StartLine + LeftRect.Height) then
  begin
    if ViewLines[FSelectedLine].Changed then
      BGPen := Brown
    else
      BGPen := Blue;
    if Enable then
    begin
      SetChar(0, LeftRect.Top + FSelectedLine - StartLine, '>');
      SetChar(ScreenWidth - 1, LeftRect.Top + FSelectedLine - StartLine, '<');
      BGPen := Cyan;
      FGPen := Black;
      f1 := IntToStr(FSelectedLine + 1) + '/';
      f2 := IntToStr(Length(ViewLines));
      SetText((RightRect.Left) - Length(f1), 0, f1);
      SetText((RightRect.Left), 0, f2);
    end
    else
    begin
      SetChar(0, LeftRect.Top + FSelectedLine - StartLine, ' ');
      SetChar(ScreenWidth - 1, LeftRect.Top + FSelectedLine - StartLine, ' ');
    end;
  end;
end;

procedure TDiffViewer.FormatText;
var
  CurL: LongInt;
  i: Integer;
begin
  SetLength(ViewLines, 100);
  CurL := 0;

  SetLength(ViewLines[0].Compares, 100);
  ViewLines[0].Len := 0;
  ViewLines[0].Changed := False;

  for i := 0 to Diff.Count - 1 do
  begin
    with ViewLines[CurL] do
    begin
      Compares[Len].chr1 := Diff.Compares[i].chr1;
      Compares[Len].chr2 := Diff.Compares[i].chr2;
      Compares[Len].Kind := Diff.Compares[i].Kind;
      if Diff.Compares[i].Kind <> ckNone then
        Changed := True;
      Inc(Len);
      if ((Compares[Len - 1].chr1 = #13) or (Compares[Len - 1].chr2 = #13)) or ((Len > 0) and ((Compares[Len - 1].chr1 = #10) or (Compares[Len - 1].chr2 = #10))) then
      begin
        Compares[Len - 1].chr1 := ' ';
        Compares[Len - 1].chr2 := ' ';
        Inc(CurL);
        if CurL > High(ViewLines) then
          SetLength(ViewLines, CurL + 100);
        SetLength(ViewLines[Curl].Compares, 100);
        ViewLines[Curl].Len := 0;
        ViewLines[Curl].Changed := False;
      end
      else
        if Len > High(Compares) then
          SetLength(Compares, Len + 100);
    end;
  end;
  SetLength(ViewLines, CurL + 1);
end;

procedure TDiffViewer.Paint;
var
  i, x, j, y, y1, CurL: Integer;
  c1,c2: Char;
  Changed: Boolean;
  f1, f2: String;
begin
  BGPen := Blue;
  for y1 := 0 to ScreenHeight - 1 do
    for i := 0 to ScreenWidth - 1 do
      SetChar(i, y1, ' ');

  // draw
  BGPen := Cyan;
  FGPen := Black;
  //
  for i := 0 to ScreenWidth - 1 do
  begin
    SetChar(i, 0, ' ');
  end;

  LeftRect := Rect(1, 1, (ScreenWidth div 2) - 1, ScreenHeight - 4);
  RightRect := Rect((ScreenWidth div 2) + 1, 1, ScreenWidth - 1, ScreenHeight - 4);

  f1 := LimitName(File1, LeftRect.Width - 5, True);
  f2 := LimitName(File2, LeftRect.Width - 5, True);

  SetText(0, 0, f1);
  SetText(ScreenWidth - Length(f2), 0, f2);

  BGPen := Blue;
  FGPen := White;
  //
  for i := 1 to ScreenHeight - 4 do
    SetChar(ScreenWidth div 2, i, VLine);
  for i := 0 to ScreenWidth - 1 do
    SetChar(i, ScreenHeight - 3, HLine);



  x := 0;
  y1 := 0;
  Changed := False;

  for CurL := StartLine to High(ViewLines) do
  begin
    BGPen := Blue;
    for i := 0 to LeftRect.Width - 1 do
    begin
      SetChar(LeftRect.Left + i, y1 + LeftRect.Top, ' ');
      SetChar(RightRect.Left + i, y1 + RightRect.Top, ' ');
    end;
    for i := 0 to ViewLines[CurL].Len - 1 do
    begin
      c1 := ViewLines[CurL].Compares[i].chr1;
      c2 := ViewLines[CurL].Compares[i].chr2;
      FGPen := White;
      case ViewLines[CurL].Compares[i].Kind of
        ckAdd: begin
          Changed := True;
          BGPen := LightGray;
          SetChar(LeftRect.Left + i, y1 + LeftRect.Top, c1);
          BGPen := Green;
          SetChar(RightRect.Left + i, y1 + RightRect.Top, c2);
        end;
        ckDelete:begin
          Changed := True;
          BGPen := Red;
          SetChar(LeftRect.Left + i, y1 + LeftRect.Top, c1);
          BGPen := LightGray;
          SetChar(RightRect.Left + i, y1 + LeftRect.Top, c2);
        end;
        ckModify: begin
          Changed := True;
          BGPen := Brown;
          SetChar(LeftRect.Left + i, y1 + LeftRect.Top, c1);
          SetChar(RightRect.Left + i, y1 + LeftRect.Top, c2);
        end;
        ckNone: begin
          BGPen := Blue;
          SetChar(LeftRect.Left + i, y1 + LeftRect.Top, c1);
          SetChar(RightRect.Left + i, y1 + LeftRect.Top, c2);
        end;
      end;
      x := i;
      if x >= LeftRect.Width then
        Break;
    end;
    BGPen := Blue;
    for j := x + 1 to LeftRect.Width do
    begin
      SetChar(j + LeftRect.Left, LeftRect.Top + y1, ' ');
      SetChar(j + RightRect.Left, RightRect.Top + y1, ' ');
    end;
    if Changed then
      BGPen := Yellow
    else
      BGPen := Blue;
    Changed := False;
    SetChar(0, LeftRect.Top + y1, ' ');
    SetChar(ScreenWidth - 1, LeftRect.Top + y1, ' ');
    // next line
    Inc(y1);
    if y1 > LeftRect.Height then
      Break;
  end;

  BGPen := Blue;
  for y := x to LeftRect.Width do
  begin
    SetChar(LeftRect.Left + y, LeftRect.Top + CurL, ' ');
    SetChar(RightRect.Left + y, RightRect.Top + CurL, ' ');
  end;
  for y := CurL to LeftRect.Height do
  begin
    SetChar(0, LeftRect.Top + y, ' ');
    for x := 0 to LeftRect.Width do
    begin
      SetChar(LeftRect.Left + x, LeftRect.Top + y, ' ');
      SetChar(RightRect.Left + x, RightRect.Top + y, ' ');
    end;
  end;

  DrawCurrentLine;
  SetLineIndicator(True);
  UpdateScreen(True);
end;

function TDiffViewer.PollNextKey: TKeyEvent;
//var
//  Me: TMouseEvent;
begin
  Result := GetNextKeyEvent;
  if Result = ResizeKey then
  begin
    //FormatText;
    Paint;
  end;
  //if GetNextMouseEvent(Me) then
  //  GotMouseEvent(Me);
end;

constructor TDiffViewer.Create;
begin
  Diff := nil;
end;

destructor TDiffViewer.Destroy;
begin
  Diff.Free;
  inherited Destroy;
end;

procedure TDiffViewer.Execute(AFilename1, AFilename2: string);
var
  FS: TFileStream;
  Key: TKeyEvent;
  st: Byte;
begin
  Text1 := nil;
  Text2 := nil;
  Diff := TDiff.Create(nil);
  try
   FS := TFileStream.Create(AFilename1, fmOpenRead);
   Len1 := FS.Size;
   Text1 := AllocMem(Len1);
   FS.Read(Text1^, Len1);
   FS.Free;
   FS := nil;
   FS := TFileStream.Create(AFilename2, fmOpenRead);
   Len2 := FS.Size;
   Text2 := AllocMem(Len2);
   FS.Read(Text2^, Len2);
   FS.Free;
   FS := nil;
   if not Diff.Execute(Text1, Text2, Len1, Len2) then
   begin
     // message?
     Exit;
   end;
  File1 := AFilename1;
  File2 := AFilename2;
  FormatText;
  StartLine := 0;
  XOffset := 0;
  Paint;
  Terminated := False;
  repeat
    Key := PollNextKey;
    if Terminated then
      Break;
    if Key > 0 then
    begin
      st := GetKeyEventShiftState(Key);
      case (TranslateKeyEvent(Key) and $FFFF) of
        kbdUp: begin
          SetLineIndicator(False);
          XOffset := 0;
          FSelectedLine := Max(FSelectedLine - 1, 0);
          if FSelectedLine > StartLine then
          begin
            DrawCurrentLine;
            SetLineIndicator(True);
            UpdateScreen(False);
          end
          else
          begin
            while FSelectedLine < StartLine do
              StartLine := Max(StartLine - 3, 0);
            Paint;
          end;
        end;
        kbdDown: begin
          SetLineIndicator(False);
          XOffset := 0;
          FSelectedLine := Min(FSelectedLine + 1, High(ViewLines));
          if FSelectedLine <= StartLine + LeftRect.Height then
          begin
            DrawCurrentLine;
            SetLineIndicator(True);
            UpdateScreen(False);
          end
          else
          begin
            while FSelectedLine >= StartLine + LeftRect.Height do
              StartLine := Min(StartLine + 3, High(ViewLines) - (LeftRect.Height - 1));
            Paint;
          end;
        end;

        kbdPgUp, $39, $8D00:begin
          SetLineIndicator(False);
          XOffset := 0;
          FSelectedLine := Max(FSelectedLine - LeftRect.Height, 0);
          if FSelectedLine > StartLine then
          begin
            DrawCurrentLine;
            SetLineIndicator(True);
            UpdateScreen(False);
          end
          else
          begin
            while FSelectedLine < StartLine do
              StartLine := Max(StartLine - 3, 0);
            Paint;
          end;
        end;                                       // pg up -> Move around
        kbdPgDn, $33, $9100:  begin
          SetLineIndicator(False);
          XOffset := 0;
          FSelectedLine := Min(FSelectedLine + LeftRect.Height, High(ViewLines));
          if FSelectedLine <= StartLine + LeftRect.Height then
          begin
            DrawCurrentLine;
            SetLineIndicator(True);
            UpdateScreen(False);
          end
          else
          begin
            while FSelectedLine >= StartLine + LeftRect.Height do
              StartLine := Min(StartLine + 3, High(ViewLines) - (LeftRect.Height - 1));
            Paint;
          end;
        end;
        kbdHome, $37, $7300: begin
          SetLineIndicator(False);
          XOffset := 0;
          FSelectedLine := 0;
          if FSelectedLine > StartLine then
          begin
            DrawCurrentLine;
            SetLineIndicator(True);
            UpdateScreen(False);
          end
          else
          begin
            StartLine := 0;
            Paint;
          end;
        end;
        kbdEnd, $31, $7400: begin
          SetLineIndicator(False);
          XOffset := 0;
          FSelectedLine := High(ViewLines);
          if FSelectedLine <= StartLine + LeftRect.Height then
          begin
            DrawCurrentLine;
            SetLineIndicator(True);
            UpdateScreen(False);
          end
          else
          begin
            StartLine := High(ViewLines) - (LeftRect.Height - 1);
            Paint;
          end;
        end;
        kbdF10,kbdF3: Break;

        kbdRight: begin
          XOffset := XOffset + 1;
          DrawCurrentLine;
          UpdateScreen(False);
        end;
        kbdLeft: begin
          XOffset := Max(0, XOffset - 1);
          DrawCurrentLine;
          UpdateScreen(False);
        end;


        { $2106: begin                                       // Ctrl + f -> toggle visibility of bottom menu
          ShowMenu := not ShowMenu;
          ClearScreen;
          Paint;
        end;
        kbdUp: begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := StartLine - 1;
          if Mode = vmHex then
            CurrentByte := Max(0, CurrentByte - NumBytesPerLine);
        end;
        kbdDown: begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := StartLine + 1;
          if Mode = vmHex then
            CurrentByte := Min(NumBytes - 1, Int64(CurrentByte) + NumBytesPerLine);
        end;
        kbdRight: begin
          FUntilByte := 0;
          if Mode = vmHex then
            CurrentByte :=  Min(NumBytes - 1, Int64(CurrentByte) + 1);
        end;
        kbdLeft: begin
          FUntilByte := 0;
          if Mode = vmHex then
            CurrentByte := Max(0, CurrentByte - 1);
        end;
        kbdPgUp, $39, $8D00:begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := StartLine - 10;
          if Mode = vmHex then
            CurrentByte := Max(0, CurrentByte - Int64(ScreenHeight) * NumBytesPerLine);
        end;                                       // pg up -> Move around
        kbdPgDn, $33, $9100:  begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := StartLine + 10;
          if Mode = vmHex then
            CurrentByte := Min(NumBytes - 1, CurrentByte + Int64(ScreenHeight) * NumBytesPerLine);
        end;                                      // pg down -> Move around
        kbdHome, $37, $7300: begin
          FUntilByte := 0;
          FCurrentByte := 0;
          StartLine := 0;                      // Home -> Move around
        end;
        kbdEnd, $31, $7400: begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := MaxInt                  // end -> Move around
          else
            CurrentByte := NumBytes - 1;
        end;
        kbdF10,kbdF3: Break;
        kbdF1: begin ShowViewHelp; Paint; end;
        kbdF4: SwitchMode;
        kbdF5: begin
          FUntilByte := 0;
          GoToLineNumber;
        end;
        kbdF7: begin
          FUntilByte := 0;
          if Mode = vmText then
          begin
            if st and kbShift <> 0 then
              FindText(LastTextSearch, Point(FromSel.X + 1, FromSel.Y))
            else
              SearchText;
          end;
          if Mode = vmHex then
          begin
            if st and kbShift <> 0 then
              FindBinary(LastBinSearch, FCurrentByte + 1)
            else
              SearchBinary;
          end;
        end;}
      end;
    end;
    Sleep(25);
  until (Key and $ff00) = $0100;

  finally
    FreeMem(Text1);
    FreeMem(Text2);
    Diff.Free;
    Diff := nil;
  end;
end;

end.

