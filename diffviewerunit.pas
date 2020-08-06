unit diffviewerunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Diff, keyboard, Math, dialogunit, FileListUnit;

// Help Text for the Viewer
const           //.........1.........2.........3.........4.........5........6.........7
  HelpDiffText = ' ---- Diff Viewer Help -----  '#13#10 +
                 ' F1         - Help     '#13#10 +
                 ' ESC        - Leave Viewer'#13#10 +
                 ' Up, Down                              - navigate in Text'#13#10  +
                 ' Ctrl Up,Down,Pg Up,9,Pg Down,3 - fast navigation'#13#10 +
                 ' Ctrl Left,Right,Home,7,End,1   - to start/end'#13#10 +
                 ' Shift Up,Down                  - next/prev. difference';

type
  TCompRec = record
    Kind: TChangeKind;
    chr1, chr2 : Char;
  end;

  TCompareLine = record
    Compares: array of TCompRec;
    Len: Integer;
    Line1: LongInt;
    Line2: LongInt;
    Changed: Boolean;
  end;

  { TDiffViewer }

  TDiffViewer = class(TPaintedClass)
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

    function PollNextKey: TKeyEvent;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Paint; override;
    procedure Execute(AFilename1, AFilename2: string);
  end;

implementation
uses
  Video, eventunit;

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
      f1 := IntToStr(ViewLines[FSelectedLine].Line1) + VLINE;
      f2 := IntToStr(ViewLines[FSelectedLine].Line2);
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
  L1, L2: LongInt;
begin
  SetLength(ViewLines, 100);
  CurL := 0;
  L1 := 1;
  L2 := 1;

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
        Line1 := L1;
        Line2 := L2;
        if Compares[Len - 1].chr1 in [#13, #10] then
        begin
          Inc(L1);
          Compares[Len - 1].chr1 := ' ';
        end;
        if Compares[Len - 1].chr2 in [#13, #10] then
        begin
          Inc(L2);
          Compares[Len - 1].chr2 := ' ';
        end;
        //
        Inc(CurL);
        if CurL > High(ViewLines) then
          SetLength(ViewLines, CurL + 100);
        SetLength(ViewLines[Curl].Compares, 100);
        ViewLines[Curl].Len := 0;
        ViewLines[Curl].Changed := False;
        ViewLines[Curl].Line1 := L1;
        ViewLines[Curl].Line2 := L2;
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
  i, x, j, y, CurL: Integer;
  c1,c2: Char;
  Changed: Boolean;
  f1, f2: String;
begin
  BGPen := Blue;
  for y := 0 to ScreenHeight - 1 do
    for i := 0 to ScreenWidth - 1 do
      SetChar(i, y, #0);

  // draw
  BGPen := Cyan;
  FGPen := Black;
  //
  for i := 0 to ScreenWidth - 1 do
    SetChar(i, 0, ' ');

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
  y := 0;
  Changed := False;

  for CurL := StartLine to High(ViewLines) do
  begin
    BGPen := Blue;
    for i := 0 to LeftRect.Width - 1 do
    begin
      SetChar(LeftRect.Left + i, y + LeftRect.Top, ' ');
      SetChar(RightRect.Left + i, y + RightRect.Top, ' ');
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
          SetChar(LeftRect.Left + i, y + LeftRect.Top, c1);
          BGPen := Green;
          SetChar(RightRect.Left + i, y + RightRect.Top, c2);
        end;
        ckDelete:begin
          Changed := True;
          BGPen := Red;
          SetChar(LeftRect.Left + i, y + LeftRect.Top, c1);
          BGPen := LightGray;
          SetChar(RightRect.Left + i, y + LeftRect.Top, c2);
        end;
        ckModify: begin
          Changed := True;
          BGPen := Brown;
          SetChar(LeftRect.Left + i, y + LeftRect.Top, c1);
          SetChar(RightRect.Left + i, y + LeftRect.Top, c2);
        end;
        ckNone: begin
          BGPen := Blue;
          SetChar(LeftRect.Left + i, y + LeftRect.Top, c1);
          SetChar(RightRect.Left + i, y + LeftRect.Top, c2);
        end;
      end;
      x := i;
      if x >= LeftRect.Width then
        Break;
    end;
    if Changed then
      BGPen := Yellow
    else
      BGPen := Blue;
    Changed := False;
    SetChar(0, LeftRect.Top + y, ' ');
    SetChar(ScreenWidth - 1, LeftRect.Top + y, ' ');
    // next line
    Inc(y);
    if y > LeftRect.Height then
      Break;
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
  inherited;
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
  Found: Boolean;
  i: LongInt;
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
        kbdF1: begin
          ShowMessage(HelpDiffText);
          Paint;
        end;
        kbdUp: begin
          SetLineIndicator(False);
          XOffset := 0;
          if st and kbShift <> 0 then
          begin
            Found := False;
            for i := FSelectedLine - 1 downto 0 do
            begin
              if ViewLines[i].Changed then
              begin
                Found := True;
                FSelectedLine := i;
                Break;
              end;
            end;
            if not Found then
            begin
              ShowMessage('no more changes found');
              Paint;
            end;
          end
          else
          begin
            FSelectedLine := Max(FSelectedLine - 1, 0);
          end;
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
          if st and kbShift <> 0 then
          begin
            Found := False;
            for i := FSelectedLine + 1 to High(ViewLines) do
            begin
              if ViewLines[i].Changed then
              begin
                Found := True;
                FSelectedLine := i;
                Break;
              end;
            end;
            if not Found then
            begin
              ShowMessage('no more changes found');
              Paint;
            end;
          end
          else
          begin
            FSelectedLine := Min(FSelectedLine + 1, High(ViewLines));
          end;
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

