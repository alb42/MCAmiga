unit viewerunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video, Keyboard, Mouse, Math,
  EventUnit;

const
  VLine = #179;

procedure FileViewer(AFileName: string; ASearch: string = ''; Posi: LongInt = -1);

implementation

uses
  FileListUnit, DialogUnit, toolsunit;

type
  TViewerMode = (vmText, vmHex);

  { TFileViewer }

  TFileViewer = class(TPaintedClass)
  private
    Terminated: Boolean;
    Buffer: PByte;
    BufferSize: Int64;
    NumBytes: LongWord;
    Num4BytesPerLine: LongWord;
    NumBytesPerLine: LongWord;
    OffsetForChars: LongWord;
    FCurrentByte: LongWord;
    FUntilByte: LongWord;
    Mode: TViewerMode;
    FileName: string;
    FStartLine: Integer;
    LineStarts: TList;
    function PollNextKey: TKeyEvent;
    procedure GotMouseEvent(Me: TMouseEvent);
    procedure FormatText;
    procedure DrawMenu;
    procedure SetCurrentByte(AValue: LongWord);
    procedure SetStartLine(AValue: Integer);
    procedure SwitchMode;
    procedure GoToLineNumber;

  private
    ShowMenu: Boolean;
    MemLine: PChar;
    MemLineLength: Integer;
    LastBinSearch: string;
    procedure SearchBinary;
    function GetTextLine(Line: Integer): string;
    procedure FindBinary(SearchString: string; FromWhere: Integer);
    procedure SearchBinaryPosition(Posi: LongInt; Len: LongInt);
    procedure SearchTextPosition(Posi: LongInt; Len: LongInt);
  private
    FromSel, ToSel: TPoint;
    LastTextSearch: string;
    procedure SearchText;
    procedure FindText(SearchString: string; FromWhere: TPoint);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure Execute(AFilename: string; ASearch: string = ''; Posi: LongInt = -1);
    property StartLine: Integer read FStartLine write SetStartLine;
    property CurrentByte: LongWord read FCurrentByte write SetCurrentByte;
  end;

procedure FileViewer(AFileName: string; ASearch: string = ''; Posi: LongInt = -1);
begin
  With TFileViewer.Create do
  begin
    Execute(AFileName, ASearch, Posi);
    Free;
  end;
end;

{ TFileViewer }

function TFileViewer.PollNextKey: TKeyEvent;
var
  Me: TMouseEvent;
begin
  Result := GetNextKeyEvent;
  if Result = ResizeKey then
  begin
    FormatText;
    Paint;
  end;
  if GetNextMouseEvent(Me) then
    GotMouseEvent(Me);
end;

procedure TFileViewer.GotMouseEvent(Me: TMouseEvent);
var
  yPos, XPos: LongWord;
  l: LongWord;
  Len: LongInt;
begin
  if Me.Action = MouseActionDown then
  begin
    if (Me.y = ScreenHeight - 1) and ShowMenu then
    begin
      Len := ScreenWidth div 10;
      case Me.x div Len of
        0:begin //F1
          ShowViewHelp;
          Paint;
        end;
        2,9: Terminated := True; // F3, F10
        3: SwitchMode;           // F4
        4: begin                 // F5
          FUntilByte := 0;
          GoToLineNumber;
        end;
        6: begin                 // F7
          FUntilByte := 0;
          if Mode = vmText then
          begin
            if Me.buttons = MouseRightButton then
              FindText(LastTextSearch, Point(FromSel.X + 1, FromSel.Y));
            if Me.buttons = MouseLeftButton then
              SearchText;
          end;
          if Mode = vmHex then
          begin
            if Me.buttons = MouseRightButton then
              FindBinary(LastBinSearch, FCurrentByte + 1);
            if Me.buttons = MouseLeftButton then
              SearchBinary;
          end;
        end;
      end;
    end
    else
    if Mode = vmHex then
    begin
      yPos := me.y - 1 + FStartLine;
      if me.x < 9 then
        Exit;
      if me.x < OffsetForChars then
      begin // clicked to HexStrings
        xPos := me.x - 9;
        l := xPos div 13;
        xPos := (xPos mod 13) div 3;
        xPos := xPos + l * 4;
        CurrentByte := yPos * NumBytesPerLine + xPos;
      end
      else
      begin // clicked to Chars
        xPos := me.x - OffsetForChars;
        if xPos >= NumBytesPerLine then
          Exit;
        CurrentByte := yPos * NumBytesPerLine + xPos;
      end;
    end;
  end;
end;

procedure ConvertText(var s: string);
var
  i: Integer;
  c: Char;
begin
  for i := 1 to Length(s) do
  begin
    c := s[i];
    ConvertChar(c);
    s[i] := c;
  end;
end;

procedure TFileViewer.FormatText;
var
  i: Integer;
  P: PByte;
  LastSpace: LongInt;
  LineStart: LongInt;
begin
  LineStarts.Clear;
  P := Buffer;
  LineStarts.Add(P);
  LineStart := 0;
  LastSpace := -1;
  for i := 0 to BufferSize - 1 do
  begin
    if p[i] in [$20, 9] then
      LastSpace := i;
    if p[i] = 10 then
    begin
      if i < BufferSize - 1 then
      begin
        LineStarts.Add(@(p[i + 1]));
        LineStart := i + 1;
      end;
    end;
    if (i - LineStart >= ScreenWidth - 1) then
    begin
      if LastSpace > 0 then
      begin
        LineStarts.Add(@(p[LastSpace + 1]));
        LineStart := LastSpace + 1;
        LastSpace := -1
      end
      else
      begin
        LineStarts.Add(@(p[i]));
        LineStart := i;
      end;
    end;
  end;
end;

const                                                    //  HEX
  ViewMenuNamesTXT: array[1..10] of string = ('Help', '', 'Quit', 'Hex', 'Goto', '', 'Search', '', ' ', 'Quit');
  ViewMenuNamesHEX: array[1..10] of string = ('Help', '', 'Quit', 'Ascii', 'Goto', '', 'Search', '', ' ', 'Quit');

procedure TFileViewer.DrawMenu;
var
  Len, x, y, i: LongInt;
  s: string;
begin
  Len := ScreenWidth div 10;
  BGPen := Cyan;
  FGPen := Black;
  for i := 0 to ScreenWidth - 1 do
  begin
    SetChar(i, ScreenHeight - 1, ' ');
  end;

  for i := 1 to 10 do
  begin
    x := (i - 1) * Len;
    y := ScreenHeight - 1;
    s := Format('%2d', [i]);
    FGPen := White;
    BGPen := Black;
    SetText(x,y, s);
    x := x + 2;
    if Mode = vmHex then
      s := Copy(ViewMenuNamesHex[i], 1, Len - 2)
    else
      s := Copy(ViewMenuNamesTXT[i], 1, Len - 2);
    BGPen := Cyan;
    FGPen := Black;
    SetText(x,y, s);
  end;
end;

procedure TFileViewer.Paint;
var
  i, l,  cx, l1: Integer;
  j, StartByte, cb, EndOfDisplay: LongWord;
  s, s1, s2: String;
  pl: PByte;
  EndScreen: LongInt;

  function IsByteSelected(Num: LongWord): Boolean;
  begin
    if FUntilByte = 0 then
      Result := Num = FCurrentByte
    else
      Result := InRange(Num, FCurrentByte, FUntilByte);
  end;

begin
  // draw
  BGPen := Cyan;
  FGPen := Black;
  //
  for i := 0 to ScreenWidth - 1 do
  begin
    SetChar(i, 0, ' ');
  end;

  SetTextA(0, 0, ExtractFileName(FileName));

  if ShowMenu then
    EndScreen := ScreenHeight - 3
  else
    EndScreen := ScreenHeight - 2;

  if Mode = vmText then
  begin
    s :=  IntToStr(StartLine + 1) + ' - ' + IntToStr(Min(LineStarts.Count, (StartLine + 1) + EndScreen)) + '/' + IntToStr(LineStarts.Count);
    SetText(ScreenWidth - Length(s), 0, s);

    BGPen := Blue;
    FGPen := White;
    for i := 0 to EndScreen do
    begin
      if i + StartLine < LineStarts.Count then
      begin
        s := GetTextLine(i + StartLine);
        {for j := 0 to Min(ScreenWidth - 1, MemLineLength - 1) do
        begin
          BGPen := Blue;
          SetChar(j, i + 1, MemLine[j]);
        end;}
        l := Length(s);
        if InRange(i + StartLine, FromSel.Y, ToSel.Y) then
        begin
          s1 := s;
          s2 := Copy(s1, 1, FromSel.X - 1);
          l1 := Length(s2);
          SetTextA(0, i + 1, s2);
          BGPen := Cyan;
          s2 := Copy(s1, FromSel.X, ToSel.X - FromSel.X);
          Delete(s1, 1, ToSel.X - 1);
          SetTextA(l1, i + 1, s2);
          l1 := l1 + Length(s2);
          BGPen := Blue;
          SetTextA(l1, i + 1, s1);
        end
        else
        begin
          SetTextA(0, i + 1, s);
        end;
      end
      else
        l := 0;
      for j := l to ScreenWidth do
        SetChar(j, i + 1, ' ');
    end;
  end else
  if Mode = vmHex then
  begin
    s :=  IntToStr(CurrentByte) + ' = $' + HexStr(CurrentByte, 8) + '  ' + IntToStr(NumBytes) + ' bytes';
    SetText(ScreenWidth - Length(s), 0, s);

    // what we need:
    // 8 chars address + space + Space between Hex and char view = 10 chars
    // per 4 byte = 2*4 Chars FF + 4 * Space + VertLine + 4 * Char view = 17 Chars
    Num4BytesPerLine := ((ScreenWidth - 10) div 17);
    NumBytesPerLine :=  Num4BytesPerLine * 4;
    OffsetForChars := 9 + 13 * Num4BytesPerLine; // Startoffset for the char display
    EndOfDisplay := 9 + 17 * Num4BytesPerLine; // end of our display (rest must be cleared)

    StartByte := NumBytesPerLine * LongWord(FStartLine);
    pl := Buffer;
    Inc(pl, StartByte);
    BGPen := Blue;

    for i := 0 to EndScreen do
    begin
      // print Address
      cb := Min(StartByte + Int64(i) * NumBytesPerLine, $FFFFFFFF);
      if cb <= NumBytes then
      begin
        s := HexStr(cb, 8) + ' ';
        FGPen := Yellow;
        SetText(0, i + 1, s);
        FGPen := White;
        cx := 9;
        for j := 0 to NumBytesPerLine - 1 do
        begin
          if cb + j < NumBytes then
          begin
            s := HexStr(pl^, 2);
            if IsByteSelected(cb + j) then
              BGPen := Cyan;
            SetText(cx, i + 1, s);
            BGPen := Blue;
            SetText(cx + 2, i + 1, ' ');
            Inc(cx, 3);
            if IsByteSelected(cb + j) then
              BGPen := Cyan;
            if InRange(pl^, 32, 126) then
              SetChar(OffsetForChars + j, i + 1, Char(Pl^))
            else
              SetChar(OffsetForChars + j, i + 1, '.');
            BGPen := Blue;
            if (j + 1) mod 4 = 0 then
            begin
              SetChar(cx, i + 1, VLine);
              Inc(cx);
            end;
          end
          else
          begin
            BGPen := Blue;
            SetText(cx, i + 1, '   ');
            Inc(cx, 3);
            SetChar(OffsetForChars + j, i + 1, ' ');
            if (j + 1) mod 4 = 0 then
            begin
              SetChar(cx, i + 1, VLine);
              Inc(cx);
            end;
          end;
          Inc(pl);
        end;
        l := EndOfDisplay;
      end
      else
        l := 0;

      for j := l to ScreenWidth do
        SetChar(j, i + 1, ' ');

    end;
  end;

  if ShowMenu then
    DrawMenu;
  UpdateScreen(False);
end;

procedure TFileViewer.SetCurrentByte(AValue: LongWord);
var
  Offset: LongInt;
begin
  if FCurrentByte = AValue then Exit;
  if ShowMenu then
    Offset := +1
  else
    Offset := 0;
  FCurrentByte := EnsureRange(AValue, 0, NumBytes);
  while Max(FCurrentByte, FUntilByte) >= ((FStartLine + (Int64(ScreenHeight) - 1 - Offset))) * NumBytesPerLine do
  begin
    if StartLine >= NumBytes div NumBytesPerLine - 1 - Offset then
      Break;
    Inc(FStartLine);
  end;
  while FCurrentByte < Int64(FStartLine) * NumBytesPerLine - 1 - Offset do
  begin
    if FStartLine = 0 then
      Break;
    Dec(FStartLine);
  end;
  Paint;
end;

procedure TFileViewer.SetStartLine(AValue: Integer);
var
  Offset: Integer;
begin
  if FStartLine = AValue then Exit;

  if ShowMenu then
    Offset := -1
  else
    Offset := 0;


  FStartLine := Max(AValue, 0);
  if Mode = vmText then
  begin
    FStartLine := Min(FStartLine, Max(LineStarts.Count - ScreenHeight + 1 - Offset, 0));
    Paint;
  end
  else
  if Mode = vmHex then
  begin
    FStartLine := Min(FStartLine, (NumBytes div NumBytesPerLine) - 1 - Offset);
    Paint;
  end;
end;

procedure TFileViewer.SwitchMode;
begin
  FStartLine := 0;
  case Mode of
    vmText: begin
      Mode := vmHex;
      Paint;
    end;
    vmHex: begin
      Mode := vmText;
      FormatText;
      Paint;
    end;
  end;
end;

procedure TFileViewer.GoToLineNumber;
var
  NewLine: Integer;
begin
  if Mode = vmText then
  begin
    NewLine := 1;
    if AskForNumber('Goto line (1 to ' + IntToStr(LineStarts.Count)  +')', NewLine) then
      StartLine := NewLine - 1
    else
      Paint;
  end
  else
  if Mode = vmHex then
  begin
    if AskForNumber('Goto offset (0 to ' + IntToStr(NumBytes - 1)  +')', NewLine) then
      CurrentByte := NewLine
    else
      Paint;
  end;
end;

procedure TFileViewer.SearchBinary;
var
  SearchString: string;
  Pl: PByte;
begin
  Pl := Buffer;
  Inc(PL, CurrentByte);
  SearchString := '';//HexStr(PL^, 2);
  if AskForHexNumber('Search for binary, Hex (0..9,A..F) ', SearchString) then
  begin
    SearchString := StringReplace(UpperCase(SearchString), ' ', '', [rfReplaceAll]);
    LastBinSearch := SearchString;
    FindBinary(SearchString, 0);
  end;

  Paint;
end;

function TFileViewer.GetTextLine(Line: Integer): string;
var
  StartPC, EndPC: PByte;
  i: Integer;
begin
  StartPC := LineStarts[Line];
  if Line + 1 >= LineStarts.Count then
    EndPC := Buffer + BufferSize
  else
    EndPC := LineStarts[Line + 1];
  FillChar(MemLine^, MemLineLength, 0);
  Move(StartPC^, MemLine^, Min(MemLineLength, EndPC - StartPC));
  for i := 0 to Min(MemLineLength, EndPC - StartPC - 1) do
  begin
    if MemLine[i] = #0 then
      MemLine[i] := '.';
    if MemLine[i] = #10 then
      MemLine[i] := ' ';
  end;
  Result := MemLine;
end;

procedure TFileViewer.FindBinary(SearchString: string; FromWhere: Integer);
var
  Pl, Pl2: PByte;
  s: string;
  Sa: array of Byte;
  Idx, i, j: Integer;
  Offset: SizeInt;
  AbsOffset: LongWord;
  Found: Boolean;
begin
  sa := [];
  SetLength(sa, 10);
  Idx := 0;
  while Length(Searchstring) > 0 do
  begin
    s := Copy(SearchString, 1, 2);
    Delete(SearchString, 1, 2);
    if TryStrToInt('$' + s, i) then
    begin
      sa[Idx] := i;
      Inc(Idx);
      if Idx > High(sa) then
        SetLength(sa, Idx + 10);
    end
    else
    begin
      Showmessage('Illegal Hex number $' + s);
      Paint;
      Exit;
    end;
  end;
  // the actual search
  if Idx > 0 then
  begin
    SetLength(sa, Idx);
    PL := Buffer;
    Inc(Pl, FromWhere);
    AbsOffset := FromWhere;
    repeat
      Offset := IndexByte(PL^, NumBytes - FromWhere, sa[0]);
      if Offset < 0 then
      begin
        ShowMessage('No more entries found.');
        Paint;
        Exit;
      end;
      // found!
      Inc(Pl, Offset + 1);
      AbsOffset := AbsOffset + LongWord(Offset + 1);
      Found := True;
      Pl2 := Pl;
      for j := 1 to High(sa) do
      begin
        if AbsOffset + 1 > NumBytes then
        begin
          Found := False;
          Break;
        end;
        if Pl2^ <> sa[j] then
          Found := False;
        if not Found then
          Break;
        Inc(Pl2);
      end;
      if Found then
      begin
        FUntilByte := Min(NumBytes, Int64(AbsOffset - 1) + Length(sa)) - 1;
        CurrentByte := AbsOffset - 1;
        Break;
      end;
    until False;
  end;
end;

procedure TFileViewer.SearchBinaryPosition(Posi: LongInt; Len: LongInt);
begin
  FUntilByte := Posi + Len - 1;
  CurrentByte := Posi;
end;

procedure TFileViewer.SearchTextPosition(Posi: LongInt; Len: LongInt);
var
  AbsPos, NextLine: NativeUInt;
  i: Integer;
begin
  {$HINTS OFF}
  AbsPos := PtrUInt(LineStarts[0]) + LongWord(Posi);
  for i := 0 to LineStarts.Count - 1 do
  begin
    NextLine := NativeUInt(LineStarts[i]);
    if NextLine > AbsPos then
    begin
      FromSel.Y := i - 1;
      FromSel.X := AbsPos - NativeUInt(LineStarts[i - 1]) + 1;
      ToSel.Y := i - 1;
      ToSel.X := FromSel.X + Len;
      if FStartLine < FromSel.Y then
        StartLine := FromSel.Y;
      if FStartLine + (ScreenHeight - 1) < ToSel.Y then
        StartLine := ToSel.Y - (ScreenHeight - 1);
      Paint;
      Break;
    end;
  end;
  {$HINTS ON}
end;

procedure TFileViewer.SearchText;
var
  SearchString: string;
begin
  FromSel := Point(-1,-1);
  ToSel := Point(-1,-1);
  SearchString := '';
  if AskForName('Search for text ', SearchString, False) then
  begin
    LastTextSearch := SearchString;
    FindText(SearchString, Point(0, 0));
  end;
  Paint;
end;

procedure TFileViewer.FindText(SearchString: string; FromWhere: TPoint);
var
  x, i, p: Integer;
  s: string;
  Found: Boolean;
begin
  FromSel := Point(-1,-1);
  ToSel := Point(-1,-1);
  Found := False;
  for i := FromWhere.Y to LineStarts.Count - 1 do
  begin
    s := GetTextLine(i);
    if i = FromWhere.Y then
      for x := 1 to FromWhere.X - 1 do
        s[x] := #0;
    p := Pos(SearchString, s);
    Found := p > 0;
    if Found then
    begin
      FromSel := Point(p, i);
      ToSel := Point(P + Length(SearchString), i);
      if FStartLine < FromSel.Y then
        StartLine := FromSel.Y;
      if FStartLine + (ScreenHeight - 1) < ToSel.Y then
        StartLine := ToSel.Y - (ScreenHeight - 1);
      Break;
    end;
  end;
  if not Found then
    ShowMessage('No more entries found.');
  Paint
end;

constructor TFileViewer.Create;
begin
  inherited;
  ShowMenu := DefShowMenu;
  MemLine := nil;
  LineStarts := TList.Create;
  Mode := vmText;
  NumBytesPerLine := 16;
  FUntilByte := 0;
  FromSel := Point(-1, -1);
  ToSel := Point(-1, -1);
end;

destructor TFileViewer.Destroy;
begin
  LineStarts.Free;
  FreeMem(Buffer);
  FreeMem(MemLine);
  inherited Destroy;
end;

function IsASCIIByte(a: Byte): Boolean; inline;
begin
  Result := a in [9, 10, 13, 26, 32..127, 169, 196..252];
end;

procedure TFileViewer.Execute(AFilename: string; ASearch: string = ''; Posi: LongInt = -1);
var
  Key: TKeyEvent;
  st: Byte;
  FS: TFileStream;
  Magic: array[0..255] of Byte;
  MaxMagic, i: Integer;
  IsASCII: Boolean;
begin
  MemLineLength := ScreenWidth + 10;
  MemLine := AllocMem(MemLineLength);
  //
  FileName := AFilename;
  LineStarts.Clear;
  Magic[0] := 0;
  FS := TFileStream.Create(FileName, fmOpenRead);
  MaxMagic := FS.Read(Magic[0], Length(Magic));
  FS.Position := 0;
  BufferSize := FS.Size;
  NumBytes := BufferSize;
  Buffer := AllocMem(BufferSize);
  if not Assigned(Buffer) then
    Exit;
  FS.Read(Buffer^, FS.Size);
  FS.Free;
  IsASCII := True;
  //
  if BufferSize > 1024*1024 then
  begin
    IsASCII := False
  end
  else
    for i := 0 to MaxMagic - 1 do
    begin
      if not IsASCIIByte(Magic[i]) then
      begin
        //writeln('not ASCII ', Magic[i]);
        IsASCII := False;
        Break;
      end;
    end;
  if IsASCII then
  begin
    FStartLine := 0;
    FormatText;
    Paint;
  end
  else
    SwitchMode;
  //
  // we got a search pattern
  if Posi >= 0 then
  begin
    if Mode = vmHex then
      SearchBinaryPosition(Posi, Length(ASearch))
    else
      SearchTextPosition(Posi, Length(ASearch));
  end;

  Terminated := False;
  repeat
    Key := PollNextKey;
    if Terminated then
      Break;
    if Key > 0 then
    begin
      st := GetKeyEventShiftState(Key);
      case (TranslateKeyEvent(Key) and $FFFF) of
        $2106: begin                                       // Ctrl + f -> toggle visibility of bottom menu
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
        end;
      end;
    end;
    Sleep(25);
  until (Key and $ff00) = $0100;
end;

end.

