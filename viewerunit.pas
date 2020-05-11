unit viewerunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video, Keyboard, Math,
  EventUnit;

const
  VLine = #179;

procedure FileViewer(AFileName: string);

implementation

uses
  FileListUnit, DialogUnit;

type
  TViewerMode = (vmText, vmHex);

  { TFileViewer }

  TFileViewer = class
  private
    Buffer: PByte;
    NumBytes: LongWord;
    Num4BytesPerLine: LongWord;
    NumBytesPerLine: LongWord;
    FCurrentByte: LongWord;
    FUntilByte: LongWord;
    Mode: TViewerMode;
    FileName: string;
    FStartLine: Integer;
    OrigText: TStringList;
    ShownText: TStringList;
    function PollNextKey: TKeyEvent;
    procedure FormatText;
    procedure Paint;
    procedure SetCurrentByte(AValue: LongWord);
    procedure SetStartLine(AValue: Integer);
    procedure SwitchMode;
    procedure GoToLineNumber;

  private
    LastBinSearch: string;
    procedure SearchBinary;
    procedure FindBinary(SearchString: string; FromWhere: Integer);
  private
    FromSel, ToSel: TPoint;
    LastTextSearch: string;
    procedure SearchText;
    procedure FindText(SearchString: string; FromWhere: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(AFilename: string);
    property StartLine: Integer read FStartLine write SetStartLine;
    property CurrentByte: LongWord read FCurrentByte write SetCurrentByte;
  end;

procedure FileViewer(AFileName: string);
begin
  With TFileViewer.Create do
  begin
    Execute(AFileName);
    Free;
  end;
end;

{ TFileViewer }

function TFileViewer.PollNextKey: TKeyEvent;
begin
  Result := GetNextKeyEvent;
  if Result = ResizeKey then
  begin
    FormatText;
    Paint;
  end;
end;

procedure ConvertChar(var c: Char); inline;
begin
  case c of
    #$C7: c := #128; // C
    #$FC: c := #129; // ue
    #$DC: c := #154; // UE
    #$E4: c := #132; // ae
    #$C4: c := #142; // AE
    #$F6: c := #148; // oe
    #$D6: c := #153; // OE
    #$DF: c := #225; // sz
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
  i, p: Integer;
  s: String;
begin
  ShownText.Clear;
  ShownText.Assign(OrigText);
  i := 0;
  while i < ShownText.Count do
  begin
    s := StringReplace(ShownText[i], #9, '  ', [rfReplaceAll]);
    ConvertText(s);
    if Length(s) > ScreenWidth then
    begin
      P := LastDelimiter(' ', Copy(s, 1, ScreenWidth));
      if p <= 1 then
        p := ScreenWidth;
      ShownText[i] := Copy(s, 1, p);
      Delete(s, 1, p);
      ShownText.Insert(i + 1, s);
    end
    else
      ShownText[i] := s;
    Inc(i);
  end;
end;

procedure TFileViewer.Paint;
var
  i, l,  cx, l1: Integer;
  j, StartByte, cb, OffsetForChars, EndOfDisplay: LongWord;
  s, s1, s2: String;
  pl: PByte;

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

  for i := 0 to ScreenWidth - 1 do
  begin
    SetChar(i, 0, ' ');
  end;

  SetText(0, 0, ExtractFileName(FileName));


  if Mode = vmText then
  begin
    s :=  IntToStr(StartLine + 1) + ' - ' + IntToStr(Min(ShownText.Count, (StartLine + 1) + (ScreenHeight - 2))) + '/' + IntToStr(ShownText.Count);
    SetText(ScreenWidth - Length(s), 0, s);

    BGPen := Blue;
    FGPen := White;
    for i := 0 to ScreenHeight - 2 do
    begin
      if i + StartLine < ShownText.Count then
      begin
        s := ShownText[i + StartLine];
        l := Length(s);
        if InRange(i + StartLine, FromSel.Y, ToSel.Y) then
        begin
          s1 := s;
          s2 := Copy(s1, 1, FromSel.X - 1);
          l1 := Length(s2);
          SetText(0, i + 1, s2);
          BGPen := Cyan;
          s2 := Copy(s1, FromSel.X, ToSel.X - FromSel.X);
          Delete(s1, 1, ToSel.X - 1);
          SetText(l1, i + 1, s2);
          l1 := l1 + Length(s2);
          BGPen := Blue;
          SetText(l1, i + 1, s1);
        end
        else
        begin
          SetText(0, i + 1, s);
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

    for i := 0 to ScreenHeight - 2 do
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
  UpdateScreen(False);
end;

procedure TFileViewer.SetCurrentByte(AValue: LongWord);
begin
  if FCurrentByte = AValue then Exit;
  FCurrentByte := EnsureRange(AValue, 0, NumBytes);
  while FCurrentByte >= (FStartLine + (ScreenHeight - 1)) * NumBytesPerLine do
  begin
    if StartLine >= NumBytes div NumBytesPerLine - 1 then
      Break;
    Inc(FStartLine);
  end;
  while FCurrentByte < FStartLine * NumBytesPerLine - 1 do
  begin
    if FStartLine = 0 then
      Break;
    Dec(FStartLine);
  end;
  Paint;
end;

procedure TFileViewer.SetStartLine(AValue: Integer);
begin
  if FStartLine = AValue then Exit;
  FStartLine := Max(AValue, 0);
  if Mode = vmText then
  begin
    FStartLine := Min(FStartLine, Max(ShownText.Count - ScreenHeight + 1, 0));
    Paint;
  end
  else
  if Mode = vmHex then
  begin
    FStartLine := Min(FStartLine, (NumBytes div NumBytesPerLine) - 1);
    Paint;
  end;
end;

procedure TFileViewer.SwitchMode;
var
  FS: TFileStream;
begin
  FStartLine := 0;
  case Mode of
    vmText: begin
      Mode := vmHex;
      ShownText.Clear;
      FS := TFileStream.Create(FileName, fmOpenRead);
      try
        If Assigned(Buffer) then
          FreeMem(Buffer);
        GetMem(Buffer, FS.Size);
        FS.Position := 0;
        NumBytes := FS.Size;
        FS.Read(Buffer^, FS.Size);
      finally
        FS.Free;
      end;
      Paint;
    end;
    vmHex: begin
      Mode := vmText;
      if Assigned(Buffer) then
        FreeMem(Buffer);
      Buffer := nil;
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
    if AskForNumber('Goto line (1 to ' + IntToStr(ShownText.Count)  +')', NewLine) then
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
      AbsOffset := AbsOffset + Offset + 1;
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
  for i := FromWhere.Y to ShownText.Count - 1 do
  begin
    s := ShownText[i];
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
  OrigText := TStringList.Create;
  ShownText := TStringList.Create;
  Mode := vmText;
  NumBytesPerLine := 16;
  FUntilByte := 0;
  FromSel := Point(-1, -1);
  ToSel := Point(-1, -1);
end;

destructor TFileViewer.Destroy;
begin
  OrigText.Clear;
  ShownText.Clear;
  if Assigned(Buffer) then
    FreeMem(BUffer);
  inherited Destroy;
end;

procedure TFileViewer.Execute(AFilename: string);
var
  Key: TKeyEvent;
  st: Byte;
begin
  FileName := AFilename;
  OrigText.Clear;
  OrigText.LoadFromFile(AFileName);
  FStartLine := 0;
  FormatText;
  Paint;
  //
  repeat
    Key := PollNextKey;
    if Key > 0 then
    begin
      st := GetKeyEventShiftState(Key);
      case (TranslateKeyEvent(Key) and $FFFF) of
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
        kbdPgUp, $39:begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := StartLine - 10;
          if Mode = vmHex then
            CurrentByte := Max(0, CurrentByte - Int64(ScreenHeight) * NumBytesPerLine);
        end;                                       // pg up -> Move around
        kbdPgDn, $33:  begin
          FUntilByte := 0;
          if Mode = vmText then
            StartLine := StartLine + 10;
          if Mode = vmHex then
            CurrentByte := Min(NumBytes - 1, CurrentByte + Int64(ScreenHeight) * NumBytesPerLine);
        end;                                      // pg down -> Move around
        kbdHome, $37: begin
          FUntilByte := 0;
          FCurrentByte := 0;
          StartLine := 0;                      // Home -> Move around
        end;
        kbdEnd, $31: begin
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

