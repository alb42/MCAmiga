unit searchunit;

{$mode objfpc}{$H+}

interface

uses
  AmigaDOS, Utility,
  Classes, SysUtils, FileListUnit, Video, KeyBoard, Mouse, dialogunit, Math, SyncObjs, contnrs;

procedure StartSearch(ASrcPanel: TFileList);

implementation

uses
  viewerunit, StrUtils;

type

  { TSearchClass }

  TSearchClass = class(TBaseDialog)
  protected
    ActiveBox: Integer;
    CurPos: Integer;
    Finished: Boolean;
    EndResult: TDialogResult;
    procedure ProcessMouse(MouseEvent: TMouseEvent); override;
    procedure TextInput(Key: TKeyEvent);
  protected
    procedure DrawButtons; override;
    procedure Paint; override;
  public
    Place: string;
    FilePattern: string;
    ContentsPattern: string;
    DoRecursive: Boolean;
    DoFileCaseSensitive: Boolean;
    DoContentCaseSensitive: Boolean;
    SrcPanel: TFileList;
    constructor Create; virtual;
    function Execute: TDialogResult; override;
  end;

  TSearchResult = class
    Name: string;
    Position: LongWord;
  end;

  { TSearchResults }

  TSearchResults = class
  strict private
    FResults: TObjectList;
    FNumResults: LongWord;
    ResLock: TCriticalSection;
    EntryLock: TCriticalSection;
    FCurEntry: string;
    function GetCurEntry: string;
    function GetItem(Idx: LongWord): TSearchResult;
    function GetNumFiles: LongWord;
    function GetNumResults: LongWord;
    procedure SetCurEntry(AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddResult(ANewSearchResult: TSearchResult);

    property NumFiles: LongWord read GetNumFiles;
    property Item[Idx: LongWord]: TSearchResult read GetItem;
    property CurEntry: string read GetCurEntry write SetCurEntry;
  end;


  { TSearchThread }

  TSearchThread = class(TThread)
  strict private
    SDir: TStringList;
    procedure SearchInDir(CurDir: string);
    function CheckForContents(Filename: string): Boolean;
  protected
    procedure Execute; override;
  public
    sc: TSearchClass;
    SResults: TSearchResults;
  end;

  { TSearchShowWin }

  TSearchShowWin = class(TShowMessage)
  private
    function GetSelectedName: string;
    function GetSelectedPos: LongInt;
  protected
    FTopEntry: LongInt;
    FSelected: LongInt;
    OutS: TStringList;
    ListOfResults: TObjectList;
    procedure Paint; override;
    procedure ConfigureButtons; override;
    procedure ProcessMouse(MouseEvent: TMouseEvent); override;
    procedure FormatOutput;
  public
    St: TSearchThread;
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute: TDialogResult; override;
    property SelectedName: string read GetSelectedName;
    property SelectedPos: LongInt read GetSelectedPos;
  end;


procedure StartSearch(ASrcPanel: TFileList);
var
  Res: TDialogResult;
  sc: TSearchClass;
  st: TSearchThread;
  sw: TSearchShowWin;
  sr: TSearchResults;
  s: string;
  Ret, p: LongInt;
begin
  sc := TSearchClass.Create;
  sc.SrcPanel := ASrcPanel;
  sc.Place := ASrcPanel.CurrentPath;
  repeat
    Res := sc.Execute;
    if Res = mrOK then
    begin
      sr := TSearchResults.Create;
      st := TSearchThread.Create(True);
      st.sc := sc;
      st.SResults := sr;
      st.Start;
      //
      Sw := TSearchShowWin.Create;
      sw.St := st;
      repeat
        Res := sw.Execute;
        s := sw.SelectedName;
        p := sw.SelectedPos;
        if s <> '' then
        case Res of
          mrOK: begin
            ASrcPanel.CurrentPath := ExtractFilePath(s);
            ASrcPanel.ActivateFile(ExtractFileName(s));
          end;
          mrView: begin
            if s[Length(s)] <> '/' then
            begin
              if ViewerLink = '' then
              begin
                FileViewer(s, sc.ContentsPattern, p);
                ASrcPanel.Update(False);
                ASrcPanel.OtherSide.Update(False);
              end
              else
              begin
                NonWaitMessage('Starting ' + ExtractFileName(ViewerLink));
                Ret := SystemTags(PChar(ViewerLink + ' "' + s + '"'), [TAG_DONE]);
                if Ret <> 0 then
                  ShowMessage(ViewerLink + ' returned with error message: ' + IntToStr(Ret));
              end;
            end;
          end;
          mrEdit:  begin
            if s[Length(s)] <> '/' then
            begin
              if EditLink <> '' then
              begin
                NonWaitMessage('Starting ' + ExtractFileName(EditLink));
                Ret := SystemTags(PChar(EditLink + ' "' + s + '"'), [TAG_DONE]);
                if Ret <> 0 then
                  ShowMessage(EditLink + ' returned with error message: ' + IntToStr(Ret));
              end;
            end;
          end;
          mrAgain: begin
            ASrcPanel.Update(False);
            ASrcPanel.OtherSide.Update(False);
          end
          else
            //
        end;
        //
      until Res in [mrAgain, mrOK, mrCancel];
      st.Terminate;
      st.WaitFor;
      st.Free;

      sr.Free;
      sw.Free;
    end;
  until Res <> mrAgain;
  sc.Free;
end;

{ TSearchShowWin }

function TSearchShowWin.GetSelectedName: string;
var
  Obj: TObject;
begin
  Result := '';
  if InRange(FSelected, 0, OutS.Count - 1) then
  begin
    Obj := OutS.Objects[FSelected];
    if Assigned(Obj) then
    begin
      Result := TSearchResult(Obj).Name;
    end
    else
      Result := OutS[FSelected];
  end;
end;

function TSearchShowWin.GetSelectedPos: LongInt;
var
  Obj: TObject;
begin
  Result := -1;
  if InRange(FSelected, 0, OutS.Count - 1) then
  begin
    Obj := OutS.Objects[FSelected];
    if Assigned(Obj) then
      Result := TSearchResult(Obj).Position;
  end;
end;

procedure TSearchShowWin.Paint;
var
  i, p: LongInt;
  s: string;
begin
  FGPen := Black;
  BGPen := LightGray;

  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;
  WindowRect := Rect(1, 1, ScreenWidth - 2, ScreenHeight - 2);

  DrawWindowBorder;

  FGPen := Black;
  BGPen := LightGray;

  p := 0;
  for i := FTopEntry to OutS.Count - 1 do
  begin
    if i = FSelected then
      BGPen := Cyan
    else
      BGPen := LightGray;
    //
    if InnerRect.Top + p > InnerRect.Bottom - 2 then
      Break;
    SetTextA(InnerRect.Left, InnerRect.Top + p, OutS[i]);
    Inc(p);
  end;
  BGPen := LightGray;
  if St.Terminated then
    s := 'Finished ' + IntToStr(ListOfResults.Count) + ' Hits'
  else
    s := 'Running ' + IntToStr(ListOfResults.Count) + ' Hits - inspect  ' + st.SResults.CurEntry;
  if Length(s) > InnerRect.Width - 2 then
    SetText(InnerRect.Left, InnerRect.Bottom, Copy(s, 1, InnerRect.Width - 2))
  else
    SetText(InnerRect.Left, InnerRect.Bottom, s);
  //
  if FTopEntry > 0 then
    SetChar(InnerRect.Right, InnerRect.Top, ArrowUp);
  if (FTopEntry + (InnerRect.Height - 2)) < OutS.Count then
    SetChar(InnerRect.Right, InnerRect.Bottom, ArrowDown);
  //
  DrawButtons;
  UpdateScreen(False);
end;

procedure TSearchShowWin.ConfigureButtons;
begin
  SetLength(ButtonsArray, 5);
  ButtonsArray[0].Rect := Rect(Mid.x - 24, WindowRect.Bottom, Mid.x - 18, WindowRect.Bottom + 1);
  ButtonsArray[0].Pressed := False;
  ButtonsArray[0].Title := 'Goto';
  ButtonsArray[0].Result := mrOK;

  ButtonsArray[1].Rect := Rect(Mid.x - 16, WindowRect.Bottom, Mid.x - 6, WindowRect.Bottom + 1);
  ButtonsArray[1].Pressed := False;
  ButtonsArray[1].Title := 'F3 - View';
  ButtonsArray[1].Result := mrView;

  ButtonsArray[2].Rect := Rect(Mid.x - 4, WindowRect.Bottom, Mid.x + 4, WindowRect.Bottom + 1);
  ButtonsArray[2].Pressed := False;
  ButtonsArray[2].Title := 'F4 - Edit';
  ButtonsArray[2].Result := mrEdit;

  ButtonsArray[3].Rect := Rect(Mid.x + 8, WindowRect.Bottom, Mid.x + 16, WindowRect.Bottom + 1);
  ButtonsArray[3].Pressed := False;
  ButtonsArray[3].Title := 'Again';
  ButtonsArray[3].Result := mrAgain;

  ButtonsArray[4].Rect := Rect(Mid.x + 18, WindowRect.Bottom, Mid.x + 24, WindowRect.Bottom + 1);
  ButtonsArray[4].Pressed := False;
  ButtonsArray[4].Title := 'Cancel';
  ButtonsArray[4].Result := mrCancel;
end;

procedure TSearchShowWin.ProcessMouse(MouseEvent: TMouseEvent);
var
  Idx: LongInt;
begin
  inherited ProcessMouse(MouseEvent);
  if (MouseEvent.Action = MouseActionDown) and (MouseEvent.buttons = MouseLeftButton) then
  begin
    if InRange(MouseEvent.y, InnerRect.Top, InnerRect.Bottom - 2) and (InRange(MouseEvent.X, InnerRect.Left, InnerRect.Right - 1)) then
    begin
      Idx := (MouseEvent.y - InnerRect.Top) + FTopEntry;
      if InRange(Idx, 0, OutS.Count - 1) then
      begin
        FSelected := Idx;
        Paint;
      end;
    end;
  end;
end;

procedure TSearchShowWin.FormatOutput;
var
  s, LastPath, Path: string;
  i: LongInt;
begin
  OutS.Clear;
  LastPath := '';
  for i := 0 to ListOfResults.Count - 1 do
  begin
    s := TSearchResult(ListOfResults[i]).Name;
    Path := ExtractFilePath(s);
    if Path <> LastPath then
    begin
      LastPath := Path;
      OutS.AddObject(LastPath, nil);
    end;
    if St.sc.ContentsPattern <> '' then
      OutS.AddObject('  ' + ExtractFileName(s) + ':' + IntToStr(TSearchResult(ListOfResults[i]).Position), ListOfResults[i])
    else
      OutS.AddObject('  ' + ExtractFileName(s), ListOfResults[i]);
  end;
end;

constructor TSearchShowWin.Create;
begin
  inherited;
  ListOfResults := TObjectList.Create(False);
  OutS := TStringList.Create;
end;

destructor TSearchShowWin.Destroy;
begin
  ListOfResults.Free;
  OutS.Free;
  inherited Destroy;
end;

function TSearchShowWin.Execute: TDialogResult;
var
  Key: TKeyEvent;
  t1: LongWord;
  LastCall, Num: LongWord;
  i: LongInt;
  Running: Boolean;
begin
  Result := mrNone;
  Paint;
  LastCall := 0;
  Running := True;
  repeat
    Key := PollNextKey;
    for i := 0 to High(ButtonsArray) do
    begin
      if ButtonsArray[i].Pressed then
      begin
        Result := ButtonsArray[i].Result;
        Exit;
      end;
    end;
    {$WARNINGS OFF}
    t1 := GetTickCount;
    {$WARNINGS ON}
    case (TranslateKeyEvent(Key) and $FFFF) of
      $1C0D, $000D: begin
        Result := ButtonsArray[SelectedButton].Result;
        Exit;
      end;
      $011B: begin
        Result := mrCancel;
        Exit;
      end;
      kbdLeft, $34: begin    // cursor left
        SelectedButton := Max(SelectedButton - 1, 0);
        DrawButtons;
      end;
      kbdRight, $36: begin // cursor Right
        SelectedButton := Min(SelectedButton + 1, High(ButtonsArray));
        DrawButtons;
      end;
      kbdDown, $32: begin
        FSelected := Min(FSelected + 1, OutS.Count - 1);
        FTopEntry := Max(FSelected, FTopEntry + (InnerRect.Height - 2)) - (InnerRect.Height - 2);
        Paint;
      end;
      kbdUp, $38: begin
        FSelected := Max(FSelected - 1, 0);
        FTopEntry := Min(FSelected, FTopEntry);
        Paint;
      end;
      kbdPgDn, $33, $9100: begin
        FSelected := Min(FSelected + 10, OutS.Count - 1);
        FTopEntry := Max(FSelected, FTopEntry + (InnerRect.Height - 2)) - (InnerRect.Height - 2);
        Paint;
      end;
      kbdPgUp, $39, $8D00: begin
        FSelected := Max(FSelected - 10, 0);
        FTopEntry := Min(FSelected, FTopEntry);
        Paint;
      end;
      kbdF3: begin
        Result := mrView;
        Exit;
      end;
      kbdF4: begin
        Result := mrEdit;
        Exit;
      end;

    end;
    //
    if t1 - LastCall > 100 then
    begin
      // redraw
      Num := St.SResults.NumFiles;
      if Num <> LongWord(ListOfResults.Count) then
      begin
        for i := ListOfResults.Count to Num - 1 do
          ListOfResults.Add(St.SResults.Item[i]);
        FormatOutput;
        Paint;
      end
      else
      begin
        if Running or not st.Terminated then
        begin
          Running := not st.Terminated;
          Paint;
        end;
      end;
      LastCall := t1;
    end;
    Sleep(25);
  until False;
end;

{ TSearchThread }

procedure TSearchThread.SearchInDir(CurDir: string);
var
  Info: TSearchRec;
  sr: TSearchResult;
  IsValid: Boolean;
begin
  CurDir := IncludeTrailingPathDelimiter(CurDir);
  SResults.CurEntry := CurDir;
  //sysdebugln(CurDir);
  if FindFirst(CurDir + sc.FilePattern, faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      if Terminated then
      begin
        FindClose(Info);
        Exit;
      end;
      Sr := nil;
      IsValid := IsWild(Info.Name, sc.FilePattern, not sc.DoFileCaseSensitive);
      if IsValid then
      begin
        if (Info.Attr and faDirectory) <> 0 then
        begin
          if sc.ContentsPattern = '' then
          begin
            Sr := TSearchResult.Create;
            Sr.Name := IncludeTrailingPathDelimiter(CurDir + Info.Name);
            SResults.AddResult(Sr);
          end;
        end
        else
        begin
          Sr := TSearchResult.Create;
          Sr.Name := CurDir + Info.Name;
          if sc.ContentsPattern <> '' then
          begin
            SResults.CurEntry := CurDir + Info.Name;
            // find inside the file
            IsValid := CheckForContents(Sr.Name);
          end
          else
            IsValid := True;
          if IsValid then
            SResults.AddResult(Sr)
          else
            Sr.Free;
        end;
      end;
    until FindNext(info) <> 0;
    FindClose(Info);
  end;
  if sc.DoRecursive then
  begin
    if FindFirst(CurDir + '*', faDirectory, Info) = 0 then
    begin
      repeat
        if Terminated then
        begin
          FindClose(Info);
          Exit;
        end;
        if (Info.Attr and faDirectory) <> 0 then
        begin
          SDir.Add(CurDir + Info.Name)
        end;
      until FindNext(info) <> 0;
      FindClose(Info);
    end;
  end;
end;

function TSearchThread.CheckForContents(Filename: string): Boolean;
var
  Buffer: PByte;
  FS: TFileStream;
  Size: LongWord;
  i: Integer;
  CmpBuffer: PChar;
  TestBuffer: PChar;
  Len: LongWord;
  Pat: string;
  Sr: TSearchResult;
begin
  Result := False;
  Buffer := nil;
  Pat := sc.ContentsPattern;
  if not sc.DoContentCaseSensitive then
    Pat := LowerCase(Pat);
  Len := Length(Pat);
  CmpBuffer := AllocMem(Len + 1);
  TestBuffer := AllocMem(Len + 1);
  Move(Pat[1], CmpBuffer^, Len);
  //Writeln('comparebuffer: ' + CmpBuffer);
  FS := TFileStream.Create(Filename, fmOpenRead);
  try
    Size := FS.Size;
    if Size < Len then
      Exit;
    GetMem(Buffer, Size);
    FS.Read(Buffer^, Size);
    if not sc.DoContentCaseSensitive then
    begin
      for i := 0 to Size - 1 do
      begin
        if Terminated then
          Exit;
        Buffer[i] := Byte(LowerCase(Char(Buffer[i])));
      end;
    end;
    for i := 0 to (Size - Len) - 1 do
    begin
      if Terminated then
        Exit;
      Move(Buffer[i], TestBuffer^, Len);
      //Writeln(i, 'TestBuffer: ' + TestBuffer);
      if CompareMem(TestBuffer, CmpBuffer, Len) then
      begin
        Sr := TSearchResult.Create;
        Sr.Name := FileName;
        Sr.Position := i;
        SResults.AddResult(Sr);
        //writeln('add ', Filename);
      end;
    end;
  finally
    FS.Free;
    FreeMem(Buffer);
    FreeMem(CmpBuffer);
    FreeMem(TestBuffer);
  end;
end;

procedure TSearchThread.Execute;
var
  s: string;
begin
  SDir := TStringList.Create;
  try
    SDir.Add(sc.Place);
    while SDir.Count > 0 do
    begin
      s := SDir[0];
      SDir.Delete(0);
      SearchInDir(s);
      if Terminated then
        Break;
    end;
  except
    //
  end;
  Terminate;
  SDir.Free;
end;

{ TSearchResults }

function TSearchResults.GetNumResults: LongWord;
begin
  Result := InterlockedExchange(FNumResults, FNumResults);
end;

procedure TSearchResults.SetCurEntry(AValue: string);
begin
  EntryLock.Enter;
  FCurEntry := AValue;
  EntryLock.Leave;
end;

function TSearchResults.GetNumFiles: LongWord;
begin
  ResLock.Enter;
  Result := FResults.Count;
  ResLock.Leave;
end;

function TSearchResults.GetItem(Idx: LongWord): TSearchResult;
begin
  Result := nil;
  ResLock.Enter;
  if Idx < LongWord(FResults.Count) then
    Result := TSearchResult(FResults[Idx]);
  ResLock.Leave;
end;

function TSearchResults.GetCurEntry: string;
begin
  EntryLock.Enter;
  Result := FCurEntry;
  EntryLock.Leave;
end;

constructor TSearchResults.Create;
begin
  FResults := TObjectList.Create(True);
  FNumResults := 0;
  ResLock := TCriticalSection.Create;
  EntryLock := TCriticalSection.Create;
end;

destructor TSearchResults.Destroy;
begin
  ResLock.Free;
  EntryLock.Free;
  FResults.Free;
  inherited Destroy;
end;

procedure TSearchResults.AddResult(ANewSearchResult: TSearchResult);
begin
  //
  ResLock.Enter;
  FResults.Add(ANewSearchResult);
  InterlockedExchangeAdd(FNumResults, 1);
  ResLock.Leave;
end;

{ TSearchClass }

procedure TSearchClass.ProcessMouse(MouseEvent: TMouseEvent);
begin
  if (MouseEvent.Action = MouseActionDown) and (MouseEvent.buttons = MouseLeftButton) then
  begin
    // start at
    if (MouseEvent.Y = InnerRect.Top + 1) and InRange(MouseEvent.x, InnerRect.Left, InnerRect.Right) then
    begin
      ActiveBox := 0;
      CurPos := MouseEvent.X - InnerRect.Left;
      Paint;
      Exit;
    end;
    // File pattern
    if (MouseEvent.Y = InnerRect.Top + 3) and InRange(MouseEvent.x, InnerRect.Left, Mid.X - 1) then
    begin
      ActiveBox := 1;
      CurPos := MouseEvent.X - InnerRect.Left;
      Paint;
      Exit;
    end;
    // Content pattern
    if (MouseEvent.Y = InnerRect.Top + 3) and InRange(MouseEvent.x, Mid.X + 1, InnerRect.Right) then
    begin
      ActiveBox := 2;
      CurPos := MouseEvent.X - Mid.X - 1;
      Paint;
      Exit;
    end;
    // Recursive
    if (MouseEvent.Y = InnerRect.Top + 4) and InRange(MouseEvent.x, InnerRect.Left, InnerRect.Left + 3) then
    begin
      ActiveBox := 3;
      DoRecursive := not DoRecursive;
      Paint;
      Exit;
    end;
    // File case sensitive
    if (MouseEvent.Y = InnerRect.Top + 5) and InRange(MouseEvent.x, InnerRect.Left, InnerRect.Left + 3) then
    begin
      ActiveBox := 4;
      DoFileCaseSensitive := not DoFileCaseSensitive;
      Paint;
      Exit;
    end;
    // Contents case sensitive
    if (MouseEvent.Y = InnerRect.Top + 4) and InRange(MouseEvent.x, Mid.X + 1, Mid.X + 4) then
    begin
      ActiveBox := 5;
      DoContentCaseSensitive := not DoContentCaseSensitive;
      Paint;
      Exit;
    end;

    // Search Button
    if (MouseEvent.Y = WindowRect.Bottom) and InRange(MouseEvent.x, Mid.X - (Length('Search') + 3), Mid.X - 1) then
    begin
      if ActiveBox = 6 then
      begin
        EndResult := mrOK;
        Exit;
      end
      else
      begin
        ActiveBox := 6;
        Paint;
        Exit;
      end;
    end;
    // Cancel Button
    if (MouseEvent.Y = WindowRect.Bottom) and InRange(MouseEvent.x, Mid.X + 2, Mid.X + 10) then
    begin
      if ActiveBox = 7 then
      begin
        EndResult := mrCancel;
        Exit;
      end
      else
      begin
        ActiveBox := 7;
        Paint;
        Exit;
      end;
    end;
  end;
end;

procedure TSearchClass.TextInput(Key: TKeyEvent);
var
  c: Char;
  s: string;
  DoPaint: Boolean;
begin
  DoPaint := False;
  case ActiveBox of
    0: s := Place;
    1: s := FilePattern;
    2: s := ContentsPattern;
    else
      Exit;
  end;
  c := GetKeyEventChar(Key);
  case c of
    #32..#126, #$C7, #$FC, #$DC, #$E4, #$C4, #$F6, #$D6, #$DF: begin
      if (CurPos >= 0) and (Length(s) < 60) then
      begin
        Inc(CurPos);
        Insert(c, s, CurPos);
        DoPaint := True;
      end;
    end;
    #8: begin
      if CurPos > 0 then
      begin
        Delete(s, CurPos, 1);
        Dec(CurPos);
        DoPaint := True
      end;
    end;
  end;
  if DoPaint then
  begin
    case ActiveBox of
      0: Place := s;
      1: FilePattern := s;
      2: ContentsPattern := s;
      else
        Exit;
    end;
    Paint;
  end;
end;

procedure TSearchClass.DrawButtons;
begin

end;

procedure TSearchClass.Paint;
var
  i: LongInt;
begin
  inherited Paint;

  WindowRect.Left := 2;
  WindowRect.Top := Max(2, Mid.Y - 5);
  WindowRect.Right := ScreenWidth - 3;
  WindowRect.Bottom := Min(ScreenHeight - 2, Mid.Y + 5);


  BGPen := LightGray;
  FGPen := Black;

  DrawWindowBorder;

  // Start at
  BGPen := LightGray;
  FGPen := Black;
  SetText(InnerRect.Left, InnerRect.Top, 'Start at:');
  BGPen := Cyan;
  FGPen := Black;
  //
  for i := InnerRect.Left to InnerRect.Right do
    SetChar(i, InnerRect.Top + 1, ' ');
  SetTextA(InnerRect.Left, InnerRect.Top + 1, Place);

  if ActiveBox = 0 then
  begin
    CurPos := Min(CurPos, Length(Place));
    SetCursorPos(InnerRect.Left + CurPos, InnerRect.Top + 1);
  end;


  // File Pattern
  BGPen := LightGray;
  FGPen := Black;
  SetText(InnerRect.Left, InnerRect.Top + 2, 'File pattern:');
  BGPen := Cyan;
  FGPen := Black;
  //
  for i := InnerRect.Left to Mid.x - 1 do
    SetChar(i, InnerRect.Top + 3, ' ');
  SetTextA(InnerRect.Left, InnerRect.Top + 3, FilePattern);

  if ActiveBox = 1 then
  begin
    CurPos := Min(CurPos, Length(FilePattern));
    SetCursorPos(InnerRect.Left + CurPos, InnerRect.Top + 3);
  end;

  // File Content
  BGPen := LightGray;
  FGPen := Black;
  SetText(Mid.X + 1, InnerRect.Top + 2, 'File Content:');
  BGPen := Cyan;
  FGPen := Black;
  //
  for i := Mid.X + 1 to InnerRect.Right do
    SetChar(i, InnerRect.Top + 3, ' ');
  SetTextA(Mid.X + 1, InnerRect.Top + 3, ContentsPattern);

  if ActiveBox = 2 then
  begin
    CurPos := Min(CurPos, Length(ContentsPattern));
    SetCursorPos(Mid.X + 1 + CurPos, InnerRect.Top + 3);
  end;

  // File recursive
  BGPen := LightGray;
  FGPen := Black;
  SetText(InnerRect.Left, InnerRect.Top + 4, '[ ] Recursive');
  BGPen := Cyan;
  if DoRecursive then
    SetChar(InnerRect.Left + 1, InnerRect.Top + 4, 'X')
  else
    SetChar(InnerRect.Left + 1, InnerRect.Top + 4, ' ');


  if ActiveBox = 3 then
    SetCursorPos(InnerRect.Left + 1, InnerRect.Top + 4);

  // File case sensitive
  BGPen := LightGray;
  FGPen := Black;
  SetText(InnerRect.Left, InnerRect.Top + 5, '[ ] Case sensitive');
  BGPen := Cyan;
  if DoFileCaseSensitive then
    SetChar(InnerRect.Left + 1, InnerRect.Top + 5, 'X')
  else
    SetChar(InnerRect.Left + 1, InnerRect.Top + 5, ' ');

  if ActiveBox = 4 then
    SetCursorPos(InnerRect.Left + 1, InnerRect.Top + 5);

  // content case sensitive
  BGPen := LightGray;
  FGPen := Black;
  SetText(Mid.X + 1, InnerRect.Top + 4, '[ ] Case sensitive');
  BGPen := Cyan;
  if DoContentCaseSensitive then
    SetChar(Mid.X + 2, InnerRect.Top + 4, 'X')
  else
    SetChar(Mid.X + 2, InnerRect.Top + 4, ' ');

  if ActiveBox = 5 then
    SetCursorPos(Mid.X + 2, InnerRect.Top + 4);


  // Search Button
  FGPen := Black;
  if ActiveBox = 6 then
    BGPen := Cyan
  else
    BGPen := LightGray;

  SetText(Mid.X - (Length('Search') + 3), WindowRect.Bottom, LBorder + 'Search' + RBorder);

  FGPen := Black;
  if ActiveBox = 7 then
    BGPen := Cyan
  else
    BGPen := LightGray;
  SetText(Mid.X + 2, WindowRect.Bottom, LBorder + 'Cancel' + RBorder);


  UpdateScreen(False);
end;

constructor TSearchClass.Create;
begin
  Place := '';
  FilePattern := '*';
  ContentsPattern := '';
  DoRecursive := True ;
  DoFileCaseSensitive := False;
  DoContentCaseSensitive := False;
  SrcPanel := nil;
end;

function TSearchClass.Execute: TDialogResult;
var
  Key: TKeyEvent;
  //i: Integer;
begin
  EndResult := mrNone;
  Result := mrOK;
  ActiveBox := 1;
  CurPos := Length(FilePattern);
  SetCursorType(crUnderline);
  Paint;
  try
    repeat
      Key := PollNextKey;
      if EndResult <> mrNone then
      begin
        Result := EndResult;
        Exit;
      end;
      case (TranslateKeyEvent(Key) and $FFFF) of
        $1C0D, $000D: begin
          if ActiveBox = 7 then
            Result := mrCancel;
          Break;
        end;
        $0F09,$0F00: begin
          if (TranslateKeyEvent(Key) and $FFFF) = $0F09 then
            ActiveBox := ActiveBox + 1
          else
            ActiveBox := ActiveBox - 1;
          if ActiveBox > 7 then
            ActiveBox := 0;
          if ActiveBox < 0 then
            ActiveBox := 7;
          case ActiveBox of
            0: CurPos := Length(Place);
            1: CurPos := Length(FilePattern);
            2: CurPos := LEngth(ContentsPattern);
          end;
          Paint;
        end;
        kbdLeft: begin    // cursor left
          if CurPos > 0 then
          begin
            Dec(CurPos);
            Paint;
          end;
          //SelectedButton := Max(SelectedButton - 1, 0);
          DrawButtons;
        end;
        kbdRight: begin    // cursor right
          Inc(CurPos);
          Paint;
          DrawButtons;
        end;
        $011B: begin
          Result := mrCancel;
          Break;
        end;
        else
          if Key <> 0 then
          begin
          case ActiveBox of
            0,1,2: TextInput(Key);
            3: begin
              if (Key and $FFFF) = $20 then
               begin
                 DoRecursive := not DoRecursive;
                 Paint;
              end;

            end;
            4: if Key = $20 then
               begin
                 DoFileCaseSensitive := not DoFileCaseSensitive;
                 Paint;
              end;
            5: if Key = $20 then
               begin
                 DoContentCaseSensitive := not DoContentCaseSensitive;
                 Paint;
              end;
          end;
        end;
      end;
      Sleep(25);
    until False;
  finally
    SetCursorType(crHidden);
  end;
end;

end.

