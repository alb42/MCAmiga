unit FileListUnit;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef HASAMIGA}
  AmigaDOS, Exec,
  {$endif}
  Video, Keyboard, Classes, SysUtils, Math, fgl, StrUtils;

const
  UpperLeftEdge = #201;
  UpperRightEdge = #187;
  LowerRightEdge = #188;
  LowerLeftEdge = #200;
  VertLine = #186;
  HoriLine = #205;
  SingleLine = #196;

  LeftEdge = #181;
  RightEdge = #198;

  LeftDivisor = #199;
  RightDivisor = #182;

var
  FGPen: Word = LightGray;
  BGPen: Word = Blue;
  LeftPath: string;
  RightPath: string;


type
  TEntryType = (etParent, etDir, etFile, etDrive, etAssign);

  { TListEntry }

  TListEntry = class
    Name: string;
    EType: TEntryType;
    Size: Int64;
    Selected: Boolean;
  public
    constructor Create; virtual;

    procedure Assign(Src: TListEntry); virtual;
  end;

  TEntryObjectList = specialize TFPGObjectList<TListEntry>;

  { TEntryList }

  TEntryList = class(TEntryObjectList)
  public
    procedure AddFile(AName: string; Size: Int64);
    procedure AddDir(AName: string);
    procedure AddCopy(AEntry: TListEntry);
  end;


  { TFileList }

  TFileList = class
  private
    FRect: TRect;
    FInnerRect: TRect;
    FCurrentPath: string;
    FFileList: TEntryList;
    FIsActive: Boolean;
    FActiveElement: Integer;
    FTopElement: Integer;
    FBottomElement: Integer;
    ActShowStart: LongInt;
    function GetActiveEntry: TListEntry;
    procedure SetActiveElement(AValue: Integer);
    procedure SetCurrentPath(AValue: string);
    procedure DrawBorder;
    procedure DrawContents(UpdateList: Boolean);
    procedure SetIsActive(AValue: Boolean);
    procedure DrawActive(NActive: Integer);
    procedure DrawEntry(Idx: Integer);

    procedure DoListOfSelectedFile(Recursive: Boolean; FL: TEntryList; out Dirs: integer; out Files: Integer; out Size: Int64);

    procedure SortList;
    procedure CheckSelected;
  public
    constructor Create(ARect: TRect); virtual;
    destructor Destroy; override;

    procedure Resize(ARect: TRect);

    procedure Update(UpdateList: Boolean);
    procedure GoToParent;
    procedure EnterPressed;
    function ResultOfEntry(out NewPath: string): Boolean;

    procedure CopyFiles(Target: string); // F5
    procedure MoveFiles(Target: string); // F6
    procedure MakeDir(); // F7
    procedure DeleteSelected(); // F8
    procedure Rename(); // Shift F6

    procedure ActivateFile(AName: string);
    procedure SelectActiveEntry;

    procedure ScanSize;

    procedure IdleEvent;
    procedure SearchList;

    procedure ViewFile(OpenWithProgram: string = '');
    procedure EditFile(OpenWithProgram: string = '');

    procedure SelectByPattern(DoSelect: Boolean);

    property CurrentPath: string read FCurrentPath write SetCurrentPath;
    property IsActive: Boolean read FIsActive write SetIsActive;
    property ActiveElement: Integer read FActiveElement write SetActiveElement;
    property ActiveEntry: TListEntry read GetActiveEntry;
  end;


  function PosToArray(px, py: Integer): Integer; inline;
  procedure SetChar(p: Integer; c: Char); overload;
  procedure SetChar(x,y: Integer; c: Char); overload;
  procedure SetText(x,y: Integer; s: string);
  function LimitName(AName: string; MaxLength: Integer; PathMode: Boolean = False): string;

implementation

uses
  DialogUnit, EventUnit, ViewerUnit;

function LimitName(AName: string; MaxLength: Integer; PathMode: Boolean = False): string;
var
  SL: TStringList;
  s1, s2, s3: string;
  i: Integer;
begin
  Result := AName;
  if Length(AName) <= MaxLength then
    Exit;
  if Pathmode then
  begin
    SL := TStringList.Create;
    ExtractStrings([':', PathDelim], [], PChar(AName), SL);
    if SL.Count > 0 then
    begin
      s1 := SL[0] + ':';
    end;
    s2 := '';
    for i := SL.Count - 1 downto 1 do
    begin
      s3 := IncludeLeadingPathDelimiter(SL[i]) + s2;
      if Length(s3) + 3 + Length(s1) > MaxLength then
        Break;
      s2 := s3;
    end;
    Result := s1 + '...' + s2;
    if Length(Result) > MaxLength then
    begin
      Result := '...' + s2;
      if Length(Result) > MaxLength then
        Result := '...' + Copy(s2, Length(s2) - (MaxLength - 3), MaxLength - 2);
    end
  end
  else
    Result := '...' + Copy(AName, Length(AName) - (MaxLength - 3), MaxLength - 2);
end;

function FormatSize(Size: Single): string;
begin
  Size := Abs(Size);
  if Size < 1000 then
    Result := Format('%6.0f  ', [Size])
  else
  begin
    if Size < 1e6 then
      Result := Format('%6.2f k', [Size / 1000])
    else
      if Size < 1e9 then
        Result := Format('%6.2f M', [(Size / 1000) / 1000])
      else
        Result := Format('%6.2f G', [((Size / 1000) / 1000) / 1000])
  end;
end;

function PosToArray(px, py: Integer): Integer; inline;
begin
  PosToArray := px + py * ScreenWidth;
end;

procedure SetChar(p: Integer; c: Char); overload;
begin
  if (p >= 0) and (p < VideoBufSize) then
    VideoBuf^[p] := (BGPen shl 12) or (FGPen shl 8) or Byte(c);
end;

procedure SetChar(x,y: Integer; c: Char); overload;
begin
  SetChar(PosToArray(x,y), c);
end;

procedure SetText(x,y: Integer; s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    SetChar(x + i - 1, y, s[i]);
end;

{ TEntryList }

procedure TEntryList.AddFile(AName: string; Size: Int64);
var
  NEntry: TListEntry;
begin
  NEntry := TListEntry.Create;
  NEntry.Name := AName;
  NEntry.EType := etFile;
  NEntry.Size := Size;
  Self.Add(NEntry);
end;

procedure TEntryList.AddDir(AName: string);
var
  NEntry: TListEntry;
begin
  NEntry := TListEntry.Create;
  NEntry.Name := AName;
  NEntry.EType := etDir;
  Self.Add(NEntry);
end;

procedure TEntryList.AddCopy(AEntry: TListEntry);
var
  NEntry: TListEntry;
begin
  NEntry := TListEntry.Create;
  NEntry.Assign(AEntry);
  Self.Add(NEntry);
end;

{ TListEntry }

constructor TListEntry.Create;
begin
  Selected := False;
  Size := -1;
end;

procedure TListEntry.Assign(Src: TListEntry);
begin
  Self.Name := Src.Name;
  Self.EType := Src.EType;
  Self.Selected := False;
  Self.Size := Src.Size;
end;

{ TFileList }

procedure TFileList.SetCurrentPath(AValue: string);
begin
  if FCurrentPath = AValue then
    Exit;
  FCurrentPath := ExcludeTrailingPathDelimiter(AValue);
  Update(True);
end;

procedure TFileList.SetActiveElement(AValue: Integer);
begin
  if FActiveElement = AValue then
    Exit;
  ActShowStart := 0;
  DrawActive(AValue);
end;

function TFileList.GetActiveEntry: TListEntry;
begin
  Result := nil;
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
    Result := FFileList[FActiveElement];
end;

procedure TFileList.DrawBorder;
var
  i: Integer;
  s: String;
begin
  FGPen := LightGray;
  BGPen := Blue;
  SetChar(FRect.Left, FRect.Top, UpperLeftEdge);
  SetChar(FRect.Left, FRect.Bottom, LowerLeftEdge);
  SetChar(FRect.Right, FRect.Top, UpperRightEdge);
  SetChar(FRect.Right, FRect.Bottom, LowerRightEdge);
  for i := FRect.Left + 1 to FRect.Right - 1 do
  begin
    SetChar(i, FRect.Top, HoriLine);
    SetChar(i, FRect.Bottom, HoriLine);
    SetChar(i, FRect.Bottom - 2, SingleLine);
  end;
  for i := FRect.Top + 1 to FRect.Bottom - 1 do
  begin
    if i = FRect.Bottom - 2 then
    begin
      SetChar(FRect.Left, i, LeftDivisor);
      SetChar(FRect.Right, i, RightDivisor);
    end
    else
    begin
      SetChar(FRect.Left, i, VertLine);
      SetChar(FRect.Right, i, VertLine);
    end;
  end;
  s := LimitName(FCurrentPath, FRect.Width - 5, True);
  if IsActive then
  begin
    SetText(FRect.Left + 2, FRect.Top, LeftEdge);
    SetText(FRect.Left + 3 + Length(s) , FRect.Top, RightEdge);
    FGPen := Blue;
    BGPen := LightGray;
    SetText(FRect.Left + 3, FRect.Top, s);
  end
  else
    SetText(FRect.Left + 2, FRect.Top, LeftEdge + s + RightEdge);
  CheckSelected;
end;

{$ifdef AmigaOS4}
const
  DLT_DIRECTORY = DLT_LOCK;
{$endif}

procedure TFileList.DrawContents(UpdateList: Boolean);
var
  Info: TSearchRec;
  i: Integer;
  NEntry: TListEntry;
  {$ifdef HASAMIGA}
  dl: PDosList;
  {$endif}
begin
  FGPen := LightGray;
  BGPen := Blue;
  // updating contents
  if UpdateList then
  begin
    FFileList.Clear;
    if FCurrentPath = '' then
    begin
      {$ifdef HASAMIGA}
      dl := LockDosList(LDF_VOLUMES or LDF_ASSIGNS or LDF_READ);
      //
      dl := NextDosEntry(dl, LDF_VOLUMES or LDF_ASSIGNS);
      while Assigned(dl) do
      begin
        NEntry := TListEntry.Create;
        {$ifdef AROS}
        NEntry.Name := string(PChar(dl^.dol_Name)) + ':';
        {$else}
        NEntry.Name := string(PChar(BADDR(dl^.dol_Name)) + 1) + ':';
        {$endif}
        if (dl^.dol_Type and DLT_DIRECTORY) <> 0 then
          NEntry.EType := etAssign
        else
          NEntry.EType := etDrive;
        FFileList.Add(NEntry);
        dl := NextDosEntry(dl, LDF_VOLUMES or LDF_ASSIGNS);
      end;
      UnlockDosList(LDF_VOLUMES or LDF_ASSIGNS or LDF_READ);
      {$endif}
    end
    else
    begin
      {$ifdef HASAMIGA}
      NEntry := TListEntry.Create;
      NEntry.Name := '/';
      NEntry.EType := etParent;
      FFileList.Add(NEntry);
      {$endif}
      if FindFirst(IncludeTrailingPathDelimiter(FCurrentPath) + '*', faAnyFile and faDirectory, Info) = 0 then
      begin
        repeat
          NEntry := TListEntry.Create;
          if (Info.Attr and faDirectory) <> 0 then
          begin
            NEntry.Name := IncludeTrailingPathDelimiter(Info.Name);
            NEntry.EType := etDir;
          end
          else
          begin
            NEntry.Name := Info.Name;
            NEntry.EType := etFile;
            NEntry.Size := Info.Size;
          end;
          FFileList.Add(NEntry);
        until FindNext(info) <> 0;
        FindClose(Info);
      end;
    end;
    SortList;

    if (FTopElement < 0) or (FTopElement > FFileList.Count - 1) then
      FTopElement := 0
    else
    begin
      if FFileList.Count - 1 - FTopElement <= FInnerRect.Height then
        FTopElement := 0;
    end;
    FBottomElement := Min(FFileList.Count - 1, FInnerRect.Height);
    if FActiveElement >= FFileList.Count then
      FActiveElement := 0;
  end;
  for i := 0 to FInnerRect.Height do
  begin
    DrawEntry(FTopElement + i);
  end;
end;

procedure TFileList.SetIsActive(AValue: Boolean);
begin
  if FIsActive = AValue then Exit;
    FIsActive := AValue;
  DrawBorder();
  DrawActive(FActiveElement);
  UpdateScreen(False);
end;


procedure TFileList.DrawActive(NActive: Integer);
var
  OldActive, i, l, n: Integer;
  s: string;
begin
  OldActive := FActiveElement;
  FActiveElement:= EnsureRange(NActive, 0, FFileList.Count - 1);
  if FActiveElement < FTopElement then
  begin
    while FActiveElement < FTopElement do
    begin
      FTopElement := Max(0, FTopElement - 5);
      FBottomElement := Min(FBottomElement + 5, Min(FFileList.Count - 1, FTopElement + FInnerRect.Height));
    end;
    for i := FTopElement to FBottomElement do
      DrawEntry(i);
    UpdateScreen(False);
    Exit;
  end;
  if FActiveElement > FBottomElement then
  begin
    while FActiveElement > FBottomElement do
    begin
      FBottomElement := Min(FFileList.Count - 1, FBottomElement + 5);
      FTopElement := Max(0, FBottomElement - FInnerRect.Height);
    end;
    for i := FTopElement to FBottomElement do
      DrawEntry(i);
    UpdateScreen(False);
  end;

  if FActiveElement <> OldActive then
    DrawEntry(OldActive);
  DrawEntry(FActiveElement);

  l := 0;
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    s := FFileList[FActiveElement].Name;
    l := Length(s);
    if l > FInnerRect.Width then
    begin
      s := Copy(s, 1 + ActShowStart, FInnerRect.Width - 1);
      l := Length(s);
    end;
    SetText(FInnerRect.Left, FRect.Bottom - 1, s);
  end;
  for n := 0 to FInnerRect.Width - l do
    SetChar(FInnerRect.Left + l + n, FRect.Bottom - 1, ' ');

  UpdateScreen(False);
end;

procedure TFileList.DrawEntry(Idx: Integer);
var
  s: String;
  l, n: Integer;
  Col: LongWord;
begin
  if not InRange(Idx - FTopElement, FInnerRect.Top - 1, FInnerRect.Bottom - 1) then
    Exit;
  if FIsActive and (Idx = FActiveElement) then
  begin
    FGPen := Blue;
    BGPen := Brown;
  end
  else
  begin
    FGPen := LightGray;
    BGPen := Blue;
  end;
  l := 0;
  if InRange(Idx, 0, FFileList.Count - 1) then
  begin
    if FFileList[Idx].Selected then
    begin
      FGPen := Yellow;
      col := Yellow;
    end
    else
    case FFileList[Idx].EType of
      etFile: col := Cyan;
      etDir:    col := White;
      etParent: col := LightGray;
      etDrive:  col := White;
      etAssign: col := LightGray;
    end;
    if FIsActive and (Idx = FActiveElement) then
      BGPen := col and $F
    else
      FGPen := col;
    s := LimitName(FFileList[Idx].Name, FInnerRect.Width - 9);
    l := Length(s);
    SetText(FInnerRect.Left, FInnerRect.Top + Idx - FTopElement, s);
    for n := 0 to FInnerRect.Width - l do
      SetChar(FInnerRect.Left + l + n, FInnerRect.Top + Idx - FTopElement, ' ');
    // all 8 chars long
    case FFileList[Idx].EType of
      etFile: s := FormatSize(FFileList[Idx].Size);
      etDir: begin
        if FFileList[Idx].Size < 0 then
          s := '   <Dir>'
        else
          s := FormatSize(FFileList[Idx].Size);
      end;
      etParent: s := '<Parent>';
      etDrive:  s := ' <Drive>';
      etAssign: s := '<Assign>';
//      else
//        s := '';
    end;
    SetText(FInnerRect.Right - 7, FInnerRect.Top + Idx - FTopElement, s);
  end
  else
    for n := 0 to FInnerRect.Width - l do
      SetChar(FInnerRect.Left + l + n, FInnerRect.Top + Idx - FTopElement, ' ');
  FGPen := LightGray;
  BGPen := Blue;
end;


function ListCompare(const Item1, Item2: TListEntry): Integer;
begin
  Result := Ord(Item1.EType) - Ord(Item2.EType);
  if Result = 0 then
    Result := CompareText(Item1.Name, Item2.Name);
end;

procedure TFileList.SortList;
begin
  FFileList.Sort(@ListCompare);
end;

procedure TFileList.CheckSelected;
var
  n,i: LongInt;
  s: Single;
  st: String;
begin
  n := 0;
  s := 0;
  for i := 0 to FFileList.Count - 1 do
  begin
    if FFileList[i].Selected then
    begin
      Inc(n);
      if FFileList[i].Size > 0 then
        s := s + FFileList[i].Size;
    end;
  end;
  BGPen := Blue;
  FGPen := LightGray;
  for i := FRect.Left + 1 to FRect.Right - 1 do
  begin
    SetChar(i, FRect.Bottom - 2, SingleLine);
  end;
  SetText(FRect.Left + 1, FRect.Bottom - 2, ' ' + IntToStr(n) + '/' + IntToStr(FFileList.Count) + ' ');
  if s > 0 then
  begin
    st := Trim(FormatSize(s) + 'byte');
    SetText(FRect.Right - 1 - Length(st), FRect.Bottom - 2, st);
  end;
  UpdateScreen(False);
end;

constructor TFileList.Create(ARect: TRect);
begin
  inherited Create;
  FActiveElement := -1;
  FTopElement := -1;
  FBottomElement := -1;
  FRect := ARect;
  FInnerRect := FRect;
  FInnerRect.Inflate(-1, -1);
  FInnerRect.Height := FInnerRect.Height - 2;
  FFileList := TEntryList.Create(True);
end;

destructor TFileList.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

procedure TFileList.Resize(ARect: TRect);
begin
  FRect := ARect;
  FInnerRect := FRect;
  FInnerRect.Inflate(-1, -1);
  FInnerRect.Height := FInnerRect.Height - 2;
  Update(False);
end;

procedure TFileList.Update(UpdateList: Boolean);
begin
  DrawBorder();
  DrawContents(UpdateList);
  DrawActive(ActiveElement);
  CheckSelected;
  UpdateScreen(False);
end;

procedure TFileList.GoToParent;
var
  p: SizeInt;
  s, Oldpath: string;
begin
  s := CurrentPath;
  p := Length(s);
  if p = 0 then
    Exit;
  if s[p] = DriveDelim then
  begin
    CurrentPath := '';
    ActivateFile(s);
  end
  else
  begin
    p := LastDelimiter(PathDelim + DriveDelim, s);
    OldPath := Copy(s, p + 1, Length(s));
    CurrentPath := Copy(s, 1, p);
    ActivateFile(IncludeTrailingPathDelimiter(OldPath));
  end;
end;

procedure TFileList.EnterPressed;
var
  s: string;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    s := CurrentPath;
    case FFileList[FActiveElement].EType of
      etDir: begin
        CurrentPath := IncludeTrailingPathDelimiter(s) + FFileList[FActiveElement].Name;
        ActiveElement :=  0;
      end;
      etDrive,
      etAssign: begin
        CurrentPath := FFileList[FActiveElement].Name;
        ActiveElement :=  0;
      end;
      etParent: GoToParent;
      else
        //
    end;
  end;
end;

function TFileList.ResultOfEntry(out NewPath: string): Boolean;
var
  p: SizeInt;
  s: string;
begin
  Result := False;
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    s := CurrentPath;
    case FFileList[FActiveElement].EType of
      etDir: begin
        NewPath := IncludeTrailingPathDelimiter(s) + FFileList[FActiveElement].Name;
        Result := True;
      end;
      etDrive,
      etAssign: begin
        NewPath := FFileList[FActiveElement].Name;
        Result := True;
      end;
      etParent: begin
        s := CurrentPath;
        p := Length(s);
        if p = 0 then
          Exit;
        if s[p] = DriveDelim then
        begin
          NewPath := '';
          Result := True;
        end
        else
        begin
          p := LastDelimiter(PathDelim + DriveDelim, s);
          NewPath := Copy(s, 1, p);
          Result := True;
        end;
      end;
      else
        Result := False;
    end;
  end;
end;

//############ Make Dir
procedure TFileList.MakeDir();
var
  NewName: string;
begin
  if FCurrentPath = '' then
    Exit;
  NewName := 'NewDir';
  if AskForName('Name for the new directory: ', NewName) then
  begin
    if not SysUtils.CreateDir(IncludeTrailingPathDelimiter(FCurrentPath) + NewName) then
      ShowMessage('Unable to create dir "' + NewName + '"');
    Update(True);
    ActivateFile(IncludeTrailingPathDelimiter(NewName));
  end;
end;

var
  CountPG: TSingleProgress = nil;

procedure RecurseDirs(BasePath, AName: string; FL: TEntryList; var Dirs: Integer; var Files: Integer; var Size: Int64);
var
  Info: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(AName);
  if Assigned(CountPG) then
    CountPG.UpdateValue(0, BasePath + Path + ' ' + IntToStr(Files + Dirs));
  if FindFirst(BasePath + Path + '*', faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      if (Info.Attr and faDirectory) <> 0 then
      begin
        Inc(Dirs);
        if Assigned(FL) then
          FL.AddDir(Path + Info.Name);
        RecurseDirs(BasePath, Path + Info.Name, FL, Dirs, Files, Size);
      end
      else
      begin
        Inc(Files);
        if Assigned(FL) then
          FL.AddFile(Path + Info.Name, Info.Size);
        Size := Size + Info.Size;
      end;
    Until FindNext(Info) <> 0;
    end;
  FindClose(Info);
end;

procedure TFileList.DoListOfSelectedFile(Recursive: Boolean; FL: TEntryList; out Dirs: integer; out Files: Integer; out Size: Int64);
var
  i: Integer;
  Found: Boolean;
begin
  CountPG := TSingleProgress.Create;
  CountPG.Text := 'Counting Files';
  CountPG.MaxValue := FFileList.Count;
  CountPG.Execute;
  dirs := 0;
  Files := 0;
  Size := 0;
  Found := False;
  for i := 0 to FFileList.Count - 1 do
  begin
    if FFileList[i].Selected then
    begin
      if FFileList[i].EType = etFile then
      begin
        FL.AddFile(FFileList[i].Name, FFileList[i].Size);
        Inc(Files);
        Size := Size + FFileList[i].Size;
      end;
      if FFileList[i].EType = etDir then
      begin
        FL.AddDir(FFileList[i].Name);
        Inc(Dirs);
        if Recursive then
          RecurseDirs(IncludeTrailingPathDelimiter(FCurrentPath), FFileList[i].Name, FL, Dirs, Files, Size);
      end;
      Found := True;
    end;
    CountPG.UpdateValue(i, 'Counting Files ' + IntToStr(Files + Dirs));
  end;
  if (not Found) and inRange(FActiveElement, 1, FFileList.Count - 1) then
  begin
    if FFileList[FActiveElement].EType = etFile then
    begin
      FL.AddFile(FFileList[FActiveElement].Name, FFileList[FActiveElement].Size);
      Inc(Files);
      Size := Size + FFileList[FActiveElement].Size;
    end;
    if FFileList[FActiveElement].EType = etDir then
    begin
      FL.AddDir(FFileList[FActiveElement].Name);
      Inc(Dirs);
      if Recursive then
        RecurseDirs(IncludeTrailingPathDelimiter(FCurrentPath), FFileList[FActiveElement].Name, FL, Dirs, Files, Size);
    end;
  end;
  CountPG.Free;
  CountPG := nil;
end;

//############ Delete
procedure TFileList.DeleteSelected();
var
  FL: TEntryList;
  i, Dirs, Files: Integer;
  NotDeleted: Integer;
  PG: TSingleProgress;
  Size: Int64;
begin
  FL := TEntryList.Create;
  try
    dirs := 0;
    Files := 0;
    NotDeleted := 0;
    // make a list of all
    DoListOfSelectedFile(True, FL, dirs, files, Size);
    if FL.Count > 0 then
    begin
      if AskQuestion('Delete ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files (' + Trim(FormatSize(Size)) + 'byte)?') then
      begin
        PG := TSingleProgress.Create;
        PG.Text :=  'Delete';
        PG.MaxValue := FL.Count;
        PG.Execute;
        //StartProgress('Delete ', FL.Count);
        for i := FL.Count - 1 downto 0 do
        begin
          try
            //if not UpdateProgress((FL.Count - 1 - i) + 1, 'Delete ' + FL[i].Name) then
            if not PG.UpdateValue((FL.Count - 1 - i) + 1, 'Delete ' + FL[i].Name) then
            begin
              ShowMessage('Delete stopped.');
              NotDeleted := 0;
              Break;
            end;
            if not DeleteFile(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name) then
              Inc(NotDeleted);
          except
            Inc(NotDeleted);
          end;
        end;
        PG.Free;
        if NotDeleted > 0 then
          ShowMessage('Cannot delete ' + IntToStr(NotDeleted) + ' files/dirs');
        Update(True);
      end;
    end;
  finally
    FL.Free;
  end;
end;

procedure TFileList.Rename();
var
  NewName: string;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    if FFileList[FActiveElement].EType in [etDir, etFile] then
    begin
      NewName := ExcludeTrailingPathDelimiter(FFileList[FActiveElement].Name);
      if AskForName('New name for "' + NewName + '"', NewName,  True) then
      begin
        SysUtils.RenameFile(IncludeTrailingPathDelimiter(FCurrentPath) + ExcludeTrailingPathDelimiter(FFileList[FActiveElement].Name), IncludeTrailingPathDelimiter(FCurrentPath) + NewName);
        Update(True);
      end;
    end;
  end;
end;

procedure TFileList.ActivateFile(AName: string);
var
  i: LongInt;
begin
  for i := 0 to FFileList.Count - 1 do
  begin
    if LowerCase(AName) = LowerCase(FFileList[i].Name) then
    begin
      ActiveElement := i;
      Break;
    end;
  end;
end;

procedure TFileList.SelectActiveEntry;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    FFileList[FActiveElement].Selected := not FFileList[FActiveElement].Selected;
    DrawEntry(FActiveElement);
    ActiveElement := FActiveElement + 1;
    CheckSelected;
  end;
end;

procedure TFileList.ScanSize;
var
  Size: Int64;
  Files, Dirs: Integer;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    if (FFileList[FActiveElement].EType = etDir) and (FFileList[FActiveElement].Size < 0) then
    begin
      Dirs := 0;
      Files := 0;
      Size := 0;
      RecurseDirs(FCurrentPath, FFileList[FActiveElement].Name, nil, Dirs, Files, Size);
      FFileList[FActiveElement].Size := Size;
    end;
  end;
  ActiveElement := ActiveElement + 1;
end;

procedure TFileList.IdleEvent;
var
  s: String;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    s := FFileList[FActiveElement].Name;
    if Length(s) > FInnerRect.Width then
    begin
      writeln('is too long ', ActShowStart);
      Inc(ActShowStart);
      if ActShowStart > (Length(s) - FInnerRect.Width) then
        ActShowStart := 0;
      writeln('  new actshow start', ActShowStart);
      DrawActive(FActiveElement);
    end;
  end;
end;

procedure TFileList.SearchList;
var
  i: LongInt;
  Key: TKeyEvent;
  C: Char;
  NSearchName, SearchName: string;
  Found: Boolean;
  Len: Integer;
begin
  // Enter SearchMode:
  BGPen := Cyan;
  FGPen := Black;
  SearchName := '';
  NSearchName := '';
  for i := FInnerRect.Left to FInnerRect.Right do
  begin
    SetChar(i, FRect.Bottom - 1, ' ');
  end;
  SetCursorType(crUnderline);
  SetCursorPos(FInnerRect.Left, FRect.Bottom - 1);
  repeat
    Key := GetNextKeyEvent;
    if (Key and $FFFF) <> 0 then
    begin
      C := GetKeyEventChar(Key);
      if c <> #0 then
      begin
        if c = #9 then
        begin
          c := #0;
          Break;
        end;
        if c = #8 then
        begin
          if Length(SearchName) > 0 then
            NSearchName := Copy(SearchName, 1, Length(SearchName) - 1);
        end
        else
          NSearchName := SearchName + c;
        Len := Length(NSearchName);
        if Len > 30 then
          Break;
        Found := False;
        for i := FActiveElement to FFileList.Count - 1 do
        begin
          if LowerCase(Copy(FFileList[i].Name, 1, Len)) = LowerCase(NSearchName) then
          begin
            ActiveElement := i;
            SearchName := NSearchName;
            Found := True;
            Break;
          end;
        end;
        if (not Found) and (FActiveElement > 0) then
        begin
          for i := 0 to FFileList.Count - 1 do
          begin
            if LowerCase(Copy(FFileList[i].Name, 1, Len)) = LowerCase(NSearchName) then
            begin
              ActiveElement := i;
              SearchName := NSearchName;
              Found := True;
              Break;
            end;
          end;
        end;
        if Found then
        begin
          BGPen := Cyan;
          FGPen := Black;
          for i := FInnerRect.Left to FInnerRect.Right do
            SetChar(i, FRect.Bottom - 1, ' ');
          SetCursorPos(FInnerRect.Left + Length(NSearchName), FRect.Bottom - 1);
          SetText(FinnerRect.Left, FRect.Bottom - 1, NSearchName);
          UpdateScreen(False);
        end;
      end;
      // leave on all not char characters
      if c = #0 then
      begin
        Break;
      end;
    end;
    Sleep(25);
  until False;
  SetCursorType(crHidden);
  DrawActive(FActiveElement);
end;

procedure TFileList.ViewFile(OpenWithProgram: string = '');
var
  FileN: string;
  Ret: Integer;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    if FFileList[FActiveElement].EType = etFile then
    begin
      FileN := IncludeTrailingPathDelimiter(FCurrentPath) + FFileList[FActiveElement].Name;
      if OpenWithProgram = '' then
        FileViewer(FileN)
      else
      begin
        NonWaitMessage('Starting ' + ExtractFileName(OpenWithProgram));
        Ret := ExecuteProcess(OpenWithProgram, [FileN]);
        if Ret <> 0 then
          ShowMessage(OpenWithProgram + ' returned with error message: ' + IntToStr(Ret));
      end;
    end;
  end;
end;

procedure TFileList.EditFile(OpenWithProgram: string = '');
var
  FileN: string;
  Ret: Integer;
begin
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    if FFileList[FActiveElement].EType = etFile then
    begin
      FileN := IncludeTrailingPathDelimiter(FCurrentPath) + FFileList[FActiveElement].Name;
      if OpenWithProgram = '' then
        ShowMessage('No Editor set')
      else
      begin
        // message without waiting
        NonWaitMessage('Starting ' + ExtractFileName(OpenWithProgram));
        Ret := ExecuteProcess(OpenWithProgram, [FileN]);
        if Ret <> 0 then
          ShowMessage(OpenWithProgram + ' returned with error message: ' + IntToStr(Ret));
      end;
    end;
  end;
end;

procedure TFileList.SelectByPattern(DoSelect: Boolean);
var
  s, Pattern: string;
  i: Integer;
begin
  if DoSelect then
    s := 'Pattern to select: '
  else
    s := 'Pattern to deselect: ';
  Pattern := '*';
  if AskForName(s, Pattern, False) and (Pattern <> '') then
  begin
    for i := 0 to FFileList.Count - 1 do
    begin
      case FFileList[i].EType of
        etFile: if IsWild(FFileList[i].Name, Pattern, True) then FFileList[i].Selected := DoSelect;
        etDir: if IsWild(ExcludeTrailingPathDelimiter(FFileList[i].Name), Pattern, True) then FFileList[i].Selected := DoSelect;
        else
      end;
    end;
    CheckSelected;
  end;
end;

function GetFileSize(const Name: string): Int64;
var
  SRec: TSearchRec;
begin
  if FindFirst(name, faAnyfile, SRec) = 0 then
  begin
    Result := SRec.Size;
    FindClose(SRec);
  end
  else
    Result := 0;
end;

function OverwriteText(Src, dest: string): string;
var
  fileDate: Int64;
begin
  Result := '     New:      ';
  fileDate := FileAge(Src);
  if fileDate > -1 then
    Result := Result + FormatDateTime('YYYY-MM-DD hh:nn:ss', FileDateToDateTime(fileDate))
  else
    Result := '               ';
  Result := Result + '    ' + IntToStr(GetFileSize(Src)) + ' bytes     '#13#10;

  Result := Result + '     Existing: ';
  fileDate := FileAge(dest);
  if fileDate > -1 then
    Result := Result + FormatDateTime('YYYY-MM-DD hh:nn:ss', FileDateToDateTime(fileDate))
  else
    Result := '               ';
  Result := Result + '    ' + IntToStr(GetFileSize(dest)) + ' bytes     '#13#10;
end;

//############ Copy
procedure TFileList.CopyFiles(Target: string);
const
  BufferSize = 100 * 1024;
var
  FL: TEntryList;
  dirs, Files, NotCopied, i, NumBytes, AllBytes: Integer;
  Count,WrittenCount, AllNumBytes, IsSame: LongInt;
  {$ifdef HASAMIGA}
  SrcLock, DestLock: BPTR;
  {$endif}
  Buffer: PByte;
  Src, Dest: TFileStream;
  AllBytesStr: String;
  Msg, NewName: string;
  StartTime, CurTime: LongWord;
  Speed: Extended;
  PG: TSingleProgress;
  PG2: TDoubleProgress;
  Size: Int64;
  Res: TDialogResult;
begin
  Res := mrNone;
  FL := TEntryList.Create;
  try
    PG := nil;
    PG2 := nil;
    Src := nil;
    Dest := nil;
    Buffer := nil;
    dirs := 0;
    Files := 0;
    NotCopied := 0;
    // make a list of all
    DoListOfSelectedFile(True, FL, dirs, files, Size);
    if FL.Count > 0 then
    begin
      if AskQuestion('Copy ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files (' + Trim(FormatSize(Size))  + 'byte)? ') then
      begin
        //---------- copy one file
        if (FL.Count = 1) and (FL[0].EType = etFile) then
        begin
          PG := TSingleProgress.Create;
          PG.Text := 'Copy ';
          PG.MaxValue := FL[0].Size;
          PG.Execute;
          Buffer := AllocMem(BufferSize);
          try
            PG.UpdateValue(0, FL[0].Name);
            // check if the same file
            if FileExists(IncludeTrailingPathDelimiter(Target) + FL[0].Name) then
            begin
              {$ifdef HASAMIGA}
              SrcLock := Lock(PChar(IncludeTrailingPathDelimiter(FCurrentPath) + FL[0].Name), SHARED_LOCK);
              DestLock := Lock(PChar(IncludeTrailingPathDelimiter(Target) + FL[0].Name), SHARED_LOCK);
              IsSame := SameLock(SrcLock, DestLock);
              Unlock(SrcLock);
              UnLock(DestLock);
              if IsSame = LOCK_SAME then
                raise Exception.Create('Cannot copy on itself');
              {$endif}
              if not AskQuestion('File "' + FL[0].Name +'" already exists, Overwrite?'#13#10 + OverwriteText(IncludeTrailingPathDelimiter(FCurrentPath) + FL[0].Name, IncludeTrailingPathDelimiter(Target) + FL[0].Name)) then
                Exit;
            end;
            Src := TFileStream.Create(IncludeTrailingPathDelimiter(FCurrentPath) + FL[0].Name, fmOpenRead);
            Dest := TFileStream.Create(IncludeTrailingPathDelimiter(Target) + FL[0].Name, fmCreate);
            NumBytes := 0;
            AllBytesStr := FormatSize(FL[0].Size);
            {$WARNINGS OFF}
            StartTime := GetTickCount;
            {$WARNINGS ON}
            repeat
              Count := Src.Read(Buffer^, BufferSize);
              NumBytes := NumBytes + Count;
              {$WARNINGS OFF}
              CurTime := GetTickCount;
              {$WARNINGS ON}
              Speed := 0.0;
              if CurTime - StartTime > 0 then
              begin
                Speed := NumBytes / ((CurTime - StartTime) / 1000);
              end;
              if not PG.UpdateValue(NumBytes, 'Copy file ' + FormatSize(NumBytes) + '/' + AllBytesStr + ' with ' + FormatSize(Speed) + 'byte/s') then
                raise Exception.Create('Copy stopped');
              if Dest.Write(Buffer^, Count) <> Count then
                raise Exception.Create('fail to write');
            until Count = 0
          except
            on E:Exception do
            begin
              Src.Free;
              Src := nil;
              // only delete the file if the dest was created ;)
              if Assigned(nil) then
              begin
                Dest.Free;
                Dest := nil;
                DeleteFile(IncludeTrailingPathDelimiter(Target) + ExtractFileName(FL[0].Name));
              end;
              NotCopied := 1;
              Msg := E.Message;
            end;
          end;
          PG.Free;
          Src.Free;
          Dest.Free;
          FreeMem(Buffer);
        end
        else
        begin //------------- Copy multiple files
          AllBytes := 0;
          AllNumBytes := 0;
          for i := 0 to FL.Count - 1 do
          begin
            if FL[i].EType = etFile then
              AllBytes := AllBytes + FL[i].Size;
          end;
          PG2 := TDoubleProgress.Create;
          PG2.Text := 'Copy';
          PG2.Text2 := '';
          PG2.MaxValue := FL.Count;
          PG2.MaxValue2 := AllBytes;
          PG2.Execute;
          //
          Buffer := AllocMem(BufferSize);
          AllBytesStr := FormatSize(AllBytes);
          {$WARNINGS OFF}
          StartTime := GetTickCount;
          {$WARNINGS ON}
          for i := 0 to FL.Count - 1 do
          begin
            Src := nil;
            Dest := nil;
            try
              if not PG2.UpdateValue2(i + 1, 'Copy ' + FL[i].Name, AllNumBytes, FormatSize(AllNumBytes) + 'byte / ' + AllBytesStr + 'byte') then
              begin
                ShowMessage('Copy stopped.');
                NotCopied := 0;
                Break;
              end;
              NewName := IncludeTrailingPathDelimiter(Target) + FL[i].Name;
              if FL[i].EType = etDir then
              begin
                if not DirectoryExists(ExcludeTrailingPathDelimiter(NewName)) then
                  CreateDir(ExcludeTrailingPathDelimiter(NewName));
              end;
              if FL[i].EType = etFile then
              begin
                if FileExists(IncludeTrailingPathDelimiter(Target) + FL[i].Name) then
                begin
                  {$ifdef HASAMIGA}
                  SrcLock := Lock(PChar(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name), SHARED_LOCK);
                  DestLock := Lock(PChar(NewName), SHARED_LOCK);
                  IsSame := SameLock(SrcLock, DestLock);
                  Unlock(SrcLock);
                  UnLock(DestLock);
                  if IsSame = LOCK_SAME then
                    raise Exception.Create('Can''t copy on itself');
                  {$endif}
                  if Res = mrNone then
                    Res := AskMultipleQuestion('File "' + FL[i].Name +'" already exists, Overwrite?'#13#10 + OverwriteText(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name, IncludeTrailingPathDelimiter(Target) + FL[i].Name));
                  case Res of
                    mrOK: begin Res := mrNone; end;
                    mrCancel: begin Res := mrNone; Continue; end;
                    mrAll: ;
                    mrNoAll: Continue;
                    mrAbort: Exit;
                    else
                      ;
                  end;
                end;
                Src := TFileStream.Create(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name, fmOpenRead);
                Dest := TFileStream.Create(NewName, fmCreate);
                repeat
                  Count := Src.Read(Buffer^, BufferSize);
                  if Count = 0 then
                    Break;
                  AllNumBytes := AllNumBytes + Count;
                  {$WARNINGS OFF}
                  CurTime := GetTickCount;
                  {$WARNINGS ON}
                  Speed := 0.0;
                  if CurTime - StartTime > 0 then
                    Speed := AllNumBytes / ((CurTime - StartTime) / 1000);
                  //if not UpdateProgress2(i + 1, AllNumBytes, 'Copy ' + FL[i].Name, FormatSize(AllNumBytes) + 'byte /' + AllBytesStr + 'byte with ' + FormatSize(Speed) + 'byte/s') then
                  if not PG2.UpdateValue2(i + 1, 'Copy ' + FL[i].Name, AllNumBytes, FormatSize(AllNumBytes) + 'byte / ' + AllBytesStr + 'byte with ' + FormatSize(Speed) + 'byte/s') then
                    raise Exception.Create('Copy stopped');
                  WrittenCount := Dest.Write(Buffer^, Count);
                  if WrittenCount <> Count then
                    raise Exception.Create('fail to write');
                until Count = 0;
                Src.Free;
                Src := nil;
                Dest.Free;
                Dest := nil;
              end;
            except
              on E:Exception do
              begin
                Src.Free;
                Src := nil;
                Dest.Free;
                Dest := nil;
                DeleteFile(IncludeTrailingPathDelimiter(Target) + ExtractFileName(FL[0].Name));
                NotCopied := 1;
                Msg := E.Message;
                Break;
              end;
            end;
          end;
          PG2.Free;
          FreeMem(Buffer);
        end;
        if NotCopied > 0 then
        begin
          if NotCopied = 1 then
            ShowMessage('Error: ' + Msg)
          else
            ShowMessage('Can''t copy ' + IntToStr(NotCopied) + ' files/dirs');
        end
        else
        begin
          for i := 0 to FFileList.Count - 1 do
            FFileList[i].Selected := False;
          CheckSelected;
        end;
      end;
    end;
  finally
    FL.Free;
  end;
end;

//############ Copy
procedure TFileList.MoveFiles(Target: string);
const
  BufferSize = 100 * 1024;
var
  FL: TEntryList;
  dirs, Files, NotCopied, i, NumBytes, AllBytes: Integer;
  Count,WrittenCount, AllNumBytes, IsSame: LongInt;
  {$ifdef HASAMIGA}
  SrcLock, DestLock: BPTR;
  {$endif}
  Buffer: PByte;
  Src, Dest: TFileStream;
  AllBytesStr: String;
  Msg, NewName: string;
  StartTime, CurTime: LongWord;
  Speed: Extended;
  PG: TSingleProgress;
  PG2: TDoubleProgress;
  Size: Int64;
  IsSameDevice: Boolean;
  Res: TDialogResult;
begin
  Res := mrNone;
  FL := TEntryList.Create;
  try
    PG := nil;
    PG2 := nil;
    Src := nil;
    Dest := nil;
    Buffer := nil;
    dirs := 0;
    Files := 0;
    NotCopied := 0;
    IsSameDevice := False;
    //check if we can use the shortcut move
    {$ifdef HASAMIGA}
    SrcLock := Lock(PChar(ExcludeTrailingPathDelimiter(FCurrentPath)), SHARED_LOCK);
    DestLock := Lock(PChar(ExcludeTrailingPathDelimiter(Target)), SHARED_LOCK);
    IsSameDevice := SameDevice(SrcLock, DestLock);
    Unlock(SrcLock);
    UnLock(DestLock);
    {$endif}
    if IsSameDevice then
    begin
      DoListOfSelectedFile(False, FL, dirs, files, Size);
      if AskQuestion('Move ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files (' + Trim(FormatSize(Size))  + 'byte)? ') then
      begin
        PG := TSingleProgress.Create;
        PG.Text := 'Move ';
        PG.MaxValue := FL.Count;
        PG.Execute;
        for i := 0 to FL.Count - 1 do
        begin
          if (FL[i].EType = etFile) and FileExists(IncludeTrailingPathDelimiter(Target) + FL[i].Name) then
          begin
            if Res = mrNone then
              Res := AskMultipleQuestion('File "' + FL[i].Name +'" already exists, Overwrite?'#13#10 + OverwriteText(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name, IncludeTrailingPathDelimiter(Target) + FL[i].Name));
            case Res of
              mrOK: begin DeleteFile(IncludeTrailingPathDelimiter(Target) + FL[i].Name); Res := mrNone; end;
              mrCancel: begin Res := mrNone; Continue; end;
              mrAll: DeleteFile(IncludeTrailingPathDelimiter(Target) + FL[i].Name);
              mrNoAll: Continue;
              mrAbort: Exit;
              else
                ;
            end;
          end;
          if FL[i].EType in [etDir, etFile] then
          begin
            PG.UpdateValue(i, FL[i].Name);
            SysUtils.RenameFile(IncludeTrailingPathDelimiter(FCurrentPath) + ExcludeTrailingPathDelimiter(FL[i].Name), IncludeTrailingPathDelimiter(Target) + ExcludeTrailingPathDelimiter(FL[i].Name));
          end;
        end;
        Update(True);
      end;
      Exit;
    end;
    FL.Clear;
    // make a list of all
    DoListOfSelectedFile(True, FL, dirs, files, Size);
    if FL.Count > 0 then
    begin
      if AskQuestion('Move ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files (' + Trim(FormatSize(Size))  + 'byte)? ') then
      begin
        //---------- move one file
        if (FL.Count = 1) and (FL[0].EType = etFile) then
        begin
          PG := TSingleProgress.Create;
          PG.Text := 'Move ';
          PG.MaxValue := FL[0].Size;
          PG.Execute;
          Buffer := AllocMem(BufferSize);
          try
            PG.UpdateValue(0, FL[0].Name);
            // check if the same file
            if FileExists(IncludeTrailingPathDelimiter(Target) + FL[0].Name) then
            begin
              {$ifdef HASAMIGA}
              SrcLock := Lock(PChar(IncludeTrailingPathDelimiter(FCurrentPath) + FL[0].Name), SHARED_LOCK);
              DestLock := Lock(PChar(IncludeTrailingPathDelimiter(Target) + FL[0].Name), SHARED_LOCK);
              IsSame := SameLock(SrcLock, DestLock);
              Unlock(SrcLock);
              UnLock(DestLock);
              if IsSame = LOCK_SAME then
                raise Exception.Create('Can''t move  on itself');
              {$endif}
              if not AskQuestion('File "' + FL[0].Name +'" already exists, Overwrite?'#13#10 + OverwriteText(IncludeTrailingPathDelimiter(FCurrentPath) + FL[0].Name, IncludeTrailingPathDelimiter(Target) + FL[0].Name)) then
                Exit;
            end;
            Src := TFileStream.Create(IncludeTrailingPathDelimiter(FCurrentPath) + FL[0].Name, fmOpenRead);
            Dest := TFileStream.Create(IncludeTrailingPathDelimiter(Target) + FL[0].Name, fmCreate);
            NumBytes := 0;
            AllBytesStr := FormatSize(FL[0].Size);
            {$WARNINGS OFF}
            StartTime := GetTickCount;
            {$WARNINGS ON}
            repeat
              Count := Src.Read(Buffer^, BufferSize);
              NumBytes := NumBytes + Count;
              {$WARNINGS OFF}
              CurTime := GetTickCount;
              {$WARNINGS ON}
              Speed := 0.0;
              if CurTime - StartTime > 0 then
              begin
                Speed := NumBytes / ((CurTime - StartTime) / 1000);
              end;
              if not PG.UpdateValue(NumBytes, 'Move file ' + FormatSize(NumBytes) + '/' + AllBytesStr + ' with ' + FormatSize(Speed) + 'byte/s') then
                raise Exception.Create('Move stopped');
              if Dest.Write(Buffer^, Count) <> Count then
                raise Exception.Create('fail to write');
            until Count = 0
          except
            on E:Exception do
            begin
              Src.Free;
              Src := nil;
              // only delete the file if the dest was created ;)
              if Assigned(nil) then
              begin
                Dest.Free;
                Dest := nil;
                DeleteFile(IncludeTrailingPathDelimiter(Target) + ExtractFileName(FL[0].Name));
              end;
              NotCopied := 1;
              Msg := E.Message;
            end;
          end;
          PG.Free;
          Src.Free;
          Dest.Free;
          FreeMem(Buffer);
        end
        else
        begin //------------- Copy multiple files
          AllBytes := 0;
          AllNumBytes := 0;
          for i := 0 to FL.Count - 1 do
          begin
            if FL[i].EType = etFile then
              AllBytes := AllBytes + FL[i].Size;
          end;
          PG2 := TDoubleProgress.Create;
          PG2.Text := 'Move';
          PG2.Text2 := '';
          PG2.MaxValue := FL.Count;
          PG2.MaxValue2 := AllBytes;
          PG2.Execute;
          //
          Buffer := AllocMem(BufferSize);
          AllBytesStr := FormatSize(AllBytes);
          {$WARNINGS OFF}
          StartTime := GetTickCount;
          {$WARNINGS ON}
          for i := 0 to FL.Count - 1 do
          begin
            Src := nil;
            Dest := nil;
            try
              if not PG2.UpdateValue2(i + 1, 'Move ' + FL[i].Name, AllNumBytes, FormatSize(AllNumBytes) + 'byte / ' + AllBytesStr + 'byte') then
              begin
                ShowMessage('Move stopped.');
                NotCopied := 0;
                Break;
              end;
              NewName := IncludeTrailingPathDelimiter(Target) + FL[i].Name;
              if FL[i].EType = etDir then
              begin
                if not DirectoryExists(ExcludeTrailingPathDelimiter(NewName)) then
                  CreateDir(ExcludeTrailingPathDelimiter(NewName));
              end;
              if FL[i].EType = etFile then
              begin
                if FileExists(IncludeTrailingPathDelimiter(Target) + FL[i].Name) then
                begin
                  {$ifdef HASAMIGA}
                  SrcLock := Lock(PChar(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name), SHARED_LOCK);
                  DestLock := Lock(PChar(NewName), SHARED_LOCK);
                  IsSame := SameLock(SrcLock, DestLock);
                  Unlock(SrcLock);
                  UnLock(DestLock);
                  if IsSame = LOCK_SAME then
                    raise Exception.Create('Can''t move on itself');
                  {$endif}
                  if Res = mrNone then
                    Res := AskMultipleQuestion('File "' + FL[i].Name +'" already exists, Overwrite?'#13#10 + OverwriteText(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name, IncludeTrailingPathDelimiter(Target) + FL[i].Name));
                  case Res of
                    mrOK: begin Res := mrNone; end;
                    mrCancel: begin Res := mrNone; Continue; end;
                    mrAll: ;
                    mrNoAll: Continue;
                    mrAbort: Exit;
                    else
                      ;
                  end;
                end;
                Src := TFileStream.Create(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name, fmOpenRead);
                Dest := TFileStream.Create(NewName, fmCreate);
                repeat
                  Count := Src.Read(Buffer^, BufferSize);
                  if Count = 0 then
                    Break;
                  AllNumBytes := AllNumBytes + Count;
                  {$WARNINGS OFF}
                  CurTime := GetTickCount;
                  {$WARNINGS ON}
                  Speed := 0.0;
                  if CurTime - StartTime > 0 then
                    Speed := AllNumBytes / ((CurTime - StartTime) / 1000);
                  if not PG2.UpdateValue2(i + 1, 'Copy ' + FL[i].Name, AllNumBytes, FormatSize(AllNumBytes) + 'byte / ' + AllBytesStr + 'byte with ' + FormatSize(Speed) + 'byte/s') then
                    raise Exception.Create('Move stopped');
                  WrittenCount := Dest.Write(Buffer^, Count);
                  if WrittenCount <> Count then
                    raise Exception.Create('fail to write');
                until Count = 0;
                Src.Free;
                Src := nil;
                Dest.Free;
                Dest := nil;
              end;
            except
              on E:Exception do
              begin
                Src.Free;
                Src := nil;
                Dest.Free;
                Dest := nil;
                DeleteFile(IncludeTrailingPathDelimiter(Target) + ExtractFileName(FL[0].Name));
                NotCopied := 1;
                Msg := E.Message;
                Break;
              end;
            end;
          end;
          PG2.Free;
          FreeMem(Buffer);
        end;
        if NotCopied > 0 then
        begin
          if NotCopied = 1 then
            ShowMessage('Error: ' + Msg)
          else
            ShowMessage('Can''t move ' + IntToStr(NotCopied) + ' files/dirs');
        end
        else
        begin
          for i := FL.Count - 1 downto 0 do
            DeleteFile(IncludeTrailingPathDelimiter(FCurrentPath) + ExcludeTrailingPathDelimiter(FL[i].Name));
        end;
        Update(True);
      end;
    end;
  finally
    FL.Free;
  end;
end;


initialization

end.

