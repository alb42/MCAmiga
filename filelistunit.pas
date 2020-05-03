unit FileListUnit;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef HASAMIGA}
  AmigaDOS,
  {$endif}
  Video, Classes, SysUtils, Math, fgl, StrUtils;

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
    Size: Integer;
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
    procedure SetActiveElement(AValue: Integer);
    procedure SetCurrentPath(AValue: string);
    procedure DrawBorder;
    procedure DrawContents(UpdateList: Boolean);
    procedure SetIsActive(AValue: Boolean);
    procedure DrawActive(NActive: Integer);
    procedure DrawEntry(Idx: Integer);

    procedure DoListOfSelectedFile(Recursive: Boolean; FL: TEntryList; out Dirs: integer; out Files: Integer);

    procedure SortList;
  public
    constructor Create(ARect: TRect); virtual;
    destructor Destroy; override;

    procedure Update(UpdateList: Boolean);
    procedure GoToParent;
    procedure EnterPressed;

    procedure MakeDir(); // F7
    procedure DeleteSelected(); // F8

    procedure SelectFile(AName: string);

    property CurrentPath: string read FCurrentPath write SetCurrentPath;
    property IsActive: Boolean read FIsActive write SetIsActive;
    property ActiveElement: Integer read FActiveElement write SetActiveElement;
  end;


  function PosToArray(px, py: Integer): Integer; inline;
  procedure SetChar(p: Integer; c: Char); overload;
  procedure SetChar(x,y: Integer; c: Char); overload;
  procedure SetText(x,y: Integer; s: string);

implementation

uses
  DialogUnit;

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
        Result := '...' + Copy(s2, Length(s2) - (MaxLength - 3), MaxLength - 3);
    end
  end
  else
    Result := '...' + Copy(AName, Length(AName) - (MaxLength - 3), MaxLength - 3);
end;

function FormatSize(Size: Int64): string;
begin
  if Size < 1000 then
    Result := Format('%6.d  ', [Size])
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
end;

procedure TListEntry.Assign(Src: TListEntry);
begin
  Self.Name := Src.Name;
  Self.EType := Src.EType;
  Self.Selected := False;
  Self.Size := Src.Size;
end;

{ TFileList }

procedure Tfilelist.Setcurrentpath(Avalue: String);
begin
  if FCurrentPath = AValue then
    Exit;
  FCurrentPath := ExcludeTrailingPathDelimiter(AValue);
  Update(True);
end;

procedure Tfilelist.Setactiveelement(Avalue: Integer);
begin
  if FActiveElement = AValue then
    Exit;
  DrawActive(AValue);
end;

procedure Tfilelist.Drawborder;
var
  i: Integer;
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
  SetText(FRect.Left + 2, FRect.Top, LeftEdge + LimitName(FCurrentPath, FRect.Width - 5, True) + RightEdge);
end;

procedure Tfilelist.Drawcontents(Updatelist: Boolean);
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

    FTopElement := 0;
    FBottomElement := Min(FFileList.Count - 1, FInnerRect.Height);
    if FActiveElement >= FFileList.Count then
      FActiveElement := 0;
  end;
  for i := 0 to FInnerRect.Height do
  begin
    DrawEntry(i);
  end;
end;

procedure Tfilelist.Setisactive(Avalue: Boolean);
begin
  if FIsActive = AValue then Exit;
    FIsActive := AValue;
  DrawActive(FActiveElement);
  UpdateScreen(False);
end;


procedure Tfilelist.Drawactive(Nactive: Integer);
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
    Exit;
  end;

  if FActiveElement <> OldActive then
    DrawEntry(OldActive);
  DrawEntry(FActiveElement);

  l := 0;
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    s := FFileList[FActiveElement].Name;
    l := Length(s);
    SetText(FInnerRect.Left, FRect.Bottom - 1, s);
  end;
  for n := 0 to FInnerRect.Width - l do
    SetChar(FInnerRect.Left + l + n, FRect.Bottom - 1, ' ');

  UpdateScreen(False);
end;

procedure Tfilelist.Drawentry(Idx: Integer);
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
    case FFileList[Idx].EType of
      etFile: col := Green;
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
      etDir:    s := '   <Dir>';
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

procedure Tfilelist.Sortlist;
begin
  FFileList.Sort(@ListCompare);
end;

constructor Tfilelist.Create(Arect: Trect);
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

destructor Tfilelist.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

procedure Tfilelist.Update(Updatelist: Boolean);
begin
  DrawBorder();
  DrawContents(UpdateList);
  UpdateScreen(False);
end;

procedure Tfilelist.Gotoparent;
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
  end
  else
  begin
    p := LastDelimiter(PathDelim + DriveDelim, s);
    OldPath := Copy(s, p + 1, Length(s));
    CurrentPath := Copy(s, 1, p);
    SelectFile(IncludeTrailingPathDelimiter(OldPath));
  end;
end;

procedure Tfilelist.Enterpressed;
var
  s: string;
begin
  //
  if InRange(FActiveElement, 0, FFileList.Count - 1) then
  begin
    s := CurrentPath;
    case FFileList[FActiveElement].EType of
      etDir: CurrentPath := IncludeTrailingPathDelimiter(s) + FFileList[FActiveElement].Name;
      etDrive,
      etAssign: CurrentPath := FFileList[FActiveElement].Name;
      etParent: GoToParent;
      else
        //
    end;
    //
    //
  end;
end;

procedure Tfilelist.Makedir();
var
  NewName: string;
begin
  if FCurrentPath = '' then
    Exit;
  NewName := 'NewDir';
  if AskForName('Name for the new directory: ', NewName) then
  begin
    SysUtils.CreateDir(IncludeTrailingPathDelimiter(FCurrentPath) + NewName);
    Update(True);
    SelectFile(IncludeTrailingPathDelimiter(NewName));
  end;
end;


procedure Tfilelist.Dolistofselectedfile(Recursive: Boolean; Fl: Tentrylist; out Dirs: Integer; out Files: Integer);
var
  i: Integer;
  Found: Boolean;
begin
  dirs := 0;
  Files := 0;
  Found := False;
  for i := 0 to FFileList.Count - 1 do
  begin
    if FFileList[i].Selected then
    begin
      //
      Found := True;
    end;
  end;
  if (not Found) and inRange(FActiveElement, 1, FFileList.Count - 1) then
  begin
    if FFileList[FActiveElement].EType = etFile then
    begin
      FL.AddFile(IncludeTrailingPathDelimiter(FCurrentPath) + FFileList[FActiveElement].Name, 0);
      Inc(Files);
    end;
    if FFileList[FActiveElement].EType = etDir then
    begin
      FL.AddDir(IncludeTrailingPathDelimiter(FCurrentPath) + FFileList[FActiveElement].Name);
      Inc(Dirs);
      if Recursive then
      begin
        //
      end;
    end;
  end;
end;

procedure Tfilelist.Deleteselected();
var
  FL: TEntryList;
  i, Dirs, Files: Integer;
begin
  FL := TEntryList.Create;
  try
    dirs := 0;
    Files := 0;
    // make a list of all
    DoListOfSelectedFile(True, FL, dirs, files);
    if FL.Count > 0 then
    begin
      if AskQuestion('Delete ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files?') then
      begin
        for i := FL.Count - 1 downto 0 do
        begin
          DeleteFile(FL[i].Name);
        end;
        Update(True);
      end;
    end;
  finally
    FL.Free;
  end;
end;

procedure Tfilelist.Selectfile(Aname: String);
var
  i: LongInt;
begin
  writeln('select file ', AName);
  for i := 0 to FFileList.Count - 1 do
  begin
    if LowerCase(AName) = LowerCase(FFileList[i].Name) then
    begin
      writeln('found');
      ActiveElement := i;
      Break;
    end;
  end;
end;

end.

