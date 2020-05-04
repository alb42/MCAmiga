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

    procedure CopyFiles(Target: string); // F5
    procedure MakeDir(); // F7
    procedure DeleteSelected(); // F8

    procedure ActivateFile(AName: string);
    procedure SelectActiveEntry;

    procedure SelectByPattern(DoSelect: Boolean);

    property CurrentPath: string read FCurrentPath write SetCurrentPath;
    property IsActive: Boolean read FIsActive write SetIsActive;
    property ActiveElement: Integer read FActiveElement write SetActiveElement;
  end;


  function PosToArray(px, py: Integer): Integer; inline;
  procedure SetChar(p: Integer; c: Char); overload;
  procedure SetChar(x,y: Integer; c: Char); overload;
  procedure SetText(x,y: Integer; s: string);
  function LimitName(AName: string; MaxLength: Integer; PathMode: Boolean = False): string;

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
        Result := '...' + Copy(s2, Length(s2) - (MaxLength - 3), MaxLength - 2);
    end
  end
  else
    Result := '...' + Copy(AName, Length(AName) - (MaxLength - 3), MaxLength - 2);
end;

function FormatSize(Size: Single): string;
begin
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
  DrawActive(AValue);
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

end;

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

    FTopElement := 0;
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
    if l > FInnerRect.Width then
    begin
      s := Copy(s, 1, FInnerRect.Width - 1);
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

procedure TFileList.SortList;
begin
  FFileList.Sort(@ListCompare);
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

procedure TFileList.Update(UpdateList: Boolean);
begin
  DrawBorder();
  DrawContents(UpdateList);
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

procedure RecurseDirs(BasePath, AName: string; FL: TEntryList; var Dirs: Integer; var Files: Integer);
var
  Info: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(AName);
  if FindFirst (BasePath + Path + '*', faAnyFile and faDirectory, Info) = 0 then
  begin
    repeat
      if (Info.Attr and faDirectory) <> 0 then
      begin
        Inc(Dirs);
        FL.AddDir(Path + Info.Name);
        RecurseDirs(BasePath, Path + Info.Name, FL, Dirs, Files);
      end
      else
      begin
        Inc(Files);
        FL.AddFile(Path + Info.Name, Info.Size);
      end;
    Until FindNext(Info) <> 0;
    end;
  FindClose(Info);
end;

procedure TFileList.DoListOfSelectedFile(Recursive: Boolean; FL: TEntryList; out Dirs: integer; out Files: Integer);
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
      if FFileList[i].EType = etFile then
      begin
        FL.AddFile(FFileList[i].Name, FFileList[i].Size);
        Inc(Files);
      end;
      if FFileList[i].EType = etDir then
      begin
        FL.AddDir(FFileList[i].Name);
        Inc(Dirs);
        if Recursive then
        begin
          RecurseDirs(IncludeTrailingPathDelimiter(FCurrentPath), FFileList[i].Name, FL, Dirs, Files);
        end;
      end;
      Found := True;
    end;
  end;
  if (not Found) and inRange(FActiveElement, 1, FFileList.Count - 1) then
  begin
    if FFileList[FActiveElement].EType = etFile then
    begin
      FL.AddFile(FFileList[FActiveElement].Name, FFileList[FActiveElement].Size);
      Inc(Files);
    end;
    if FFileList[FActiveElement].EType = etDir then
    begin
      FL.AddDir(FFileList[FActiveElement].Name);
      Inc(Dirs);
      if Recursive then
      begin
        RecurseDirs(IncludeTrailingPathDelimiter(FCurrentPath), FFileList[FActiveElement].Name, FL, Dirs, Files);
      end;
    end;
  end;
end;

//############ Delete
procedure TFileList.DeleteSelected();
var
  FL: TEntryList;
  i, Dirs, Files: Integer;
  NotDeleted: Integer;
begin
  FL := TEntryList.Create;
  try
    dirs := 0;
    Files := 0;
    NotDeleted := 0;
    // make a list of all
    DoListOfSelectedFile(True, FL, dirs, files);
    if FL.Count > 0 then
    begin
      if AskQuestion('Delete ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files?') then
      begin
        StartProgress('Delete ', FL.Count);
        for i := FL.Count - 1 downto 0 do
        begin
          try
            if not UpdateProgress((FL.Count - 1 - i) + 1, 'Delete ' + FL[i].Name) then
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
        if NotDeleted > 0 then
          ShowMessage('Cannot delete ' + IntToStr(NotDeleted) + ' files/dirs');
        Update(True);
      end;
    end;
  finally
    FL.Free;
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
  end;
end;

//############ Copy
procedure TFileList.CopyFiles(Target: string);
const
  BufferSize = 100 * 1024;
var
  FL: TEntryList;
  dirs, Files, NotCopied, i, NumBytes, AllBytes: Integer;
  Count,WrittenCount, AllNumBytes, SrcLock, DestLock, IsSame: LongInt;
  Buffer: PByte;
  Src, Dest: TFileStream;
  AllBytesStr: String;
  Msg, NewName: string;
  StartTime, CurTime: LongWord;
  Speed: Extended;
begin
  FL := TEntryList.Create;
  try
    Src := nil;
    Dest := nil;
    dirs := 0;
    Files := 0;
    NotCopied := 0;
    // make a list of all
    DoListOfSelectedFile(True, FL, dirs, files);
    if FL.Count > 0 then
    begin
      if AskQuestion('Copy ' + IfThen(dirs > 0, IntToStr(Dirs) + ' directories and ', '') + IntToStr(Files) + ' Files?') then
      begin
        //---------- copy one file
        if (FL.Count = 1) and (FL[0].EType = etFile) then
        begin
          StartProgress('Copy ', FL[0].Size);
          Buffer := AllocMem(BufferSize);
          try
            UpdateProgress(0, FL[0].Name);
            // check if the same file
            {$ifdef HASAMIGA}
            if FileExists(IncludeTrailingPathDelimiter(Target) + FL[0].Name) then
            begin
              SrcLock := Lock(IncludeTrailingPathDelimiter(Target) + FL[0].Name, SHARED_LOCK);
              DestLock := Lock(IncludeTrailingPathDelimiter(Target) + FL[0].Name, SHARED_LOCK);
              IsSame := SameLock(SrcLock, DestLock);
              Unlock(SrcLock);
              UnLock(DestLock);
              if IsSame = LOCK_SAME then
                raise Exception.Create('Cannot copy on itself');
            end;
            {$endif}
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
              if not UpdateProgress(NumBytes, 'Copy file ' + FormatSize(NumBytes) + '/' + AllBytesStr + ' with ' + FormatSize(Speed) + 'byte/s') then
                raise Exception.Create('copy stopped');
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
          StartProgress2('Copy ', FL.Count, AllBytes);
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
              if not UpdateProgress2(i + 1, AllNumBytes, 'Copy ' + FL[i].Name, FormatSize(AllNumBytes) + 'byte / ' + AllBytesStr + 'byte') then
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
                {$ifdef HASAMIGA}
                if FileExists(IncludeTrailingPathDelimiter(Target) + FL[0].Name) then
                begin
                  SrcLock := Lock(IncludeTrailingPathDelimiter(FCurrentPath) + FL[i].Name, SHARED_LOCK);
                  DestLock := Lock(NewName, SHARED_LOCK);
                  IsSame := SameLock(SrcLock, DestLock);
                  Unlock(SrcLock);
                  UnLock(DestLock);
                  if IsSame = LOCK_SAME then
                    raise Exception.Create('Cannot copy on itself');
                end;
                {$endif}
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
                  if not UpdateProgress2(i + 1, AllNumBytes, 'Copy ' + FL[i].Name, FormatSize(AllNumBytes) + 'byte /' + AllBytesStr + 'byte with ' + FormatSize(Speed) + 'byte/s') then
                    raise Exception.Create('copy stopped');
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
          FreeMem(Buffer);
        end;
        if NotCopied > 0 then
        begin
          if NotCopied = 1 then
            ShowMessage('Error: ' + Msg)
          else
            ShowMessage('Cannot copy ' + IntToStr(NotCopied) + ' files/dirs');
        end
        else
        begin
          for i := 0 to FFileList.Count - 1 do
            FFileList[i].Selected := False;
        end;
      end;
    end;
  finally
    FL.Free;
  end;
end;


initialization

end.

