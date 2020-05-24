unit archiveunit;

{$mode objfpc}{$H+}

interface

uses
  AmigaDos, Exec, Utility,
  Classes, SysUtils, fgl;

type
  TArchiveEntry = class
  public
    Name: string;
  end;

  TArchiveFile = class(TArchiveEntry)
    Size: LongWord;
  end;

  TArchiveDirList = specialize TFPGObjectList<TArchiveEntry>;

  { TArchiveDir }

  TArchiveDir = class(TArchiveEntry)
  public
    Entries: TArchiveDirList;
    constructor Create; virtual;
    destructor Destroy; override;
    function EntryByName(AName: string): TArchiveEntry;
  end;


  { TArchiveBase }

  TArchiveBase = class
  protected
    FArchiveName: string;
  public
    AD: TArchiveDir;
    constructor Create; virtual;
    destructor Destroy; override;
    //
    function ReadArchive(AFilename: string): Boolean; virtual; abstract;
    function PackFile(AFileName: string; FilePathInArchive: string): Boolean; virtual; abstract;
    function ExtractFile(FilePathInArchive: string; DestFilename: string): Boolean; virtual; abstract;
    function DeleteFile(AFileName: string): Boolean; virtual; abstract;
    function CreateDir(ADirName: string): Boolean; virtual; abstract;
    function RenameFile(OldFile: string; NewFile: string): Boolean; virtual; abstract;
    function RenameDir(OldDir: string; NewDir: string): Boolean; virtual; abstract;
    //
    function RescanArchive: Boolean; virtual;
    //
    class function FileIsArchive(AFileName: string): Boolean; virtual;
    class function IsAvailable: Boolean; virtual; abstract;

    property ArchiveName: string read FArchiveName;
  end;

  TArchiveClass = class of TArchiveBase;

  { TLHAArchive }

  TLHAArchive = class(TArchiveBase)
  public
    function ReadArchive(AFilename: string): Boolean; override;
    function PackFile(AFileName: string; FilePathInArchive: string): Boolean; override;
    function ExtractFile(FilePathInArchive: string; DestFilename: string): Boolean; override;
    function DeleteFile(AFileName: string): Boolean; override;
    function CreateDir(ADirName: string): Boolean; override;
    function RenameFile(OldFile: string; NewFile: string): Boolean; override;
    function RenameDir(OldDir: string; NewDir: string): Boolean; override;

    class function FileIsArchive(AFileName: string): Boolean; override;
    class function IsAvailable: Boolean; override;
  end;


// Search for the archiver class for this archive
function GetArchiver(AFilename: string): TArchiveClass;


implementation
uses
  toolsunit;


var
  AvailableArchivers: array of TArchiveClass;

procedure RegisterArchiver(ArchiveClass: TArchiveClass);
var
  Idx: LongInt;
begin
  if ArchiveClass.IsAvailable then
  begin
    Idx := Length(AvailableArchivers);
    SetLength(AvailableArchivers, Idx + 1);
    AvailableArchivers[Idx] := ArchiveClass;
  end;
end;

function GetArchiver(AFilename: string): TArchiveClass;
var
  i: LongInt;
begin
  for i := 0 to High(AvailableArchivers) do
  begin
    if AvailableArchivers[i].FileIsArchive(AFilename) then
    begin
      Result := AvailableArchivers[i];
      Break;
    end;
  end;
end;

{ TLHAArchive }

function TLHAArchive.ReadArchive(AFilename: string): Boolean;
var
  TempName, Cmd, AFName, s: string;
  Outfile: BPTR;
  SL, Parts: TStringList;
  CurPos, i, LastAEntry: LongInt;
  IsDir: Boolean;
  CDir: TArchiveDir;
  AE: TArchiveEntry;
begin
  AD.Entries.Clear;
  //
  FArchiveName := AFilename;
  //
  TempName := GetTempFileName('t:', 'mcfile');
  Outfile := DOSOpen(PChar(TempName), MODE_NEWFILE);
  cmd := 'c:lha vv "' + FArchiveName + '"';
  Systemtags(PChar(cmd), [SYS_Output, AsTag(Outfile), TAG_END]);
  DosClose(OutFile);
  SL := TStringList.Create;
  SL.LoadFromFile(TempName);
  DOSDeleteFile(PChar(TempName));
  //search for start
  CurPos := -1;
  for i := 0 to SL.Count - 1 do
  begin
    if Pos('Listing of archive', SL[i]) = 1 then
      CurPos := i;
  end;
  if CurPos < 0 then
  begin
    writeln('listing not found');
    writeln(cmd);
    writeln(SL.Text);
    Exit;
  end;
  CurPos := CurPos + 1;
  if Pos('Original', SL[CurPos]) < 0 then
  begin
    writeln('Original not found');
    writeln(SL.Text);
    Exit;
  end;
  CurPos := CurPos + 2; // jump over -----

  Parts := TStringList.Create;
  while CurPos < SL.Count do
  begin
    AFName := Trim(SL[CurPos]);
    if AFName[1] = ':' then
    begin
      CurPos := CurPos + 1;
      AFName := Trim(SL[CurPos]);
    end;
    if (AFName = '') or (Pos('----', AFName) = 1) then
      Break;
    IsDir := AFName[Length(AFName)] = '/';
    //writeln('check "', AFName + '"', IsDir);
    CurPos := CurPos + 1;
    Parts.Clear;
    ExtractStrings(['/'], [], PChar(AFName), Parts);
    // go to dirs
    CDir := AD;
    LastAEntry := Parts.Count - 2;
    if IsDir then
      LastAEntry := Parts.Count - 1;
    for i := 0 to LastAEntry do
    begin
      AE := CDir.EntryByName(Parts[i]);
      if not Assigned(AE) then
      begin
        AE := TArchiveDir.Create;
        AE.Name := Parts[i];
        CDir.Entries.Add(AE);
      end;
      //
      if AE is TArchiveFile then
      begin
        //how this is possible?
        writeln('how is that possible? ', AE.Name, ' is a file, but should be dir');
        Result := False;
        AD.Entries.Clear;
        Parts.Free;
        Exit;
      end;
      if AE is TArchiveDir then
        CDir := TArchiveDir(AE);
    end;
    if not IsDir then
    begin
      AE := TArchiveFile.Create;
      AE.Name := Parts[Parts.Count - 1];
      if CurPos >= SL.Count then
        Exit;
      // get Size;
      s := trim(SL[CurPos]);
      s := Copy(s, 1, Pos(' ', s) - 1);
      TArchiveFile(AE).Size := StrToIntDef(s, 0);
      CDir.Entries.Add(AE);
    end;
    CurPos := CurPos + 1;
  end;
  Parts.Free;
  Result := AD.Entries.Count > 0;
end;

function TLHAArchive.PackFile(AFileName: string; FilePathInArchive: string): Boolean;
const
  BufferSize = 1024 * 1024;
var
  TempName: string;
  Path, cmd: string;
  SFile, DFile: TFileStream;
  Buffer: Pointer;
  BytesRead: LongInt;
  SL: TStringList;
begin
  //
  Result := False;
  SFile := nil;
  DFile := nil;
  Buffer := nil;
  SL := nil;
  try
    DeleteFile(FilePathInArchive);
    //
    TempName := GetTempFileName('t:', 'mcdir');
    Path := ExtractFilePath(FilePathInArchive);
    CreateAllDir(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(TempName) + Path));
    //
    SFile := TFileStream.Create(AFilename, fmOpenRead);
    DFile := TFileStream.Create(IncludeTrailingPathDelimiter(TempName) + FilePathInArchive, fmCreate);
    //
    Buffer := AllocMem(BufferSize);
    repeat
      BytesRead := SFile.Read(Buffer^, BufferSize);
      DFile.Write(Buffer^, BytesRead);
    until BytesRead = 0;
    FreeAndNil(SFile);
    FreeAndNil(DFile);
    //
    SL := TStringList.Create;
    SL.Add('cd ' + TempName);
    SL.Add('c:lha -x1 a "' + FArchiveName + '" "' + FilePathInArchive + '"');
    SL.SaveToFile(IncludeTrailingPathDelimiter(TempName) + 'cmdadd');
    FreeAndNil(SL);
    //
    cmd := 'c:execute ' + IncludeTrailingPathDelimiter(TempName) + 'cmdadd';
    SystemTags(PChar(cmd), [TAG_END]);
    //
    Result := True;
    //
  finally
    DeleteAll(TempName);
    FreeMem(Buffer);
    SFile.Free;
    DFile.Free;
    SL.Free;
  end;
end;

function TLHAArchive.ExtractFile(FilePathInArchive: string; DestFilename: string): Boolean;
const
  BufferSize = 1024 * 1024;
var
  SL :TStringList;
  DestPath, TempName, cmd: string;
  SFile, DFile: TFileStream;
  Buffer: Pointer;
  BytesRead: LongInt;
begin
  Result := False;
  SL := nil;
  Buffer := nil;
  DestPath := ExtractFilePath(DestFilename);
  try
    CreateAllDir(DestPath);
    //
    TempName := GetTempFileName('t:', 'mcdir');

    CreateAllDir(TempName);

    SL := TStringList.Create;
    SL.Add('cd ' + TempName);
    SL.Add('c:lha -x1 x "' + FArchiveName + '" "' + FilePathInArchive + '" "' + TempName  + '"');
    SL.SaveToFile(IncludeTrailingPathDelimiter(TempName) + 'cmdadd');
    FreeAndNil(SL);
    //
    cmd := 'c:execute ' + IncludeTrailingPathDelimiter(TempName) + 'cmdadd';
    SystemTags(PChar(cmd), [TAG_END]);

    SFile := TFileStream.Create(IncludeTrailingPathDelimiter(TempName) + FilePathInArchive, fmOpenRead);
    DFile := TFileStream.Create(DestFilename, fmCreate);
    //
    Buffer := AllocMem(BufferSize);
    repeat
      BytesRead := SFile.Read(Buffer^, BufferSize);
      DFile.Write(Buffer^, BytesRead);
    until BytesRead = 0;
    FreeAndNil(SFile);
    FreeAndNil(DFile);
    //
  finally
    FreeMem(Buffer);
    DeleteAll(TempName);
    SFile.Free;
    DFile.Free;
    SL.Free;
  end;
end;

function TLHAArchive.DeleteFile(AFileName: string): Boolean;
var
  cmd: string;
begin
  cmd := 'c:lha d "' + FArchiveName + '" "' + AFileName + '"';
  SystemTags(PChar(cmd), [TAG_END]);
  Result := True;
end;

function TLHAArchive.CreateDir(ADirName: string): Boolean;
var
  SL: TStringList;
  TempName, NewDir, cmd: string;
begin
  Result := False;
  TempName := GetTempFileName('t:', 'mcdir');
  SL := nil;
  try
    NewDir := IncludeTrailingPathDelimiter(TempName) + ADirName;
    CreateAllDir(NewDir);

    SL := TStringList.Create;
    SL.Add('');
    SL.SaveToFile(IncludeTrailingPathDelimiter(NewDir) + 'delete_me');
    //
    SL.Clear;
    SL.Add('cd ' + TempName);
    SL.Add('c:lha -e -x1 a "' + FArchiveName + '" "' + IncludeTrailingPathDelimiter(NewDir) + 'delete_me' + '"');
    SL.SaveToFile(IncludeTrailingPathDelimiter(TempName) + 'cmdadd');
    FreeAndNil(SL);
    //
    cmd := 'c:execute ' + IncludeTrailingPathDelimiter(TempName) + 'cmdadd';
    SystemTags(PChar(cmd), [TAG_END]);
    Result := True;
  finally
    SL.Free;
    DeleteAll(NewDir);
  end;
end;

function TLHAArchive.RenameFile(OldFile: string; NewFile: string): Boolean;
var
  TempName, cmd: string;
  SL: TStringList;
begin
  //
  TempName := GetTempFileName('t:', 'mcdir');
  SL := nil;
  try
    CreateAllDir(TempName);

    SL := TStringList.Create;
    SL.Add('cd ' + TempName);
    SL.Add('c:lha x "' + FArchiveName + '" "' + OldFile + '" ' + '"' + TempName + '"');
    SL.Add('rename "' + OldFile + '" "' + NewFile + '"');
    SL.Add('c:lha d "' + FArchiveName + '" "' + OldFile + '"');
    SL.Add('c:lha a "' + FArchiveName + '" "' + NewFile + '"');
    SL.SaveToFile(IncludeTrailingPathDelimiter(TempName) + 'cmdadd');
    FreeAndNil(SL);
    //
    cmd := 'c:execute ' + IncludeTrailingPathDelimiter(TempName) + 'cmdadd';
    SystemTags(PChar(cmd), [TAG_END]);
    //
    Result := True;
  finally
    SL.Free;
    //DeleteAll(TempName);
  end;
end;

function TLHAArchive.RenameDir(OldDir: string; NewDir: string): Boolean;
var
  TempName, cmd: string;
  SL: TStringList;
begin
  //
  TempName := GetTempFileName('t:', 'mcdir');
  SL := nil;
  try
    CreateAllDir(TempName);

    SL := TStringList.Create;
    SL.Add('cd ' + TempName);
    SL.Add('c:lha x "' + FArchiveName + '" "' + IncludeTrailingPathDelimiter(OldDir) + '#?" ' + '"' + TempName + '"');
    SL.Add('rename "' + OldDir + '" "' + NewDir + '"');
    SL.Add('c:lha d "' + FArchiveName + '" "' + IncludeTrailingPathDelimiter(OldDir) + '#?"');
    SL.Add('c:lha a "' + FArchiveName + '" "' + IncludeTrailingPathDelimiter(NewDir) + '#?"');
    SL.SaveToFile(IncludeTrailingPathDelimiter(TempName) + 'cmdadd');
    FreeAndNil(SL);
    //
    cmd := 'c:execute ' + IncludeTrailingPathDelimiter(TempName) + 'cmdadd';
    SystemTags(PChar(cmd), [TAG_END]);
    //
    Result := True;
  finally
    SL.Free;
    //DeleteAll(TempName);
  end;
end;

class function TLHAArchive.FileIsArchive(AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  Result := (Ext = '.lha') or (Ext = '.lzh');
end;

class function TLHAArchive.IsAvailable: Boolean;
begin
  Result := FileExists('c:lha');
end;

{ TArchiveBase }

constructor TArchiveBase.Create;
begin
  AD := TArchiveDir.Create;
end;

destructor TArchiveBase.Destroy;
begin
  AD.Free;
  inherited Destroy;
end;

function TArchiveBase.RescanArchive: Boolean;
begin
  Result := ReadArchive(FArchiveName);
end;

class function TArchiveBase.FileIsArchive(AFileName: string): Boolean;
begin
  Result := False;
end;


{ TArchiveDir }

constructor TArchiveDir.Create;
begin
  Entries := TArchiveDirList.Create(True);
end;

destructor TArchiveDir.Destroy;
begin
  Entries.Free;
  inherited Destroy;
end;

function TArchiveDir.EntryByName(AName: string): TArchiveEntry;
var
  i: Integer;
begin
  Result := nil;
  AName := Lowercase(AName);
  for i := 0 to Entries.Count - 1 do
  begin
    if LowerCase(Entries[i].Name) = AName then
    begin
      Result := Entries[i];
      Exit;
    end;
  end;
end;

initialization
  RegisterArchiver(TLHAArchive);

end.

