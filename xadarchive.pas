unit xadarchive;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ArchiveUnit, xad, Utility;

type
  { TxadArchive }

  TxadArchive = class(TArchiveBase)
  private
    Ai: PxadArchiveInfo;
  protected
    function GetIsReadOnly: Boolean; override;
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
    class function Prio: LongInt; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TxadArchive }

function TxadArchive.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

function TxadArchive.ReadArchive(AFilename: string): Boolean;
var
  Err, i: LongInt;
  Fi: PxadFileInfo;
  Parts: TStringList;
  CDir: TArchiveDir;
  LastAEntry: Integer;
  AFName: string;
  AE: TArchiveEntry;
  IsDir: Boolean;
begin
  Result := False;
  if not Assigned(XADMasterBase) then
    Exit;
  xadFreeInfo(Ai);
  Err := xadGetInfo(Ai, [XAD_INFILENAME, AsTag(PChar(AFileName)), TAG_DONE]);
  if Err <> 0 then
    Exit;
  FArchiveName := AFilename;
  //
  Parts := TStringList.Create;
  Fi := Ai^.xai_FileInfo;
  while Assigned(Fi) do
  begin
    if (Fi^.xfi_Flags and XADFIF_LINK) <> 0 then
    begin
      Fi := Fi^.xfi_Next;
      Continue;
    end;
    AFName := Fi^.xfi_FileName;
    IsDir := (Fi^.xfi_Flags and XADFIF_DIRECTORY) <> 0;
    Parts.Clear;
    ExtractStrings(['/'], [], PChar(AFName), Parts);
    // go to dirs
    CDir := AD;
    LastAEntry := Parts.Count - 2;
    //if IsDir then
    //  LastAEntry := Parts.Count - 1;
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
      // get Size;
      TArchiveFile(AE).Size := fi^.xfi_Size;
      CDir.Entries.Add(AE);
    end;
    Fi := Fi^.xfi_Next;
  end;
  Parts.Free;
  Result := AD.Entries.Count > 0;
end;

function TxadArchive.PackFile(AFileName: string; FilePathInArchive: string): Boolean;
begin
  Result := False;
end;

function TxadArchive.ExtractFile(FilePathInArchive: string; DestFilename: string): Boolean;
var
  Fi: PxadFileInfo;
  Buffer: Pointer;
  FS: TFileStream;
begin
  Fi := Ai^.xai_FileInfo;
  while Assigned(Fi) do
  begin
    if (Fi^.xfi_Flags and XADFIF_LINK) <> 0 then
    begin
      Fi := Fi^.xfi_Next;
      Continue;
    end;
    if LowerCase(FilePathInArchive) = LowerCase(Fi^.xfi_FileName) then
    begin
      // found the file to extract
      Buffer := nil;
      FS := nil;
      try
        Buffer := AllocMem(Fi^.xfi_Size);
        Result := xadFileUnArc(Ai, [XAD_OUTMEMORY, AsTag(Buffer), XAD_ENTRYNUMBER, Fi^.xfi_EntryNumber, XAD_OUTSIZE, Fi^.xfi_Size, TAG_DONE]) = 0;
        if Result then
        begin
          FS := TFileStream.Create(DestFilename, fmCreate);
          FS.Write(Buffer^, Fi^.xfi_Size);
        end;
        Break;
      finally
        FS.Free;
        FreeMem(Buffer);
      end;
    end;
    Fi := Fi^.xfi_Next;
  end;
end;

function TxadArchive.DeleteFile(AFileName: string): Boolean;
begin
  Result := False;
end;

function TxadArchive.CreateDir(ADirName: string): Boolean;
begin
  Result := False;
end;

function TxadArchive.RenameFile(OldFile: string; NewFile: string): Boolean;
begin
  Result := False;
end;

function TxadArchive.RenameDir(OldDir: string; NewDir: string): Boolean;
begin
  Result := False;
end;

class function TxadArchive.FileIsArchive(AFileName: string): Boolean;
var
  AAi: PxadArchiveInfo;
  Err: LongInt;
begin
  Result := False;
  if not Assigned(XADMasterBase) then
    Exit;
  //
  AAi := PxadArchiveInfo(xadAllocObjectA(XADOBJ_ARCHIVEINFO, nil));
  Err := xadGetInfo(AAi, [XAD_INFILENAME, AsTag(PChar(AFileName)), TAG_DONE]);
  Result := Err = 0;
  xadFreeInfo(AAi);
  xadFreeObjectA(AAi, nil);
end;

class function TxadArchive.IsAvailable: Boolean;
begin
  Result := Assigned(XADMasterBase);
end;

class function TxadArchive.Prio: LongInt;
begin
  {$ifdef AROS}
  Result := 100; // higher than any other, becasue LHA on AROS is very limited not even possibnle to extract a single file
  {$else}
  Result := -100; // lower than ayn other any other, becasue it only can read, not write
  {$endif}
end;

constructor TxadArchive.Create;
begin
  inherited Create;
  Ai := PxadArchiveInfo(xadAllocObjectA(XADOBJ_ARCHIVEINFO, nil));
end;

destructor TxadArchive.Destroy;
begin
  inherited Destroy;
  if Assigned(Ai) then
  begin
    xadFreeInfo(Ai);
    xadFreeObjectA(Ai, nil);
  end;
  Ai := nil;
end;

initialization
  RegisterArchiver(TxadArchive);
end.

