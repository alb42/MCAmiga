unit toolsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
// recursively create the directories
procedure CreateAllDir(AName: string);
// Recursively Delete a complete Temp directory (contents and directory)
procedure DeleteAll(TempName: string);

function OverwriteText(Src, dest: string): string;

procedure ConvertChar(var c: Char); inline;
procedure ConvertCharBack(var c: Char); inline;

function IsExecutable(AFilename: string): Boolean;

implementation

const
  // Magic of Amiga exe (first 4 bytes), check before start
  {$ifdef Amiga68k}
  ExecStart: array of Byte = ($00, $00, $03, $F3);
  {$else}
  ExecStart: array of Byte = ($7F, $45, $4c, $46);
  {$endif}


function IsExecutable(AFilename: string): Boolean;
var
  Magic: array of Byte;
  FS: TFileStream;
begin
  Result := False;
  FS := nil;
  try
    SetLength(Magic, Length(ExecStart));
    FillChar(Magic[0], Length(Magic), 0);
    //
    FS := TFileStream.Create(AFileName, fmOpenRead);
    FS.Read(Magic[0], Length(Magic));
    Result := CompareMem(@Magic[0], @ExecStart[0], Length(ExecStart));
  finally
    FS.Free;
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

procedure ConvertCharBack(var c: Char); inline;
begin
  case c of
    #128: c := #$C7; // C
    #129: c := #$FC; // ue
    #154: c := #$DC; // UE
    #132: c := #$E4; // ae
    #142: c := #$C4; // AE
    #148: c := #$F6; // oe
    #153: c := #$D6; // OE
    #225: c := #$DF; // sz
  end;
end;

procedure CreateAllDir(AName: string);
var
  SL: TStringList;
  Name: string;
  i: Integer;
begin
  SL := TStringList.Create;
  ExtractStrings(['/'], [], PChar(AName), SL);
  Name := '';
  for i := 0 to SL.Count - 1 do
  begin
    if Name = '' then
      Name := SL[i]
    else
      Name := IncludeTrailingPathDelimiter(Name) + SL[i];
    if not FileExists(Name) then
      CreateDir(Name);
  end;
  SL.Free;
end;

procedure DeleteAll(TempName: string);
var
  FindInfo: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(TempName) + '*', faAnyFile and faDirectory, FindInfo) = 0 then
  begin
    repeat
      if (FindInfo.Attr and faDirectory) <> 0 then
        DeleteAll(IncludeTrailingPathDelimiter(TempName) + FindInfo.Name)
      else
      begin
        DeleteFile(IncludeTrailingPathDelimiter(TempName) + FindInfo.Name);
      end;
    until FindNext(FindInfo) <> 0;
    FindClose(FindInfo);
  end;
  DeleteFile(Tempname);
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
    Result := -1;
end;

function OverwriteText(Src, dest: string): string;
var
  fileDate: Int64;
  Size: Int64;
begin
  Result := '';
  if Src <> '' then
  begin
    Result := '     New:      ';
    fileDate := FileAge(Src);
    if fileDate > -1 then
      Result := Result + FormatDateTime('YYYY-MM-DD hh:nn:ss', FileDateToDateTime(fileDate))
    else
      Result := '               ';
    Size := GetFileSize(Src);
    if Size >= 0 then
      Result := Result + '    ' + IntToStr(Size) + ' bytes     '#13#10
    else
      Result := '';
  end;

  if dest <> '' then
  begin
    Result := Result + '     Existing: ';
    fileDate := FileAge(dest);
    if fileDate > -1 then
      Result := Result + FormatDateTime('YYYY-MM-DD hh:nn:ss', FileDateToDateTime(fileDate))
    else
      Result := '               ';
    Result := Result + '    ' + IntToStr(GetFileSize(dest)) + ' bytes     '#13#10;
  end;
end;

end.

