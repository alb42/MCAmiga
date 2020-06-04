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

implementation

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

