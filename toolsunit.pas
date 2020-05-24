unit toolsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
// recursively create the directories
procedure CreateAllDir(AName: string);
// Recursively Delete a complete Temp directory (contents and directory)
procedure DeleteAll(TempName: string);

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

end.

