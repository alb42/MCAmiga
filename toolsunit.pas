unit toolsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Exec, AmigaDOS;
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
  // https://en.wikipedia.org/wiki/Code_page_437
  case c of
    // line 8
    #$C7: c := #128; // C
    #$FC: c := #129; // ue
    #$E9: c := #130; // e'
    #$E2: c := #131; // a^
    #$E4: c := #132; // ae
    #$E0: c := #133; // a`
    #$E5: c := #134; // a°
    #$e7: c := #135; // c
    #$ea: c := #136; // e^
    #$eb: c := #137; // ee
    #$E8: c := #138; // e`
    #$ef: c := #139; // ie
    #$ee: c := #140; // i^
    #$ec: c := #141; // i`
    #$C4: c := #142; // AE
    // line 9
    #$C9: c := #144; // Ee
    #$e6: c := #145; // a-e
    #$c6: c := #146; // A-E
    #$F4: c := #147; // o^
    #$F6: c := #148; // oe
    #$F2: c := #149; // o`
    #$FB: c := #150; // u^
    #$F9: c := #151; // u`
    #$FF: c := #152; // ye
    #$D6: c := #153; // OE
    #$DC: c := #154; // UE
    #$A2: c := #155; // cent
    #$A3: c := #156; // Pound
    #$A5: c := #157; // Yen
    // line A
    #$E1: c := #160; // a'
    #$ED: c := #161; // i'
    #$F3: c := #162; // o'
    #$FA: c := #163; // u'
    #$F1: c := #164; // n~
    #$D1: c := #165; // N~
    // line E
    #$DF: c := #225; // sz

  end;
end;

procedure ConvertCharBack(var c: Char); inline;
begin
  case c of
    // line 8
    #128: c := #$C7; // C
    #129: c := #$FC; // ue
    #130: c := #$E9; // e'
    #131: c := #$E2; // a^
    #132: c := #$E4; // ae
    #133: c := #$E0; // a`
    #134: c := #$E5; // a°
    #135: c := #$e7; // c
    #136: c := #$ea; // e^
    #137: c := #$eb; // ee
    #138: c := #$E8; // e`
    #139: c := #$ef; // ie
    #140: c := #$ee; // i^
    #141: c := #$ec; // i`
    #142: c := #$C4; // AE
    // line 9
    #144: c := #$C9; // Ee
    #145: c := #$e6; // a-e
    #146: c := #$c6; // A-E
    #147: c := #$F4; // o^
    #148: c := #$F6; // oe
    #149: c := #$F2; // o`
    #150: c := #$FB; // u^
    #151: c := #$F9; // u`
    #152: c := #$FF; // ye
    #153: c := #$D6; // OE
    #154: c := #$DC; // UE
    #155: c := #$A2; // cent
    #156: c := #$A3; // Pound
    #157: c := #$A5; // Yen
    // line A
    #160: c := #$E1; // a'
    #161: c := #$ED; // i'
    #162: c := #$F3; // o'
    #163: c := #$FA; // u'
    #164: c := #$F1; // n~
    #165: c := #$D1; // N~
    // line E
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
      SysUtils.CreateDir(Name);
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
  ib: TFileInfoBlock;
  DirLock: BPTR;
begin
  Result := -1;
  DirLock := Lock(PChar(Name), ACCESS_READ);
  if NativeInt(DirLock) = 0 then
    Exit;
  if LongBool(Examine(DirLock, @ib)) then
    Result := ib.fib_Size;
  Unlock(DirLock);
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

