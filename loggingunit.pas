unit LoggingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LogOut(Msg: string);

var
  LogEnabled: Boolean = False;

implementation

var
  SL: TStringList = nil;
  LogFilename: string;

procedure LogOut(Msg: string);
begin
  if not LogEnabled then
    Exit;
  if not Assigned(SL) then
    SL := TStringList.Create;
  SL.Add(Msg);
end;

initialization
  LogFileName := ChangeFileExt(ParamStr(0), '_' + FormatDateTime('YYYYMMDD_hhnnss', Now) + '.log');
  if not Assigned(SL) then
    SL := TStringList.Create;
finalization
  if LogEnabled then
  begin
    try
      SL.SaveToFile(LogFilename);
    except
      //
    end;
  end;
  SL.Free;

end.

