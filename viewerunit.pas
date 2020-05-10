unit viewerunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video, Keyboard, Math,
  EventUnit;

procedure FileViewer(AFileName: string);

implementation

uses
  FileListUnit;

type

  { TFileViewer }

  TFileViewer = class
  private
    FileName: string;
    FStartLine: Integer;
    OrigText: TStringList;
    ShownText: TStringList;
    function PollNextKey: TKeyEvent;
    procedure FormatText;
    procedure Paint;
    procedure SetStartLine(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(AFilename: string);
    property StartLine: Integer read FStartLine write SetStartLine;
  end;

procedure FileViewer(AFileName: string);
begin
  With TFileViewer.Create do
  begin
    Execute(AFileName);
    Free;
  end;
end;

{ TFileViewer }

function TFileViewer.PollNextKey: TKeyEvent;
begin
  Result := GetNextKeyEvent;
  if Result = ResizeKey then
  begin
    FormatText;
    Paint;
  end;
end;

procedure TFileViewer.FormatText;
var
  i, p: Integer;
  s: String;
begin
  ShownText.Clear;
  ShownText.Assign(OrigText);
  i := 0;
  while i < ShownText.Count do
  begin
    s := ShownText[i];
    if Length(s) > ScreenWidth then
    begin
      P := LastDelimiter(' ', Copy(s, 1, ScreenWidth));
      if p <= 1 then
        p := ScreenWidth;
      ShownText[i] := Copy(s, 1, p);
      Delete(s, 1, p);
      ShownText.Insert(i + 1, s);
    end;
    Inc(i);
  end;
end;

procedure TFileViewer.Paint;
var
  i, l, j: Integer;
  s: String;
begin
  BGPen := Cyan;
  FGPen := Black;

  for i := 0 to ScreenWidth - 1 do
  begin
    SetChar(i, 0, ' ');
  end;

  SetText(0, 0, ExtractFileName(FileName));

  s :=  IntToStr(StartLine + 1) + ' - ' + IntToStr(Min(ShownText.Count, (StartLine + 1) + (ScreenHeight - 2))) + '/' + IntToStr(ShownText.Count);
  SetText(ScreenWidth - Length(s), 0, s);

  BGPen := Blue;
  FGPen := White;
  for i := 0 to ScreenHeight - 2 do
  begin
    if i + StartLine < ShownText.Count then
    begin
      s := ShownText[i + StartLine];
      l := Length(s);
      SetText(0, i + 1, s);
    end
    else
      l := 0;
    for j := l to ScreenWidth do
      SetChar(j, i + 1, ' ');
  end;
  UpdateScreen(False);
end;

procedure TFileViewer.SetStartLine(AValue: Integer);
begin
  if FStartLine = AValue then Exit;
  FStartLine := Max(AValue, 0);
  FStartLine := Min(FStartLine, Max(ShownText.Count - ScreenHeight + 1, 0));
  Paint;
end;

constructor TFileViewer.Create;
begin
  OrigText := TStringList.Create;
  ShownText := TStringList.Create;
end;

destructor TFileViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TFileViewer.Execute(AFilename: string);
var
  Key: TKeyEvent;
begin
  FileName := AFilename;
  OrigText.Clear;
  OrigText.LoadFromFile(AFileName);
  FStartLine := 0;
  FormatText;
  Paint;
  //
  repeat
    Key := PollNextKey;
    case (TranslateKeyEvent(Key) and $FFFF) of
      kbdUp: StartLine := StartLine - 1;
      kbdDown: StartLine := StartLine + 1;
      kbdPgUp, $39: StartLine := StartLine - 10; // pg up -> Move around
      kbdPgDn, $33: StartLine := StartLine + 10; // pg down -> Move around
      kbdHome, $37: StartLine := 0;                      // Home -> Move around
      kbdEnd, $31: StartLine := MaxInt;                  // end -> Move around
      kbdF10: Break;
    end;
    Sleep(25);
  until (Key and $ff00) = $0100;
end;

end.

