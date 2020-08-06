unit dialogunit;

{$mode objfpc}{$H+}

interface

uses
  Utility, AmigaDos, FileListUnit, Intuition,
  Types, Classes, SysUtils, Video, Keyboard, Mouse, Math, EventUnit;

const
  NumVERSION = '0.9';
  VERSION = '$VER: MCAmiga 0.9 (06.08.2020)';

type
                //yes     no,    yes to All    No to all      Abort   None
  TDialogResult=(mrOK, mrCancel, mrAll,         mrNoAll,    mrAbort, mrView, mrEdit, mrAgain, mrNone);

  { TBaseDialog }

  TBaseDialog = class
  protected
    Mid: TPoint;  // middle Point of the screen
    WindowRect: TRect; // Outer rectangle of the dialog
    InnerRect: TRect;  // inner rectangle of the dialog
    procedure ProcessMouse(MouseEvent: TMouseEvent); virtual; // process mouse event
  protected
    function PollNextKey: TKeyEvent; virtual; // get next Key event or 0 if no key is pressed
    procedure DrawButtons; virtual; abstract; // Draw buttons if buttons avail
    procedure DrawWindowBorder; virtual;      // Draw the border of the dialog, set Windowrect first
    procedure Paint; virtual;                 // Draw the dialog
  public
    function Execute: TDialogResult; virtual; abstract; // Start dialog and return the number of the Button pressed
  end;

  { TShowMessage }
  // a stndard message to the user with an OK Button, als used as base class for
  // all other messages with a text and some buttons
  TShowMessage = class(TBaseDialog)
  protected
    WithScroll: Boolean;           // has a scroll bar
    TopLine: LongInt;              // top visible line of the text
    SelectedButton: Integer;       // currently selected button, Index in ButtonsArray
    ButtonsArray: array of record  // all Buttons of the Dialog, use Configure Buttons to set
      Rect: TRect;                   // ractangle of the button
      Pressed: Boolean;              // after GetNextKeyEvent, check if this button was pressed during the event
      Title: string;                 // text of the Button
      Result: TDialogResult;         // Result of to be set for Execute when Button is pressed
    end;
    procedure DrawButtons; override;     // redraw all Buttons (w.g. when changed focus)
    procedure ConfigureButtons; virtual; // set properties of all Buttons
    procedure Paint; override;           // Draw the window
    procedure ProcessMouse(MouseEvent: TMouseEvent); override; // check if the mouse clicked on something
  public
    Text: string; // set before Execute!
    function Execute: TDialogResult; override; // returns the number of the Button pressed
  end;

  { TNonWaitMessage }
  // standard text message without a button, which closes itself after the
  // action is done, for example, start en external program with blocking
  TNonWaitMessage = class(TShowMessage)
  protected
    procedure DrawButtons; override;
  public
    function Execute: TDialogResult; override;
  end;

  { TAskQuestion }
  // Dialog asking a Yes/No Question
  TAskQuestion = class(TShowMessage)
  protected
    procedure ConfigureButtons; override;
  end;

  { TAskMultipleQuestion }
  // Dialog Asking an Yes/Yes To All/No/No to All/Cancel question (overwrite file)
  TAskMultipleQuestion = class(TAskQuestion)
  protected
    procedure ConfigureButtons; override;
  end;

  { TAskForName }
  // ask for a text input
  TAskForName = class(TAskQuestion)
  private
     TxtL, TxtR: LongInt;
  protected
     //procedure DrawButtons; override;
  protected
    procedure Paint; override; // Draw the window
    function IsValidChar(c: Char): Boolean; virtual;
    function ValidInput(s: string): Boolean; virtual;
  public
    NewName: string;
    AsName: Boolean;
    function Execute: TDialogResult; override; // returns the number of the Button pressed
  end;

  { TAskForNumber }
  // ASk for a number (e.g. jump to line)
  TAskForNumber = class(TAskForName)
  protected
    function IsValidChar(c: Char): Boolean; override;
    function ValidInput(s: string): Boolean; override;
  public
    HexNumber: Boolean;
  end;

  { TSingleProgress }
  // Progress bar with one text + progressbar and a cancel button
  // usual way to use
  // .Create
  // Set .MaxValue
  // Set .Text
  // .Execute
  // in the loop .UpdateValue
  // .Free
  TSingleProgress = class(TBaseDialog)
  protected
    LastCall: LongWord;  // when it was last called, prevents too frequent redraws
    CurValue: LongWord;
    Pup: LongInt;        // Postion saves to find the position of text and progressbar
    PGL: LongInt;
    PGR: LongInt;
    CancelPressed: Boolean;
  protected
    procedure DrawButtons; override;
    procedure ProcessMouse(MouseEvent: TMouseEvent); override; // click to ok
  public
    MaxValue: LongWord;  // must be set before call Execute, last Value of the Progressbar
    Text: string;        // initial Text above the progress bar
    procedure Paint; override;
    function Execute: TDialogResult; override; // Init and start progressbar
    function UpdateValue(AValue: LongWord; NText: string = ''): Boolean; virtual; // Update Progress value and text, if Text Empty, do not change the current text
  end;

  { TDoubleProgress }
  // progressbar with a text a progress bar a text another progressbar and a cancel button
  TDoubleProgress = class(TSingleProgress)
  protected
    CurValue2: LongWord;
  public
    MaxValue2: LongWord; // set before execute, Indicies 2 means the lower progress bar, the inherited without index are the upper progressbar and text
    Text2: string;
    function Execute: TDialogResult; override; // Start and init the progressbars
    procedure Paint; override;
    function UpdateValue(AValue: LongWord; NText: string = ''): Boolean; override; // only update the upper bar
    function UpdateValue2(AValue1: LongWord; NText1: string = ''; AValue2: LongWord = 0; NText2: string = ''): Boolean; // update both bars
  end;

  { TToolsMenu }

  TToolsEvent = procedure of object; // event when a tools entry is selected

  // the tools menu
  TToolsMenu = class(TBaseDialog)
  private
    Finished: Boolean;      // true -> close the tools menu
    CurrentEntry: LongInt;  // currently selected entry
    Tools: array of record  // list of Tools in the menu (max 10 until now)
      AName: string;        // Text for the entry
      Event: TToolsEvent;   // event to be called when menu entry is selected
    end;
    MaxLen: Integer;        // Max length of text of all Tool Entries, for easier drawing and dialog size determination
    procedure AddToolsEntry(Name: string; Event: TToolsEvent); // add a new tool to the menu
    // events
    procedure LhaPackEvent;    // pack selected files as lha to dest
    procedure LzxPackEvent;    // pack selected files as lzx to dest
    procedure ShellEvent;      // Open a new shell
    procedure StartProgEvent;  // start a program with the current file as argument
    procedure UnpackArchive;   // Unpack current archive to dest
    procedure SearchStart;     // open the search requester
    procedure SelectInfo;
    procedure DiffViewer;
  protected
    procedure ProcessMouse(MouseEvent: TMouseEvent); override;
    procedure DrawButtons; override;
    procedure Paint; override;
  public
    DestP, SrcP: TFileList; // Links to both FilePanels, set before Execute
    constructor Create; virtual;
    function Execute: TDialogResult; override; // Start the tools menu
  end;

// Shortcuts for easy use of the dialogs

// ask a Yes/No Question
function AskQuestion(AText: string): Boolean; // yes = true, no = False;

// ask a Yes/YesToAll/No/NoToAll/Cancel Question, (e.g. overwrite message)
function AskMultipleQuestion(AText: string): TDialogResult;

// ask for a name, useasName = True Limit input chars to valid file/dir names
function AskForName(AText: string; var ANewName: string; UseAsName: Boolean = True): Boolean;

// ask for a integer number (e.g go to line)
function AskForNumber(AText: string; var ANewNumber: Integer): Boolean;

// ask for a hexadeciaml number (0..9,A..F)
function AskForHexNumber(AText: string; var HexString: string): Boolean;

// Show help Text
procedure ShowHelp;
// Show Viewer Help TExt
procedure ShowViewHelp;
// Simple message
procedure ShowMessage(AText: string);
// message without button, returns directly, for blocking actions (e.g. starting external program)
procedure NonWaitMessage(AText: string);

// Show tools menu
procedure ShowTools(SrcPanel, DestPanel: TFileList);

// constants for dialog borders and contents drawing
const
  URCorner = #191;
  LLCorner = #192;
  ULCorner = #218;
  LRCorner = #217;
  HLine = #196;
  VLine = #179;

  RBorder = #195;
  LBorder = #180;

  ProgressEmpty = #176;
  //ProgressHalf = #221;
  ProgressFull = #219;

  ArrowUp = #24;
  ArrowDown = #25;


var
  DefaultShell: string = ''; // default shell variable (e.g. CON:0/0/100/200/CLOSE) set by ToolType

implementation

uses
  ArchiveUnit, searchunit, ToolsUnit, diffviewerunit;

// Help text for the main application! MaxLength 70
const       //.........1.........2.........3.........4.........5........6.........7.........8
  HelpText = '     MyCommander Amiga Version ' + NumVERSION + ' '+{$INCLUDE %FPCTARGETCPU%} + '-' + {$INCLUDE %FPCTARGETOS%} +'  '#13#10 +
             '   =================================================  '#13#10 +
             ' F1 - Help               Alt F1/F2 - Show Drives'#13#10 +
             ' F2 - Tools menu         Ins  - Select File'#13#10 +
             ' F3 - View (Shift alt.)  +    - Select by pattern'#13#10 +
             ' F4 - Edit (Shift alt.)  -    - Deselect by pattern'#13#10 +
             ' F5 - Copy               TAB  - Switch Focus'#13#10 +
             ' F6 - Move               Shift F6 - Rename'#13#10 +
             ' F7 - Create Directory   Backspace - Parent'#13#10 +
             ' F8 - Delete Files       Shift + Enter - Open Dir in other side'#13#10 +
             ' F10/ESC - Quit Program'#13#10 +
             ' Ctrl + R - Rescan Directory'#13#10 +
             ' Ctrl + O - Set Destination Directory to Source Directory'#13#10 +
             ' Ctrl + S - type to find entry in current directory'#13#10 +
             ' Ctrl + F - Toggle bottom menu'#13#10 +
             ' Cursor Keys,4,6,8,2                   - navigate'#13#10  +
             ' Ctrl Cursor Up,Down,Pg Up,9,Pg Down,3 - fast navigation'#13#10 +
             ' Ctrl Cursor Left,Right,Home,7,End,1   - Jump to start/end'#13#10;

// Help Text for the Viewer
const           //.........1.........2.........3.........4.........5........6.........7
  HelpViewText = ' ---- Viewer Help -----  '#13#10 +
                 ' F1         - Help     '#13#10 +
                 ' F3/F10/ESC - Leave Viewer'#13#10 +
                 ' F4         - Toggle ASCII and Hex View '#13#10 +
                 ' F5         - Jump to line or address'#13#10 +
                 ' F7         - Search'#13#10 +
                 ' Shift F7   - Search again'#13#10 +
                 ' Cursor Keys,4,6,8,2                   - navigate in Text/Hex'#13#10  +
                 ' Ctrl Cursor Up,Down,Pg Up,9,Pg Down,3 - fast navigation'#13#10 +
                 ' Ctrl Cursor Left,Right,Home,7,End,1   - Jump to start/end'#13#10;

procedure ShowHelp;
begin
  with TShowMessage.Create do
  begin
    Text := HelpText;
    Execute;
    Free;
  end;
end;

procedure ShowViewHelp;
begin
  with TShowMessage.Create do
  begin
    Text := HelpViewText;
    Execute;
    Free;
  end;
end;

procedure ShowMessage(AText: string);
begin
  with TShowMessage.Create do
  begin
    Text := AText;
    Execute;
    Free;
  end;
end;

procedure NonWaitMessage(AText: string);
begin
  with TNonWaitMessage.Create do
  begin
    Text := AText;
    Execute;
    Free;
  end;
end;

procedure ShowTools(SrcPanel, DestPanel: TFileList);
begin
  With TToolsMenu.Create do
  begin
    SrcP := SrcPanel;
    DestP := DestPanel;
    Execute;
    Free;
  end;
end;

function AskQuestion(AText: string): Boolean;
begin
  with TAskQuestion.Create do
  begin
    Text := AText;
    Result := Execute = mrOk;
    Free;
  end;
end;

function AskMultipleQuestion(AText: string): TDialogResult;
begin
  with TAskMultipleQuestion.Create do
  begin
    Text := AText;
    Result := Execute;
    Free;
  end;
end;

function AskForName(AText: String; var ANewname: String; UseAsName: Boolean = True): Boolean;
begin
  with TAskForName.Create do
  begin
    Text := AText;
    NewName := ANewName;
    AsName := UseAsName;
    Result := Execute = mrOK;
    if Result then
      ANewName := NewName;
    Free;
  end;
end;

function AskForNumber(AText: string; var ANewNumber: Integer): Boolean;
begin
  with TAskForNumber.Create do
  begin
    Text := AText;
    NewName := '';
    HexNumber := False;
    Result := Execute = mrOK;
    if Result then
      Result := TryStrToInt(NewName, ANewNumber);
    Free;
  end;
end;

function AskForHexNumber(AText: string; var HexString: string): Boolean;
begin
  with TAskForNumber.Create do
  begin
    Text := AText;
    NewName := HexString;
    HexNumber := True;
    Result := Execute = mrOK;
    if Result then
      HexString := NewName;
    Free;
  end;
end;

{ TToolsMenu }

procedure TToolsMenu.AddToolsEntry(Name: string; Event: TToolsEvent);
var
  Idx: Integer;
begin
  Idx := Length(Tools);
  SetLength(Tools, Idx + 1);
  Tools[Idx].AName := Name;
  Tools[Idx].Event := Event;
end;

procedure TToolsMenu.LhaPackEvent;
begin
  SrcP.PackArchive(atLHA);
end;

procedure TToolsMenu.LzxPackEvent;
begin
  SrcP.PackArchive(atLZX);
end;

procedure TToolsMenu.ShellEvent;
var
  s: string;
begin
  s := SrcP.CurrentPath;
  if Pos(#10, s) > 0 then
  begin
    s := Copy(s, 1, Pos(#10, s) - 1);
    s := ExtractFilePath(s);
  end;
  SetCurrentDir(s);
  if FullScreen then
    WBenchToFront;
  SystemTags(PChar('c:run >NIL: newcli ' + DefaultShell), [NP_CLI, AsTag(True), TAG_END]);
end;

procedure TToolsMenu.StartProgEvent;
var
  s, Progline: string;
begin
  ProgLine := '';
  if AskForName('Enter program name to start file with', Progline, False) then
  begin
    NonWaitMessage('Start ' + ExtractFileName(ProgLine));
    s := IncludeTrailingPathDelimiter(SrcP.CurrentPath) + SrcP.ActiveEntry.Name;
    if FullScreen then
      WBenchToFront;
    SystemTags(PChar(Progline + ' "' + s + '"'), [TAG_END]);
    if FullScreen then
      ScreenToFront(VideoWindow^.WScreen);
  end;
end;

procedure TToolsMenu.UnpackArchive;
var
  Arc: TArchiveBase;
  ArcClass: TArchiveClass;
  s: string;
  PG: TSingleProgress;
  AllCount: Integer;

  procedure UnpackFiles(ArchiveDir: TArchiveDir; Base: string);
  var
    i: LongInt;
    AE: TArchiveEntry;
  begin
    for i := 0 to ArchiveDir.Entries.Count - 1 do
    begin
      AE := ArchiveDir.Entries[i];
      if AE is TArchiveDir then
        UnpackFiles(TArchiveDir(AE), IncludeTrailingPathDelimiter(Base + AE.Name))
      else
      if AE is TArchiveFile then
      begin
        AllCount := AllCount + 1;
        if not PG.UpdateValue(AllCount, 'Extract ' + AE.Name) then
          raise Exception.Create('Stopped by user');
        Arc.ExtractFile(Base + AE.Name, IncludeTrailingPathDelimiter(DestP.CurrentPath) + Base + AE.Name);
      end;
    end;
  end;

  function CountFiles(ArchiveDir: TArchiveDir): LongInt;
  var
    i: LongInt;
    AE: TArchiveEntry;
  begin
    Result := 0;
    for i := 0 to ArchiveDir.Entries.Count - 1 do
    begin
      AE := ArchiveDir.Entries[i];
      if AE is TArchiveDir then
        Result := Result + CountFiles(TArchiveDir(AE))
      else
      if AE is TArchiveFile then
        Result := Result + 1;
    end;
  end;

begin
  if DestP.InArchive then
  begin
    ShowMessage('Unpack archive to archive not supported');
    Exit;
  end;
  s := IncludeTrailingPathDelimiter(SrcP.CurrentPath) + SrcP.ActiveEntry.Name;
  Arc := nil;
  ArcClass := GetArchiver(s);
  if Assigned(ArcClass) then
  begin
    try
      Arc := ArcClass.Create;
      if not Arc.ReadArchive(s) then
      begin
        ShowMessage('Error read archive');
        Exit;
      end;
      PG := TSingleProgress.Create;
      //Count Files
      PG.MaxValue := CountFiles(Arc.AD);
      PG.Text := 'Extract Files';
      PG.Execute;
      AllCount := 0;
      //
      try
        UnpackFiles(Arc.AD, '');
      except
        on E:Exception do
          ShowMessage(E.Message);
      end;
    finally
      DestP.Update(True);
      Arc.Free;
    end;
  end
  else
  begin
    ShowMessage('No matching unpacker found.')
  end;
end;

procedure TToolsMenu.SearchStart;
begin
  StartSearch(SrcP);
end;

procedure TToolsMenu.SelectInfo;
begin
  SrcP.SelectInfoFiles;
end;

procedure TToolsMenu.DiffViewer;
var
  Diff: TDiffViewer;
  s1, s2: string;
begin
  Diff := TDiffViewer.Create;
  try
    s1 := IncludeTrailingPathDelimiter(SrcP.CurrentPath) + SrcP.ActiveEntry.Name;
    s2 := IncludeTrailingPathDelimiter(DestP.CurrentPath) + DestP.ActiveEntry.Name;
    Diff.Execute(s1, s2);
  finally
    Diff.Free;
  end;
end;

procedure TToolsMenu.ProcessMouse(MouseEvent: TMouseEvent);
var
  NEntry: Integer;
begin
  if (MouseEvent.Action = MouseActionDown) and (MouseEvent.buttons = MouseLeftButton) then
  begin
    NEntry := EnsureRange(MouseEvent.Y - InnerRect.Top, 0, High(Tools));
    if NEntry = CurrentEntry then
    begin
      Tools[CurrentEntry].Event();
      Finished := True;
    end
    else
      CurrentEntry := NEntry;
    Paint;
  end;
end;

procedure TToolsMenu.DrawButtons;
begin

end;

procedure TToolsMenu.Paint;
var
  i: Integer;
  s: string;
  AbsLen: LongInt;
begin
  BGPen := Cyan;
  FGPen := White;

  inherited;
  AbsLen := MaxLen + 5 + 2;
  WindowRect.Left := Max(2, mid.x - AbsLen div 2 + 1);
  WindowRect.Top := Max(2, mid.y - Length(Tools) div 2 - 1);
  WindowRect.Bottom := Min(ScreenHeight - 3, WindowRect.Top +  Length(Tools) + 1);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + AbsLen div 2);

  DrawWindowBorder;

  for i := 0 to High(Tools) do
  begin
    if i = CurrentEntry then
      BGPen := Black
    else
      BGPen := Cyan;

    s := IntToStr(i + 1) + '    ' + Tools[i].AName;
    s := s + Space(Max(0, InnerRect.Width - AbsLen - 1));
    SetTextA(InnerRect.Left + 2, InnerRect.Top + i, s);
  end;
  UpdateScreen(False);
end;

constructor TToolsMenu.Create;
begin
  AddToolsEntry('Open new shell window', @ShellEvent);
  AddToolsEntry('Pack selected with lha', @LhaPackEvent);
  AddToolsEntry('Pack selected with lzx', @lzxPackEvent);
  AddToolsEntry('Open file in external program', @StartProgEvent);
  AddToolsEntry('Extract archive contents', @UnpackArchive);
  AddToolsEntry('Find Files', @SearchStart);
  AddToolsEntry('Select Icons of selected files', @SelectInfo);
  AddToolsEntry('Diff Files', @DiffViewer);
end;

function TToolsMenu.Execute: TDialogResult;
var
  i: Integer;
  Key: TKeyEvent;
  c: Char;
begin
  Finished := False;
  CurrentEntry := 0;
  MaxLen := 10;
  for i := 0 to High(Tools) do
    MaxLen := Max(Length(Tools[i].AName), MaxLen);
  MaxLen := MaxLen + 5;
  Paint;
  repeat
    Key := PollNextKey;
    if Finished then
      Break;
    c := GetKeyEventChar(Key);
    if c in ['1'..'9'] then
    begin
      i := StrToInt(c) - 1;
      if i <= High(Tools) then
      begin
        Tools[i].Event();
        Break;
      end;
    end;
    case TranslateKeyEvent(Key) and $ffff of
      kbdUp: begin
        if CurrentEntry = 0 then
          CurrentEntry := High(Tools)
        else
          Dec(CurrentEntry);
        Paint;
      end;
      kbdDown: begin
        if CurrentEntry = High(Tools) then
          CurrentEntry := 0
        else
          Inc(CurrentEntry);
        Paint;
      end;
       $1C0D, $000D: begin
        Tools[CurrentEntry].Event();
        Break;
      end;
      $011B: begin
        Result := mrCancel;
        Exit;
      end;
    end;
    Sleep(25);
  until False;
  Result := mrOK;
end;

{ TAskMultipleQuestion }

procedure TAskMultipleQuestion.ConfigureButtons;
begin
  SetLength(ButtonsArray, 5);
  ButtonsArray[0].Rect := Rect(Mid.x - 22, WindowRect.Bottom, Mid.X - 17, WindowRect.Bottom + 1);
  ButtonsArray[0].Pressed := False;
  ButtonsArray[0].Title := 'Yes';
  ButtonsArray[0].Result := mrOK;

  ButtonsArray[1].Rect := Rect(Mid.x - 16, WindowRect.Bottom, Mid.x - 12, WindowRect.Bottom + 1);
  ButtonsArray[1].Pressed := False;
  ButtonsArray[1].Title := 'No';
  ButtonsArray[1].Result := mrCancel;

  ButtonsArray[2].Rect := Rect(Mid.x - 10, WindowRect.Bottom, Mid.x + 2, WindowRect.Bottom + 1);
  ButtonsArray[2].Pressed := False;
  ButtonsArray[2].Title := 'Yes to All';
  ButtonsArray[2].Result := mrAll;

  ButtonsArray[3].Rect := Rect(Mid.x + 3, WindowRect.Bottom, Mid.x + 14, WindowRect.Bottom + 1);
  ButtonsArray[3].Pressed := False;
  ButtonsArray[3].Title := 'No to All';
  ButtonsArray[3].Result := mrNoAll;

  ButtonsArray[4].Rect := Rect(Mid.x + 16, WindowRect.Bottom, Mid.x + 23, WindowRect.Bottom + 1);
  ButtonsArray[4].Pressed := False;
  ButtonsArray[4].Title := 'Abort';
  ButtonsArray[4].Result := mrNoAll;
end;


{ TAskForNumber }

function TAskForNumber.IsValidChar(c: Char): Boolean;
begin
  if HexNumber then
    Result := c in ['0'..'9','a'..'f','A'..'F',' ']
  else
    Result := c in ['0'..'9','$','a'..'f','A'..'F'];
end;

function TAskForNumber.ValidInput(s: string): Boolean;
var
  i: Integer;
begin
  Result := HexNumber or TryStrToInt(s, i);
  if s= '$' then
    Result := True;
end;

{ TNonWaitMessage }

procedure TNonWaitMessage.DrawButtons;
begin
  // No button here ;)
end;

function TNonWaitMessage.Execute: TDialogResult;
begin
  Result := mrOK;
  Paint;
end;

{ TDoubleProgress }

procedure TDoubleProgress.Paint;
var
  x: Integer;
begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;
  //
  WindowRect.Left := Max(2, mid.x - 40);
  WindowRect.Top := Max(2, mid.y - 3);
  WindowRect.Bottom := Min(ScreenHeight - 3, mid.y + 3);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + 40);
  BGPen := LightGray;
  FGPen := Black;
  DrawWindowBorder;

  SetTextA(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  PGL := WindowRect.Left + 2;
  PGR := WindowRect.Right - 2;
  Pup := Mid.y;

  BGPen := LightGray;
  FGPen := Black;
  for x := PGL to PGR do
  begin
    SetChar(x, Pup - 1, ProgressEmpty);
    SetChar(x, Pup + 1, ProgressEmpty);
  end;
  if MaxValue <= 0 then
    MaxValue := 1;
  if MaxValue2 <= 0 then
    MaxValue2 := 1;
  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
end;

function TDoubleProgress.Execute: TDialogResult;
begin
  Result := inherited Execute;
end;

function TDoubleProgress.UpdateValue(AValue: LongWord; NText: string): Boolean;
begin
  Result := UpdateValue2(AValue, NText, CurValue, Text2);
end;

function TDoubleProgress.UpdateValue2(AValue1: LongWord; NText1: string; AValue2: LongWord; NText2: string): Boolean;
var
  w,p,x: LongInt;
  t1: LongWord;
  Key: TKeyEvent;
begin
  CancelPressed := False;
  CurValue := AValue1;
  Text := NText1;
  CurValue2 := AValue2;
  Text2 := NText2;

  Result := True;
  {$WARNINGS OFF}
  t1 := GetTickCount;
  {$WARNINGS ON}
  if t1 - LastCall > 100 then
  begin
    LastCall := t1;
    BGPen := LightGray;
    FGPen := Black;
    w := PGR - PGL;
    p := Round(CurValue/MaxValue * w);
    //writeln('num1: ', num1 , ' maxNum ', MaxNum,' w ', w, ' p ', P, ' PGL ', PGL, ' PGR ', PGR );
    for x := PGL to PGL + P do
    begin
      SetChar(x, Pup - 1, ProgressFull);
    end;
    p := Round(CurValue2/MaxValue2 * w);
    for x := PGL to PGL + P do
    begin
      SetChar(x, Pup + 1, ProgressFull);
    end;
    if Text <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup - 2, ' ');
      end;
      Text := LimitName(Text, w - 10, False);
      p := w div 2 - Length(Text) div 2 + 5;
      SetTextA(p, Pup - 2, Text);
    end;
    if Text2 <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup, ' ');
      end;
      Text2 := LimitName(Text2, w - 10, False);
      p := (PGR - 2) - Length(Text2);
      SetTextA(p, Pup, Text2);
    end;
    UpdateScreen(False);
  end;
  Key := PollNextKey;
  // Break on Enter -> Cancel
  if ((Key and $FF) = $0D) or CancelPressed then
    Result := False;

end;

{ TSingleProgress }

procedure TSingleProgress.DrawButtons;
begin
  BGPen := Cyan;
  FGPen := Black;
  SetText(Mid.x - 4, WindowRect.Bottom, LBorder + 'Cancel' + RBorder);
  BGPen := LightGray;
end;

procedure TSingleProgress.Paint;
var
  x: LongInt;
begin
  inherited;
  WindowRect.Left := Max(2, mid.x - 40);
  WindowRect.Top := Max(2, mid.y - 2);
  WindowRect.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + 40);
  BGPen := LightGray;
  FGPen := Black;
  DrawWindowBorder;

  SetTextA(Mid.X - Length(Text) div 2, Mid.Y - 1, Text);

  PGL := WindowRect.Left + 2;
  PGR := WindowRect.Right - 2;
  Pup := Mid.y;

  BGPen := LightGray;
  FGPen := Black;
  for x := PGL to PGR do
  begin
    SetChar(x, Mid.y, ProgressEmpty);
  end;

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
end;

procedure TSingleProgress.ProcessMouse(MouseEvent: TMouseEvent);
begin
  if (MouseEvent.Action = MouseActionDown) and (MouseEvent.buttons = MouseLeftButton) then
  begin
    SetText(Mid.x - 4, WindowRect.Bottom, LBorder + 'Cancel' + RBorder);
    if (MouseEvent.y = WindowRect.Bottom) and InRange(MouseEvent.x, Mid.x - 4, Mid.x + 4) then
      CancelPressed := True;
  end;
end;

function TSingleProgress.Execute: TDialogResult;
begin
  CancelPressed := False;
  Result := mrOK;
  {$WARNINGS OFF}
  LastCall := GetTickCount;
  {$WARNINGS ON}
  CurValue := 0;
  if MaxValue = 0 then
    MaxValue := 1;
  Paint;
end;

function TSingleProgress.UpdateValue(AValue: LongWord; NText: string): Boolean;
var
  x, w, p: LongInt;
  t1: LongWord;
  Key: TKeyEvent;
begin
  CurValue := AValue;
  if NText <> '' then
    Text := NText;
  Result := True;
  {$WARNINGS OFF}
  t1 := GetTickCount;
  {$WARNINGS ON}
  if t1 - LastCall > 100 then
  begin
    LastCall := t1;
    BGPen := LightGray;
    FGPen := Black;
    w := PGR - PGL;
    p := Round(CurValue/MaxValue * w);
    for x := PGL to P do
    begin
      SetChar(x, Pup, ProgressFull);
    end;
    if Text <> '' then
    begin
      for x := PGL to PGR do
      begin
        SetChar(x, Pup - 1, ' ');
      end;
      Text := LimitName(Text, w - 10, False);
      p := w div 2 - Length(Text) div 2;
      SetTextA(p, Pup - 1, Text);
    end;
    UpdateScreen(False);
  end;
  Key := PollNextKey;
  // Break on Enter -> Cancel
  if ((Key and $FFFF) = $0D) or CancelPressed then
    Result := False;
end;

{ TAskForName }

function TAskForName.IsValidChar(c: Char): Boolean;
begin
  Result := InRange(Ord(c), 26, 126) or (c in [#$C7, #$FC, #$DC, #$E4, #$C4, #$F6, #$D6, #$DF]);
  if AsName and Result then
    Result := not (c in [':', '?', '#', '*', '\', '"', '''', '|', '>', '<']);
end;

function TAskForName.ValidInput(s: string): Boolean;
begin
  Result := True;
end;

procedure TAskForName.Paint;
var
  x: LongInt;
  l: Integer;
begin
  inherited;

  l := Length(Text) div 2;

  WindowRect.Left := Max(2, mid.x - Max(20, l + 1));
  WindowRect.Top := Max(2, mid.y - 2);
  WindowRect.Bottom := Min(ScreenHeight - 3, mid.y + 2);
  WindowRect.Right :=  Min(ScreenWidth - 3, mid.x + Max(20, l + 1));
  BGPen := LightGray;
  FGPen := Black;
  DrawWindowBorder;

  SetTextA(Mid.X - l, Mid.Y - 1, Text);

  TxtL := WindowRect.Left + 2;
  TxtR := WindowRect.Right - 2;

  BGPen := Black;
  FGPen := LightGray;
  for x := TxtL to TxtR do
  begin
    SetChar(x, Mid.y, ' ');
  end;

  SetCursorType(crUnderline);
  SetCursorPos(TxtL + Length(NewName), Mid.Y);
  SetTextA(TxtL, Mid.y, Newname);

  // draw Buttons
  FGPen := Black;
  BGPen := LightGray;
  DrawButtons;
  UpdateScreen(False);
end;

function TAskForName.Execute: TDialogResult;
var
  Key: TKeyEvent;
  c: Char;
  p, i: LongInt;
  OldName: String;
begin
  Paint;
  repeat
    Key := PollNextKey;
    for i := 0 to High(ButtonsArray) do
    begin
      if ButtonsArray[i].Pressed then
      begin
        Result := ButtonsArray[i].Result;
        SetCursorType(crHidden);
        Exit;
      end;
    end;
    if Key <> 0 then
    case (TranslateKeyEvent(Key) and $FFFF) of
      kbdLeft: begin // cursor left
        if CursorX > TxtL then
          SetCursorPos(CursorX - 1, CursorY);
      end;
      kbdRight: begin // cursor right
        if CursorX < TxtL + Length(NewName) then
          SetCursorPos(CursorX + 1, CursorY);
      end;
      kbdDelete: begin // Delete delete char
        p := CursorX - TxtL + 1;
        if CursorX < TxtL + Length(NewName) then
        begin
          Delete(NewName, p, 1);
          BGPen := Black;
          FGPen := LightGray;
          SetTextA(TxtL, Mid.y, Newname);
          SetChar(TxtL + Length(NewName), Mid.y, ' ');
          SetCursorPos(CursorX, Mid.Y);
          UpdateScreen(False);
        end;
      end;
      $1C0D, $000D: begin
        Result := ButtonsArray[SelectedButton].Result;
        Break;
      end;
      $011B: begin
        Result := mrCancel;
        Break;
      end;
      $0F09: begin
        SelectedButton := (SelectedButton + 1) mod Length(ButtonsArray);
        FGPen := Black;
        BGPen := LightGray;
        DrawButtons;
      end
      else
      begin
        c := GetKeyEventChar(Key);
        case c of
          #32..#252: begin
            ConvertCharBack(c);
            if IsValidChar(c) then
            begin
              p := CursorX - TxtL;
              if (p >= 0) and (Length(NewName) < 30) then
              begin
                OldName := NewName;
                Insert(c, OldName, p + 1);
                if ValidInput(OldName) then
                begin
                  NewName := OldName;
                  BGPen := Black;
                  FGPen := LightGray;
                  SetTextA(TxtL, Mid.y, Newname);
                  SetCursorPos(CursorX + 1, Mid.Y);
                  UpdateScreen(False);
                end;
              end;
            end;
          end;
          #8: begin
            p := CursorX - TxtL;
            if p > 0 then
            begin
              Delete(NewName, p, 1);
              BGPen := Black;
              FGPen := LightGray;
              SetTextA(TxtL, Mid.y, Newname);
              SetChar(TxtL + Length(NewName), Mid.y, ' ');
              SetCursorPos(CursorX - 1, Mid.Y);
              UpdateScreen(False);
            end;
          end;
        end
      end;
    end;
    Sleep(25);
  until False;
  SetCursorType(crHidden);
end;

{ TAskQuestion }

procedure TAskQuestion.ConfigureButtons;
begin
  SetLength(ButtonsArray, 2);
  ButtonsArray[0].Rect := Rect(Mid.x - 6, WindowRect.Bottom, Mid.X - 1, WindowRect.Bottom + 1);
  ButtonsArray[0].Pressed := False;
  ButtonsArray[0].Title := 'Yes';
  ButtonsArray[0].Result := mrOK;

  ButtonsArray[1].Rect := Rect(Mid.x + 2, WindowRect.Bottom, Mid.x + 6, WindowRect.Bottom + 1);
  ButtonsArray[1].Pressed := False;
  ButtonsArray[1].Title := 'No';
  ButtonsArray[1].Result := mrCancel;
end;

{ TBaseDialog }

procedure TBaseDialog.ProcessMouse(MouseEvent: TMouseEvent);
begin
  // no default mouse event
end;

function TBaseDialog.PollNextKey: TKeyEvent;
var
  MouseEvent: TMouseEvent;
begin
  Result := GetNextKeyEvent;
  if Result = ResizeKey then
  begin
    Result := 0;
    Paint;
  end;
  MouseEvent.Action := 0;
  if GetNextMouseEvent(MouseEvent) then
    ProcessMouse(MouseEvent);
end;

procedure TBaseDialog.DrawWindowBorder;
var
  x,y: Integer;
begin
  for y := WindowRect.Top to WindowRect.Bottom do
  begin
    for x := WindowRect.Left to WindowRect.Right do
    begin
      if (y = WindowRect.Top) or (y = WindowRect.Bottom) then
        SetChar(x,y, HLine)
      else
      begin
        if (x = WindowRect.Left) or (x = WindowRect.Right) then
          SetChar(x,y, VLine)
        else
          SetChar(x,y, ' ');
      end;
    end;
  end;
  SetChar(WindowRect.Left, WindowRect.Top, ULCorner);
  SetChar(WindowRect.Left, WindowRect.Bottom, LLCorner);
  SetChar(WindowRect.Right, WindowRect.Bottom, LRCorner);
  SetChar(WindowRect.Right, WindowRect.Top, URCorner);

  InnerRect := WindowRect;
  InnerRect.Inflate(-1,-1);
end;

procedure TBaseDialog.Paint;
begin
  mid.x := ScreenWidth div 2;
  mid.y := ScreenHeight div 2;
end;

{ TShowMessage }

procedure TShowMessage.DrawButtons;
var
  i: Integer;
begin
  ConfigureButtons;
  FGPen := Black;
  for i := 0 to High(ButtonsArray) do
  begin
    if SelectedButton = i then
      BGPen := Cyan
    else
      BGPen := LightGray;
    SetTextA(ButtonsArray[i].Rect.Left, ButtonsArray[i].Rect.Top, LBorder + ButtonsArray[i].Title + RBorder);
  end;
  BGPen := LightGray;
  UpdateScreen(False);
end;

procedure TShowMessage.ConfigureButtons;
begin
  //
  SetLength(ButtonsArray, 1);
  ButtonsArray[0].Rect := Rect(Mid.x, WindowRect.Bottom, Mid.x + 4, WindowRect.Bottom + 1);
  ButtonsArray[0].Pressed := False;
  ButtonsArray[0].Title := 'OK';
  ButtonsArray[0].Result := mrOK;
end;

procedure TShowMessage.Paint;
var
  i, j: Integer;
  SL: TStringList;
  MaxX, MaxY: Integer;
  s,s1: string;
  DrawUp, DrawDown: Boolean;
begin
  inherited;
  //
  SL := TStringList.Create;
  SL.Text := Text;

  MaxX := 1;
  i := 0;
  while i < SL.Count do
  begin
    if Length(SL[i]) > ScreenWidth - 6 then
    begin
      s := SL[i];
      s1 := Copy(s, 1, ScreenWidth - 6);
      SL[i] := s1;
      Delete(s, 1, ScreenWidth - 6);
      if Length(SL[i]) > MaxX then
        MaxX := Length(SL[i]);
      SL.Insert(i + 1, s);
    end
    else
    begin
      if Length(SL[i]) > MAxX then
        MaxX := Length(SL[i]);
      i := i + 1;
    end;
  end;
  MaxX := MaxX + 4;
  MaxY := SL.Count + 4;

  WindowRect.Left := Max(1, Mid.X - Ceil(MaxX / 2));
  WindowRect.Top := Max(1, Mid.Y - Ceil(MaxY / 2));
  WindowRect.Right :=  Min(ScreenWidth - 2, Mid.X + Floor(MaxX / 2) - 1);
  WindowRect.Bottom := Min(ScreenHeight - 2 ,Mid.Y + Floor(MaxY / 2) - 1);

  BGPen := LightGray;
  FGPen := Black;

  DrawWindowBorder;

  j := 0;
  WithScroll := SL.Count > InnerRect.Height - 1;
  if WithScroll then
  begin
    TopLine := EnsureRange(TopLine, 0, SL.Count - (InnerRect.Height - 1));
  end
  else
    TopLine := 0;
  DrawUp := TopLine > 0;
  DrawDown := WithScroll and (TopLine < SL.Count - (InnerRect.Height - 1));
  for i := TopLine to SL.Count - 1 do
  begin
    if InnerRect.Top + 1 + j < ScreenHeight - 3 then
      SetTextA(InnerRect.left + 1, InnerRect.Top + 1 + j, SL[i]);
    j := j + 1;
  end;
  SL.Free;

  if DrawUp then
    SetChar(InnerRect.Right, InnerRect.Top, ArrowUp);
  if DrawDown then
    SetChar(InnerRect.Right, InnerRect.Bottom, ArrowDown);

  // draw Buttons
  DrawButtons;
  UpdateScreen(False);
end;

procedure TShowMessage.ProcessMouse(MouseEvent: TMouseEvent);
var
  i: Integer;
begin
  if (MouseEvent.Action = MouseActionDown) and (MouseEvent.buttons = MouseLeftButton) then
  begin
    for i := 0 to High(ButtonsArray) do
    begin
      if PtInRect(ButtonsArray[i].Rect, Point(MouseEvent.x, MouseEvent.y)) then
      begin
        if SelectedButton = i then
          ButtonsArray[i].Pressed := True
        else
        begin
          SelectedButton := i;
          DrawButtons;
        end;
        Exit;
      end;
    end;
  end;
end;

function TShowMessage.Execute: TDialogResult;
var
  Key: TKeyEvent;
  i: Integer;
begin
  WithScroll := False;
  TopLine := 0;
  SelectedButton := 0;
  Result := mrOK;
  Paint;
  repeat
    Key := PollNextKey;
    for i := 0 to High(ButtonsArray) do
    begin
      if ButtonsArray[i].Pressed then
      begin
        Result := ButtonsArray[i].Result;
        Exit;
      end;
    end;
    case (TranslateKeyEvent(Key) and $FFFF) of
      $1C0D, $000D: begin
        Result := ButtonsArray[SelectedButton].Result;
        Break;
      end;
      $0F09: begin
        SelectedButton := (SelectedButton + 1) mod Length(ButtonsArray);
        DrawButtons;
      end;
      kbdLeft, $34: begin    // cursor left
        SelectedButton := Max(SelectedButton - 1, 0);
        DrawButtons;
      end;
      kbdRight, $36: begin    // cursor right
        SelectedButton := Min(SelectedButton + 1, High(ButtonsArray));
        DrawButtons;
      end;
      kbdUp, $38: begin // cursor up
        TopLine := Max(0, TopLine - 1);
        Paint;
      end;
      kbdDown, $32: begin // cursor down
        TopLine := TopLine + 1;
        Paint;
      end;
      $011B: begin
        Result := mrCancel;
        Exit;
      end;
    end;
    Sleep(25);
  until False;
end;

end.

