unit colorunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Video;

type
  TColorIndexes = (BackgroundColor, FileColor, DirColor, AssignColor, SelNActiveColor, SelBackColor, SelFrontColor, BorderColor, SelectedColor, SelectedBGColor);

procedure SetLowColormode(Enable: Boolean);
function GetColor(ColIdx: TColorIndexes): LongInt;

implementation


const
  MyColors: array[TColorIndexes] of array[Boolean] of Byte =
    ((Blue, LightGray),       // BackgroundColor
     (Cyan, Black),           // FileColor
     (White, LightBlue),      // DirColor    // Parent
     (LightGray, White),      // AssignColor // Drive
     (LightGray, White),      // SelNActiveColor
     (Brown, White),          // SelBackColor
     (LightGray, White),      // SelFrontColor
     (LightGray, Black),      //  BorderColor
     (Yellow, White),         // SelectedColor
     (Yellow, Black)          // SelectedBGColor
    );

var
  LowColorMode: Boolean = False;

procedure SetLowColormode(Enable: Boolean);
begin
  LowColorMode := Enable;
end;

function GetColor(ColIdx: TColorIndexes): LongInt;
begin
  Result := MyColors[ColIdx, LowColorMode];
end;

end.

