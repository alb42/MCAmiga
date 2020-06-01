MyCommander for Amiga,AmigaOS4,AROS,MorphOS
-------------------------------------------
License: CC0, source at https://www.github.com/alb42/MCAmiga

Midnight/Norton commander style file manager for all Amiga systems.

if the font is too big try to set the Environment variable FPC_VIDEO_BUILTINFONT to vga8
e.g. echo vga8 >env:FPC_VIDEO_BUILTINFONT
and restart the program, or create it in Envarc: to make it available as default.

Requirements:
#############

AmigaOS 3.x, 68020, 4 MB RAM
AmigaOS 4.x PowerPC (except X5000)
MorphOS 3.x PowerPC
AROS i386-ABIv0, RasPi1-3 ARM ABIv0, x64 ABIv1 NonSMP

Needed Assigns:
---------------
ENV: T:

Optional
--------
lha, lzx commands in c:
(AROS's lha is incompatible, use xad)
installed xadmaster.library to read other archive types

Keys:
#####

Movement:
---------

Cursor up, 8         - Move cursor one up 
Cursor down, 2       - Move cursor one down
Pg up, 9, Ctrl up    - move cursor 10 up
Pg dwn, 3, Ctrl down - move cursor 10 down
Home, 7, Ctrl left   - go to first item
end, 1, Ctrl right   - go to last item
Tab                  - change focus to other panel
Enter                - Enter Directory if cursor on directory or archive
Shift-Enter          - Open directory or archive on other panel
Backspace            - go to Parent
Strg + F1, Alt + F1  - go to drives view on left panel
Strg + F2, Alt + F2  - go to drives view on right panel
Ctrl + D, Alt + D    - go to drives view
Alt + O              - set destination dir to source dir
Ctrl + S             - type start of name to fast jump to it

Selection:
----------

INS, 0, Space, # - De-/Select item and move one item down
Shift Up         - De-/Select item and move one item up
Shift Up         - De-/Select item and move one item down
+                - Select items by pattern
-                - Desel items by pattern

Operations:
-----------

F3       - View                      Shift F3 - Alternative View
F4       - Edit                      Shift F4 - Alternative Edit
F5       - Copy
F6       - Move                      Shift F6 - Rename file under cursor
F7       - Create directory
F8, Del  - Delete
Enter    - start executable (wait)   Shift Enter - run executable (don't wait for finish)

General:
--------

F1       - Help
F2       - Tools menu
F10, ESC - Quit
Ctrl + R - Rescan current directory
Ctrl + F - Toggle visibility of bottom menu
Shift + Space - Scan Directory size

Tooltypes:
##########

VIEWER=   Viewer for F3, Empty = internal viewer
VIEWER2=  Viewer for Shift F3, Empty = internal viewer
EDITOR=   Editor for F4
EDITOR2=  Editor for Shift F4

LEFT=     Startup path for left panel
RIGHT=    Startup path for right panel

WITHDEVICES=1 Show devices in the top overview also (together with assigns and volumes, careful some of them will crash on access)

SHOWMENU  if this ToolType is present the Bottom menu is shown by program start

DEFAULTSHELL= parameter for to define settings for the tools menu newshell define console size, name and parameter

History:
########

0.7
- Search function in tools menu
- bugfixes for starting programs Fullscreen
- fake depth gadget in fullscreen
- archive bugfixes

0.6
- fixed wordwrap in viewer fixed missing char
- bottom menu line (with tooltype setting)
- fixed parent select when leaving archive
- tooltype for fullscreen (own screen) setting
- Scroll help screen text if needed
- catch exception for external programs
- unpack archive in tools menu

0.5
- video fixes
- browse, copy, move, rename, view/edit in lha/lzx archives
- browse, extract, view files from xad supported archive types
- start executables by pressing (shift) enter
- tools menu

0.4
- mouse control
- drag'n'drop
- scrolling too long names

0.3
- fixed archive contents
- rename
- jump to line in viewer
- find in viewer

0.2
- First Release
- text viewer, Hex viewer
- copy, move, makedir, delete
- browse directories

