MyCommander for Amiga,AmigaOS4,AROS,MorphOS
-------------------------------------------
License: CC0, source at https://www.github.com/alb42/MCAmiga

Midnight/Norton commander style file manager for all Amiga systems.

if the font is too big try to set the Environment variable FPC_VIDEO_BUILTINFONT to vga8
e.g. echo vga8 >env:FPC_VIDEO_BUILTINFONT
and restart the program, or create it in Envarc: to make it available as default.


Keys:
#####

Movement:
---------

Cursor up, 8     - Move cursor one up 
Cursor down, 2   - Move cursor one down
Pg up, 9         - move cursor 10 up
Pg dwn, 3        - move cursor 10 down
Home, 7          - go to first item
end, 1           - go to last item
Tab              - change focus to other panel
Enter            - Enter Directory if cursor on directory
Shift-Enter      - Open directory on other panel
Backspace        - go to Parent
Strg F1, Alt F1  - go to drives view on left panel
Strg F2, Alt F2  - go to drives view on right panel
Ctrl D, Alt D    - go to drives view
Alt O            - set destination dir to source dir
Ctrl S           - type start of name to fast jump to it

Selection:
----------

INS, 0, Space, # - De-/Select item at cursor
+                - Select items by pattern
-                - Desel items by pattern

Operations:
-----------

F3       - View                      Shift F3 - Alternative View
F4       - Edit                      Shift F4 - Alternative Edit
F5       - Copy
F6       - Move                      Shift F6 - Rename file under cursor
F7       - Create directory
F8       - Delete
Enter    - open lha file, start executable

General:
--------

F1       - Help
F2       - Tools menu
F10, ESC - Quit
Ctrl + R - Rescan current directory
Shift + Space - Scan Directory size

Tooltypes:
##########

VIEWER=   Viewer for F3, Empty = internal viewer
VIEWER2=  Viewer for Shift F3, Empty = internal viewer
EDITOR=   Editor for F4
EDITOR2=  Editor for Shift F4

LEFT=     Startup path for left panel
RIGHT=    Startup path for right panel


History:
########

0.5
- video fixes
- browse, copy, move, rename, view/edit in lha/lzx archives
- browse, extract, view files from xad supported archive types

0.4
- mouse control
- drag'n'drop

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

