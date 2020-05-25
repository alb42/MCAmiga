unit xad;
{$mode objfpc}
interface
{$PACKRECORDS 2}
uses
  Exec, Utility;

const
  XADNAME = 'xadmaster.library';

type

(* NOTE: Nearly all structures need to be allocated using the
   xadAllocObject function. *)

(************************************************************************
*									                                                      *
*    library base structure						                                  *
*								                                                      	*
************************************************************************)

  PxadMasterBase = ^TxadMasterBase;
  TxadMasterBase = record
    xmb_LibNode: TLibrary;
    xmb_SysBase: PExecBase;
    xmb_DOSBase: PLibrary; //PDosBase
    xmb_UtilityBase: PUtilityBase;
    xmb_RecogSize: LongWord;	     // read only
    xmb_DefaultName: STRPTR;       // name for XADFIF_NOFILENAME (V6)
  end;

(************************************************************************
*									                                                      *
*    tag-function call flags						                                *
*									                                                      *
************************************************************************)
const
  // input tags for xadGetInfo, only one can be specified per call
  XAD_INSIZE        = TAG_USER + 1;  // input data size
  XAD_INFILENAME    = TAG_USER + 2;
  XAD_INFILEHANDLE  = TAG_USER + 3;
  XAD_INMEMORY      = TAG_USER + 4;
  XAD_INHOOK        = TAG_USER + 5;
  XAD_INSPLITTED    = TAG_USER + 6; // (V2)
  XAD_INDISKARCHIVE = TAG_USER + 7; // (V4)
  XAD_INXADSTREAM   = TAG_USER + 8; // (V8)
  XAD_INDEVICE      = TAG_USER + 9; // (V11)

  // output tags, only one can be specified per call, xadXXXXUnArc
  XAD_OUTSIZE       = TAG_USER + 10;  // output data size
  XAD_OUTFILENAME   = TAG_USER + 11;
  XAD_OUTFILEHANDLE = TAG_USER + 12;
  XAD_OUTMEMORY     = TAG_USER + 13;
  XAD_OUTHOOK       = TAG_USER + 14;
  XAD_OUTDEVICE     = TAG_USER + 15;
  XAD_OUTXADSTREAM  = TAG_USER + 16; // (V8)

  // object allocation tags for xadAllocObjectA
  XAD_OBJNAMESIZE     = TAG_USER + 20; // XADOBJ_FILEINFO, size of needed name space
  XAD_OBJCOMMENTSIZE  = TAG_USER + 21; // XADOBJ_FILEINFO, size of needed comment space
  XAD_OBJPRIVINFOSIZE = TAG_USER + 22; // XADOBJ_FILEINFO and XADOBJ_DISKINFO, self use size
  XAD_OBJBLOCKENTRIES = TAG_USER + 23; // XADOBJ_DISKINFO, number of needed entries

  // tags for xadGetInfo, xadFileUnArc and xadDiskUnArc
  XAD_NOEXTERN        = TAG_USER + 50; // do not use extern clients
  XAD_PASSWORD        = TAG_USER + 51; // password when needed
  XAD_ENTRYNUMBER     = TAG_USER + 52; // number of wanted entry
  XAD_PROGRESSHOOK    = TAG_USER + 53; // the progress hook
  XAD_OVERWRITE       = TAG_USER + 54; // overwrite file ?
  XAD_MAKEDIRECTORY   = TAG_USER + 55; // create directory tree
  XAD_IGNOREGEOMETRY  = TAG_USER + 56; // ignore drive geometry ?
  XAD_LOWCYLINDER     = TAG_USER + 57; // lowest cylinder
  XAD_HIGHCYLINDER    = TAG_USER + 58; // highest cylinder
  XAD_VERIFY          = TAG_USER + 59; // verify for disk hook
  XAD_NOKILLPARTIAL   = TAG_USER + 60; // do not delete partial/corrupt files (V3.3)
  XAD_FORMAT          = TAG_USER + 61; // format output device (V5)
  XAD_USESECTORLABELS = TAG_USER + 62; // sector labels are stored on disk (V9)
  XAD_IGNOREFLAGS     = TAG_USER + 63; // ignore the client, if certain flags are set (V11)
  XAD_ONLYFLAGS       = TAG_USER + 64; // ignore the client, if certain flags are NOT set (V11)

  // input tags for xadConvertDates, only one can be passed
  XAD_DATEUNIX        = TAG_USER + 70; // unix date variable
  XAD_DATEAMIGA       = TAG_USER + 71; // amiga date variable
  XAD_DATEDATESTAMP   = TAG_USER + 72; // struct DateStamp
  XAD_DATEXADDATE     = TAG_USER + 73; // struct xadDate
  XAD_DATECLOCKDATA   = TAG_USER + 74; // struct ClockData
  XAD_DATECURRENTTIME = TAG_USER + 75; // input is system time
  XAD_DATEMSDOS       = TAG_USER + 76; // MS-DOS packed format (V2)
  XAD_DATEMAC         = TAG_USER + 77; // Mac date variable (V8)
  XAD_DATECPM         = TAG_USER + 78; // CP/M data structure (V10)
  XAD_DATECPM2        = TAG_USER + 79; // CP/M data structure type 2 (V10)
  XAD_DATEISO9660     = TAG_USER + 300; // ISO9660 date structure (V11)

  // output tags, there can be specified multiple tags for one call
  XAD_GETDATEUNIX      = TAG_USER + 80; // unix date variable
  XAD_GETDATEAMIGA     = TAG_USER + 81; // amiga date variable
  XAD_GETDATEDATESTAMP = TAG_USER + 82; // struct DateStamp
  XAD_GETDATEXADDATE   = TAG_USER + 83; // struct xadDate
  XAD_GETDATECLOCKDATA = TAG_USER + 84; // struct ClockData
  XAD_GETDATEMSDOS     = TAG_USER + 86; // MS-DOS packed format (V2)
  XAD_GETDATEMAC       = TAG_USER + 87; // Mac date variable (V8)
  XAD_GETDATECPM       = TAG_USER + 88; // CP/M data structure (V10)
  XAD_GETDATECPM2      = TAG_USER + 89; // CP/M data structure type 2 (V10)
  XAD_GETDATEISO9660   = TAG_USER + 320; // ISO9660 date structure (V11)

  // following tags need locale.library to be installed
  XAD_MAKEGMTDATE   = TAG_USER + 90; // make local to GMT time
  XAD_MAKELOCALDATE = TAG_USER + 91; // make GMT to local time

  // tags for xadHookTagAccess (V3)
  XAD_USESKIPINFO  = TAG_USER + 104; // the hook uses xadSkipInfo (V3)
  XAD_SECTORLABELS = TAG_USER + 105; // pass sector labels with XADAC_WRITE (V9)

  XAD_GETCRC16 = TAG_USER + 120; // pointer to UWORD value (V3)
  XAD_GETCRC32 = TAG_USER + 121; // pointer to ULONG value (V3)

  XAD_CRC16ID = TAG_USER + 130; // ID for crc calculation (V3)
  XAD_CRC32ID = TAG_USER + 131; // ID for crc calculation (V3)

  // tags for xadConvertProtection (V4)
  XAD_PROTAMIGA    = TAG_USER + 160; // Amiga type protection bits (V4)
  XAD_PROTUNIX     = TAG_USER + 161; // protection bits in UNIX mode (V4)
  XAD_PROTMSDOS    = TAG_USER + 162; // MSDOS type protection bits (V4)
  XAD_PROTFILEINFO = TAG_USER + 163; // input is a xadFileInfo structure (V11)

  XAD_GETPROTAMIGA    = TAG_USER + 170; // return Amiga protection bits (V4)
  XAD_GETPROTUNIX     = TAG_USER + 171; // return UNIX protection bits (V11)
  XAD_GETPROTMSDOS    = TAG_USER + 172; // return MSDOS protection bits (V11)
  XAD_GETPROTFILEINFO = TAG_USER + 173; // fill xadFileInfo protection fields (V11)

  // tags for xadGetDiskInfo (V7)
  XAD_STARTCLIENT  = TAG_USER + 180; // the client to start with (V7)
  XAD_NOEMPTYERROR = TAG_USER + 181; // do not create XADERR_EMPTY (V8)

  // tags for xadFreeHookAccess (V8)
  XAD_WASERROR = TAG_USER + 190; // error occured, call abort method (V8)

  // tags for miscellaneous stuff
  XAD_ARCHIVEINFO = TAG_USER + 200; // xadArchiveInfo for stream hooks (V8)
  XAD_ERRORCODE   = TAG_USER + 201; // error code of function (V12)

  // tags for xadAddFileEntry and xadAddDiskEntry (V10)
  XAD_SETINPOS        = TAG_USER + 240; // set xai_InPos after call (V10)
  XAD_INSERTDIRSFIRST = TAG_USER + 241; // insert dirs at list start (V10)

  // tags for xadConvertName (V12)
  XAD_PATHSEPERATOR    = TAG_USER + 260; // UWORD *, default is {'/','\\',0} in source charset (V12)
  XAD_CHARACTERSET     = TAG_USER + 261; // the characterset of string (V12)
  XAD_STRINGSIZE       = TAG_USER + 262; // maximum size of following (V12)
  XAD_CSTRING          = TAG_USER + 263; // zero-terminated string (V12)
  XAD_PSTRING          = TAG_USER + 264; // lengthed Pascal string (V12)
  XAD_XADSTRING        = TAG_USER + 265; // an xad string (V12)
  XAD_ADDPATHSEPERATOR = TAG_USER + 266; // default is TRUE (V12)

  //* tags for xadGetFilename (V12)
  XAD_NOLEADINGPATH      = TAG_USER + 280; // default is FALSE (V12)
  XAD_NOTRAILINGPATH     = TAG_USER + 281; // default is FALSE (V12)
  XAD_MASKCHARACTERS     = TAG_USER + 282; // default are #?()[]~%*:|",1-31,127-160 (V12)
  XAD_MASKINGCHAR        = TAG_USER + 283; // default is '_' (V12)
  XAD_REQUIREDBUFFERSIZE = TAG_USER + 284; // pointer which should hold buf size (V12)


  // Places 300-339 used for dates!

(**********************************************************************
*                                                                     *
*    objects for xadAllocObjectA                                      *
*                                                                     *
***********************************************************************)

  XADOBJ_ARCHIVEINFO  = $0001; // struct xadArchiveInfo
  XADOBJ_FILEINFO     = $0002; // struct xadFileInfo
  XADOBJ_DISKINFO     = $0003; // struct xadDiskInfo
  XADOBJ_HOOKPARAM    = $0004; // struct HookParam
  XADOBJ_DEVICEINFO   = $0005; // struct xadDeviceInfo
  XADOBJ_PROGRESSINFO = $0006; // struct xadProgressInfo
  XADOBJ_TEXTINFO     = $0007; // struct xadTextInfo
  XADOBJ_SPLITFILE    = $0008; // struct xadSplitFile (V2)
  XADOBJ_SKIPINFO     = $0009; // struct xadSkipInfo (V3)
  XADOBJ_IMAGEINFO    = $000A; // struct xadImageInfo (V4)
  XADOBJ_SPECIAL      = $000B; // struct xadSpecial (V11)

  // result type of xadAllocVec
  XADOBJ_MEMBLOCK = $0100; // memory of requested size and type
  // private type
  XADOBJ_STRING = $0101; // an typed XAD string (V12)

 (***********************************************************************
  *                                                                     *
  *    modes for xadCalcCRC126 and xadCalcCRC32                         *
  *                                                                     *
  ***********************************************************************)

  XADCRC16_ID1  = $A001;
  XADCRC32_ID1  = $EDB88320;

 (***********************************************************************
  *                                                                     *
  *    hook related stuff                                               *
  *                                                                     *
  ***********************************************************************)

   XADHC_READ      = 1; // read data into buffer
   XADHC_WRITE     = 2; // write buffer data to file/memory
   XADHC_SEEK      = 3; // seek in file
   XADHC_INIT      = 4; // initialize the hook
   XADHC_FREE      = 5; // end up hook work, free stuff
   XADHC_ABORT     = 6; // an error occured, delete partial stuff
   XADHC_FULLSIZE  = 7; // complete input size is needed
   XADHC_IMAGEINFO = 8; // return disk image info (V4)

type
  TxadHookParam = record
    xhp_Command: LongWord;
    xhp_CommandData: LongInt;
    xhp_BufferPtr: APTR;
    xhp_BufferSize: LongWord;
    xhp_DataPos: LongWord; // current seek position
    xhp_PrivatePtr: APTR;
    xhp_TagList: APTR; // allows to transport tags to hook (V9)
  end;

  // xadHookAccess commands
const
  XADAC_READ       = 10; // get data
  XADAC_WRITE      = 11; // write data
  XADAC_COPY       = 12; // copy input to output
  XADAC_INPUTSEEK  = 13; // seek in input file
  XADAC_OUTPUTSEEK = 14; // seek in output file

 (***********************************************************************
  *                                                                     *
  *    support structures                                               *
  *                                                                     *
  ***********************************************************************)
  // Own date structure to cover all possible dates in a human friendly format. xadConvertDates may be used to convert between different date structures and variables.
type
  TxadDate = packed record
    xd_Micros: LongWord; // values 0 to 999999
    xd_Year: LongInt; // values 1 to 2147483648
    xd_Month: Byte; // values 1 to 12
    xd_WeekDay: Byte; // values 1 to 7
    xd_Day: Byte; // values 1 to 31
    xd_Hour: Byte; // values 0 to 23
    xd_Minute: Byte; // values 0 to 59
    xd_Second: Byte; // values 0 to 59
  end;

const
   XADDAY_MONDAY    = 1; // monday is the first day and
   XADDAY_TUESDAY   = 2;
   XADDAY_WEDNESDAY = 3;
   XADDAY_THURSDAY  = 4;
   XADDAY_FRIDAY    = 5;
   XADDAY_SATURDAY  = 6;
   XADDAY_SUNDAY    = 7; // sunday the last day of a week
type
  TxadDeviceInfo = record    // for XAD_OUTDEVICE tag
    xdi_DeviceName: STRPTR;  // name of device
    xdi_Unit: LongWord;      // unit of device
    xdi_DOSName: STRPTR;     // instead of Device+Unit, dos name without ':'
  end;

  PxadSplitFile = ^TxadSplitFile;
  TxadSplitFile = record    // for XAD_INSPLITTED
    xsf_Next: PxadSplitFile;
    xsf_Type: LongWord;     // XAD_INFILENAME, XAD_INFILEHANDLE, XAD_INMEMORY, XAD_INHOOK
    xsf_Size: LongWord;     // necessary for XAD_INMEMORY, useful for others
    xsf_Data: LongWord;     // FileName, Filehandle, Hookpointer or Memory
  end;

  PxadSkipInfo = ^TxadSkipInfo;
  TxadSkipInfo = record
    xsi_Next: PxadSkipInfo;
    xsi_Position: LongWord; // position, where it should be skipped
    xsi_SkipSize: LongWord; // size to skip
  end;

  PxadImageInfo = ^TxadImageInfo;
  TxadImageInfo = record        // for XADHC_IMAGEINFO
    xii_SectorSize: LongWord;   // usually 512
    xii_FirstSector: LongWord;  // of the image file
    xii_NumSectors: LongWord;   // of the image file
    xii_TotalSectors: LongWord; // of this device type
  end;
  // If the image file holds total data of disk xii_TotalSectors equals
  //  xii_NumSectors and xii_FirstSector is zero. Addition of xii_FirstSector
  //  and xii_NumSectors cannot exceed xii_TotalSectors value!

const
  XADFOREMAN_SECURITY = $70FF4E75; // MOVEQ #-1,D0 and RTS
  XADFOREMAN_ID       = $58414446; // 'XADF' identification ID
  XADFOREMAN_VERSION  = 1;

type
  PxadClient = ^TxadClient;
  TxadClient = record
    xc_Next: PxadClient;
    xc_Version: Word; // set to XADCLIENT_VERSION
    xc_MasterVersion: Word;
    xc_ClientVersion: Word;
    xc_ClientRevision: Word;
    xc_RecogSize: LongWord;  // needed size to recog the type
    xc_Flags: LongWord;      // see XADCF_xxx defines
    xc_Identifier: LongWord; // ID of internal clients
    xc_ArchiverName: STRPTR;
    xc_RecogData: Pointer; // function(): Bool;
    xc_GetInfo: Pointer;   // function(): LongInt;
    xc_UnArchive: Pointer; // function(): LongInt;
    xc_Free: Pointer;      // procedure();
  end;
const
  XADCLIENT_VERSION  = 1;

  XADCB_FILEARCHIVER    = 0; // archiver is a file archiver
  XADCB_DISKARCHIVER    = 1; // archiver is a disk archiver
  XADCB_EXTERN          = 2; // external client, set by xadmaster
  XADCB_FILESYSTEM      = 3; // filesystem clients (V5)
  XADCB_NOCHECKSIZE     = 4; // do not check size for recog call (V6)
  XADCB_DATACRUNCHER    = 5; // file archiver is plain data file (V11)
  XADCB_EXECRUNCHER     = 6; // file archiver is executable file (V11)
  XADCB_ADDRESSCRUNCHER = 7; // file archiver is address crunched file (V11)
  XADCB_LINKER          =  8; // file archiver is a linker file (V11)
  XADCB_FREEXADSTRINGS   = 25; // master frees XAD strings (V12)
  XADCB_FREESPECIALINFO  = 26; // master frees xadSpecial  structures (V11)
  XADCB_FREESKIPINFO     = 27; // master frees xadSkipInfo structures (V3)
  XADCB_FREETEXTINFO     = 28; // master frees xadTextInfo structures (V2)
  XADCB_FREETEXTINFOTEXT = 29; // master frees xadTextInfo text block (V2)
  XADCB_FREEFILEINFO     = 30; // master frees xadFileInfo structures (V2)
  XADCB_FREEDISKINFO     = 31; // master frees xadDiskInfo structures (V2)

  XADCF_FILEARCHIVER = 1 shl XADCB_FILEARCHIVER;
  XADCF_DISKARCHIVER = 1 shl XADCB_DISKARCHIVER;
  XADCF_EXTERN       = 1 shl XADCB_EXTERN;
  XADCF_FILESYSTEM   = 1 shl XADCB_FILESYSTEM;
  XADCF_NOCHECKSIZE  = 1 shl XADCB_NOCHECKSIZE;
  XADCF_DATACRUNCHER = 1 shl XADCB_DATACRUNCHER;
  XADCF_EXECRUNCHER  = 1 shl XADCB_EXECRUNCHER;
  XADCF_ADDRESSCRUNCHER = 1 shl XADCB_ADDRESSCRUNCHER;
  XADCF_LINKER          = 1 shl XADCB_LINKER;
  XADCF_FREEXADSTRINGS  = 1 shl XADCB_FREEXADSTRINGS;
  XADCF_FREESPECIALINFO = 1 shl XADCB_FREESPECIALINFO;
  XADCF_FREESKIPINFO    = 1 shl XADCB_FREESKIPINFO;
  XADCF_FREETEXTINFO    = 1 shl XADCB_FREETEXTINFO;
  XADCF_FREETEXTINFOTEXT = 1 shl XADCB_FREETEXTINFOTEXT;
  XADCF_FREEFILEINFO = 1 shl XADCB_FREEFILEINFO;
  XADCF_FREEDISKINFO = 1 shl XADCB_FREEDISKINFO;

 // The types 5 to 9 always need XADCB_FILEARCHIVER set also. These only specify
 // the type of the archiver somewhat better. Do not mix real archivers and these
 // single file data clients.

type


  PxadSpecialUnixDevice = ^TxadSpecialUnixDevice;
  TxadSpecialUnixDevice = record
    xfis_MajorVersion: LongWord; // major device version
    xfis_MinorVersion: LongWord; // minor device version
  end;

  PxadSpecialAmigaAddress = ^TxadSpecialAmigaAddress;
  TxadSpecialAmigaAddress = record
    xfis_JumpAddress: LongWord;      // code executaion start address
    xfis_DecrunchAddress: LongWord; // decrunch start of code
  end;

  PxadSpecialCBM8bit = ^TxadSpecialCBM8bit;
  TxadSpecialCBM8bit = record
    xfis_FileType: Byte;       // File type XADCBM8BITTYPE_xxx
    xfis_RecordLength: Byte;   // record length if relative file
  end;

  PxadSpecial = ^TxadSpecial;
  TxadSpecial = record
    xfis_Type: LongWord; // XADSPECIALTYPE to define type of block (V11)
    xfis_Next: PxadSpecial; // pointer to next entry
    xfis_Data: record
      case Byte of
        1: (xfis_UnixDevice: TxadSpecialUnixDevice;);
        2: (xfis_AmigaAddress: TxadSpecialAmigaAddress;);
        3: (xfis_CBM8bit: TxadSpecialCBM8bit;);
    end;
  end;

  PxadFileInfo = ^TxadFileInfo;
  TxadFileInfo = record
    xfi_Next: PxadFileInfo;
    xfi_EntryNumber: LongWord; // number of entry
    xfi_EntryInfo: STRPTR;     // additional archiver text
    xfi_PrivateInfo: APTR;     // client private, see XAD_OBJPRIVINFOSIZE
    xfi_Flags: LongWord;       // see XADFIF_xxx defines
    xfi_FileName: STRPTR;      // see XAD_OBJNAMESIZE tag
    xfi_Comment: STRPTR;       // see XAD_OBJCOMMENTSIZE tag
    xfi_Protection: LongWord;  // OS 3 bits (including multiuser)
    xfi_OwnerUID: LongWord; // user ID
    xfi_OwnerGID: Longword; // group ID
    xfi_UserName: STRPTR;   // user name
    xfi_GroupName: STRPTR;  // group name
    xfi_Size: Longword;     // size of this file
    xfi_GroupCrSize: Longword; // crunched size of group
    xfi_CrunchSize: Longword;  // crunched size
    xfi_LinkName: STRPTR;      // name and path of link
    xfi_Date: TxadDate;
    xfi_Generation: Word;      // File Generation [0...$FFFF] (V3)
    xfi_DataPos: Longword;     // crunched data position (V3)
    xfi_MacFork: PxadFileInfo; // pointer to 2nd fork for Mac (V7)
    xfi_UnixProtect: Word; // protection bits for Unix (V11)
    xfi_DosProtect: Byte;  // protection bits for MS-DOS (V11)
    xfi_FileType: Byte;    // XADFILETYPE to define type of exe files (V11)
    xfi_Special: PxadSpecial; // pointer to special data (V11)
  end;

const
  XADCBM8BITTYPE_UNKNOWN = $00; //   Unknown / Unused
  XADCBM8BITTYPE_BASIC   = $01; // Tape - BASIC program file
  XADCBM8BITTYPE_DATA    = $02; // Tape - Data block (SEQ file)
  XADCBM8BITTYPE_FIXED   = $03; // Tape - Fixed addres program file
  XADCBM8BITTYPE_SEQDATA = $04; // Tape - Sequential data file

  XADCBM8BITTYPE_SEQ = $81; // Disk - Sequential file "SEQ"
  XADCBM8BITTYPE_PRG = $82; // Disk - Program file "PRG"
  XADCBM8BITTYPE_USR = $83; // Disk - User-defined file "USR"
  XADCBM8BITTYPE_REL = $84; // Disk - Relative records file "REL"
  XADCBM8BITTYPE_CBM = $85; // Disk - CBM (partition) "CBM"

  // These are used for xfi_FileType to define file type. (V11)
  XADFILETYPE_DATACRUNCHER = 1; // infile was only one data file
  XADFILETYPE_TEXTLINKER   = 2; // infile was text-linked

  XADFILETYPE_AMIGAEXECRUNCHER = 11; // infile was an Amiga exe cruncher
  XADFILETYPE_AMIGAEXELINKER   = 12; // infile was an Amiga exe linker
  XADFILETYPE_AMIGATEXTLINKER  = 13; // infile was an Amiga text-exe linker
  XADFILETYPE_AMIGAADDRESS     = 14; // infile was an Amiga address cruncher

  XADFILETYPE_UNIXBLOCKDEVICE = 21; // this file is a block device
  XADFILETYPE_UNIXCHARDEVICE  = 22; // this file is a character device
  XADFILETYPE_UNIXFIFO        = 23; // this file is a named pipe
  XADFILETYPE_UNIXSOCKET      = 24; // this file is a socket

  XADFILETYPE_MSDOSEXECRUNCHER = 31 ; // infile was an MSDOS exe cruncher

  XADSPECIALTYPE_UNIXDEVICE   = 1; // xadSpecial entry is xadSpecialUnixDevice
  XADSPECIALTYPE_AMIGAADDRESS = 2; // xadSpecial entry is xadSpecialAmigaAddress
  XADSPECIALTYPE_CBM8BIT      = 3; // xadSpecial entry is xadSpecialCBM8bit

type
  PxadTextInfo = ^TxadTextInfo;
  TxadTextInfo = record
    xti_Next: PxadTextInfo;
    xti_Size: LongWord;  // maybe zero - no text - e.g. when crypted
    xti_Text: STRPTR;    // and there is no password in xadGetInfo()
    xti_Flags: LongWord; // see XADTIF_xxx defines
  end;

const
  XADTIB_CRYPTED = 0; // entry is empty, as data was crypted
  XADTIB_BANNER  = 1; // text is a banner
  XADTIB_FILEDIZ = 2; // text is a file description

  XADTIF_CRYPTED = 1 shl XADTIB_CRYPTED;
  XADTIF_BANNER  = 1 shl XADTIB_BANNER;
  XADTIF_FILEDIZ = 1 shl XADTIB_FILEDIZ;

type
  PxadDiskInfo = ^TxadDiskInfo;
  TxadDiskInfo = record
    xdi_Next: PxadDiskInfo;
    xdi_EntryNumber: LongWord; // number of entry
    xdi_EntryInfo: STRPTR; // additional archiver text
    xdi_PrivateInfo: APTR; // client private, see XAD_OBJPRIVINFOSIZE
    xdi_Flags: LongWord; // see XADDIF_xxx defines
    xdi_SectorSize: LongWord;
    xdi_TotalSectors: LongWord; // see devices/trackdisk.h
    xdi_Cylinders: LongWord; // to find out what these
    xdi_CylSectors: LongWord; // fields mean, they are equal
    xdi_Heads: LongWord; // to struct DriveGeometry
    xdi_TrackSectors: LongWord;
    xdi_LowCyl: LongWord; // lowest cylinder stored
    xdi_HighCyl: LongWord; // highest cylinder stored
    xdi_BlockInfoSize: LongWord; // number of BlockInfo entries
    xdi_BlockInfo: PByte; // see XADBIF_xxx defines and XAD_OBJBLOCKENTRIES tag
    xdi_TextInfo: PxadTextInfo; // linked list with info texts
    xdi_DataPos: LongWord; // crunched data position (V3)
  end;

 (* BlockInfo points to a UBYTE field for every track from first sector of
     lowest cylinder to last sector of highest cylinder. When not used,
     pointer must be 0. Do not use it, when there are no entries!
     This is just for information. The applications still asks the client
     to unarchive whole cylinders and not archived blocks are cleared for
     unarchiving.*)
const
  XADDIB_CRYPTED        = 0; // entry is crypted
  XADDIB_SEEKDATAPOS    = 1; // before unarchiving the datapos is set (V3)
  XADDIB_SECTORLABELS   = 2; // the clients delivers sector labels (V9)
  XADDIB_EXTRACTONBUILD = 3; // allows extract disk during scanning (V10)
  XADDIB_ENTRYMAYCHANGE = 4; // this entry may change until GetInfo is finished (V11)

 (* Some of the crunchers do not store all necessary information, so it
  may be needed to guess some of them. Set the following flags in that case
  and geometry check will ignore these fields.*)
  XADDIB_GUESSSECTORSIZE   = 5; // sectorsize is guessed (V10)
  XADDIB_GUESSTOTALSECTORS = 6; // totalsectors number is guessed (V10)
  XADDIB_GUESSCYLINDERS    = 7; // cylinder number is guessed
  XADDIB_GUESSCYLSECTORS   = 8; // cylsectors is guessed
  XADDIB_GUESSHEADS        = 9; // number of heads is guessed
  XADDIB_GUESSTRACKSECTORS = 10; // tracksectors is guessed
  XADDIB_GUESSLOWCYL       = 11; // lowcyl is guessed
  XADDIB_GUESSHIGHCYL      = 12; // highcyl is guessed

 (* If it is impossible to set some of the fields, you need to set some of
  these flags. NOTE: XADDIB_NOCYLINDERS is really important, as this turns
  of usage of lowcyl and highcyl keywords. When you have cylinder information,
  you should not use these and instead use guess flags and calculate
  possible values for the missing fields.*)
  XADDIB_NOCYLINDERS    = 15; // cylinder number is not set
  XADDIB_NOCYLSECTORS   = 16; // cylsectors is not set
  XADDIB_NOHEADS        = 17; // number of heads is not set
  XADDIB_NOTRACKSECTORS = 18; // tracksectors is not set
  XADDIB_NOLOWCYL       = 19; // lowcyl is not set
  XADDIB_NOHIGHCYL      = 20; // highcyl is not set

  XADDIF_CRYPTED        = 1 shl XADDIB_CRYPTED;
  XADDIF_SEEKDATAPOS    = 1 shl XADDIB_SEEKDATAPOS;
  XADDIF_SECTORLABELS   = 1 shl XADDIB_SECTORLABELS;
  XADDIF_EXTRACTONBUILD = 1 shl XADDIB_EXTRACTONBUILD;
  XADDIF_ENTRYMAYCHANGE = 1 shl XADDIB_ENTRYMAYCHANGE;

  XADDIF_GUESSSECTORSIZE   = 1 shl XADDIB_GUESSSECTORSIZE;
  XADDIF_GUESSTOTALSECTORS = 1 shl XADDIB_GUESSTOTALSECTORS;
  XADDIF_GUESSCYLINDERS    = 1 shl XADDIB_GUESSCYLINDERS;
  XADDIF_GUESSCYLSECTORS   = 1 shl XADDIB_GUESSCYLSECTORS;
  XADDIF_GUESSHEADS        = 1 shl XADDIB_GUESSHEADS;
  XADDIF_GUESSTRACKSECTORS = 1 shl XADDIB_GUESSTRACKSECTORS;
  XADDIF_GUESSLOWCYL       = 1 shl XADDIB_GUESSLOWCYL;
  XADDIF_GUESSHIGHCYL      = 1 shl XADDIB_GUESSHIGHCYL;

  XADDIF_NOCYLINDERS    = 1 shl XADDIB_NOCYLINDERS;
  XADDIF_NOCYLSECTORS   = 1 shl XADDIB_NOCYLSECTORS;
  XADDIF_NOHEADS        = 1 shl XADDIB_NOHEADS;
  XADDIF_NOTRACKSECTORS = 1 shl XADDIB_NOTRACKSECTORS;
  XADDIF_NOLOWCYL       = 1 shl XADDIB_NOLOWCYL;
  XADDIF_NOHIGHCYL      = 1 shl XADDIB_NOHIGHCYL;

  // defines for BlockInfo
  XADBIB_CLEARED = 0; // this block was cleared for archiving
  XADBIB_UNUSED  = 1; // this block was not archived

  XADBIF_CLEARED = 1 shl XADBIB_CLEARED;
  XADBIF_UNUSED  = 1 shl XADBIB_UNUSED;

 (***********************************************************************
  *                                                                     *
  *    information structures                                           *
  *                                                                     *
  ***********************************************************************)
type
  PxadArchiveInfo = ^TxadArchiveInfo;
  TxadArchiveInfo = record
    xai_Client: PxadClient; // pointer to unarchiving client
    xai_PrivateClient: APTR; // private client data
    xai_Password: STRPTR; // password for crypted archives
    xai_Flags: LongWord; // read only XADAIF_ flags
    xai_LowCyl: LongWord; // lowest cylinder to unarchive
    xai_HighCyl: LongWord; // highest cylinder to unarchive
    xai_InPos: LongWord; // input position, read only
    xai_InSize: LongWord; // input size, read only
    xai_OutPos: LongWord; // output position, read only
    xai_OutSize: LongWord; // output file size, read only
    xai_FileInfo: PxadFileInfo; // data pointer for file arcs
    xai_DiskInfo: PxadDiskInfo; // data pointer for disk arcs
    xai_CurFile: PxadFileInfo; // data pointer for current file arc
    xai_CurDisk: PxadDiskInfo; // data pointer for current disk arc
    xai_LastError: LongInt; // last error, when XADAIF_FILECORRUPT (V2)
    xai_MultiVolume: PLongWord; // array of start offsets from parts (V2)
    xai_SkipInfo: PxadSkipInfo; // linked list of skip entries (V3)
    xai_ImageInfo: PxadImageInfo; // for filesystem clients (V5)
    xai_InName: STRPTR; // Input archive name if available (V7)
  end;
 (* This structure is nearly complete private to either xadmaster or its
  clients. An application program may access for reading only xai_Client,
  xai_Flags, xai_FileInfo and xai_DiskInfo. For xai_Flags only XADAIF_CRYPTED
  and XADAIF_FILECORRUPT are useful. All the other stuff is private and should
  not be accessed!*)
const
   XADAIB_CRYPTED         = 0; // archive entries are encrypted
   XADAIB_FILECORRUPT     = 1; // file is corrupt, but valid entries are in the list
   XADAIB_FILEARCHIVE     = 2; // unarchive file entry
   XADAIB_DISKARCHIVE     = 3; // unarchive disk entry
   XADAIB_OVERWRITE       = 4; // overwrite the file (PRIVATE)
   XADAIB_MAKEDIRECTORY   = 5; // create directory when missing (PRIVATE)
   XADAIB_IGNOREGEOMETRY  = 6; // ignore drive geometry (PRIVATE)
   XADAIB_VERIFY          = 7; // verify is turned on for disk hook (PRIVATE)
   XADAIB_NOKILLPARTIAL   = 8; // do not delete partial files (PRIVATE)
   XADAIB_DISKIMAGE       = 9; // is disk image extraction (V5)
   XADAIB_FORMAT          = 10; // format in disk hook (PRIVATE)
   XADAIB_NOEMPTYERROR    = 11; // do not create empty error (PRIVATE)
   XADAIB_ONLYIN          = 12; // in stuff only (PRIVATE)
   XADAIB_ONLYOUT         = 13; // out stuff only (PRIVATE)
   XADAIB_USESECTORLABELS = 14; // use SectorLabels (PRIVATE)

   XADAIF_CRYPTED         = 1 shl XADAIB_CRYPTED;
   XADAIF_FILECORRUPT     = 1 shl XADAIB_FILECORRUPT;
   XADAIF_FILEARCHIVE     = 1 shl XADAIB_FILEARCHIVE;
   XADAIF_DISKARCHIVE     = 1 shl XADAIB_DISKARCHIVE;
   XADAIF_OVERWRITE       = 1 shl XADAIB_OVERWRITE;
   XADAIF_MAKEDIRECTORY   = 1 shl XADAIB_MAKEDIRECTORY;
   XADAIF_IGNOREGEOMETRY  = 1 shl XADAIB_IGNOREGEOMETRY;
   XADAIF_VERIFY          = 1 shl XADAIB_VERIFY;
   XADAIF_NOKILLPARTIAL   = 1 shl XADAIB_NOKILLPARTIAL;
   XADAIF_DISKIMAGE       = 1 shl XADAIB_DISKIMAGE;
   XADAIF_FORMAT          = 1 shl XADAIB_FORMAT;
   XADAIF_NOEMPTYERROR    = 1 shl XADAIB_NOEMPTYERROR;
   XADAIF_ONLYIN          = 1 shl XADAIB_ONLYIN;
   XADAIF_ONLYOUT         = 1 shl XADAIB_ONLYOUT;
   XADAIF_USESECTORLABELS = 1 shl XADAIB_USESECTORLABELS;

 (* Multiuser fields (xfi_OwnerUID, xfi_OwnerUID, xfi_UserName, xfi_GroupName)
    and multiuser bits (see <dos/dos.h>) are currently not supported with normal
    Amiga filesystem. But the clients support them, if archive format holds
    such information.

    The protection bits (all 3 fields) should always be set using the
    xadConvertProtection procedure. Call it with as much protection information
    as possible. It extracts the relevant data at best (and also sets the 2 flags).
    DO NOT USE these fields directly, but always through xadConvertProtection
    call.*)

   XADFIB_CRYPTED    = 0; // entry is crypted
   XADFIB_DIRECTORY  = 1; // entry is a directory
   XADFIB_LINK       = 2; // entry is a link
   XADFIB_INFOTEXT   = 3; // file is an information text
   XADFIB_GROUPED    = 4; // file is in a crunch group
   XADFIB_ENDOFGROUP = 5; // crunch group ends here
   XADFIB_NODATE     = 6; // no date supported, CURRENT date is set
   XADFIB_DELETED    = 7; // file is marked as deleted (V3)
   XADFIB_SEEKDATAPOS = 8; // before unarchiving the datapos is set (V3)
   XADFIB_NOFILENAME  = 9; // there was no filename, using internal one (V6)
   XADFIB_NOUNCRUNCHSIZE = 10; // file size is unknown and thus set to zero (V6)
   XADFIB_PARTIALFILE    = 11; // file is only partial (V6)
   XADFIB_MACDATA        = 12; // file is Apple data fork (V7)
   XADFIB_MACRESOURCE    = 13; // file is Apple resource fork (V7)
   XADFIB_EXTRACTONBUILD = 14; // allows extract file during scanning (V10)
   XADFIB_UNIXPROTECTION = 15; // UNIX protection bits are present (V11)
   XADFIB_DOSPROTECTION  = 16; // MSDOS protection bits are present (V11)
   XADFIB_ENTRYMAYCHANGE = 17; // this entry may change until GetInfo is finished (V11)
   XADFIB_XADSTRFILENAME = 18; // the xfi_FileName fields is an XAD string (V12)
   XADFIB_XADSTRLINKNAME = 19; // the xfi_LinkName fields is an XAD string (V12)
   XADFIB_XADSTRCOMMENT  = 20; // the xfi_Comment fields is an XAD string (V12)

   XADFIF_CRYPTED     = 1 shl XADFIB_CRYPTED;
   XADFIF_DIRECTORY   = 1 shl XADFIB_DIRECTORY;
   XADFIF_LINK        = 1 shl XADFIB_LINK;
   XADFIF_INFOTEXT    = 1 shl XADFIB_INFOTEXT;
   XADFIF_GROUPED     = 1 shl XADFIB_GROUPED;
   XADFIF_ENDOFGROUP  = 1 shl XADFIB_ENDOFGROUP;
   XADFIF_NODATE      = 1 shl XADFIB_NODATE;
   XADFIF_DELETED     = 1 shl XADFIB_DELETED;
   XADFIF_SEEKDATAPOS = 1 shl XADFIB_SEEKDATAPOS;
   XADFIF_NOFILENAME  = 1 shl XADFIB_NOFILENAME;
   XADFIF_NOUNCRUNCHSIZE = 1 shl XADFIB_NOUNCRUNCHSIZE;
   XADFIF_PARTIALFILE    = 1 shl XADFIB_PARTIALFILE;
   XADFIF_MACDATA        = 1 shl XADFIB_MACDATA;
   XADFIF_MACRESOURCE    = 1 shl XADFIB_MACRESOURCE;
   XADFIF_EXTRACTONBUILD = 1 shl XADFIB_EXTRACTONBUILD;
   XADFIF_UNIXPROTECTION = 1 shl XADFIB_UNIXPROTECTION;
   XADFIF_DOSPROTECTION  = 1 shl XADFIB_DOSPROTECTION;
   XADFIF_ENTRYMAYCHANGE = 1 shl XADFIB_ENTRYMAYCHANGE;
   XADFIF_XADSTRFILENAME = 1 shl XADFIB_XADSTRFILENAME;
   XADFIF_XADSTRLINKNAME = 1 shl XADFIB_XADSTRLINKNAME;
   XADFIF_XADSTRCOMMENT  = 1 shl XADFIB_XADSTRCOMMENT;

 (* NOTE: the texts passed with that structure must not always be printable.
    Although the clients should add an additional (not counted) zero at the text
    end, the whole file may contain other unprintable stuff (e.g. for DMS).
    So when printing this texts do it on a byte for byte base including
    printability checks.*)

 (***********************************************************************
  *                                                                     *
  *    progress report stuff                                            *
  *                                                                     *
  ***********************************************************************)
type
  PxadProgressInfo = ^TxadProgressInfo;
  TxadProgressInfo = record
    xpi_Mode: LongWord;         // work modus
    xpi_Client: PxadClient;     // the client doing the work
    xpi_DiskInfo: PxadDiskInfo; // current diskinfo, for disks
    xpi_FileInfo: PxadFileInfo; // current info for files
    xpi_CurrentSize: LongWord;  // current filesize
    xpi_LowCyl: LongWord;       // for disks only
    xpi_HighCyl: LongWord;      // for disks only
    xpi_Status: LongWord;       // see XADPIF flags
    xpi_Error: LongInt;         // any of the error codes
    xpi_FileName: STRPTR;       // name of file to overwrite (V2)
    xpi_NewName: STRPTR;        // new name buffer, passed by hook (V2)
  end;
  // NOTE: For disks CurrentSize is Sector*SectorSize, where SectorSize can
  // be found in xadDiskInfo structure. So you may output the sector value.

  // different progress modes
const
  XADPMODE_ASK        = 1;
  XADPMODE_PROGRESS   = 2;
  XADPMODE_END        = 3;
  XADPMODE_ERROR      = 4;
  XADPMODE_NEWENTRY   = 5; // (V10)
  XADPMODE_GETINFOEND = 6; // (V11)

  // flags for progress hook and ProgressInfo status field
  XADPIB_OVERWRITE      = 0; // overwrite the file
  XADPIB_MAKEDIRECTORY  = 1; // create the directory
  XADPIB_IGNOREGEOMETRY = 2; // ignore drive geometry
  XADPIB_ISDIRECTORY    = 3; // destination is a directory (V10)
  XADPIB_RENAME = 10; // rename the file (V2)
  XADPIB_OK     = 16; // all ok, proceed
  XADPIB_SKIP   = 17; // skip file

   XADPIF_OVERWRITE      = 1 shl XADPIB_OVERWRITE;
   XADPIF_MAKEDIRECTORY  = 1 shl XADPIB_MAKEDIRECTORY;
   XADPIF_IGNOREGEOMETRY = 1 shl XADPIB_IGNOREGEOMETRY;
   XADPIF_ISDIRECTORY    = 1 shl XADPIB_ISDIRECTORY;
   XADPIF_RENAME = 1 shl XADPIB_RENAME;
   XADPIF_OK     = 1 shl XADPIB_OK;
   XADPIF_SKIP   = 1 shl XADPIB_SKIP;

 (***********************************************************************
  *                                                                     *
  *    errors                                                           *
  *                                                                     *
  ***********************************************************************)

   XADERR_OK           = $0000; // no error
   XADERR_UNKNOWN      = $0001; // unknown error
   XADERR_INPUT        = $0002; // input data buffers border exceeded
   XADERR_OUTPUT       = $0003; // output data buffers border exceeded
   XADERR_BADPARAMS    = $0004; // function called with illegal parameters
   XADERR_NOMEMORY     = $0005; // not enough memory available
   XADERR_ILLEGALDATA  = $0006; // data is corrupted
   XADERR_NOTSUPPORTED = $0007; // command is not supported
   XADERR_RESOURCE     = $0008; // required resource missing
   XADERR_DECRUNCH     = $0009; // error on decrunching
   XADERR_FILETYPE     = $000A; // unknown file type
   XADERR_OPENFILE     = $000B; // opening file failed
   XADERR_SKIP         = $000C; // file, disk has been skipped
   XADERR_BREAK        = $000D; // user break in progress hook
   XADERR_FILEEXISTS   = $000E; // file already exists
   XADERR_PASSWORD     = $000F; // missing or wrong password
   XADERR_MAKEDIR      = $0010; // could not create directory
   XADERR_CHECKSUM     = $0011; // wrong checksum
   XADERR_VERIFY       = $0012; // verify failed (disk hook)
   XADERR_GEOMETRY     = $0013; // wrong drive geometry
   XADERR_DATAFORMAT   = $0014; // unknown data format
   XADERR_EMPTY        = $0015; // source contains no files
   XADERR_FILESYSTEM   = $0016; // unknown filesystem
   XADERR_FILEDIR      = $0017; // name of file exists as directory
   XADERR_SHORTBUFFER  = $0018; // buffer was to short
   XADERR_ENCODING     = $0019; // text encoding was defective

 (***********************************************************************
  *                                                                     *
  *    characterset and filename conversion                             *
  *                                                                     *
  ***********************************************************************)

  CHARSET_HOST = 0; // this is the ONLY destination setting for clients!

  CHARSET_UNICODE_UCS2_HOST         = 10; // 16bit Unicode (usually no source type)
  CHARSET_UNICODE_UCS2_BIGENDIAN    = 11; // 16bit Unicode big endian storage
  CHARSET_UNICODE_UCS2_LITTLEENDIAN = 12; // 16bit Unicode little endian storage
  CHARSET_UNICODE_UTF8              = 13; // variable size unicode encoding

  // all the 1xx types are generic types which also maybe a bit dynamic
  CHARSET_AMIGA = 100; // the default Amiga charset
  CHARSET_MSDOS = 101; // the default MSDOS charset
  CHARSET_MACOS = 102; // the default MacOS charset
  CHARSET_C64 = 103; // the default C64 charset
  CHARSET_ATARI_ST = 104; // the default Atari ST charset
  CHARSET_WINDOWS =  105; // the default Windows charset

  // all the 2xx to 9xx types are real charsets, use them whenever you know what the data really is
  CHARSET_ASCII =  200; // the lower 7 bits of ASCII charsets
  CHARSET_ISO_8859_1 = 201; // the base charset
  CHARSET_ISO_8859_15 = 215; // Euro-sign fixed ISO variant
  CHARSET_ATARI_ST_US = 300; // Atari ST (US) charset
  CHARSET_PETSCII_C64_LC = 301; // C64 lower case charset
  CHARSET_CODEPAGE_437 = 400; // IBM Codepage 437 charset
  CHARSET_CODEPAGE_1252 = 401; // Windows Codepage 1252 charset

 (***********************************************************************
  *                                                                     *
  *    client related stuff                                             *
  *                                                                     *
  ***********************************************************************)

type
  PxadForeman = ^TxadForeman;
  TxadForeman = record
    xfm_Security: LongWord; // should be XADFOREMAN_SECURITY
    xfm_ID: LongWord; // must be XADFOREMAN_ID
    xfm_Version: Word; // set to XADFOREMAN_VERSION
    xfm_Reserved: Word;
    xfm_VersString: STRPTR; // pointer to $VER: string
    xfm_FirstClient: PxadClient; // pointer to first client
  end;

 (***********************************************************************
  *                                                                     *
  *    client ID's                                                      *
  *                                                                     *
  ***********************************************************************)
  // If an external client has set the xc_Identifier field, the internal client is replaced.

  // disk archivers start with 1000
const
  XADCID_XMASH       = 1000;
  XADCID_SUPERDUPER3 = 1001;
  XADCID_XDISK       = 1002;
  XADCID_PACKDEV     = 1003;
  XADCID_ZOOM        = 1004;
  XADCID_ZOOM5       = 1005;
  XADCID_CRUNCHDISK  = 1006;
  XADCID_PACKDISK    = 1007;
  XADCID_MDC         = 1008;
  XADCID_COMPDISK    = 1009;
  XADCID_LHWARP           = 1010;
  XADCID_SAVAGECOMPRESSOR = 1011;
  XADCID_WARP             = 1012;
  XADCID_GDC              = 1013;
  XADCID_DCS              = 1014;

  // file archivers start with 5000
  XADCID_TAR             = 5000;
  XADCID_SDSSFX          = 5001;
  XADCID_LZX             = 5002;
  XADCID_MXMSIMPLEARC    = 5003;
  XADCID_LHPAK           = 5004;
  XADCID_AMIGAPLUSUNPACK = 5005;
  XADCID_AMIPACK         = 5006;
  XADCID_LHA             = 5007;
  XADCID_LHASFX          = 5008;
  XADCID_PCOMPARC        = 5009;
  XADCID_SOMNI           = 5010;
  XADCID_LHSFX           = 5011;
  XADCID_XPKARCHIVE      = 5012;
  XADCID_SHRINK          = 5013;
  XADCID_SPACK           = 5014;
  XADCID_SPACKSFX        = 5015;
  XADCID_ZIP             = 5016;
  XADCID_WINZIPEXE       = 5017;
  XADCID_GZIP            = 5018;
  XADCID_ARC             = 5019;
  XADCID_ZOO             = 5020;
  XADCID_LHAEXE          = 5021;
  XADCID_ARJ             = 5022;
  XADCID_ARJEXE          = 5023;
  XADCID_ZIPEXE          = 5024;
  XADCID_LHF             = 5025;
  XADCID_COMPRESS        = 5026;
  XADCID_ACE             = 5027;
  XADCID_ACEEXE          = 5028;
  XADCID_GZIPSFX         = 5029;
  XADCID_HA              = 5030;
  XADCID_SQ              = 5031;
  XADCID_LHAC64SFX       = 5032;
  XADCID_SIT             = 5033;
  XADCID_SIT5            = 5034;
  XADCID_SIT5EXE         = 5035;
  XADCID_MACBINARY       = 5036;
  XADCID_CPIO            = 5037;
  XADCID_PACKIT          = 5038;
  XADCID_CRUNCH          = 5039;
  XADCID_ARCCBM          = 5040;
  XADCID_ARCCBMSFX       = 5041;

  // filesystem client start with 8000
  XADCID_FSAMIGA    = 8000;
  XADCID_FSSANITYOS = 8001;
  XADCID_FSFAT      = 8002;

  // mixed archivers start with 9000
  XADCID_DMS    = 9000;
  XADCID_DMSSFX = 9001;



(*
  ; // function interface
  ASM(BOOL) xc_RecogData(REG(d0, ULONG size), REG(a0, STRPTR data),
    REG(a6, struct xadMasterBase *xadMasterBase));
  ASM(LONG) xc_GetInfo(REG(a0, struct xadArchiveInfo *ai),
    REG(a6, struct xadMasterBase *xadMasterBase));
  ASM(LONG) xc_UnArchive(REG(a0, struct xadArchiveInfo *ai),
    REG(a6, struct xadMasterBase *xadMasterBase));
  ASM(void) xc_Free(REG(a0, struct xadArchiveInfo *ai),
    REG(a6, struct xadMasterBase *xadMasterBase));


 ; // xc_RecogData returns 1 when recognized and 0 when not, all the others
     return 0 when ok and XADERR values on error. xc_Free has no return
     value.

     Filesystem clients need to clear xc_RecogSize and xc_RecogData. The
     recognition is automatically done by GetInfo. XADERR_FILESYSTEM is
     returned in case of unknown format. If it is known detection should
     go on and any other code may be returned, if it fails.
     The field xc_ArchiverName means xc_FileSystemName for filesystem
     clients.
 *)

var
  XADMasterBase: PxadMasterBase = nil;

{$if defined(Amiga68k) or defined(MorphOS)}
function xadAllocObjectA(Type_: LongInt location 'd0'; Tags: PTagItem location 'a0'): APTR; syscall XADMasterBase 30;
procedure xadFreeObjectA(Object_: APTR location 'a0'; Tags: PTagItem location 'a1'); syscall XADMasterBase 36;
function xadRecogFileA(Size: LongWord location 'd0'; Memory: APTR location 'a0'; Tags: PTagItem location 'a1'): PxadClient; syscall XADMasterBase 42;
function xadGetInfoA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 48;
procedure xadFreeInfo(ai: PxadArchiveInfo location 'a0'); syscall XADMasterBase 54;
function xadFileUnArcA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 60;
function xadDiskUnArcA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 66;
function xadGetErrorText(errnum: LongWord location 'd0'): STRPTR; syscall XADMasterBase 72;
function xadGetClientInfo(): PxadClient; syscall XADMasterBase 78;
function xadHookAccess(Command: LongWord location 'd0'; Data: LongInt location 'd1'; Buffer: APTR location 'a0'; ai: PxadArchiveInfo location 'a1'): LongInt; syscall XADMasterBase 84;
function xadConvertDatesA(Tags: PTagItem location 'a0'): LongInt; syscall XADMasterBase 90;
function xadCalcCRC16(id: LongWord location 'd0'; Init: LongWord location 'd1'; Size: LongWord location 'd2'; Buffer: STRPTR location 'a0'): Word; syscall XADMasterBase 96;
function xadCalcCRC32(id: LongWord location 'd0'; Init: LongWord location 'd1'; Size: LongWord location 'd2'; Buffer: STRPTR location 'a0'): LongWord; syscall XADMasterBase 102;
function xadAllocVec(Size: LongWord location 'd0'; Flags: LongWord location 'd1'): APTR; syscall XADMasterBase 108;
procedure xadCopyMem(Src: Pointer location 'a0'; Dest: Pointer location 'a1'; Size: LongWord location 'd0'); syscall XADMasterBase 114;
function xadHookTagAccessA(Command: LongWord location 'd0'; Data: LongInt location 'd1'; Buffer: APTR location 'a0'; ai: PxadArchiveInfo location 'a1'; Tags: PTagItem location 'a2'): LongInt; syscall XADMasterBase 120;
function xadConvertProtectionA(Tags: PTagItem location 'a0'): LongInt; syscall XADMasterBase 126;
function xadGetDiskInfoA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 132;
function xadDiskFileUnArcA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 138;
function xadGetHookAccessA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 144;
function xadFreeHookAccessA(ai: PxadArchiveInfo location 'a0'; Tags: PTagItem location 'a1'): LongInt; syscall XADMasterBase 150;
function xadAddFileEntryA(fi: PxadFileInfo location 'a0'; ai: PxadArchiveInfo location 'a1'; Tags: PTagItem location 'a2'): LongInt; syscall XADMasterBase 156;
function xadAddDiskEntryA(di: PxadDiskInfo location 'a0'; ai: PxadArchiveInfo location 'a1'; Tags: PTagItem location 'a2'): LongInt; syscall XADMasterBase 162;
function xadGetFilenameA(BufferSize: LongWord location 'd0'; Buffer: STRPTR location 'a0'; Path: STRPTR location 'a1'; Name: STRPTR location 'a2'; Tags: PTagItem location 'a3'): LongInt; syscall XADMasterBase 168;
function xadConvertNameA(Charset: LongWord location 'd0'; Tags: PTagItem location 'a0'): STRPTR; syscall XADMasterBase 174;
{$endif}

{$ifdef AROS}
function xadAllocObjectA(Type_: LongInt; Tags: PTagItem): APTR; syscall XADMasterBase 5;
procedure xadFreeObjectA(Object_: APTR; Tags: PTagItem); syscall XADMasterBase 6;
function xadRecogFileA(Size: LongWord; Memory: APTR; Tags: PTagItem): PxadClient; syscall XADMasterBase 7;
function xadGetInfoA(ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 8;
procedure xadFreeInfo(ai: PxadArchiveInfo); syscall XADMasterBase 9;
function xadFileUnArcA(ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 10;
function xadDiskUnArcA(ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 11;
function xadGetErrorText(errnum: LongWord): STRPTR; syscall XADMasterBase 12;
function xadGetClientInfo(): PxadClient; syscall XADMasterBase 13;
function xadHookAccess(Command: LongWord; Data: LongInt; Buffer: APTR; ai: PxadArchiveInfo): LongInt; syscall XADMasterBase 14;
function xadConvertDatesA(Tags: PTagItem): LongInt; syscall XADMasterBase 15;
function xadCalcCRC16(id: LongWord; Init: LongWord; Size: LongWord; Buffer: STRPTR): Word; syscall XADMasterBase 16;
function xadCalcCRC32(id: LongWord; Init: LongWord; Size: LongWord; Buffer: STRPTR): LongWord; syscall XADMasterBase 17;
function xadAllocVec(Size: LongWord; Flags: LongWord): APTR; syscall XADMasterBase 18;
procedure xadCopyMem(Src: Pointer; Dest: Pointer; Size: LongWord); syscall XADMasterBase 19;
function xadHookTagAccessA(Command: LongWord; Data: LongInt; Buffer: APTR; ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 20;
function xadConvertProtectionA(Tags: PTagItem): LongInt; syscall XADMasterBase 21;
function xadGetDiskInfoA(ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 22;
function xadGetHookAccessA(ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 23;
function xadFreeHookAccessA(ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 24;
function xadAddFileEntryA(fi: PxadFileInfo; ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 25;
function xadAddDiskEntryA(di: PxadDiskInfo; ai: PxadArchiveInfo; Tags: PTagItem): LongInt; syscall XADMasterBase 26;
function xadGetFilenameA(BufferSize: LongWord; Buffer: STRPTR; Path: STRPTR; Name: STRPTR; Tags: PTagItem): LongInt; syscall XADMasterBase 27;
function xadConvertNameA(Charset: LongWord; Tags: PTagItem): STRPTR; syscall XADMasterBase 28;
//function xadGetDefaultNameA(Tags: PTagItem): STRPTR; syscall XADMasterBase 29;
//function xadGetSystemInfo(): PxadSystemInfo; syscall XADMasterBase 30;
{$endif}


function xadAllocObject(Type_: LongInt; const TagList: array of PtrUInt): APTR;
procedure xadFreeObject(Object_: APTR; const TagList: array of PtrUInt);
function xadRecogFile(Size: LongWord; Memory: APTR; const TagList: array of PtrUInt): PxadClient;
function xadGetInfo(ai: PxadArchiveInfo; const TagList: array of PtrUInt): LongInt;
function xadFileUnArc(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadDiskUnArc(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadConvertDates(const TagsList: array of PtrUInt): LongInt;
function xadHookTagAccess(Command: LongWord; Data: LongInt; Buffer: APTR; ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadConvertProtection(const TagsList: array of PtrUInt): LongInt;
function xadGetDiskInfo(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
{$if defined(Amiga68k) or defined(MorphOS)}
function xadDiskFileUnArc(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
{$endif}
function xadGetHookAccess(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadFreeHookAccess(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadAddFileEntry(fi: PxadFileInfo; ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadAddDiskEntry(di: PxadDiskInfo; ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
function xadGetFilename(BufferSize: LongWord; Buffer: STRPTR; Path: STRPTR; Name: STRPTR; const TagsList: array of PtrUInt): LongInt;
function xadConvertNameA(Charset: LongWord; const TagsList: array of PtrUInt): STRPTR;

implementation


function xadAllocObject(Type_: LongInt; const TagList: array of PtrUInt): APTR;
begin
  Result := xadAllocObjectA(Type_, @TagList);
end;

procedure xadFreeObject(Object_: APTR; const TagList: array of PtrUInt);
begin
  xadFreeObjectA(Object_, @TagList);
end;

function xadRecogFile(Size: LongWord; Memory: APTR; const TagList: array of PtrUInt): PxadClient;
begin
  Result := xadRecogFileA(Size, Memory, @TagList);
end;

function xadGetInfo(ai: PxadArchiveInfo; const TagList: array of PtrUInt): LongInt;
begin
  Result := xadGetInfoA(ai, @TagList);
end;

function xadFileUnArc(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadFileUnArcA(ai, @TagsList);
end;

function xadDiskUnArc(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadDiskUnArcA(ai, @TagsList);
end;

function xadConvertDates(const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadConvertDatesA(@TagsList);
end;

function xadHookTagAccess(Command: LongWord; Data: LongInt; Buffer: APTR; ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadHookTagAccessA(Command, Data, Buffer, ai, @TagsList);
end;

function xadConvertProtection(const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadConvertProtectionA(@TagsList);
end;

function xadGetDiskInfo(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadGetDiskInfoA(ai, @TagsList);
end;

{$if defined(Amiga68k) or defined(MorphOS)}
function xadDiskFileUnArc(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadDiskFileUnArcA(ai, @TagsList);
end;
{$endif}

function xadGetHookAccess(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadGetHookAccessA(ai, @TagsList);
end;

function xadFreeHookAccess(ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadFreeHookAccessA(ai, @TagsList);
end;

function xadAddFileEntry(fi: PxadFileInfo; ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadAddFileEntryA(fi, ai, @TagsList);
end;

function xadAddDiskEntry(di: PxadDiskInfo; ai: PxadArchiveInfo; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadAddDiskEntryA(di, ai, @TagsList);
end;

function xadGetFilename(BufferSize: LongWord; Buffer: STRPTR; Path: STRPTR; Name: STRPTR; const TagsList: array of PtrUInt): LongInt;
begin
  Result := xadGetFilenameA(BufferSize, Buffer, Path, Name, @TagsList);
end;

function xadConvertNameA(Charset: LongWord; const TagsList: array of PtrUInt): STRPTR;
begin
  Result := xadConvertNameA(CharSet, @TagsList);
end;

initialization
  XADMasterBase := PxadMasterBase(OpenLibrary(XADNAME, 0));
finalization
  if Assigned(XADMasterBase) then
    CloseLibrary(PLibrary(XADMasterBase));
end.

