(*
**
** Filebase header record information
**
** Created: 21-Jul-2002
** Last update: 08-Aug-2002
**
** (c) 2002 by Maarten Bekers
**
**
*)

type
  FILESHDRrecord = record
                     Name           : String[12];
                     Size,
                     CRC32          : LongInt;
                     Uploader       : String[35];
                     UploadDate,
                     FileDate,
                     LastDL         : LongInt;
                     TimesDL        : SmallWord;
                     Attrib         : Byte;
                     PassWord       : String[15];
                     KeyWord        : Array[1..5] of String[15];
                     Cost           : SmallWord;
                     LongDescPtr    : LongInt;
                     LfnPtr         : Longint;
                     FreeSpace      : Array[1..16] of Byte;
                   end;

          {Attrib - Bit 0 : Deleted
                        1 : Unlisted
                        2 : Free (don't adjust ratio) - Does NOT affect "Cost"
                        3 : Not available (don't allow downloads)
                        4 : Locked (no kill)
                        5 : Missing/offline
                        6 : No time restrictions - always allow DL
          }

  FILESIDXrecord = record
                     Name           : String[12];
                     UploadDate     : LongInt;
                     KeywordCRC     : Array[1..5] of LongInt;
                     LongDescPtr    : LongInt;
                   end;

