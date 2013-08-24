(*
**
** File/Message Area/Group record information
**
** Created: 21-Jul-2002
** Last update: 08-Aug-2002
**
** (c) 2002 by Maarten Bekers
**
**
*)

type
  MESSAGErecord  = record
                     AreaNum,
                     Unused         : SmallWord;
                     Name           : String[40];
                     Typ            : MsgType;
                     MsgKinds       : MsgKindsType;
                     Attribute      : Byte;

                      { Bit 0 : Enable EchoInfo
                            1 : Combined access
                            2 : File attaches
                            3 : Allow aliases
                            4 : Use SoftCRs as characters
                            5 : Force handle
                            6 : Allow deletes
                            7 : Is a JAM area }

                     DaysKill,    { Kill older than 'x' days }
                     RecvKill       : Byte; { Kill recv msgs, recv for more than 'x' days }
                     CountKill      : SmallWord;

                     ReadSecurity   : SmallWord;
                     ReadFlags,
                     ReadNotFlags   : FlagType;

                     WriteSecurity  : SmallWord;
                     WriteFlags,
                     WriteNotFlags  : FlagType;

                     SysopSecurity  : SmallWord;
                     SysopFlags,
                     SysopNotFlags  : FlagType;

                     OriginLine     : String[60];
                     AkaAddress     : Byte;

                     Age            : Byte;

                     JAMbase        : String[60];
                     Group          : SmallWord;
                     AltGroup       : Array[1..3] of SmallWord;

                     Attribute2     : Byte;

                      { Bit 0 : Include in all groups }

                     NetmailArea    : SmallWord;
                     FreeSpace2     : Array[1..7] of Byte;
                   end;

  GROUPrecord    = record
                     AreaNum        : SmallWord;
                     Name           : String[40];
                     Security       : SmallWord;
                     Flags,
                     NotFlagsMask   : FlagType;
                     Typ            : Integer[1]; { 0 = normal, 1 = forum }
                     FreeSpace      : Array[1..99] of Byte;
                   end;

  FILESrecord    = record
                     AreaNum,
                     Unused         : SmallWord;
                     Name           : String[40];
                     Attrib         : Byte;

                      { Bit 0 : Include in new files scan
                            1 : Include in upload dupe scan
                                                        2 : Permit long descriptions
                            3 : Area is on CD-ROM
                            4 : All files are FREE
                            5 : Allow DLs not in FDB
                            6 : Allow users to password uploads
                            7 : Scan uploads }

                     FilePath       : String[40];
                     KillDaysDL,
                     KillDaysFD     : SmallWord;
                     PassWord       : String[15];
                     MoveArea       : SmallWord;
                     Age,
                     ConvertExt     : Byte;
                     Group          : SmallWord;
                     Attrib2        : Byte;

                      { Bit 0 : Include in all groups }

                     DefCost,
                     UploadArea,
                     UploadSecurity : SmallWord;
                     UploadFlags,
                     UploadNotFlags : FlagType;
                     Security       : SmallWord;
                     Flags,
                     NotFlags       : FlagType;
                     ListSecurity   : SmallWord;
                     ListFlags,
                     ListNotFlags   : FlagType;
                     AltGroup       : Array[1..3] of SmallWord;
                     Device         : Byte;
                     FreeSpace      : Array[1..13] of Byte;
                   end;

  EleFilesRecord   = Record
    AreaNum             : Longint;
    ExportURL,                                  { URL used by EleFILE HTMLIST }
    ftpPath             : String[250];
    ftpLoginName,
    ftpPassword         : String[35];
    Attribute           : Byte;                 { Bit 0: Available for usenet }
    FreeSpace           : Array[1..140] of Longint;
  end; { EleFilesRecord }



  ForumInfoRecord = record
                      Moderator   : String;
                      Description : String;
                      PruneDays   : Longint;
                    end; { record }

