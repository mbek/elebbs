(*
**
** Extended user file support routines
**
** Created: 21-Jul-2002
** Last update: 21-Jul-2002
**
** (c) 2002 by Maarten Bekers
**
**
*)

type
  USERSrecord    = record
                     Name           : MSGTOIDXrecord;
                     Location       : String[25];
                     Organisation,
                     Address1,
                     Address2,
                     Address3       : String[50];
                     Handle         : String[35];
                     Comment        : String[80];
                     PasswordCRC    : LongInt;
                     DataPhone,
                     VoicePhone     : String[15];
                     LastTime       : Time;
                     LastDate       : Date;

                     Attribute,

                      { Bit 0 : Deleted
                            1 : Clear screen
                            2 : More prompt
                            3 : ANSI
                            4 : No-kill
                            5 : Xfer priority
                            6 : Full screen msg editor
                            7 : Quiet mode }

                     Attribute2     : Byte;

                      { Bit 0 : Hot-keys
                            1 : AVT/0
                            2 : Full screen message viewer
                            3 : Hidden from userlist
                            4 : Page priority
                            5 : No echomail in mailbox scan
                            6 : Guest account
                            7 : Post bill enabled }

                     Flags          : FlagType;
                     Credit,
                     Pending        : LongInt;
                     MsgsPosted,
                     Security       : SmallWord;
                     LastRead,
                     NoCalls,
                     Uploads,
                     Downloads,
                     UploadsK,
                     DownloadsK,
                     TodayK         : LongInt;
                     Elapsed        : SmallInt;
                     ScreenLength   : SmallWord;
                     LastPwdChange  : Byte;
                     Group          : SmallWord;
                     CombinedInfo   : COMBINEDrecord;
                     FirstDate,
                     BirthDate,
                     SubDate        : Date;
                     ScreenWidth,
                     Language,
                     DateFormat     : Byte;
                     {  No.      Sort of format to use : }
                     {  01       DD-MM-YY        }
                     {  02       MM-DD-YY        }
                     {  03       YY-MM-DD        }
                     {  04       DD-Mmm-YY       }
                     {  05       DD-MM-YYYY      }
                     {  06       MM-DD-YYYY      }
                     {  07       YYYY-MM-DD      }
                     {  08       DD-Mmm-YYYY     }

                     ForwardTo      : String[35];
                     MsgArea,
                     FileArea       : SmallWord;
                     DefaultProtocol: Char;
                     FileGroup      : SmallWord;
                     LastDOBCheck   : Byte;
                     Sex            : Byte;
                     XIrecord       : LongInt;
                     MsgGroup       : SmallWord;

                     Attribute3     : Byte;

                      { Bit 0 : Mailbox check: scan selected areas only }

                     PassWord       : String[15];

                     FreeSpace      : Array[1..31] of Byte;
                   end;


type
  UsrEleRecord = record
                   Pth: String[250];
                 end; { record }

type
  UsrExtraInfo = record
                   MailAddress : String;
                   IcqNumber   : String;
                   HomePage    : String;
                   Avatar      : String;
                   MemberType  : String;
                   Signature   : String;
                   LastMsgDate : Integer[4];
                 end; { record }

