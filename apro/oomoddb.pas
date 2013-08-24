{$X+,V-,B-,I-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                   OOMODDB.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1995.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoModDB;
  {-Modem database management}

interface

uses
  {$IFDEF UseTPro}
  TpString,
  {$ENDIF}
  {$IFDEF UseOPro}
  OpString,
  {$ENDIF}
  ApMisc,
  ApPort,
  OoIniDB;

{key Strings}
const
  mkName         = 'Name';           { Modem name             }
  mkInitCmd      = 'InitCmd';        { Initialization command }
  mkDialCmd      = 'DialCmd';        { Dial command           }
  mkDialTerm     = 'DialTerm';       { Dial terminator        }
  mkDialCancel   = 'DialCancel';     { Dial cancel            }
  mkHangupCmd    = 'HangupCmd';      { Hangup command         }
  mkConfig       = 'ConfigCmd';      { Configuration command  }
  mkAnswerCmd    = 'AnswerCmd';      { Answer command         }
  mkOkMsg        = 'OkMsg';          { OK response            }
  mkConnectMsg   = 'ConnectMsg';     { CONNECT response       }
  mkBusyMsg      = 'BusyMsg';        { BUSY response          }
  mkVoiceMsg     = 'VoiceMsg';       { VOICE response         }
  mkNoCarrier    = 'NoCarrierMsg';   { NO CARRIER response    }
  mkNoDialTone   = 'NoDialToneMsg';  { NO DIALTONE response   }
  mkErrorStr     = 'ErrorMsg';       { ERROR response         }
  mkRing         = 'RingMsg';        { RING response          }
  mkErrorTags    = 'ErrorCheckTags'; { Error correction tags  }
  mkCompressTags = 'CompressTags';   { Data compression tags  }
  mkLockDTE      = 'LockDTE';        { Lock DTE rate          }
  mkDefBaud      = 'DefaultBaud';    { Default baud rate      }
  MaxTags      = 5;     {Maximum number of err corr or data comp tags}
  TagSepChar   = ',';   {Character that separates tags in a profile String}
  ModemNameLen = 31;    {Length of a modem name String}
  CmdLen       = 41;    {Maximum length of a modem command}
  RspLen       = 21;    {Maximum length of a modem response}
  TagLen       = 21;    {Maximum length of a tag String}
  TagProfLen   = 105;   {Maximum length of a tag profile String}
  BoolLen      = 5;     {Maximum length of a boolean String}
  BaudLen      = 7;     {Maximum length of a baud rate String}
  ConfigLen    = 255;   {Maximum length of a configuration String}

type
  ModemNameType    = String[ModemNameLen];
  CmdStrType       = String[CmdLen];
  RspStringType    = String[RspLen];
  TagStringType    = String[TagLen];
  TagProfStrType   = String[TagProfLen];
  ConfigStringType = String[ConfigLen];
  BooleanStrType   = String[BoolLen];
  BaudStrType      = String[BaudLen];

  TTagArray = array[1..MaxTags] of TagStringType;

  ModemBaseDataPtr = ^ModemBaseData;
  ModemBaseData = record
    Name          : ModemNameType;
    InitCmd       : CmdStrType;
    DialCmd       : CmdStrType;
    DialTerm      : CmdStrType;
    DialCancel    : CmdStrType;
    HangupCmd     : CmdStrType;
    ConfigCmd     : ConfigStringType;
    AnswerCmd     : CmdStrType;
    OkMsg         : RspStringType;
    ConnectMsg    : RspStringType;
    BusyMsg       : RspStringType;
    VoiceMsg      : RspStringType;
    NoCarrierMsg  : RspStringType;
    NoDialToneMsg : RspStringType;
    ErrorMsg      : RspStringType;
    RingMsg       : RspStringType;
  end;

  ModemDataPtr = ^ModemData;
  ModemData = record
    Data         : ModemBaseData;
    NumErrors    : Word;
    Errors       : TTagArray;
    NumComps     : Word;
    Compression  : TTagArray;
    LockDTE      : Boolean;
    DefBaud      : LongInt;
  end;

  ModemXFerPtr = ^ModemXFer;
  ModemXFer = record
    Data     : ModemBaseData;
    Errors   : TagProfStrType;
    Compress : TagProfStrType;
    LockDTE  : BooleanStrType;
    DefBaud  : BaudStrType;
  end;

  ModemDBasePtr = ^ModemDBase;
  ModemDBase =
    object (IniDataBase)
      constructor Init(FName : String; ReadOnly : Boolean; AlwaysRW: Boolean);
        {-Initialize a modem database}

      destructor Done; virtual;
        {-Destroy a modem database}

      procedure AddModem(Modem : ModemData);
        {-Add a modem to the database}

      procedure UpdModem(ModemName : ModemNameType; Modem : ModemData);
        {-Update a modem's record in the database}

      procedure DelModem(ModemName : ModemNameType);
        {-Delete a modem from the database}

      procedure RetrieveModem(ModemName : ModemNameType; var Modem : ModemData);
        {-Get a modem from the database}

      function NumModems : Integer;
        {-Return the number of modems in a database}

      procedure WriteModemToIni(Rec : ModemData; Group : String;
                                IniFile : String);
        {-Write the modem to a user-specified .INI file}

      procedure ReadModemFromIni(var Rec : ModemData; Group : String;
                                 IniFile : String);
        {-Read the modem from a user-specified .INI file}

      {+++internal+++}
      procedure UnpackTags(Data : TagProfStrType; var Tags : TTagArray;
                           var NumTags : Word);
      procedure PackTags(NumTags : Word; Tags : TTagArray;
                         var Dest : TagProfStrType);
      function Str2Boolean(St : String) : Boolean;
      function Boolean2Str(B : Boolean) : String;
      procedure TagsToXfer(var XFer  : ModemXFer;
                           Modem : ModemData);
      procedure XferToTags(var XFer  : ModemXFer;
                           var Modem : ModemData);

    end;


const
  DefBaudRate   : LongInt    = 2400;

  ModemDefaults : ModemXFer =
    ( Data            : (
        Name          : '';
        InitCmd       : 'ATZ^M';
        DialCmd       : 'ATD';
        DialTerm      : '^M';
        DialCancel    : '^M';
        HangupCmd     : '+++~~~ATH0^M';
        ConfigCmd     : 'ATE1Q0X1V1^M';
        AnswerCmd     : 'ATA^M';
        OkMsg         : 'OK';
        ConnectMsg    : 'CONNECT';
        BusyMsg       : 'BUSY';
        VoiceMsg      : 'VOICE';
        NoCarrierMsg  : 'NO CARRIER';
        NoDialToneMsg : 'NO DIAL';
        ErrorMsg      : 'ERROR';
        RingMsg       : 'RING'
      );
      Errors          : '';
      Compress        : '';
      LockDTE         : 'TRUE';
      DefBaud         : '2400'
    );

implementation

type
  TModemIniDef =
    record
      Name : String[MaxNameLen];
      Len  : Word;
    end;

  const
    ModemIniFields : array[1..19] of TModemIniDef =
      ( ( Name : mkInitCmd     ; Len : CmdLen     ),
        ( Name : mkDialCmd     ; Len : CmdLen     ),
        ( Name : mkDialTerm    ; Len : CmdLen     ),
        ( Name : mkDialCancel  ; Len : CmdLen     ),
        ( Name : mkHangupCmd   ; Len : CmdLen     ),
        ( Name : mkConfig      ; Len : ConfigLen  ),
        ( Name : mkAnswerCmd   ; Len : CmdLen     ),
        ( Name : mkOkMsg       ; Len : RspLen     ),
        ( Name : mkConnectMsg  ; Len : RspLen     ),
        ( Name : mkBusyMsg     ; Len : RspLen     ),
        ( Name : mkVoiceMsg    ; Len : RspLen     ),
        ( Name : mkNoCarrier   ; Len : RspLen     ),
        ( Name : mkNoDialTone  ; Len : RspLen     ),
        ( Name : mkErrorStr    ; Len : RspLen     ),
        ( Name : mkRing        ; Len : RspLen     ),
        ( Name : mkErrorTags   ; Len : TagProfLen ),
        ( Name : mkCompressTags; Len : TagProfLen ),
        ( Name : mkLockDTE     ; Len : BoolLen    ),
        ( Name : mkDefBaud     ; Len : BaudLen    ) );

  constructor ModemDBase.Init(FName : String; ReadOnly : Boolean;
                               AlwaysRW: Boolean);
    {-Initialize a modem database}
  var
    I : Integer;

  begin
    IniDataBase.Init(FName, ReadOnly, AlwaysRW);
    if (AsyncStatus <> ecOK) then
      Exit;

    AddIniDBStringField(mkName, ModemNameLen, True);
    if (AsyncStatus <> ecOK) then begin
      Done;
      Fail;
    end;

    for I := 1 to 19 do begin
      AddIniDBStringField(ModemIniFields[I].Name, ModemIniFields[I].Len, False);
      if (AsyncStatus <> ecOK) then begin
        Done;
        Fail;
      end;
    end;

    PrepareIniDatabase(@ModemDefaults);
    if AsyncStatus <> ecOK then begin
      Done;
      Fail;
    end;
  end;

  destructor ModemDBase.Done;
    {-Destroy a modem database}
  begin
    IniDatabase.Done;
  end;

  procedure ModemDBase.UnpackTags(Data : TagProfStrType; var Tags : TTagArray;
                                   var NumTags : Word);
    {-Unpack a set of tags from a profile String.}
  var
    I : Word;
    J : Word;

  begin
    FillChar(Tags,SizeOf(Tags),0);

    if Data = '' then begin
      NumTags := 0;
      Exit;
    end;

    J := 1;
    for I := 1 to Length(Data) do
      if Data[I] = TagSepChar then
        Inc(J)
      else Tags[J] := Tags[J] + Data[I];

    if Data[I] = TagSepChar
      then NumTags := J - 1
      else NumTags := J;
  end;

  procedure ModemDBase.PackTags(NumTags : Word; Tags : TTagArray;
                                 var Dest : TagProfStrType);
    {-Pack a tag String into a profile String.}
  var
    I : Word;

  begin
    Dest := '';
    if (NumTags > 0) then begin
      for I := 1 to Pred(NumTags) do begin
        Dest := Dest + Tags[I];
        Dest := Dest + TagSepChar;
      end;
      Dest := Dest + Tags[NumTags];
    end;
  end;

  function Long2Str(L : LongInt) : String;
    {-Convert a long/word/integer/byte/shortint to a String}
  var
    S : String;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  function ModemDBase.Str2Boolean(St : String) : Boolean;
    {-Return a Boolean based on St}
  begin
    if (Length(St) > BoolLen) then
      Str2Boolean := False
    else begin
      St := StUpCase(St);
      Str2Boolean := (St = 'TRUE') or (St  = 'YES') or (St = 'ON') or (St = '1');
    end;
  end;

  function ModemDBase.Boolean2Str(B : Boolean) : String;
    {-Return a String based on a Bool}
  begin
    if B then
      Boolean2Str := 'TRUE'
    else
      Boolean2Str := 'FALSE';
  end;

  procedure ModemDBase.TagsToXfer(var XFer  : ModemXFer;
                                  Modem : ModemData);
    {-Convert tag profile Strings into a tag String}
  begin
    XFer.Data := Modem.Data;
    PackTags(Modem.NumErrors, Modem.Errors, XFer.Errors);
    PackTags(Modem.NumComps, Modem.Compression, XFer.Compress);
    XFer.LockDTE := Boolean2Str(Modem.LockDTE);
    XFer.DefBaud := Long2Str(Modem.DefBaud);
  end;

  procedure ModemDBase.XferToTags(var XFer  : ModemXFer;
                                   var Modem : ModemData);
    {-Convert tag array into tag profile Strings}
  var
    Code : Integer;

  begin
    Modem.Data := XFer.Data;
    UnpackTags(XFer.Errors, Modem.Errors, Modem.NumErrors);
    UnpackTags(XFer.Compress, Modem.Compression, Modem.NumComps);
    Modem.LockDTE := Str2Boolean(XFer.LockDTE);
    Val(XFer.DefBaud, Modem.DefBaud, Code);
    if Code <> 0 then
      Modem.DefBaud := DefBaudRate;
  end;

  procedure ModemDBase.AddModem(Modem : ModemData);
    {-Add a modem to the database}
  var
    XFer : ModemXFer;

  begin
    FillChar(XFer, SizeOf(XFer), 0);
    TagsToXFer(XFer, Modem);
    AddIniRecord(XFer);
  end;

  procedure ModemDBase.UpdModem(ModemName : ModemNameType;
                                Modem : ModemData);
    {-Update a modem's record in the database}
  var
    XFer : ModemXFer;

  begin
    TagsToXfer(XFer, Modem);
    UpdIniRecord(ModemName, XFer);
  end;

  procedure ModemDBase.DelModem(ModemName : ModemNameType);
    {-Delete a modem from the database}
  begin
    DelIniRecord(ModemName);
  end;

  function ModemDBase.NumModems : Integer;
    {-Return the number of modems in a database}
  begin
    NumModems := NumIniRecs;
  end;

  procedure ModemDBase.RetrieveModem(ModemName : ModemNameType;
                                     var Modem : ModemData);
    {-Get a modem from the database}
  var
    XFer : ModemXFer;

  begin
    FillChar(Modem, SizeOf(Modem), 0);
    GetIniRecord(ModemName, XFer);
    if (AsyncStatus = ecOK) then
      XferToTags(Xfer, Modem);
  end;


  procedure ModemDBase.WriteModemToIni(Rec : ModemData; Group : String;
                                       IniFile : String);
    {-Write the modem to a user-specified .INI file }
  var
    XFer : ModemXFer;

  begin
    TagsToXfer(Xfer, Rec);
    WriteToIni(XFer, Group, IniFile);
  end;

  procedure ModemDBase.ReadModemFromIni(var Rec : ModemData; Group : String;
                                         IniFile : String);
    {-Read the modem from a user-specified .INI file }
  var
    Code : Integer;
    XFer : ModemXFer;

  begin
    ReadFromIni(XFer, Group, IniFile);
    if (AsyncStatus = ecOK) then
      XferToTags(Xfer, Rec);
  end;

end.
