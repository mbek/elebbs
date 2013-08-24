Unit MKMsgAbs;       {Abstract Msg Object}
  {$I compiler.inc}
  {$I MKB.Def}

{
     MKMsgAbs - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     Rime: ->MYTHKING
     You may also reach me at maym@dmapub.dma.org
}


Interface

{$IFDEF WIN32}
uses CfgRec, SysUtils;
{$ELSE}
uses CfgRec, Dos;
{$ENDIF}


Type MsgMailType = (mmtNormal, mmtEchoMail, mmtNetMail);

Type AbsMsgObj = Object
  LastSoft: Boolean;
  Constructor Init; {Initialize}
  Destructor Done; Virtual; {Done}
  Procedure SetMsgPath(MP: String); Virtual; {Set msg path/other info}
  Function  OpenMsgBase: Word; Virtual; {Open the message base}
  Function  CloseMsgBase: Word; Virtual; {Close the message base}
  Function  CreateMsgBase(MaxMsg: Word; MaxDays: Word): Word; Virtual;
    {Create new message base}
  Function  MsgBaseExists: Boolean; Virtual; {Does msg base exist}
  Function  LockMsgBase: Boolean; Virtual; {Lock the message base}
  Function  UnLockMsgBase: Boolean; Virtual; {Unlock the message base}
  Procedure SetDest(Var Addr: AddrType); Virtual; {Set Zone/Net/Node/Point for Dest}
  Procedure SetOrig(Var Addr: AddrType); Virtual; {Set Zone/Net/Node/Point for Orig}
  Procedure SetFrom(Name: String); Virtual; {Set message from}
  Procedure SetTo(Name: String); Virtual; {Set message to}
  Procedure SetSubj(Str: String); Virtual; {Set message subject}
  Procedure SetCost(SCost: Word); Virtual; {Set message cost}
  Procedure SetRefer(SRefer: LongInt); Virtual; {Set message reference}
  Procedure SetSeeAlso(SAlso: LongInt); Virtual; {Set message see also}
  Procedure SetDate(SDate: String); Virtual; {Set message date}
  Procedure SetTime(STime: String); Virtual; {Set message time}
  Procedure SetLocal(LS: Boolean); Virtual; {Set local status}
  Procedure SetRcvd(RS: Boolean); Virtual; {Set received status}
  Procedure SetPriv(PS: Boolean); Virtual; {Set priveledge vs public status}
  Procedure SetCrash(SS: Boolean); Virtual; {Set crash netmail status}
  Procedure SetKillSent(SS: Boolean); Virtual; {Set kill/sent netmail status}
  Procedure SetSent(SS: Boolean); Virtual; {Set sent netmail status}
  Procedure SetFAttach(SS: Boolean); Virtual; {Set file attach status}
  Procedure SetReqRct(SS: Boolean); Virtual; {Set request receipt status}
  Procedure SetReqAud(SS: Boolean); Virtual; {Set request audit status}
  Procedure SetRetRct(SS: Boolean); Virtual; {Set return receipt status}
  Procedure SetFileReq(SS: Boolean); Virtual; {Set file request status}
  Procedure DoString(Str: String); Virtual; {Add string to message text}
  Procedure DoChar(Ch: Char); Virtual; {Add character to message text}
  Procedure DoStringLn(Str: String); Virtual; {Add string and newline to msg text}
  Procedure DoKludgeLn(Str: String); Virtual; {Add ^A kludge line to msg}
  Function  WriteMsg: Word; Virtual; {Write msg to msg base}
  Function  GetChar: Char; Virtual; {Get msg text character}
  Function  EOM: Boolean; Virtual; {No more msg text}
  Function  GetString(MaxLen: Word): String; Virtual; {Get wordwrapped string}
  Function  GetNoKludgeStr(MaxLen: Word): String; Virtual; {Get ww str no ^A lines}
  Function  WasWrap: Boolean; Virtual; {Last line was soft wrapped no CR}
  Function  GetFrom: String; Virtual; {Get from name on current msg}
  Function  GetTo: String; Virtual; {Get to name on current msg}
  Function  GetSubj: String; Virtual; {Get subject on current msg}
  Function  GetCost: Word; Virtual; {Get cost of current msg}
  Function  GetDate: String; Virtual; {Get date of current msg}
  Function  GetTime: String; Virtual; {Get time of current msg}
  Function  GetRefer: LongInt; Virtual; {Get reply to of current msg}
  Function  GetSeeAlso: LongInt; Virtual; {Get see also of current msg}
  Function  GetNextSeeAlso: LongInt; Virtual;
  Procedure SetNextSeeAlso(SAlso: LongInt); Virtual;
  Function  GetMsgNum: LongInt; Virtual; {Get message number}
  Procedure GetOrig(Var Addr: AddrType); Virtual; {Get origin address}
  Procedure GetDest(Var Addr: AddrType); Virtual; {Get destination address}
  Function  IsLocal: Boolean; Virtual; {Is current msg local}
  Function  IsCrash: Boolean; Virtual; {Is current msg crash}
  Function  IsKillSent: Boolean; Virtual; {Is current msg kill sent}
  Function  IsSent: Boolean; Virtual; {Is current msg sent}
  Function  IsFAttach: Boolean; Virtual; {Is current msg file attach}
  Function  IsReqRct: Boolean; Virtual; {Is current msg request receipt}
  Function  IsReqAud: Boolean; Virtual; {Is current msg request audit}
  Function  IsRetRct: Boolean; Virtual; {Is current msg a return receipt}
  Function  IsFileReq: Boolean; Virtual; {Is current msg a file request}
  Function  IsRcvd: Boolean; Virtual; {Is current msg received}
  Function  IsPriv: Boolean; Virtual; {Is current msg priviledged/private}
  Function  IsDeleted: Boolean; Virtual; {Is current msg deleted}
  Function  IsEchoed: Boolean; Virtual; {Is current msg unmoved echomail msg}
  Function  GetMsgLoc: LongInt; Virtual; {To allow reseeking to message}
  Procedure SetMsgLoc(ML: LongInt); Virtual; {Reseek to message}
  Procedure MsgStartUp; Virtual; {Do message set-up tasks}
  Procedure MsgTxtStartUp; Virtual; {Do message text start up tasks}
  Procedure StartNewMsg; Virtual; {Initialize for adding message}
  Procedure SeekFirst(MsgNum: LongInt); Virtual; {Start msg seek}
  Procedure SeekNext; Virtual; {Find next matching msg}
  Procedure SeekPrior; Virtual; {Prior msg}
  Function  SeekFound: Boolean; Virtual; {Msg was found}
  Procedure YoursFirst(Name: String; Handle: String); Virtual; {Seek your mail}
  Procedure YoursNext; Virtual; {Seek next your mail}
  Function  YoursFound: Boolean; Virtual; {Message found}
  Function  GetHighMsgNum: LongInt; Virtual; {Get highest msg number}
  Procedure SetMailType(MT: MsgMailType); Virtual; {Set message base type}
  Function  GetSubArea: Word; Virtual; {Get sub area number}
  Procedure ReWriteHdr; Virtual; {Rewrite msg header after changes}
  Procedure DeleteMsg; Virtual; {Delete current message}
  Procedure SetEcho(ES: Boolean); Virtual; {Set echo status}
  Function  NumberOfMsgs: LongInt; Virtual; {Number of messages}
  Function  GetLastRead(UNum, UCRc: LongInt): LongInt; Virtual; {Get last read for user num}
  Procedure SetLastRead(UNum: LongInt; LR: LongInt; UCRc: Longint); Virtual; {Set last read}
  Function  GetMsgDisplayNum: LongInt; Virtual; {Get msg number to display}
  Function  GetTxtPos: LongInt; Virtual; {Get indicator of msg text position}
  Procedure SetTxtPos(TP: LongInt); Virtual; {Set text position}
  Function  GetHighActiveMsgNum: LongInt; Virtual; {Get highest active msg num}

  Function  IsNetMail: Boolean; virtual;
  Procedure SetNetMail(SN: Boolean); virtual;
  procedure ClearMsgText; virtual;
  procedure UpdateMsgText(var WriteError: Word); virtual;

  {$IFNDEF MSDOS}
  function GetMsgBuffer: AnsiString; virtual;
  {$ELSE}
  function GetMsgBuffer: String; virtual;
  {$ENDIF}
  End;


Type AbsMsgPtr = ^AbsMsgObj;


Implementation


Constructor AbsMsgObj.Init;
  Begin
  End;


Destructor AbsMsgObj.Done;
  Begin
  End;


Procedure AbsMsgObj.SetMsgPath(MP: String);
  Begin
  End;


Function AbsMsgObj.OpenMsgBase: Word;
  Begin
  End;


Function AbsMsgObj.CloseMsgBase: Word;
  Begin
  End;


Function AbsMsgObj.LockMsgBase: Boolean;
  Begin
  End;


Function AbsMsgObj.UnLockMsgBase: Boolean;
  Begin
  End;


Procedure AbsMsgObj.SetDest(Var Addr: AddrType);
  Begin
  End;


Procedure AbsMsgObj.SetOrig(Var Addr: AddrType);
  Begin
  End;


Procedure AbsMsgObj.SetFrom(Name: String);
  Begin
  End;


Procedure AbsMsgObj.SetTo(Name: String);
  Begin
  End;


Procedure AbsMsgObj.SetSubj(Str: String);
  Begin
  End;


Procedure AbsMsgObj.SetCost(SCost: Word);
  Begin
  End;


Procedure AbsMsgObj.SetRefer(SRefer: LongInt);
  Begin
  End;


Procedure AbsMsgObj.SetSeeAlso(SAlso: LongInt);
  Begin
  End;


Procedure AbsMsgObj.SetDate(SDate: String);
  Begin
  End;


Procedure AbsMsgObj.SetTime(STime: String);
  Begin
  End;


Procedure AbsMsgObj.SetLocal(LS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetRcvd(RS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetPriv(PS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetCrash(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetKillSent(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetSent(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetFAttach(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetReqRct(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetReqAud(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetRetRct(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SetFileReq(SS: Boolean);
  Begin
  End;


Procedure AbsMsgObj.DoString(Str: String);
  Var
    i: Word;

  Begin
  For i := 1 to Length(Str) Do
    DoChar(Str[i]);
  End;


Procedure AbsMsgObj.DoChar(Ch: Char);
  Begin
  End;


Procedure AbsMsgObj.DoStringLn(Str: String);
  Begin
  DoString(Str);
  DoChar(#13);
  End;

Procedure AbsMsgObj.DoKludgeLn(Str: String);
  Begin
  DoStringLn(Str);
  End;


procedure AbsMsgObj.ClearMsgText;
  begin
    WriteLn('!! Not implemented for this message base !!');
  end;

procedure AbsMsgObj.UpdateMsgText(var WriteError: Word);
  begin
    WriteLn('!! Not implemented for this message base !!');
  end;

{$IFNDEF MSDOS}
function AbsMsgObj.GetMsgBuffer: AnsiString;
begin
  GetMsgBuffer := 'Not implemented for this messagebase!';
end; { func. GetMsgBuffer }
{$ELSE}
function AbsMsgObj.GetMsgBuffer: String;
begin
  GetMsgBuffer := 'Not impelemented for this messagebase!';
end; { func. GetMsgBuffer }
{$ENDIF}

Function AbsMsgObj.WriteMsg: Word;
  Begin
  End;


Function AbsMsgObj.GetChar: Char;
  Begin
  End;


Function AbsMsgObj.EOM: Boolean;
  Begin
  End;


Function AbsMsgObj.GetString(MaxLen: Word): String;
  Var
    WPos: LongInt;
    WLen: Byte;
    StrDone: Boolean;
    TxtOver: Boolean;
    StartSoft: Boolean;
    CurrLen: Word;
    PPos: LongInt;
    TmpCh: Char;
    OldPos: LongInt;

  Begin
  If EOM Then
    GetString := ''
  Else
    Begin
    StrDone := False;
    CurrLen := 0;
    PPos := GetTxtPos;
    WPos := GetTxtPos;
    WLen := 0;
    StartSoft := LastSoft;
    LastSoft := True;
    OldPos := GetTxtPos;
    TmpCh := GetChar;
    While ((Not StrDone) And (CurrLen < MaxLen) And (Not EOM)) Do
      Begin
      Case TmpCh of
        #$00:;
        #$0d: Begin
              StrDone := True;
              LastSoft := False;
              End;
        #$8d:;
        #$0a:;
        #$20: Begin
              If ((CurrLen <> 0) or (Not StartSoft)) Then
                Begin
                Inc(CurrLen);
                WLen := CurrLen;
                GetString[CurrLen] := TmpCh;
                WPos := GetTxtPos;
                End
              Else
                StartSoft := False;
              End;
        Else
          Begin
          Inc(CurrLen);
          GetString[CurrLen] := TmpCh;
          End;
        End;
      If Not StrDone Then
        Begin
        OldPos := GetTxtPos;
        TmpCh := GetChar;
        End;
      End;
    If StrDone Then
      Begin
      GetString[0] := Chr(CurrLen);
      End
    Else
      If EOM Then
        Begin
        GetString[0] := Chr(CurrLen);
        End
      Else
        Begin
        If WLen = 0 Then
          Begin
          GetString[0] := Chr(CurrLen);
          SetTxtPos(OldPos);
          End
        Else
          Begin
          GetString[0] := Chr(WLen);
          SetTxtPos(WPos);
          End;
        End;
    End;
  End;



Function AbsMsgObj.WasWrap: Boolean;
  Begin
  WasWrap := LastSoft;
  End;


Procedure AbsMsgObj.SeekFirst(MsgNum: LongInt);
  Begin
  End;


Procedure AbsMsgObj.SeekNext;
  Begin
  End;


Function AbsMsgObj.GetFrom: String;
  Begin
  End;


Function AbsMsgObj.GetTo: String;
  Begin
  End;


Function AbsMsgObj.GetSubj: String;
  Begin
  End;


Function AbsMsgObj.GetCost: Word;
  Begin
  End;


Function AbsMsgObj.GetDate: String;
  Begin
  End;


Function AbsMsgObj.GetTime: String;
  Begin
  End;


Function AbsMsgObj.GetRefer: LongInt;
  Begin
  End;


Function AbsMsgObj.GetSeeAlso: LongInt;
  Begin
  End;


Function AbsMsgObj.GetMsgNum: LongInt;
  Begin
  End;


Procedure AbsMsgObj.GetOrig(Var Addr: AddrType);
  Begin
  End;


Procedure AbsMsgObj.GetDest(Var Addr: AddrType);
  Begin
  End;


Function AbsMsgObj.IsLocal: Boolean;
  Begin
  End;


Function AbsMsgObj.IsCrash: Boolean;
  Begin
  End;


Function AbsMsgObj.IsKillSent: Boolean;
  Begin
  End;


Function AbsMsgObj.IsSent: Boolean;
  Begin
  End;


Function AbsMsgObj.IsFAttach: Boolean;
  Begin
  End;


Function AbsMsgObj.IsReqRct: Boolean;
  Begin
  End;


Function AbsMsgObj.IsReqAud: Boolean;
  Begin
  End;


Function AbsMsgObj.IsRetRct: Boolean;
  Begin
  End;


Function AbsMsgObj.IsFileReq: Boolean;
  Begin
  End;


Function AbsMsgObj.IsRcvd: Boolean;
  Begin
  End;


Function AbsMsgObj.IsPriv: Boolean;
  Begin
  End;


Function AbsMsgObj.IsDeleted: Boolean;
  Begin
  End;


Function AbsMsgObj.IsEchoed: Boolean;
  Begin
  End;


Function AbsMsgObj.GetMsgLoc: LongInt;
  Begin
  End;


Procedure AbsMsgObj.SetMsgLoc(ML: LongInt);
  Begin
  End;


Procedure AbsMsgObj.MsgStartUp;
  Begin
  End;


Procedure AbsMsgObj.MsgTxtStartUp;
  Begin
  End;


Procedure AbsMsgObj.YoursFirst(Name: String; Handle: String);
  Begin
  End;


Procedure AbsMsgObj.YoursNext;
  Begin
  End;


Function AbsMsgObj.YoursFound: Boolean;
  Begin
  End;


Function AbsMsgObj.CreateMsgBase(MaxMsg: Word; MaxDays: Word): Word;
  Begin
  End;


Function AbsMsgObj.MsgBaseExists: Boolean;
  Begin
  End;


Procedure AbsMsgObj.StartNewMsg;
  Begin
  End;


Function AbsMsgObj.GetHighMsgNum: LongInt;
  Begin
  End;


Function AbsMsgObj.SeekFound: Boolean;
  Begin
  End;


Procedure AbsMsgObj.SetMailType(MT: MsgMailType);
  Begin
  End;


Function AbsMsgObj.GetSubArea: Word;
  Begin
  GetSubArea := 0;
  End;


Procedure AbsMsgObj.ReWriteHdr;
  Begin
  End;


Procedure AbsMsgObj.DeleteMsg;
  Begin
  End;


Procedure AbsMsgObj.SetEcho(ES: Boolean);
  Begin
  End;


Procedure AbsMsgObj.SeekPrior;
  Begin
  End;


Function AbsMsgObj.NumberOfMsgs: LongInt;
  Begin
  End;


Function AbsMsgObj.GetLastRead(UNum, UCRC: LongInt): LongInt;
  Begin
  End;

Procedure AbsMsgObj.SetLastRead(UNum: LongInt; LR: LongInt; UCRC: Longint);
  Begin
  End;

Function AbsMsgObj.GetMsgDisplayNum: LongInt;
  Begin
  GetMsgDisplayNum := GetMsgNum;
  End;

Function AbsMsgObj.GetTxtPos: LongInt;
  Begin
  GetTxtPos := 0;
  End;

Procedure AbsMsgObj.SetTxtPos(TP: LongInt);
  Begin
  End;


 {SetNextSeeAlso provided by 2:201/623@FidoNet Jonas@iis.bbs.bad.se}


Procedure AbsMsgObj.SetNextSeeAlso(SAlso: LongInt);
  Begin
  End;

Function AbsMsgObj.GetNextSeeAlso: LongInt;
  Begin
  GetNextSeeAlso:=0;
  End;


Function  AbsMsgObj.GetNoKludgeStr(MaxLen: Word): String;
{From a suggestion by Johan Corstjens 2:281/610}
  Var
    TmpStr: String;
  Begin
  TmpStr := GetString(MaxLen);
  While ((Length(TmpStr) > 0) and (TmpStr[1] = #1) and (Not EOM)) Do
    TmpStr := GetString(MaxLen);
  GetNoKludgeStr := TmpStr;
  End;


Function  AbsMsgObj.IsNetMail: Boolean;
  begin
    IsNetMail := False;
  end;

Procedure AbsMsgObj.SetNetMail(SN: Boolean);
  begin
  end; { SetNetMail }

Function AbsMsgObj.GetHighActiveMsgNum: LongInt;
  Begin
{ was:   SeekFirst(GetHighMsgNum); }
  SeekFirst(GetHighMsgNum + 1);

  if NOT SeekFound then SeekPrior;

  If SeekFound Then
   begin
    MsgStartup;
    GetHighActiveMsgNum := GetMsgNum
   end
  Else
    GetHighActiveMsgNum := 0;
  End;

End.
