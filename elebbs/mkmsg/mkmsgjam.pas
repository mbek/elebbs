Unit MKMsgJam;       {JAM Msg Object Unit}

{
     Thanks to the following for creating the JAM message base format
}


{
     JAM(mbp) - Copyright 1993 Joaquim Homrighausen, Andrew Milner,
                               Mats Birch, Mats Wallin.
                               ALL RIGHTS RESERVED.
}

{
     The JAM developers have requested that you include the above
     copyright notice in any JAM products you develop
}

{
     MKMsgJam - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     You may also reach me at maym@dmapub.dma.org

     This implementation handles unlimited text size (well actually
     limited by available disk space and a maximum number of characters
     of 2,147,483,647) by using a temporary buffer file for larger msgs.
     Smaller msgs (under 4k) are still handled in memory for maximum
     speed.

     Mark May
     MK Software
}

  {$I compiler.inc}
  {$I MKB.Def}
{$X+}

Interface

Uses MKMsgAbs, MkFile, CfgRec{$ifndef Win32}, strings{$Endif}, GenDos,
     FileObj,
{$IFDEF WINGUI}
  SysUtils;
{$ELSE}
  Dos;
{$ENDIF}

Const BasePath : String = '';   { To generate ECHOMAIL.JAM }

{$IFNDEF VirtualPascal}
  Type SmallWord = Word;
{$ENDIF}

Type JamHdrType = Record
  Signature: Array[1..4] of Char;
  Created: LongInt;
  ModCounter: LongInt;
  ActiveMsgs: LongInt;
  PwdCRC: LongInt;
  BaseMsgNum: LongInt;
  Extra: Array[1..1000] of Char;
  End;


Type JamMsgHdrType = Record
  Signature: Array[1..4] of Char;
  Rev: SmallWord;
  Resvd: SmallWord;
  SubFieldLen: LongInt;
  TimesRead: LongInt;
  MsgIdCrc: LongInt;
  ReplyCrc: LongInt;
  ReplyTo: LongInt;
  ReplyFirst: LongInt;
  ReplyNext: LongInt;
  DateWritten: LongInt;
  DateRcvd: LongInt;
  DateArrived: LongInt;
  MsgNum: LongInt;
  Attr1: LongInt;
  Attr2: LongInt;
  TextOfs: LongInt;
  TextLen: LongInt;
  PwdCrc: LongInt;
  Cost: LongInt;
  End;


Type JamIdxType = Record
  MsgToCrc: LongInt;
  HdrLoc: LongInt;
  End;


Type JamLastType = Record
  NameCrc: LongInt;
  UserNum: LongInt;
  LastRead: LongInt;
  HighRead: LongInt;
  End;


Const JamIdxBufSize = 500;


Type JamIdxArrayType = Array[0..JamIdxBufSize] of JamIdxType;


Const JamSubBufSize = 4000;


Type JamSubBuffer = Array[1..JamSubBufSize] of Char;


Const JamTxtBufSize = 4000;

Const TxtSubBufSize = 2000;            {Note actual size is one greater}


Type JamTxtBufType = Array[0..JamTxtBufSize] Of Char;

Type HdrType = Record
  JamHdr: JamMsgHdrType;
  SubBuf: JamSubBuffer;
  End;


Type JamMsgType = Record
  HdrFile: pFileObj;
  TxtFile: pFileOBj;
  IdxFile: pFileObj;
  MsgPath: String[128];
  BaseHdr: JamHdrType;
  Dest: AddrType;
  Orig: AddrType;
  MsgFrom: String[65];
  MsgTo: String[65];
  MsgSubj: String[100];
  MsgDate: String[8];
  MsgTime: String[5];
  CurrMsgNum: LongInt;
  YourName: String[35];
  YourHdl: String[35];
  NameCrc: LongInt;
  HdlCrc: LongInt;
  TxtPos: LongInt; {TxtPos < 0 means get from sub text}
  TxtEnd: LongInt;
  TxtBufStart: LongInt;
  TxtRead: NumReadType;
  MailType: MsgMailType;
  BufFile: pFileObj;
  LockCount: LongInt;
  IdxStart: LongInt;
  IdxRead: NumReadType;
  TxtSubBuf: Array[0..TxtSubBufSize] of Char; {temp storage for text on subfields}
  TxtSubChars: Integer;
  End;


Type JamMsgObj = Object (AbsMsgObj)
  JM: ^JamMsgType;
  MsgHdr: ^HdrType;
  JamIdx: ^JamIdxArrayType;
  TxtBuf: ^JamTxtBufType;
  Error: Longint;
  Constructor Init;                      {Initialize}
  Destructor Done; Virtual; {Done}
  Procedure SetMsgPath(St: String); Virtual; {Set netmail path}
  Function  GetHighMsgNum: LongInt; Virtual; {Get highest netmail msg number in area}
  Function  LockMsgBase: Boolean; Virtual; {Lock the message base}
  Function  UnLockMsgBase: Boolean; Virtual; {Unlock the message base}
  Function  WriteMailIdx(FN: String; MsgPos: Word): Word; Virtual;
    {Write Netmail or EchoMail.jam}
  Procedure SetDest(Var Addr: AddrType); Virtual; {Set Zone/Net/Node/Point for Dest}
  Procedure SetOrig(Var Addr: AddrType); Virtual; {Set Zone/Net/Node/Point for Orig}
  Procedure SetFrom(Name: String); Virtual; {Set message from}
  Procedure SetTo(Name: String); Virtual; {Set message to}
  Procedure SetSubj(Str: String); Virtual; {Set message subject}
  Procedure SetCost(SCost: Word); Virtual; {Set message cost}
  Procedure SetRefer(SRefer: LongInt); Virtual; {Set message reference}
  Procedure SetSeeAlso(SAlso: LongInt); Virtual; {Set message see also}
  Function  GetNextSeeAlso: LongInt; Virtual;
  Procedure SetNextSeeAlso(SAlso: LongInt); Virtual;
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
  Procedure DoKludgeLn(Str: String); Virtual; {Add ^AKludge line to msg}
  Function  WriteMsg: Word; Virtual;
  Function  GetChar: Char; Virtual;
  Procedure AddTxtSub(St: String);
  Procedure MsgStartUp; Virtual; {set up msg for reading}
  Function  EOM: Boolean; Virtual; {No more msg text}
  Function  GetString(MaxLen: Word): ShortString; Virtual; {Get wordwrapped string}
  Function  WasWrap: Boolean; Virtual; {Last line was soft wrapped no CR}
  Procedure SeekFirst(MsgNum: LongInt); Virtual; {Seek msg number}
  Procedure SeekNext; Virtual; {Find next matching msg}
  Procedure SeekPrior; Virtual; {Seek prior matching msg}
  Function  GetFrom: String; Virtual; {Get from name on current msg}
  Function  GetTo: String; Virtual; {Get to name on current msg}
  Function  GetSubj: String; Virtual; {Get subject on current msg}
  Function  GetCost: Word; Virtual; {Get cost of current msg}
  Function  GetDate: String; Virtual; {Get date of current msg}
  Function  GetTime: String; Virtual; {Get time of current msg}
  Function  GetRefer: LongInt; Virtual; {Get reply to of current msg}
  Function  GetSeeAlso: LongInt; Virtual; {Get see also of current msg}
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
  Function  IsEchoed: Boolean; Virtual; {Msg should be echoed}
  Function  GetMsgLoc: LongInt; Virtual; {Msg location}
  Procedure SetMsgLoc(ML: LongInt); Virtual; {Msg location}
  Procedure YoursFirst(Name: String; Handle: String); Virtual; {Seek your mail}
  Procedure YoursNext; Virtual; {Seek next your mail}
  Function  YoursFound: Boolean; Virtual; {Message found}
  Procedure StartNewMsg; Virtual;
  Function  OpenMsgBase: Word; Virtual;
  Function  CloseMsgBase: Word; Virtual;
  Function  MsgBaseExists: Boolean; Virtual; {Does msg base exist}
  Function  CreateMsgBase(MaxMsg: Word; MaxDays: Word): Word; Virtual;
  Function  SeekFound: Boolean; Virtual;
  Procedure SetMailType(MT: MsgMailType); Virtual; {Set message base type}
  Function  GetSubArea: Word; Virtual; {Get sub area number}
  Procedure ReWriteHdr; Virtual; {Rewrite msg header after changes}
  Procedure DeleteMsg; Virtual; {Delete current message}
  Function  NumberOfMsgs: LongInt; Virtual; {Number of messages}
  Function  GetLastRead(UNum, UCRC: LongInt): LongInt; Virtual; {Get last read for user num}
  Procedure SetLastRead(UNum: LongInt; LR, UCRC: LongInt); Virtual; {Set last read}
  Procedure MsgTxtStartUp; Virtual; {Do message text start up tasks}
  Function  GetTxtPos: LongInt; Virtual; {Get indicator of msg text position}
  Procedure SetTxtPos(TP: LongInt); Virtual; {Set text position}
  Procedure SetAttr1(Mask: LongInt; St: Boolean); {Set attribute 1}
  Function  ReadIdx: NumReadType;
  Function  WriteIdx: NumReadType;
  Procedure AddSubField(id: SmallWord; Data: String);
  Function  FindLastRead(Var LastFile: pFileOBj; UNum, UCRC: LongInt): LongInt;
  Function  ReReadIdx(Var IdxLoc : LongInt) : Word;

  Function  IsNetMail: Boolean; virtual;
  Procedure SetNetMail(SN: Boolean); virtual;

  procedure ClearMsgText; virtual;
  procedure UpdateMsgText(var WriteError: Word); virtual;

  procedure SetDateTime;

  {$IFNDEF MSDOS}
  function GetMsgBuffer: AnsiString; virtual;
  {$ENDIF}
  End;


Type JamMsgPtr = ^JamMsgObj;


Function JamStrCrc(St: String): LongInt;


Implementation

Uses StUtils, JDates, Crc_Unit, StrPath,
     UnixDate, Cases, LongStr;

Const
  Jam_Local =        $00000001;
  Jam_InTransit =    $00000002;
  Jam_Priv =         $00000004;
  Jam_Rcvd =         $00000008;
  Jam_Sent =         $00000010;
  Jam_KillSent =     $00000020;
  Jam_AchvSent =     $00000040;
  Jam_Hold =         $00000080;
  Jam_Crash =        $00000100;
  Jam_Imm =          $00000200;
  Jam_Direct =       $00000400;
  Jam_Gate =         $00000800;
  Jam_Freq =         $00001000;
  Jam_FAttch =       $00002000;
  Jam_TruncFile =    $00004000;
  Jam_KillFile =     $00008000;
  Jam_RcptReq =      $00010000;
  Jam_ConfmReq =     $00020000;
  Jam_Orphan =       $00040000;
  Jam_Encrypt =      $00080000;
  Jam_Compress =     $00100000;
  Jam_Escaped =      $00200000;
  Jam_FPU =          $00400000;
  Jam_TypeLocal =    $00800000;
  Jam_TypeEcho =     $01000000;
  Jam_TypeNet =      $02000000;
  Jam_NoDisp =       $20000000;
  Jam_Locked =       $40000000;
  Jam_Deleted =      $80000000;


Type SubFieldType = Record
  LoId: SmallWord;
  HiId: SmallWord;
  DataLen: LongInt;
  Data: Array[1..1000] of Char;
  End;

function Min(X, Y: LongInt): LongInt;
begin
  if X < Y then Min := X else Min := Y;
end;

function Max(X, Y: LongInt): LongInt;
begin
  if X > Y then Max := X else Max := Y;
end;


Constructor JamMsgObj.Init;
  Begin
  New(JM);
  New(JamIdx);
  New(MsgHdr);
  New(TxtBuf);
  If ((JM = Nil) Or (JamIdx = Nil) or (MsgHdr = Nil) or (TxtBuf = Nil)) Then
    Begin
    If JM <> Nil Then
      Dispose(JM);
    If JamIdx <> Nil Then
      Dispose(JamIdx);
    If MsgHdr <> Nil Then
      Dispose(MsgHdr);
    If TxtBuf <> Nil Then
      Dispose(TxtBuf);
    Fail;
    Exit;
    End
  Else
    Begin
    FillChar(JM^, SizeOf(JM^), #0);
    JM^.MsgPath := '';
    JM^.IdxStart := -30;
    JM^.IdxRead := 0;
    JM^.HdrFile := nil;
    JM^.TxtFile := nil;
    JM^.IdxFile := nil;
    Error := 0;
    End;
  End;


Destructor JamMsgObj.Done;
  Begin
  If JM <> Nil Then
    Dispose(JM);
  If JamIdx <> Nil Then
    Dispose(JamIdx);
  If MsgHdr <> Nil Then
    Dispose(MsgHdr);
  If TxtBuf <> Nil Then
    Dispose(TxtBuf);
  End;


Function JamStrCrc(St: String): LongInt;
  Var
    i: Word;
    crc: LongInt;

  Begin
  Crc := -1;
  For i := 1 to Length(St) Do
    Crc := Updc32(Ord(LowCase(St[i])), Crc);
  JamStrCrc := Crc;
  End;


Procedure JamMsgObj.SetMsgPath(St: String);
  Begin
  JM^.MsgPath := Copy(St, 1, 124);
  End;


Function JamMsgObj.GetHighMsgNum: LongInt;
  Begin
  (*
   writeln('FileSize    = ', FileSize(Jm^.IdxFile));
   writeln('Name        = ', StrPas(@FileRec(Jm^.IdxFile).Name));
  *)

  GetHighMsgNum := JM^.BaseHdr.BaseMsgNum + JM^.IdxFile^.FileSize - 1;
  End;


Procedure JamMsgObj.SetDest(Var Addr: AddrType);
  Begin
  JM^.Dest := Addr;
  End;


Procedure JamMsgObj.SetOrig(Var Addr: AddrType);
  Begin
  JM^.Orig := Addr;
  End;


Procedure JamMsgObj.SetFrom(Name: String);
  Begin
  JM^.MsgFrom := Name;
  End;


Procedure JamMsgObj.SetTo(Name: String);
  Begin
  JM^.MsgTo := Name;
  End;


Procedure JamMsgObj.SetSubj(Str: String);
  Begin
  JM^.MsgSubj := Str;
  End;


Procedure JamMsgObj.SetCost(SCost: Word);
  Begin
  MsgHdr^.JamHdr.Cost := SCost;
  End;


Procedure JamMsgObj.SetRefer(SRefer: LongInt);
  Begin
  MsgHdr^.JamHdr.ReplyTo := SRefer;
  End;


Procedure JamMsgObj.SetSeeAlso(SAlso: LongInt);
  Begin
  MsgHdr^.JamHdr.ReplyFirst := SAlso;
  End;


Procedure JamMsgObj.SetDate(SDate: String);
  Begin
  JM^.MsgDate := SDate;
  End;


Procedure JamMsgObj.SetTime(STime: String);
  Begin
  JM^.MsgTime := STime;
  End;


Procedure JamMsgObj.SetAttr1(Mask: LongInt; St: Boolean);
  Begin
  If St Then
    MsgHdr^.JamHdr.Attr1 := MsgHdr^.JamHdr.Attr1 Or Mask
  Else
    MsgHdr^.JamHdr.Attr1 := MsgHdr^.JamHdr.Attr1 And (Not Mask);
  End;



Procedure JamMsgObj.SetLocal(LS: Boolean);
  Begin
  SetAttr1(Jam_Local, LS);
  End;


Procedure JamMsgObj.SetRcvd(RS: Boolean);
  Begin
  SetAttr1(Jam_Rcvd, RS);
  End;


Procedure JamMsgObj.SetPriv(PS: Boolean);
  Begin
  SetAttr1(Jam_Priv, PS);
  End;


Procedure JamMsgObj.SetCrash(SS: Boolean);
  Begin
  SetAttr1(Jam_Crash, SS);
  End;


Procedure JamMsgObj.SetKillSent(SS: Boolean);
  Begin
  SetAttr1(Jam_KillSent, SS);
  End;


Procedure JamMsgObj.SetSent(SS: Boolean);
  Begin
  SetAttr1(Jam_Sent, SS);
  End;


Procedure JamMsgObj.SetFAttach(SS: Boolean);
  Begin
  SetAttr1(Jam_FAttch, SS);
  End;


Procedure JamMsgObj.SetReqRct(SS: Boolean);
  Begin
  SetAttr1(Jam_RcptReq, SS);
  End;


Procedure JamMsgObj.SetReqAud(SS: Boolean);
  Begin
  SetAttr1(Jam_ConfmReq, SS);
  End;


Procedure JamMsgObj.SetRetRct(SS: Boolean);
  Begin
  End;


Procedure JamMsgObj.SetFileReq(SS: Boolean);
  Begin
  SetAttr1(Jam_Freq, SS);
  End;


Procedure JamMsgObj.DoString(Str: String);
  Var
    i: Word;

  Begin
  i := 1;
  While i <= Length(Str) Do
    Begin
    DoChar(Str[i]);
    Inc(i);
    End;
  End;


Procedure JamMsgObj.DoChar(Ch: Char);
  Var
    TmpStr: String;
    NumWrite: Longint;
  Begin
  Case ch of
    #13: LastSoft := False;
    #10:;
    Else
      LastSoft := True;
    End;
  If (JM^.TxtPos - JM^.TxtBufStart) >= JamTxtBufSize Then
    Begin
    If JM^.TxtBufStart = 0 Then
      Begin
      GetDir(0, TmpStr);
      TmpStr := GetTempName(TmpStr);

      New(JM^.BufFile, Init);
      JM^.BufFile^.Assign(TmpStr);
      JM^.BufFile^.FileMode := fmReadWrite + fmDenyNone;
      JM^.BufFile^.Create(1);
      End;
    NumWrite := JM^.TxtPos - JM^.TxtBufStart;

    JM^.BufFile^.BlkWrite(TxtBuf^, NumWrite);
     Error := JM^.BufFile^.IoResult;
    JM^.TxtBufStart := JM^.BufFile^.FileSize;
    End;

    TxtBuf^[Longint(JM^.TxtPos - JM^.TxtBufStart)] := Ch;
    Inc(JM^.TxtPos);
  End;


Procedure JamMsgObj.DoStringLn(Str: String);
  Begin
  DoString(Str);
  DoChar(#13);
  End;


Procedure JamMsgObj.DoKludgeLn(Str: String);
  Var
    TmpStr: String;

  Begin
  If Str[1] = #1 Then
    Str := Copy(Str,2,255);
  If Copy(Str,1,3) = 'PID' Then
    Begin
    TmpStr := StripLead(Copy(Str,4,255),':');
    TmpStr := Copy(StripBoth(TmpStr, ' '),1,40);
    AddSubField(7, TmpStr);
    End
  Else If Copy(Str,1,5) = 'MSGID' Then
    Begin
    TmpStr := StripLead(Copy(Str,6,255),':');
    TmpStr := Copy(StripBoth(TmpStr,' '),1,100);
    AddSubField(4, TmpStr);
    MsgHdr^.JamHdr.MsgIdCrc := JamStrCrc(TmpStr);
    End
  Else If Copy(Str,1,4) = 'INTL' Then  {ignore}
    Begin
    End
  Else If Copy(Str,1,4) = 'TOPT' Then  {ignore}
    Begin
    End
  Else If Copy(Str,1,4) = 'FMPT' Then  {ignore}
    Begin
    End
  Else If Copy(Str,1,5) = 'REPLY' Then
    Begin
    TmpStr := StripLead(Copy(Str,8,255),':');
    TmpStr := Copy(StripBoth(TmpStr,' '),1,100);
    AddSubField(5, TmpStr);
    MsgHdr^.JamHdr.ReplyCrc := JamStrCrc(TmpStr);
    End
  Else If Copy(Str,1,4) = 'PATH' Then
    Begin
    TmpStr := StripLead(Copy(Str,5,255),':');
    TmpStr := StripBoth(TmpStr,' ');
    AddSubField(2002, TmpStr);
    End
  Else
    Begin
    AddSubField(2000, StripBoth(Str,' '));
    End;
  End;


Procedure JamMsgObj.AddSubField(id: SMallWord; Data: String);
  Type SubFieldType = Record
    LoId: SmallWord;
    HiId: SmallWord;
    DataLen: LongInt;
    Data: Array[1..256] of Char;
    End;

  Var
    SubField: ^SubFieldType;

  Begin
  SubField := @MsgHdr^.SubBuf[MsgHdr^.JamHdr.SubFieldLen + 1];
  If (MsgHdr^.JamHdr.SubFieldLen + 8 + Length(Data) < JamSubBufSize) Then
    Begin
    Inc(MsgHdr^.JamHdr.SubFieldLen, 8 + Length(Data));
    SubField^.LoId := Id;
    SubField^.HiId := 0;
    SubField^.DataLen := Length(Data);
    Move(Data[1], SubField^.Data[1], Length(Data));
    End;
  End;


procedure JamMsgObj.ClearMsgText;
begin
  JM^.TxtBufStart := 0;
  JM^.TxtPos := 0;
end; { proc. ClearMsgText }


procedure JamMsgObj.UpdateMsgText(var WriteError: Word);
var i: NumReadType;
begin
    MsgHdr^.JamHdr.TextOfs := JM^.TxtFile^.FileSize;
    MsgHdr^.Jamhdr.TextLen := JM^.TxtPos;
    If JM^.TxtBufStart > 0 Then
      Begin                            {Write text using buffer file}
      i := JM^.TxtPos - JM^.TxtBufStart;
      JM^.BufFile^.BlkWrite(TxtBuf^, i);
      WriteError := JM^.BufFile^.IoResult;
      If WriteError = 0 Then           {seek start of buffer file}
        Begin
        JM^.BufFile^.Seek(0);
        WriteError := JM^.BufFile^.IoResult;
        End;
      If WriteError = 0 Then           {seek end of text file}
        Begin
        JM^.TxtFile^.Seek(JM^.TxtFile^.Filesize);
        WriteError := JM^.TxtFile^.IoResult;
        End;
      While ((Not JM^.BufFile^.EOF) and (WriteError = 0)) Do
        Begin                          {copy buffer file to text file}
        I :=  JM^.BufFile^.BlkRead(TxtBuf^, SizeOf(TxtBuf^));
        WriteError := JM^.BufFile^.IoResult;
        If WriteError = 0 Then
          Begin
          JM^.TxtBufStart := JM^.TxtFile^.FilePos;
          JM^.TxtRead := i;
          Jm^.TxtFile^.BlkWrite(TxtBuf^, i);
          Error := JM^.TxtFile^.IoResult;
          End;
        End;
      JM^.BufFile^.Close;
      JM^.BufFile^.Erase;
      Dispose(JM^.BufFile, Done);
      End
    Else
      Begin                            {Write text using TxtBuf only}
      Jm^.TxtFile^.Seek(Jm^.TxtFile^.FileSize);
      WriteError := JM^.TxtFile^.IoResult;
      If WriteError = 0 Then
        Begin
        JM^.TxtFile^.BlkWrite(TxtBuf^, JM^.TxtPos);
        WriteError := JM^.TxtFile^.IoResult;
        JM^.TxtRead := JM^.TxtPos;
        End;
      End;
end; { proc. UpdateMsgText }

Function  JamMsgObj.WriteMsg: Word;
  Var
{    DT: DateTime; }
    WriteError: Word;
    TmpIdx: JamIdxType;
    FN: String[13];

  Begin
  WriteError := 0;
  If LastSoft Then
    Begin
    DoChar(#13);
    DoChar(#10);
    End;
  If WriteError = 0 Then
    Begin
    MsgHdr^.JamHdr.Signature[1] := 'J';{Set signature}
    MsgHdr^.JamHdr.Signature[2] := 'A';
    MsgHdr^.JamHdr.Signature[3] := 'M';
    MsgHdr^.JamHdr.Signature[4] := #0;
    Case JM^.MailType of
      mmtNormal: SetAttr1(Jam_TypeLocal, True);
      mmtEchoMail: SetAttr1(Jam_TypeEcho, True);
      mmtNetMail: SetAttr1(Jam_TypeNet, True);
      End;
    MsgHdr^.JamHdr.Rev := 1;
    MsgHdr^.JamHdr.DateArrived := Date2Unix(GetDosDate); {Get date processed}
    SetDateTime; { update the actual JAM fields }
    End;
  If WriteError = 0 Then
    Begin                              {Lock message base for update}
    If Not LockMsgBase Then
      WriteError := 5;
    End;
  If WriteError = 0 Then
    Begin                              {Handle message text}
    MsgHdr^.JamHdr.MsgNum := GetHighMsgNum + 1;
    UpdateMsgText(WriteError);

    If WriteError = 0 Then             {Add index record}
      Begin
      TmpIdx.HdrLoc := JM^.HdrFile^.FileSize;
      TmpIdx.MsgToCrc := JamStrCrc(JM^.MsgTo);
      JM^.IdxFile^.Seek(JM^.IdxFile^.FileSize);
      WriteError := JM^.IdxFile^.IoResult;
      End;
    If WriteError = 0 Then             {write index record}
      Begin
      JM^.IdxFile^.BlkWrite(TmpIdx, 1);
      WriteError := JM^.IdxFile^.IoResult;
      End;
    If WriteError = 0 Then
      Begin                            {Add subfields as needed}
      If Length(JM^.MsgTo) > 0 Then
        AddSubField(3, JM^.MsgTo);
      If Length(JM^.MsgFrom) > 0 Then
        AddSubField(2, JM^.MsgFrom);
      If Length(JM^.MsgSubj) > 0 Then
        Begin
        If IsFileReq Then
          AddSubField(11, JM^.MsgSubj)
        Else
          AddSubField(6, JM^.MsgSubj);
        End;
      If ((JM^.Dest.Zone <> 0) or (JM^.Dest.Net <> 0) or
        (JM^.Dest.Node <> 0) or (JM^.Dest.Point <> 0)) Then
        AddSubField(1, AddrStr(JM^.Dest));
      If ((JM^.Orig.Zone <> 0) or (JM^.Orig.Net <> 0) or
        (JM^.Orig.Node <> 0) or (JM^.Orig.Point <> 0)) Then
        AddSubField(0, AddrStr(JM^.Orig));
      JM^.HdrFile^.Seek(JM^.HdrFile^.Filesize);    { Seek to end of .JHR file }
      WriteError := JM^.HdrFile^.IoResult;
      End;
    If WriteError = 0 Then
      Begin                            {write msg header}
        JM^.HdrFile^.BlkWrite(MsgHdr^, SizeOf(MsgHdr^.JamHDR) + MsgHdr^.JamHdr.SubFieldLen);
        WriteError := JM^.HdrFile^.IoResult;
      End;
    If WriteError = 0 Then
      Begin                            {update msg base header}
      Inc(JM^.BaseHdr.ActiveMsgs);
      Inc(JM^.BaseHdr.ModCounter);
      End;

    If WriteError = 0 Then
      Begin
      Case JM^.MailType of
        mmtEchoMail: FN := 'echomail.jam';
        mmtNetMail: FN := 'netmail.jam';
        Else
          FN := '';
        End; {Case MsgType}
      If ((Length(FN) > 0)) Then
        WriteError := WriteMailIdx(FN, JM^.BaseHdr.ActiveMsgs - 1);
      End;
    If UnLockMsgBase Then;             {unlock msg base}
    End;
  WriteMsg := WriteError;              {return result of writing msg}
  End;


Function JamMsgObj.GetChar: Char;
  Begin
  If JM^.TxtPos < 0 Then
    Begin
    GetChar := JM^.TxtSubBuf[JM^.TxtSubChars + JM^.TxtPos];
    Inc(JM^.TxtPos);
    If JM^.TxtPos >= 0 Then
      JM^.TxtPos := MsgHdr^.JamHdr.TextOfs;
    End
  Else
    Begin
    If ((JM^.TxtPos < JM^.TxtBufStart) Or
    (JM^.TxtPos >= JM^.TxtBufStart + JM^.TxtRead)) Then
      Begin
      JM^.TxtBufStart := JM^.TxtPos - 80;
      If JM^.TxtBufStart < 0 Then
        JM^.TxtBufStart := 0;
      JM^.TxtFile^.Seek(JM^.TxtBufStart);
      Error := JM^.TxtFile^.IoResult;
      If Error = 0 Then
        Begin
        JM^.TxtRead := JM^.TxtFile^.BlkRead(TxtBuf^, SizeOf(TxtBuf^));
        Error := JM^.TxtFile^.IoResult;
        End;
      End;
    GetChar := TxtBuf^[JM^.TxtPos - JM^.TxtBufStart];
    Inc(JM^.TxtPos);
    End;
  End;



Procedure JamMsgObj.AddTxtSub(St: String);
  Var
    i: Word;

  Begin
  For i := 1 to Length(St) Do
    Begin
    If JM^.TxtSubChars <= TxtSubBufSize Then
      Begin
      JM^.TxtSubBuf[JM^.TxtSubChars] := St[i];
      Inc(JM^.TxtSubChars);
      End;
    End;
  If JM^.TxtSubChars <= TxtSubBufSize Then
    Begin
    JM^.TxtSubBuf[JM^.TxtSubChars] := #13;
    Inc(JM^.TxtSubChars);
    End;
  End;


Procedure JamMsgObj.MsgStartUp;
  Var
    SubCtr: LongInt;
    SubPtr: ^SubFieldType;
    NumRead: NumReadType;
    DT: DateTime;
    IdxLoc: LongInt;
    TmpStr: ShortString;
    TmpAddr: AddrType;

  Begin
  Error := 0;
  LastSoft := False;
  JM^.MsgFrom := '';
  JM^.MsgTo := '';
  JM^.MsgSubj := '';
  JM^.TxtSubChars := 0;
  If SeekFound Then
    Begin
    Error := ReReadIdx(IdxLoc);
    If Error = 0 Then
      Begin
      JM^.HdrFile^.Seek(JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc);
      Error := JM^.HdrFile^.IoResult;
      End;
    If Error = 0 Then
      Begin
      NumRead := JM^.HdrFile^.BlkRead(MsgHdr^, SizeOf(MsgHdr^));
      Error := JM^.HdrFile^.IoResult;
      End;
    If Error = 0 Then
      Begin
      UnixToDt(MsgHdr^.JamHdr.DateWritten, DT);
      JM^.MsgDate := FormattedDate(Dt, 'MM-DD-YY');
      JM^.MsgTime := FormattedDate(Dt, 'HH:II');
      SubCtr := 1;
      While ((SubCtr <= MsgHdr^.JamHdr.SubFieldLen) and
      (SubCtr < JamSubBufSize)) AND (SubCtr > 0) Do
        Begin
        SubPtr := @MsgHdr^.SubBuf[SubCtr];
        Inc(SubCtr, SubPtr^.DataLen + 8);
        Case(SubPtr^.LoId) Of
          0: Begin {Orig}
             FillChar(TmpAddr, SizeOf(TmpAddr), #0);
             FillChar(JM^.Orig, SizeOf(JM^.Orig), #0);
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 128 Then
               TmpStr[0] := #128;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             If ParseAddr(TmpStr, TmpAddr, JM^.Orig) Then;
             End;
          1: Begin {Dest}
             FillChar(TmpAddr, SizeOf(TmpAddr), #0);
             FillChar(JM^.Dest, SizeOf(JM^.Dest), #0);
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 128 Then
               TmpStr[0] := #128;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             If ParseAddr(TmpStr, TmpAddr, JM^.Dest) Then;
             End;
          2: Begin {MsgFrom}
             JM^.MsgFrom[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(JM^.MsgFrom[0]) > 65 Then
               JM^.MsgFrom[0] := #65;
             Move(SubPtr^.Data, JM^.MsgFrom[1], Ord(JM^.MsgFrom[0]));
             End;
          3: Begin {MsgTo}
             JM^.MsgTo[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(JM^.MsgTo[0]) > 65 Then
               JM^.MsgTo[0] := #65;
             Move(SubPtr^.Data, JM^.MsgTo[1], Ord(JM^.MsgTo[0]));
             End;
          4: Begin {MsgId}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'MSGID: ' + TmpStr);

             {-- Parse the MSGID into the orig# ----------------------------}
             if Pos('@', TmpStr) > 0 then
               TmpStr := Copy(TmpStr, 1, Pos('@', TmpStr) - 1)
                 else TmpStr := Copy(TmpStr, 1, Pos(#32, TmpStr) - 1);
             ParseAddr(TmpStr, Jm^.Orig, Jm^.Orig);
             End;
          5: Begin {Reply}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'REPLY: ' + TmpStr);
             End;
          6: Begin {MsgSubj}
             JM^.MsgSubj[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(JM^.MsgSubj[0]) > 100 Then
               JM^.MsgSubj[0] := #100;
             Move(SubPtr^.Data, JM^.MsgSubj[1], Ord(JM^.MsgSubj[0]));
             End;
          7: Begin {PID}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'PID: ' + TmpStr);
             End;
          8: Begin {VIA}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'Via ' + TmpStr);
             End;
          9: Begin {File attached}
             If IsFAttach Then
               Begin
               JM^.MsgSubj[0] := Chr(SubPtr^.DataLen and $ff);
               If Ord(JM^.MsgSubj[0]) > 100 Then
                 JM^.MsgSubj[0] := #100;
               Move(SubPtr^.Data, JM^.MsgSubj[1], Ord(JM^.MsgSubj[0]));
               End
             End;
          11: Begin {File request}
             If IsFileReq Then
               Begin
               JM^.MsgSubj[0] := Chr(SubPtr^.DataLen and $ff);
               If Ord(JM^.MsgSubj[0]) > 100 Then
                 JM^.MsgSubj[0] := #100;
               Move(SubPtr^.Data, JM^.MsgSubj[1], Ord(JM^.MsgSubj[0]));
               End
             End;
          2000: Begin {Unknown kludge}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1 + TmpStr);
             End;
          2001: Begin {SEEN-BY}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'SEEN-BY: ' + TmpStr);
             End;
          2002: Begin {PATH}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'PATH: ' + TmpStr);
             End;
          2003: Begin {FLAGS}
             TmpStr[0] := Chr(SubPtr^.DataLen and $ff);
             If Ord(TmpStr[0]) > 240 Then
               TmpSTr[0] := #240;
             Move(SubPtr^.Data, TmpStr[1], Ord(TmpStr[0]));
             AddTxtSub(#1'FLAGS: ' + TmpStr);
             End;
          End;
        End;
      End;
    End;
  End;


Procedure JamMsgObj.MsgTxtStartUp;
  Begin
  LastSoft := False;
  JM^.TxtEnd := MsgHdr^.JamHdr.TextOfs + MsgHdr^.JamHdr.TextLen - 1;
  If JM^.TxtSubChars > 0 Then
    JM^.TxtPos := - JM^.TxtSubChars
  Else
    JM^.TxtPos := MsgHdr^.JamHdr.TextOfs;
  End;


{$IFNDEF MSDOS}
function JamMsgObj.GetMsgBuffer: AnsiString;
const
  maxBuffer = 1024 * 192;

type
  TmpBufType = Array[0..maxBuffer] of Char;

var TmpBuf : ^TmpBufType;
    TmpRead: Longint;
begin
  New(TmpBuf);
  JM^.TxtFile^.Seek(MsgHdr^.JamHdr.TextOfs);
  TmpRead := JM^.TxtFile^.BlkRead(TmpBuf^, MsgHdr^.JamHdr.TextLen);
  TmpBuf^[TmpRead] := #0;

  GetMsgBuffer := TmpBuf^;
  Dispose(TmpBuf);
end; { func. GetMsgBuffer }
{$ENDIF}


Function JamMsgObj.GetString(MaxLen: Word): ShortString;
  Var
    WPos: LongInt;
    WLen: Byte;
    StrDone: Boolean;
    StartSoft: Boolean;
    CurrLen: Word;
    TmpCh: Char;

  Begin
  StrDone := False;
  CurrLen := 0;
  WPos := 0;
  WLen := 0;
  StartSoft := LastSoft;
  LastSoft := True;
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
              WPos := JM^.TxtPos;
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
      TmpCh := GetChar;
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
        Dec(JM^.TxtPos);
        End
      Else
        Begin
        GetString[0] := Chr(WLen);
        JM^.TxtPos := WPos;
        End;
      End;
  End;


Function JamMsgObj.EOM: Boolean;
  Begin
  EOM := (((JM^.TxtPos < MsgHdr^.JamHdr.TextOfs) Or
    (JM^.TxtPos > JM^.TxtEnd)) And (JM^.TxtPos >= 0));
  End;


Function JamMsgObj.WasWrap: Boolean;
  Begin
  WasWrap := LastSoft;
  End;


Function JamMsgObj.GetFrom: String; {Get from name on current msg}
  Begin
  GetFrom := JM^.MsgFrom;
  End;


Function JamMsgObj.GetTo: String; {Get to name on current msg}
  Begin
  GetTo := JM^.MsgTo;
  End;


Function JamMsgObj.GetSubj: String; {Get subject on current msg}
  Begin
  GetSubj := JM^.MsgSubj;
  End;


Function JamMsgObj.GetCost: Word; {Get cost of current msg}
  Begin
  GetCost := MsgHdr^.JamHdr.Cost;
  End;


Function JamMsgObj.GetDate: String; {Get date of current msg}
  Begin
  GetDate := JM^.MsgDate;
  End;


Function JamMsgObj.GetTime: String; {Get time of current msg}
  Begin
  GetTime := JM^.MsgTime;
  End;


Function JamMsgObj.GetRefer: LongInt; {Get reply to of current msg}
  Begin
  GetRefer := MsgHdr^.JamHdr.ReplyTo;
  End;


Function JamMsgObj.GetSeeAlso: LongInt; {Get see also of current msg}
  Begin
  GetSeeAlso := MsgHdr^.JamHdr.ReplyFirst;
  End;


Function JamMsgObj.GetMsgNum: LongInt; {Get message number}
  Begin
  GetMsgNum := MsgHdr^.JamHdr.MsgNum;
  End;


Procedure JamMsgObj.GetOrig(Var Addr: AddrType); {Get origin address}
  Begin
  Addr := JM^.Orig;
  End;


Procedure JamMsgObj.GetDest(Var Addr: AddrType); {Get destination address}
  Begin
  Addr := JM^.Dest;
  End;


Function JamMsgObj.IsLocal: Boolean; {Is current msg local}
  Begin
  IsLocal := (MsgHdr^.JamHdr.Attr1 and Jam_Local) <> 0;
  End;


Function JamMsgObj.IsCrash: Boolean; {Is current msg crash}
  Begin
  IsCrash := (MsgHdr^.JamHdr.Attr1 and Jam_Crash) <> 0;
  End;


Function JamMsgObj.IsKillSent: Boolean; {Is current msg kill sent}
  Begin
  IsKillSent := (MsgHdr^.JamHdr.Attr1 and Jam_KillSent) <> 0;
  End;


Function JamMsgObj.IsSent: Boolean; {Is current msg sent}
  Begin
  IsSent := (MsgHdr^.JamHdr.Attr1 and Jam_Sent) <> 0;
  End;


Function JamMsgObj.IsFAttach: Boolean; {Is current msg file attach}
  Begin
  IsFAttach := (MsgHdr^.JamHdr.Attr1 and Jam_FAttch) <> 0;
  End;


Function JamMsgObj.IsReqRct: Boolean; {Is current msg request receipt}
  Begin
  IsReqRct := (MsgHdr^.JamHdr.Attr1 and Jam_RcptReq) <> 0;
  End;


Function JamMsgObj.IsReqAud: Boolean; {Is current msg request audit}
  Begin
  IsReqAud := (MsgHdr^.JamHdr.Attr1 and Jam_ConfmReq) <> 0;
  End;


Function JamMsgObj.IsRetRct: Boolean; {Is current msg a return receipt}
  Begin
  IsRetRct := False;
  End;


Function JamMsgObj.IsFileReq: Boolean; {Is current msg a file request}
  Begin
  IsFileReq := (MsgHdr^.JamHdr.Attr1 and Jam_Freq) <> 0;
  End;


Function JamMsgObj.IsRcvd: Boolean; {Is current msg received}
  Begin
  IsRcvd := (MsgHdr^.JamHdr.Attr1 and Jam_Rcvd) <> 0;
  End;


Function JamMsgObj.IsPriv: Boolean; {Is current msg priviledged/private}
  Begin
  IsPriv := (MsgHdr^.JamHdr.Attr1 and Jam_Priv) <> 0;
  End;


Function JamMsgObj.IsDeleted: Boolean; {Is current msg deleted}
  Begin
  IsDeleted := (MsgHdr^.JamHdr.Attr1 and Jam_Deleted) <> 0;
  End;


Function JamMsgObj.IsEchoed: Boolean; {Is current msg echoed}
  Begin
  IsEchoed := True;
  End;



Function  JamMsgObj.IsNetMail: Boolean;
  begin
    IsNetMail := (MsgHdr^.JamHdr.Attr1 AND Jam_TypeNet) <> 00;
  end; { isNetMail }


Procedure JamMsgObj.SetNetMail(SN: Boolean);
  begin
     SetAttr1(Jam_TypeNet, SN);
  end; { proc. SetNetMail }



Procedure JamMsgObj.SeekFirst(MsgNum: LongInt); {Start msg seek}
  Begin
  JM^.CurrMsgNum := MsgNum - 1;
  If JM^.CurrMsgNum < (JM^.BaseHdr.BaseMsgNum - 1) Then
    JM^.CurrMsgNum := JM^.BaseHdr.BaseMsgNum - 1;
  SeekNext;
  End;


Procedure JamMsgObj.SeekNext; {Find next matching msg}
  Var
    IdxLoc: LongInt;
    Deleted: Boolean;

  Begin
  If JM^.CurrMsgNum <= GetHighMsgNum Then
    Inc(JM^.CurrMsgNum);
  Error := ReReadIdx(IdxLoc);

{!!  Addition to allow for empty to fields }
{!!}  Deleted := (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1);
{!!}  if (JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc > 0) AND
{!!}      (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1) then
{!!}       begin
{!!}         MsgStartup;
{!!}         if NOT IsDeleted then
{!!}           Deleted := false;
{!!}       end;

  While (((JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc < 0)) or
   (Deleted)) AND
  (JM^.CurrMsgNum <= GetHighMsgNum) Do
    Begin
    Inc(JM^.CurrMsgNum);
    Error := ReReadIdx(IdxLoc);
  {!!  Addition to allow for empty to fields }
  {!!}  Deleted := (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1);
  {!!}  if (JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc > 0) AND
  {!!}      (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1) then
  {!!}       begin
  {!!}         MsgStartup;
  {!!}         if NOT IsDeleted then
  {!!}           Deleted := false;
  {!!}       end;
    End;
  End;


Procedure JamMsgObj.SeekPrior;
  Var
    IdxLoc: LongInt;
    Deleted: Boolean;

  Begin
  If JM^.CurrMsgNum >= JM^.BaseHdr.BaseMsgNum Then
    Dec(JM^.CurrMsgNum);
  Error := ReReadIdx(IdxLoc);

{!!  Addition to allow for empty to fields }
{!!}  Deleted := (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1);
{!!}  if (JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc > 0) AND
{!!}      (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1) then
{!!}       begin
{!!}         MsgStartup;
{!!}         if NOT IsDeleted then
{!!}           Deleted := false;
{!!}       end;


   If JM^.CurrMsgNum >= JM^.BaseHdr.BaseMsgNum Then
    Begin

    While (((JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc < 0)) or
     (Deleted)) AND
    (JM^.CurrMsgNum >= JM^.BaseHdr.BaseMsgNum) Do
      Begin
      Dec(JM^.CurrMsgNum);
      Error := ReReadIdx(IdxLoc);
  {!!  Addition to allow for empty to fields }
  {!!}  Deleted := (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1);
  {!!}  if (JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc > 0) AND
  {!!}      (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = -1) then
  {!!}       begin
  {!!}         MsgStartup;
  {!!}         if NOT IsDeleted then
  {!!}           Deleted := false;
  {!!}       end;
      End;
    End;
  End;


Function JamMsgObj.SeekFound: Boolean;
  Begin
  SeekFound := ((JM^.CurrMsgNum >= JM^.BaseHdr.BaseMsgNum) and
    (JM^.CurrMsgNum <= GetHighMsgNum));
  End;


Function JamMsgObj.GetMsgLoc: LongInt; {Msg location}
  Begin
  GetMsgLoc := GetMsgNum;
  End;


Procedure JamMsgObj.SetMsgLoc(ML: LongInt); {Msg location}
  Begin
  JM^.CurrMsgNum := ML;
  End;


Procedure JamMsgObj.YoursFirst(Name: String; Handle: String);
  Begin
  JM^.YourName := Name;
  JM^.YourHdl := Handle;
  JM^.NameCrc := JamStrCrc(Name);
  JM^.HdlCrc := JamStrCrc(Handle);
  JM^.CurrMsgNum := JM^.BaseHdr.BaseMsgNum - 1;
  YoursNext;
  End;


Procedure JamMsgObj.YoursNext;
  Var
    Found: Boolean;
    IdxLoc: LongInt;
    NumRead: NumReadType;
    SubCtr: LongInt;
    SubPtr: ^SubFieldType;

  Begin
  Error := 0;
  Found := False;
  Inc(JM^.CurrMsgNum);
  While ((Not Found) and (JM^.CurrMsgNum <= GetHighMsgNum) And
  (Error = 0)) Do
    Begin
    Error := ReReadIdx(IdxLoc);
    If Error = 0 Then
      Begin                            {Check CRC values}
      If ((JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = JM^.NameCrc) or
      (JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc = JM^.HdlCrc)) Then
        Begin
        JM^.HdrFile^.Seek(JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc);
        Error := JM^.HdrFile^.IoResult;
        If Error = 0 Then              {Read message header}
          Begin
          NumRead := JM^.HdrFile^.BlkRead(MsgHdr^, SizeOf(MsgHdr^));
          Error := JM^.HdrFile^.IoResult;
          End;
        If ((Error = 0) and (Not IsRcvd)) Then
          Begin
          SubCtr := 1;
          While ((SubCtr <= MsgHdr^.JamHdr.SubFieldLen) and
          (SubCtr < JamSubBufSize)) Do
            Begin
            SubPtr := @MsgHdr^.SubBuf[SubCtr];
            Inc(SubCtr, SubPtr^.DataLen + 8);
            Case(SubPtr^.LoId) Of
              3: Begin {MsgTo}
                 JM^.MsgTo[0] := Chr(SubPtr^.DataLen and $ff);
                 If Ord(JM^.MsgTo[0]) > 65 Then
                   JM^.MsgTo[0] := #65;
                 Move(SubPtr^.Data, JM^.MsgTo[1], Ord(JM^.MsgTo[0]));
                 If ((SUpcase(JM^.MsgTo) = SUpCase(JM^.YourName)) Or
                 (SUpCase(JM^.MsgTo) = SUpCase(JM^.YourHdl))) Then
                   Found := True;
                 End;
              End;
            End;
          End;
        End;
      End;
    If (Not Found) Then
      Inc(JM^.CurrMsgNum);
    End;
  End;


Function JamMsgObj.YoursFound: Boolean;
  Begin
  YoursFound := ((JM^.CurrMsgNum >= JM^.BaseHdr.BaseMsgNum) and
    (JM^.CurrMsgNum <= GetHighMsgNum));
  End;


Procedure JamMsgObj.StartNewMsg;
  Begin
  JM^.TxtBufStart := 0;
  JM^.TxtPos := 0;
  FillChar(MsgHdr^, SizeOf(MsgHdr^), #0);
  MsgHdr^.JamHdr.SubFieldLen := 0;
  MsgHdr^.JamHdr.MsgIdCrc := -1;
  MsgHdr^.JamHdr.ReplyCrc := -1;
  MsgHdr^.JamHdr.PwdCrc := -1;
  JM^.MsgTo := '';
  JM^.MsgFrom := '';
  JM^.MsgSubj := '';
  FillChar(JM^.Orig, SizeOf(JM^.Orig), #0);
  FillChar(JM^.Dest, SizeOf(JM^.Dest), #0);
  JM^.MsgDate := Date2Str(GetDosDate);
  JM^.MsgTime := Time2Str(GetDosDate);
  End;


Function JamMsgObj.MsgBaseExists: Boolean;
  Begin
  MsgBaseExists := (FileExist(JM^.MsgPath + '.jhr'));
  End;


Function JamMsgObj.ReadIdx: NumReadType;
  Begin
  If JM^.IdxStart < 0 Then
    JM^.IdxStart := 0;
  JM^.IdxFile^.Seek(JM^.IdxStart);

  JM^.IdxRead := JM^.IdxFile^.BlkRead(JamIdx^, JamIdxBufSize);
  ReadIdx := JM^.IdxFile^.IoResult;
  End;


Function JamMsgObj.WriteIdx: NumReadType;
  Begin
  JM^.IdxFile^.Seek(JM^.IdxStart);
  JM^.IdxFile^.BlkWrite(JamIdx^, JM^.IdxRead);
  WriteIdx := JM^.IdxFile^.IoResult;
  End;


Function JamMsgObj.OpenMsgBase: Word;
{$IFDEF WINGUI}
  Type FileRec = TFileRec;
{$ENDIF}

  Var
    OpenError: Word;

  Begin
  OpenError := 0;
  JM^.LockCount := 0;
  New(JM^.HdrFile, Init);
  New(JM^.TxtFile, Init);
  New(JM^.IdxFile, Init);
  JM^.HdrFile^.Assign(JM^.MsgPath + '.jhr');
  JM^.TxtFile^.Assign(JM^.MsgPath + '.jdt');
  JM^.IdxFile^.Assign(JM^.MsgPath + '.jdx');

  JM^.HdrFile^.FileMode := fmReadWrite + fmDenyNone;
  if JM^.HdrFile^.Open(1) then
    Begin
    OpenError := 0;
    JM^.HdrFile^.Seek(1);
    JM^.HdrFile^.BlkRead(JM^.BaseHdr.Signature[2], (SizeOf(JM^.BaseHdr) - 1));
    OpenError := JM^.HdrFile^.IoResult;
    End else OpenError := 1;
  If OpenError = 0 Then
    Begin
    JM^.TxtFile^.FileMode := fmReadWrite + fmDenyNone;
    JM^.TxtFile^.Open(1);
    OpenError := JM^.TxtFile^.IoResult;
    End;
  If OpenError = 0 Then
    Begin
    JM^.IdxFile^.FileMode := fmReadWrite + fmDenyNone;
    JM^.IdxFile^.Open(SizeOf(JamIdxType));
    OpenError := JM^.IdxFile^.IoResult;
    End;
  JM^.IdxStart := -10;
  JM^.IdxRead := 0;
  JM^.TxtBufStart := - 10;
  JM^.TxtRead := 0;
  OpenMsgBase := OpenError;

  if OpenError <> 0 then
    begin
      if JM^.HdrFile <> nil then Dispose(JM^.HdrFile, Done);
      if JM^.TxtFile <> nil then Dispose(JM^.TxtFile, Done);
      if JM^.IdxFile <> nil then Dispose(JM^.IdxFile, Done);

      JM^.HdrFile := nil;
      JM^.TxtFile := nil;
      JM^.IdxFile := nil;
    end; { if }
  End;


Function JamMsgObj.CloseMsgBase: Word;
  Var
    CloseError: Word;

  Begin
  if JM^.HdrFile <> nil then Dispose(JM^.HdrFile, Done);
  JM^.HdrFile := nil;

  if JM^.TxtFile <> nil then Dispose(JM^.TxtFile, Done);
  JM^.TxtFile := nil;

  if JM^.IdxFile <> nil then Dispose(JM^.IdxFile, Done);
  JM^.IdxFile := nil;

  CloseMsgBase := 0;
  End;


Function JamMsgObj.CreateMsgBase(MaxMsg: Word; MaxDays: Word): Word;
  Var
    TmpHdr: ^JamHdrType;
    CreateError: Word;
    i: Word;

  Begin
  CreateError := 0;
  i := PosLastChar(Bschar, JM^.MsgPath);
  If i > 0 Then
    Begin
    If Not MakePath(Copy(JM^.MsgPath, 1, i)) Then
      CreateError := 0;
    End;
  New(TmpHdr);
  If TmpHdr = Nil Then
    CreateError := 500
  Else
    Begin
    FillChar(TmpHdr^, SizeOf(TmpHdr^), #0);
    TmpHdr^.Signature[1] :=  'J';
    TmpHdr^.Signature[2] :=  'A';
    TmpHdr^.Signature[3] :=  'M';
    TmpHdr^.BaseMsgNum := 1;
    TmpHdr^.Created := NowAsUnixDate;
    TmpHdr^.PwdCrc := -1;
    CreateError := SaveFile(JM^.MsgPath + '.jhr', TmpHdr^, SizeOf(TmpHdr^));
    Dispose(TmpHdr);
    If CreateError = 0 Then
      CreateError := SaveFile(JM^.MsgPath + '.jlr', CreateError, 0);
    If CreateError = 0 Then
      CreateError := SaveFile(JM^.MsgPath + '.jdt', CreateError, 0);
    If CreateError = 0 Then
      CreateError := SaveFile(JM^.MsgPath + '.jdx', CreateError , 0);
    If IoResult <> 0 Then;
    End;
  CreateMsgBase := CreateError;
  End;


Procedure JamMsgObj.SetMailType(MT: MsgMailType);
  Begin
  JM^.MailType := MT;
  End;


Function JamMsgObj.GetSubArea: Word;
  Begin
  GetSubArea := 0;
  End;


Procedure JamMsgObj.ReWriteHdr;
  Var
    IdxLoc: LongInt;

  Begin
  SetDateTime;

  If LockMsgBase Then
    Error := 0
  Else
    Error := 5;
  Error := ReReadIdx(IdxLoc);
  If Error = 0 Then
    Begin
    JM^.HdrFile^.Seek(JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc);
    Error := JM^.HdrFile^.IoResult;
    End;
  If Error = 0 Then
    Begin
    JM^.HdrFile^.BlkWrite(MsgHdr^.JamHdr, SizeOf(MsgHdr^.JamHdr));
    Error := JM^.HdrFile^.IoResult;
    End;
  If UnLockMsgBase Then;
  End;


Procedure JamMsgObj.DeleteMsg;
  Var
    DelError: Word;
    IdxLoc: LongInt;

  Begin
  If Not IsDeleted Then
    Begin
    If LockMsgBase Then
      DelError := 0
    Else
      DelError := 5;
    If DelError = 0 Then
      Begin
      SetAttr1(Jam_Deleted, True);
      Dec(JM^.BaseHdr.ActiveMsgs);
      DelError := ReReadIdx(IdxLoc);
      End;
    If DelError = 0 Then
      ReWriteHdr;
    If DelError = 0 Then
      Begin
      Inc(JM^.BaseHdr.ModCounter);
      JamIdx^[IdxLoc - JM^.IdxStart].MsgToCrc := -1;
      JamIdx^[IdxLoc - JM^.IdxStart].HdrLoc := -1;
      If WriteIdx=0 Then;
      End;
    If UnLockMsgBase Then;
    End;
  End;


Function JamMsgObj.NumberOfMsgs: LongInt;
  Begin
  NumberOfMsgs := JM^.BaseHdr.ActiveMsgs;
  End;


Function JamMsgObj.FindLastRead(var LastFile: pFileObj; UNum, UCRC: LongInt): LongInt;
  Const
    LastSize = 100;

  Type LastArray = Array[1..LastSize] of JamLastType;

  Var
    LastBuf: ^LastArray;
    LastError: Word;
    NumRead: NumReadType;
    Found: Boolean;
    i: Word;
    LastStart: LongInt;

  Begin
  FindLastRead := -1;
  Found := False;
  New(LastBuf);
  LastFile^.Seek(0);
  LastError := LastFile^.IoResult;
  While ((Not LastFile^.Eof) and (LastError = 0) And (Not Found)) Do
    Begin
    LastStart := LastFile^.FilePos;
    NumRead := LastFile^.BlkRead(LastBuf^, LastSize);
    LastError := LastFile^.IoResult;
    For i := 1 to NumRead Do
      Begin
      (** Was:
      If (LastBuf^[i].UserNum = UNum) Then
      **)
      If (LastBuf^[i].NameCRC=UCRC) Then
        Begin
        Found := True;
        FindLastRead := LastStart + i - 1;
        End;
      End;
    End;
  Dispose(LastBuf);
  End;


Function JamMsgObj.GetLastRead(UNum, UCRC: LongInt): LongInt;
  Var
    RecNum: LongInt;
    LastFile: pFileObj;
    TmpLast: JamLastType;

  Begin
  New(LastFile, Init);
  LastFile^.Assign(JM^.MsgPath + '.jlr');
  LastFile^.FileMode := fmReadWrite + fmDenyNone;
  LastFile^.Open(SizeOf(JamLastType));
  Error := LastFile^.IoResult;
  RecNum := FindLastRead(LastFile, UNum, UCRC);
  If RecNum >= 0 Then
    Begin
    Lastfile^.Seek(RecNum);
    If Error = 0 Then
      Begin
      Lastfile^.BlkRead(TmpLast, 1);
      Error := LastFile^.IoResult;
      GetLastRead := TmpLast.HighRead;
      End;
    End
  Else
    GetLastRead := 0;
  Dispose(LastFile, Done);
  Error := IoResult;
  End;


Procedure JamMsgObj.SetLastRead(UNum: LongInt; LR, UCRC: LongInt);
  Var
    RecNum: LongInt;
    LastFile: pFileObj;
    TmpLast: JamLastType;

  Begin
  New(LastFile, Init);
  LastFile^.Assign(JM^.MsgPath + '.jlr');
  LastFile^.FileMode := fmReadWrite + fmDenyNone;
  LastFile^.Open(SizeOf(JamLastType));
  Error := LastFile^.IoResult;
  RecNum := FindLastRead(LastFile, UNum, UCRC);
  If RecNum >= 0 Then
    Begin
    LastFile^.Seek(Recnum);
    If Error = 0 Then
      Begin
      Lastfile^.BlkRead(TmpLast, 1);
      Error := LastFile^.IoResult;
      TmpLast.HighRead := LR;
      TmpLast.LastRead := LR;
      If Error = 0 Then
        Begin
        LastFile^.Seek(RecNum);
        Error := LastFile^.IoResult;
        End;
      If Error = 0 Then
        Begin
        Lastfile^.BlkWrite(TmpLast, 1);
        Error := LastFile^.IoResult;
        End;
      End;
    End
  Else
    Begin
    TmpLast.UserNum := UNum;
    TmpLast.HighRead := Lr;
    TmpLast.NameCrc := UCRC;
    TmpLast.LastRead := Lr;
    LastFile^.Seek(LastFile^.FileSize);
    Error := LastFile^.IoResult;
    If Error = 0 Then
      Begin
      Lastfile^.BlkWrite(TmpLast, 1);
      Error := LastFile^.IoResult;
      End;
    End;
  Dispose(LastFile, Done);
  Error := IoResult;
  End;


Function JamMsgObj.GetTxtPos: LongInt;
  Begin
  GetTxtPos := JM^.TxtPos;
  End;


Procedure JamMsgObj.SetTxtPos(TP: LongInt);
  Begin
  JM^.TxtPos := TP;
  End;


Function JamMsgObj.LockMsgBase: Boolean;
  Var
    LockError: Word;

  Begin
  LockError := 0;
  If JM^.LockCount = 0 Then
    Begin
    If LockError = 0 Then
      Begin
      LockError := shLock(JM^.HdrFile, 0, 1);
      End;
    If LockError = 0 Then
      Begin
      JM^.HdrFile^.Seek(0);
      LockError := JM^.HdrFile^.IoResult;
      End;
    If LockError = 0 Then
      Begin
      Jm^.HdrFile^.BlkRead(JM^.BaseHdr, SizeOf(Jm^.BaseHdr));
      LockError := JM^.HdrFile^.IoResult;
      End;
    End;
  Inc(JM^.LockCount);
  LockMsgBase := (LockError = 0);
  End;


Function JamMsgObj.UnLockMsgBase: Boolean;
  Var
    LockError: Word;

  Begin
  LockError := 0;
  If JM^.LockCount > 0 Then
    Dec(JM^.LockCount);
  If JM^.LockCount = 0 Then
    Begin
    If LockError = 0 Then
      Begin
      LockError := UnlockFile(JM^.HdrFile^.FileHandle, 0, 1);
      End;
    If LockError = 0 Then
      Begin
      JM^.HdrFile^.Seek(0);
      LockError := JM^.HdrFile^.IoResult;
      End;
    If LockError = 0 Then
      Begin
      JM^.HdrFile^.BlkWrite(JM^.BaseHdr, SizeOf(JM^.BaseHdr));
      LockError := JM^.HdrFile^.IoResult;
      End;
    End;
  UnLockMsgBase := (LockError = 0);
  End;

{SetSeeAlso/GetSeeAlso provided by 2:201/623@FidoNet Jonas@iis.bbs.bad.se}


Procedure JamMsgObj.SetNextSeeAlso(SAlso: LongInt);
  Begin
  MsgHdr^.JamHdr.ReplyNext := SAlso;
  End;

Function JamMsgObj.GetNextSeeAlso: LongInt; {Get next see also of current msg}
  Begin
  GetNextSeeAlso := MsgHdr^.JamHdr.ReplyNext;
  End;


Function JamMsgObj.ReReadIdx(Var IdxLoc : LongInt) : Word;
  Begin
  ReReadIdx := 0;
  IdxLoc := JM^.CurrMsgNum - JM^.BaseHdr.BaseMsgNum;

  If ((IdxLoc < JM^.IdxStart) OR
    (IdxLoc >= (JM^.IdxStart+JM^.IdxRead))) Then
    Begin
    JM^.IdxStart := IdxLoc - 30;

    If JM^.IdxStart < 0 Then JM^.IdxStart := 0;
    ReReadIdx := ReadIdx;

{!!!}   if (IdxLoc - JM^.IdxStart) < 0 then
{!!!}     Dec(IdxLoc, +(IdxLoc - JM^.IdxStart));
    End;
  End;

Function JAMMsgObj.WriteMailIdx(FN: String; MsgPos: Word): Word; {Write Netmail or EchoMail.Bbs}
  Var
    IdxFile: pFileObj;
    IdxName: String;
    MsgPosStr: String;
  Begin
  FN := ForceBack(Basepath) + FN;
  if BasePath='' then EXIT;

  New(IdxFile, Init);
  IdxFile^.Assign(FN);
  IdxFile^.FileMode := fmReadWrite + fmDenyNone;
  if NOT IdxFile^.OpenOrCreate(1) then EXIT;

  Inc(MsgPos);    { Added this! }
  Str(MsgPos, MsgPosStr);
  IdxFile^.WriteLn(JM^.MsgPath + #32 + MsgPosStr);

  WriteMailIDx := JM^.IdxFile^.IoResult;

  Dispose(IdxFile, Done);
  End;

procedure JamMsgObj.SetDateTime;
var DT: DateTime;
begin
  DT.Year := FVal(Copy(JM^.MsgDate, 7, 2)); {Convert date written}
  DT.Month := FVal(Copy(JM^.MsgDate, 1, 2));
  DT.Day := FVal(Copy(JM^.MsgDate, 4, 2));
  If DT.Year < 80 Then
    Inc(DT.Year, 2000)
  Else
    Inc(DT.Year, 1900);
  DT.Sec := 0;
  DT.Hour := FVal(Copy(JM^.MsgTime, 1, 2));
  DT.Min := FVal(Copy(JM^.MsgTime, 4, 2));
  MsgHdr^.JamHdr.DateWritten := DTToUnixDate(DT);
end; { proc. SetDateTime }

End.
