unit Emsi;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
(*
**
** IEMSI (server) Routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 28-Jul-1997
** Last update : 29-Sep-1997
**
** note: The program doesn't follow the complete definition.
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF WITH_FULL}
function IEMSI_GetUser: Boolean;
procedure StartIEMSI_Session;
Function GetFossilBuffer(var Buffer: Array of Char; Start, ToExpect, Seconds: Word): Boolean;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, Crc_Unit, OutBlock, Comunit, StUtils, CurTime, Multi,
     LongStr, Cases, CentrStr, UnixDate, Support, ElLog_U,
     Global, CfgRec, ApFossil, InPut_U, ObjDec;

{$IFDEF WITH_FULL}
Const
  MaxSeconds = 10;  { Wait maximum 10 seconds for this operation to complete }


procedure StartIEMSI_Session;
var EMSI_IRQ   : String;
    Crc16      : Word;
    Counter    : Byte;
    TempStr    : String;
    Written    : Longint;
begin
  if Byte(GlobalCfg^.RaConfig^.Emsi_Enable)=Byte(No) then EXIT;
  If LineCfg^.Exitinfo^.Baud=00 then Exit;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logIEMSI, 'Sending EMSI_IRQ');
  {$ENDIF}


  EMSI_IRQ := '**EMSI_IRQ';

  Crc16 := 00;
  For Counter := 03 to Length(EMSI_IRQ) do
    Crc16 := UpdCrc(Ord(EMSI_IRQ[Counter]), Crc16);
  Crc16 := UpdCrc(0, Crc16);
  Crc16 := UpdCrc(0, Crc16);

  OutBlockObj^.DumpBlock;
  {$IFDEF WITH_FULL}
    TempStr := EMSI_IRQ + HexStr(Crc16) + #13;
    ComObj^.Com_SendWait(TempStr[1], Length(TempStr), Written, {$IFDEF FPC}@{$ENDIF}DoSlice);
  {$ENDIF}
end; { proc. StartIEMSI_Session }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetBracketString(EMSI: Array of Char; var StartPos: Word; var Dest: String);
var Count: Longint;
begin

  if EMSI[StartPos] <> '{' then
    begin
      Count := 0;

      While (Emsi[StartPos] <> '{') AND (Count < 6) do
        begin
          Inc(Count);
          Inc(StartPos);
        end; { while }

      if EMSI[StartPos] <> '{' then EXIT;
    end; { if }

  Inc(StartPos);
  Dest := '';

  While EMSI[Startpos] <> '}' do
    begin
      Dest := Dest + EMSI[StartPos];
      Inc(StartPos);

      if EMSI[StartPos] = '}' then
       if Emsi[StartPos + 01] = '}' then
         begin
           Dest := Dest + '}';
           Inc(Startpos, 2);
         end; { treat the bracket }

    end; { while }

  Inc(StartPos);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logIEMSI, 'GetBracketString: '+Dest);
  {$ENDIF}

  Dest := AsciiStr(Dest);
end; { proc. GetBracketString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Decode_IEMSI_Buffer(EMSI: Array of Char; var IEMSI_User: IEMSI_User_Record; BufferLen: Word);
var StartPos: Word;
    TempCH  : Char;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logIEMSI, 'Decoding IEMSI buffer');
  {$ENDIF}

  { The buffer already has been approved and CRC32 checked by the modem }
  { parser, so we just translate the data }

  FillChar(IEMSI_User, SizeOf(IEMSI_User), #00);
  StartPos := Length('**EMSI_ICI0000');

  GetBracketString(EMSI, StartPos, IEMSI_User.Name);
  GetBracketString(EMSI, StartPos, IEMSI_User.Alias);
  GetBracketString(EMSI, StartPos, IEMSI_User.Location);
  GetBracketString(EMSI, StartPos, IEMSI_User.DataNr);
  GetBracketString(EMSI, StartPos, IEMSI_User.VoiceNr);
  GetBracketString(EMSI, StartPos, IEMSI_User.Password);
  GetBracketString(EMSI, StartPos, IEMSI_User.Birthdate);
  GetBracketString(EMSI, StartPos, IEMSI_User.CrtDef);
  GetBracketString(EMSI, StartPos, IEMSI_User.Protocols);
  GetBracketString(EMSI, StartPos, IEMSI_User.Capabilities);
  GetBracketString(EMSI, StartPos, IEMSI_User.Requests);
  GetBracketString(EMSI, StartPos, IEMSI_User.Software);
  GetBracketString(EMSI, StartPos, IEMSI_User.XLatTable);
end; { proc. Decode_IEMSI_Buffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddBracketString(var Emsi: Array of Char; var Len: Word; Test: String);
var Counter: Word;
    StartLen: Word;
    TempStr : Byte;
    TempChar: String;
begin
  StartLen := Len;

  Emsi[StartLen] := '{';
  Inc(Len);
  Inc(StartLen);

  For Counter := 01 to Length(Test) do
    begin
      TempChar := SlashStr(Test[Counter]);

      For TempStr := 01 to Length(TempChar) do
        Emsi[StartLen +  Pred(Counter)] := TempChar[TempStr];
      Inc(Len);
    end; { for }

  Emsi[Len] := '}';
  Inc(Len);
end; { proc. AddBracketString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Send_EMSI_NAK;
var Crc16   : Word;
    EMSI_NAK: String;
    Counter : Word;
    TempStr : String;
    Written : Longint;
begin
  EMSI_NAK := '**EMSI_NAK';

  Crc16 := 00;
  For Counter := 03 to Length(EMSI_NAK) do
    Crc16 := UpdCrc(Ord(EMSI_NAK[Counter]), Crc16);
  Crc16 := UpdCrc(0, Crc16);
  Crc16 := UpdCrc(0, Crc16);

  OutBlockObj^.DumpBlock;
  {$IFDEF WITH_FULL}
    TempStr := EMSI_NAK + HexStr(Crc16) + #13;
    ComObj^.Com_SendWait(TempStr[1], Length(TempStr), Written, {$IFDEF FPC}@{$ENDIF}DoSlice);
  {$ENDIF}
end; { proc. Send_EMSI_NAK }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MakeServerRecord(var Emsi: Array of Char;
                           SystemID, SystemName, Location, SysOp, LocalTime,
                           Notice, WaitChar, Capabilities: String; var Len: Word);
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logIEMSI, 'MakeServerRecord');
  {$ENDIF}
  Len := 00;

  AddBracketString(EMSI, Len, SystemID);
  AddBracketString(EMSI, Len, SystemName);
  AddBracketString(EMSI, Len, Location);
  AddBracketString(EMSI, Len, SysOp);
  AddBracketString(EMSI, Len, LocalTime);
  AddBracketString(EMSI, Len, Notice);
  AddBracketString(EMSI, Len, WaitChar);
  AddBracketString(EMSI, Len, Capabilities);
end; { proc. MakeServerRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetFossilBuffer(var Buffer: Array of Char; Start, ToExpect, Seconds: Word): Boolean;
var EndTime: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logIEMSI, 'GetFossilBuffer');
  {$ENDIF}

  EndTime:= CurrentTime + Seconds;
  GetFossilBuffer := TRUE;

  While (CurrentTime <= EndTime) AND (ToExpect > 00) do
    begin
      {$IFDEF WITH_FULL}
       While (NOT InputObj^.KeyPressed)
               AND (CurrentTime <= EndTime) do
                DoSlice;
      {$ENDIF}

      {$IFDEF WITH_FULL}
       if InputObj^.KeyPressed then
         begin
            Buffer[Start] := InputObj^.ReadKey;

            if NOT LineCfg^.SysOpKey then
              begin
                Inc(Start);
                Dec(ToExpect);
              end; { if }
         end; { if }
      {$ENDIF}

    end; { while }

  if ToExpect > 00 then GetFossilBuffer := false;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logIEMSI, 'End Of GetFossilBufer');
  {$ENDIF}

  Dec(Start);
end; { func. GetFossilBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IEMSI_GetUser: Boolean;
label CheckIEMSI, TransmitICI, GetACK;

var StartingTime: Longint;
    Emsi_Buffer : Array[0..4095] of Char;
    Counter     : Longint;
    PktCrc32    : Longint;
    RecvCrc     : Longint;
    HexLongStr  : String;
    TempStr     : String;
    ServerRec   : IEMSI_Serv_Record;
    EMSI_Len    : Word;
    PackageLen  : Word;
    TempChar    : Char;
    Tries       : Word;
    EndTime     : Longint;
    AckTries    : Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logIEMSI, 'IEMSI_GetUser');{$endif}

  FillChar(Emsi_Buffer, SizeOf(Emsi_Buffer), #00);
  IEMSI_GetUser := false;

  Emsi_Buffer[0] := '*';
  Emsi_Buffer[1] := '*';
  Emsi_Buffer[2] := 'E';
  Emsi_Buffer[3] := 'M';
  Emsi_Buffer[4] := 'S';
  Emsi_Buffer[5] := 'I';
  Emsi_Buffer[6] := '_';

  if NOT GetFossilBuffer(Emsi_Buffer, 7, Length('ICI0000'), MaxSeconds) then
    begin
      Send_EMSI_NAK;
      Send_EMSI_NAK;
      EXIT;
    end; { abort IEMSI session }

  PackageLen := FVal('$' + Emsi_Buffer[10] + Emsi_Buffer[11] + Emsi_Buffer[12] + Emsi_Buffer[13]);
  if PackageLen = 00 then
      begin
        Send_EMSI_NAK;
        Send_EMSI_NAK;
        EXIT;
      end; { if }

CheckIEMSI:
  if NOT GetFossilBuffer(Emsi_Buffer, Length('**EMSI_ICI0000'), PackageLen + 08, MaxSeconds) then
    begin
      Send_EMSI_NAK;
      EXIT;
    end; { if }

  Counter := Length('**EMSI_ICI0000') + PackageLen + 08;

  Move(Emsi_Buffer[Counter-8], TempStr[1], 8);
  TempStr[0] := #8;
  TempStr := SUpcase(Trim(TempStr));

  PktCrc32 := Longint($FFFFFFFF);

  for Counter := 2 to Counter - 09
     do begin
          PktCrc32 := UpdC32(Byte(Emsi_Buffer[Counter]), PktCrc32);
        end;
  HexLongStr := HexLong(PktCrc32);

  if TempStr <> HexLongStr then
    begin { Wrong CRC }
      Send_EMSI_NAK;
      IEMSI_GetUser := False;
      EXIT;
    end; { if }

{------------------------- Remote package received ---------------------------}
{-----------------------------------------------------------------------------}
  IEMSI_GetUser := True;
  Decode_IEMSI_Buffer(EMSI_Buffer, LineCfg^.Emsi_User^, Counter);

  ServerRec.SystemID    := FullProgName + #32 + VersionID;
  ServerRec.SystemName  := GlobalCfg^.RaConfig^.Systemname;
  ServerRec.Location    := GlobalCfg^.RaConfig^.Location;
  ServerRec.SysOp       := GlobalCfg^.RaConfig^.SysOp;
  ServerRec.Localtime   := HexLong(NowAsUnixDate);
  ServerRec.Notice      := SysEmsiNotice;
  ServerRec.WaitChar    := '';
  ServerRec.Capabilities:= SysCapabilities;

  FillChar(EMSI_Buffer, SizeOf(Emsi_Buffer), #00);
  MakeServerRecord(EMSI_Buffer,
                   ServerRec.SystemID,
                   ServerRec.SystemName,
                   ServerRec.Location,
                   ServerRec.SysOp,
                   ServerRec.LocalTime,
                   ServerRec.Notice,
                   ServerRec.WaitChar,
                   ServerRec.Capabilities, EMSI_Len);

  TempStr := HexStr(EMSI_Len);
  PktCrc32 := Longint($FFFFFFFF);
  Tries := 00;

TransmitICI:
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logIEMSI, 'Transmitting ICI');
  {$ENDIF}

  Inc(Tries);
  if Tries>3 then EXIT;

  PktCrc32 := UpdC32(Byte('E'), PktCrc32);
  PktCrc32 := UpdC32(Byte('M'), PktCrc32);
  PktCrc32 := UpdC32(Byte('S'), PktCrc32);
  PktCrc32 := UpdC32(Byte('I'), PktCrc32);
  PktCrc32 := UpdC32(Byte('_'), PktCrc32);
  PktCrc32 := UpdC32(Byte('I'), PktCrc32);
  PktCrc32 := UpdC32(Byte('S'), PktCrc32);
  PktCrc32 := UpdC32(Byte('I'), PktCrc32);

  PktCrc32 := UpdC32(Byte(TempStr[1]), PktCrc32);
  PktCrc32 := UpdC32(Byte(TempStr[2]), PktCrc32);
  PktCrc32 := UpdC32(Byte(TempStr[3]), PktCrc32);
  PktCrc32 := UpdC32(Byte(TempStr[4]), PktCrc32);

  {$IFDEF WITH_FULL}
    ComObj^.Com_PurgeInBuffer;

  OutBlockObj^.DumpBlock;
  ComObj^.Com_SendString('**EMSI_ISI' + TempStr);

  For Counter := 00 to Pred(EMSI_Len) do
    begin
      PktCrc32 := UpdC32(Byte(Emsi_Buffer[Counter]), PktCRC32);

      ComObj^.Com_SendString(EMSI_Buffer[Counter]);
    end; { For Counter }

  ComObj^.Com_SendString(Hexlong(PktCrc32) + #13);

  AckTries := 00;
  {$ENDIF}

{--------------------- We skip all data from remote like ACK/NAK -------------}
{-----------------------------------------------------------------------------}
GetACK:
  Support.Delay(250);

  GetFossilBuffer(Emsi_Buffer, 1, Length('**EMSI_ACK0000'), MaxSeconds);
  GetFossilBuffer(Emsi_Buffer, 1, Length('**EMSI_ACK0000'), MaxSeconds);

  LineCfg^.Exitinfo^.Emsi_Session     := True;
  LineCfg^.Exitinfo^.Emsi_CrtDef      := LineCfg^.Emsi_User^.CrtDef;
  LineCfg^.Exitinfo^.Emsi_Protocols   := LineCfg^.Emsi_User^.Protocols;
  LineCfg^.Exitinfo^.Emsi_Capabilities:= LineCfg^.Emsi_User^.Capabilities;
  LineCfg^.Exitinfo^.Emsi_Requests    := LineCfg^.Emsi_User^.Requests;
  LineCfg^.Exitinfo^.Emsi_Software    := LineCfg^.Emsi_User^.Software;
  LineCfg^.Exitinfo^.Userinfo.Name    := LineCfg^.Emsi_User^.Name;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logIEMSI, 'Established session');
  {$ENDIF}

  RaLog('>', 'Established interactive EMSI session');
  RaLog('>', 'Remote is using '+LineCfg^.Emsi_User^.Software);
  RaLog('>', 'Request flags: '+LineCfg^.Emsi_User^.Requests);

  EndTime := CurrentTime + 15;

  if NOT ComObj^.Com_CharAvail then Support.Delay(200);

  {$IFDEF WITH_FULL}
    While (ComObj^.Com_CharAvail) AND
           (CurrentTime < EndTime) do ComObj^.Com_GetChar;
  {$ENDIF}
end; { func. IEMSI_GetUser }
{$ENDIF} { With-Full }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
