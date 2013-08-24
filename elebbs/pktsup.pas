unit PKTSUP;
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
** PKT 2+ support Routines for EleBBS
**
** Copyright (c) 1997-1999 by Maarten Bekers
**
** Created : 13-Apr-1999
** Last update : 13-Apr-1999
**
** Note: PKT definitions used from FroDo's v2.30 dev. kit
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Global, FileObj, Dos, GenDos;

Const CW_2PLUS   = $0001;                       { Support for Type 2+ bundles }
      CW_N2PLUS  = $0100;                           { Validation for CW_2PLUS }
      PKTVERSION = 2;                         { Current packet revision level }

type  FTS1PktHdr = packed record
        OrigNode,                                          { Originating node }
        DestNode,                                          { Destination node }
        Year,                                      { Year created (1993-nnnn) }
        Month,                                         { Month created (0-11) }
        Day,                                             { Day created (1-31) }
        Hour,                                           { Hour created (0-23) }
        Minute,                                       { Minute created (0-59) }
        Second,                                       { Second created (0-59) }
        Rate,                                             { Baudrate (unused) }
        Version,                                         { Packet version (2) }
        OrigNet,                                            { Originating net }
        DestNet        : SmallWord;                         { Destination net }
        PCodeLo,                                   { Product code (low-order) }
        PRevMajor      : Byte;                                { Major version }
        Password       : Array[1..8] of Char;  { Packet password (not ASCIIZ) }
        QMOrigZone,                                { Originating zone (QMail) }
        QMDestZone,                                { Destination zone (QMail) }
        AuxNet,                                      { Auxillary net (unused) }
        CWValidate     : SmallWord;                      { CapWord validation }
        PCodeHi,                                  { Product code (high-order) }
        PRevMinor      : Byte;                                { Minor version }
        CWCapWord,                                             { Capabilities }
        OrigZone,                                 { Originating zone (FSC-39) }
        DestZone,                                 { Destination zone (FSC-39) }
        OrigPoint,                               { Originating point (FSC-39) }
        DestPoint      : SmallWord;              { Destination point (FSC-39) }
        LongData       : Longint;             { Product dependent information }
      end; { FTS1PKTHDR }


      FTS1PKTMSG = packed RECORD
        Version,                                         { Packet version (2) }
        OrigNode,                                          { Originating node }
        DestNode,                                          { Destination node }
        OrigNet,                                            { Originating net }
        DestNet,                                            { Destination net }
        Attr,                                                   { FTS1 status }
        Cost           : SmallWord;                         { Cost of message }
        (*
        **  The message header follows this but is of variable length ASCIIZ
        **  strings in the order: AsciiDate, ToUser, FromUser, Subject, and
        **  MsgText.
        *)
      end; { FTS1PKTMSG }


function  GetPacketName(Dest: NetAddress): String;
procedure CreatePacket(const AreaName   : String;
                       const PacketName,
                             PktPassword: String;
                             ToWho,
                             FromWho,
                             Subject    : String;
                       const MsgDate    : DateTime;
                       const EditorLines: PChar;
                       var   OrigAddr,
                             DestAddr   : NetAddress);


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses JDates, LongStr, Strings, Sysutils, Stutils;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetDateStr(Date: DateTime): String;
begin
  { 01 Jan 86  02:34:56 }

  GetDateStr := LeadingZero(Date.Day, 2) + #32 +
                Copy(MonthString(Date.Month), 1, 3) + #32 +
                Copy(FStr(Date.Year), 3, 2) + #32 +
                LeadingZero(Date.Hour, 2) + ':' +
                LeadingZero(Date.Min, 2) + ':' +
                LeadingZero(Date.Sec, 2);
end; { func. GetDateStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CutLen(const S: String; MaxLen: Byte): String;
begin
  if Length(s) < MaxLen then CutLen := S
    else CutLen := Copy(S, 1, MaxLen);
end; { func. Cutlen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetPacketName(Dest: NetAddress): String;
var TempStr: String;
begin
  FillChar(TempStr, SizeOf(TempStr), Ord('0'));

  TempStr := HexStr(Dest.Zone) +
             HexStr(Dest.Net) +
             HexStr(Dest.Node) +
             HexStr(Dest.Point);
  TempStr[0] := #08;
  TempStr := TempStr + '.PKT';

  GetPacketName := TempStr;
end; { func. GetPacketName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreatePacket(const AreaName   : String;
                       const PacketName,
                             PktPassword: String;
                             ToWho,
                             FromWho,
                             Subject    : String;
                       const MsgDate    : DateTime;
                       const EditorLines: PChar;
                       var   OrigAddr,
                             DestAddr   : NetAddress);
var PKT_F   : pFileObj;
    PktHdr  : FTS1PKTHDR;
    PktMsg  : FTS1PKTMSG;

procedure WritePacketHeader;
begin
  With PktHdr do
    begin
      Version := PktVersion;
      CWValidate := CW_N2PLUS;
      CWCapWord := CW_2PLUS;

      OrigZone := OrigAddr.Zone;
      OrigNode := OrigAddr.Node;
      OrigNet  := OrigAddr.Net;
      OrigPoint:= OrigAddr.Point;

      DestZone := DestAddr.Zone;
      DestNet  := DestAddr.Net;
      DestNode := DestAddr.Node;
      DestPoint:= DestAddr.Point;

      QMOrigZone := OrigAddr.Zone;
      QMDestZone := DestAddr.Zone;

      Year     := MsgDate.Year;
      Month    := MsgDate.Month;
      Day      := MsgDate.Day;
      Hour     := MsgDate.Hour;
      Minute   := MsgDate.Min;
      Second   := MsgDate.Sec;

      PCodeHi  := ProductCodeHi;
      PCodeLo  := ProductCodeLo;

      PRevMajor:= VersionMajor;
      PRevMinor:= VersionMinor;

      Move(PktPassword[1], Password[1], Length(PktPassword));
    end; { with }

  PKT_F^.BlkWrite(PktHdr, SizeOf(PktHdr));
end; { proc. WritePacketHeader }

procedure SearchEOF;
var Temp: String[4];
begin
  PKT_F^.Seek(PKT_F^.FileSize - 4);
  PKT_F^.BlkRead(Temp[1], SizeOf(Temp) - 01);

  Temp[0] := #04;
  PKT_F^.Seek((PKT_F^.FileSize - (5 - Pos(#0, Temp))) + 01);
end; { proc. SearchEOF }

procedure WritePacketMsg;
var TempStr: String;
begin
  With PktMsg do
    begin
      Version  := PktVersion;
      OrigNode := OrigAddr.Node;
      OrigNet  := OrigAddr.Net;

      DestNode := DestAddr.Node;
      DestNet  := DestAddr.Net;

      Attr := 00;
      Cost := 00;
    end; { with }

  PKT_F^.BlkWrite(PktMsg, SizeOf(PktMsg));

  TempStr := GetDateStr(MsgDate) + #00;
  PKT_F^.BlkWrite(TempStr[1], Length(TempStr));           { Write AsciiDate }

  TempStr := ToWho + #00;
  PKT_F^.BlkWrite(TempStr[1], Length(TempStr));              { Write ToUser }

  TempStr := FromWho + #00;
  PKT_F^.BlkWrite(TempStr[1], Length(TempStr));            { Write FromUser }

  TempStr := Subject + #00;
  PKT_F^.BlkWrite(TempStr[1], Length(TempStr));             { Write Subject }

  TempStr := 'AREA:' + AreaName + #00;
  PKT_F^.BlkWrite(TempStr[1], Length(TempStr));            { Write Areaname }

  PKT_F^.BlkWrite(EditorLines^, StrLen(EditorLines));       { Write MsgText }

  TempStr := #00 + #00;
  PKT_F^.BlkWrite(TempStr[1], Length(TempStr));                { Write null }
end; { proc. WritePacketMsg }


begin
  FromWho := CutLen(FromWho, 35);
  ToWho := CutLen(ToWho, 35);
  Subject := CutLen(Subject, 71);

  FillChar(PktHdr, SizeOf(PktHdr), #00);
  FillChar(PktMsg, SizeOf(PktMsg), #00);

  New(Pkt_F, Init);
  Pkt_F^.Assign(PacketName);
  if NOT Pkt_F^.OpenOrCreate(1) then
    begin
      Dispose(Pkt_F, Done);
      EXIT;
    end; { if }

  {---------------------- Go to the end of the packet -----------------------}
  Pkt_F^.Seek(Pkt_F^.FileSize);
  if Pkt_F^.FileSize = 00 then WritePacketHeader
    else SearchEOF;

  {-------------------- Actually write the information ----------------------}
  WritePacketMsg;

  Dispose(Pkt_F, Done);
end; { proc. CreatePacket }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit PKTSUP }
