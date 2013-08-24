unit MnuModem;
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
** MNUMODEM.TPU, Modem unit for ELCONFIG
**
** Copyright (c) 1996-1998 by Maarten Bekers
**
** Created : 14-Nov-1998
** Last update : 14-Nov-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

procedure DoModemOptions;
procedure DoModemCommands;
procedure DoModemResponses;
procedure DoModemTelnet;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses MenuSys, StrEdit, CfgRec, MemMan, ScrnU,
      LongStr, Global, GenCfg, BitWise;

procedure DoModemOptions;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    SaveDirect: Boolean;
    TmpByte   : Byte;
begin;
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoModemOptions');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Comport', 2001, #00, 'Communications port modem is attached to (0=local operation only)', 1);
 AddPullItem(Menu.PullInf^[02], 'Speed  ', 2002, #00, 'Maximum speed the modem supports, or locked speed', 1);
 AddPullItem(Menu.PullInf^[03], 'Lock   ', 2003, #00, 'Whether to lock the modem speed at a fixed rate (for '+
                                                      'high-speed modems)', 1);
 AddPullItem(Menu.PullInf^[04], 'Answer ', 2004, #00, 'Yes means EleBBS will answer calls automatically', 1);
 AddPullItem(Menu.PullInf^[05], 'Delay  ', 2005, #00, 'Delay (in milliseconds) to insert between characters', 1);
 AddPullItem(Menu.PullInf^[06], 'Buffer ', 2006, #00, 'Size (in bytes) of the internal outbound buffer, used for '+
                                                      'tuning performance', 1);
 AddPullItem(Menu.PullInf^[07], 'Break  ', 2007, #00, 'If set to Yes, EleBBS sends a break signal to clear the '+
                                                      'modem''s buffer', 1);
 AddPullItem(Menu.PullInf^[08], 'Tries  ', 2008, #00, 'Number of times to attempt to initialise the modem before aborting', 1);
 AddPullItem(Menu.PullInf^[09], 'Offhook', 2009, #00, 'Take the modem off-hook when a call is terminated?', 1);

 Menu.Items   := 9;
 Menu.Width   := 17;
 Menu.Length  := 9;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Modem options ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 REPEAT
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(12, 06, mnuNormColor, FStr(LineCfg^.Modem^.Comport));
  WriteAT(12, 07, mnuNormColor, FStr(FixBaud(LineCfg^.Modem^.MaxSpeed)));
  WriteAT(12, 08, mnuNormColor, Bool2Str(LineCfg^.Modem^.LockModem));
  WriteAT(12, 09, mnuNormColor, Bool2Str(LineCfg^.Modem^.AnswerPhone));
  WriteAT(12, 10, mnuNormColor, FStr(LineCfg^.Modem^.ModemDelay));
  WriteAT(12, 11, mnuNormColor, FStr(LineCfg^.Modem^.BufferSize));
  WriteAT(12, 12, mnuNormColor, Bool2Str(LineCfg^.Modem^.SendBreak));
  WriteAT(12, 13, mnuNormColor, FStr(LineCfg^.Modem^.InitTries));
  WriteAT(12, 14, mnuNormColor, Bool2Str(LineCfg^.Modem^.OffHook));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
      2001 : EditByte(LineCfg^.Modem^.Comport, 12, 06, mnuEditColor, True);
      2002 : EditModemSpeed(LineCfg^.Modem^.MaxSpeed, 12, 07);
      2003 : Edit_Boolean(LineCfg^.Modem^.LockModem, 12, 08, mnuNormColor);
      2004 : Edit_Boolean(LineCfg^.Modem^.AnswerPhone, 12, 09, mnuNormColor);
      2005 : begin
               TmpByte := LineCfg^.Modem^.ModemDelay;
               EditByte(TmpByte, 12, 10, mnuEditColor, True);
               LineCfg^.Modem^.ModemDelay := TmpByte;
             end; { if }
      2006 : EditWord(LineCfg^.Modem^.BufferSize, 12, 11, mnuEditColor, True);
      2007 : Edit_Boolean(LineCfg^.Modem^.SendBreak, 12, 12, mnuNormColor);
      2008 : EditByte(LineCfg^.Modem^.InitTries, 12, 13, mnuEditColor, True);
      2009 : Edit_Boolean(LineCfg^.Modem^.OffHook, 12, 14, mnuNormColor);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoModemOptions }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoModemCommands;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    SaveDirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoModemCommands');
 Initpullmenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Init1 ', 2101, #00, 'Command to send to initialise the modem (1)', 1);
 AddPullItem(Menu.PullInf^[02], 'Init2 ', 2102, #00, 'Command to send to initialise the modem (2)', 1);
 AddPullItem(Menu.PullInf^[03], 'Busy  ', 2103, #00, 'Command to send to take the modem off-hook', 1);
 AddPullItem(Menu.PullInf^[04], 'Answer', 2104, #00, 'Command to send to make the modem answer an incoming call', 1);

 Menu.Items   := 4;
 Menu.Width   := 78;
 Menu.Length  := 4;
 Menu.X       := 2;
 Menu.Y       := 12;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Modem commands ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(12, 14, mnuNormColor, LineCfg^.Modem^.InitStr);
  WriteAT(12, 15, mnuNormColor, LineCfg^.Modem^.InitStr2);
  WriteAT(12, 16, mnuNormColor, LineCfg^.Modem^.BusyStr);
  WriteAT(12, 17, mnuNormColor, LineCfg^.Modem^.AnswerStr);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
     2101 : LineCfg^.Modem^.InitStr := Edit(LineCfg^.Modem^.InitStr, 12, 14, mnuEditColor, 65, False, False, True);
     2102 : LineCfg^.Modem^.InitStr2:= Edit(LineCfg^.Modem^.InitStr2, 12, 15, mnuEditColor, 65, False, False, True);
     2103 : LineCfg^.Modem^.BusyStr := Edit(LineCfg^.Modem^.BusyStr, 12, 16, mnuEditColor, 65, False, False, True);
     2104 : LineCfg^.Modem^.AnswerStr:= Edit(LineCfg^.Modem^.AnswerStr, 12, 17, mnuEditColor, 20, False, False, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoModemCommands }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoModemResponses;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    SaveDirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoModemresponses');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Init  ', 2201, #00, 'Modem''s response to initialisation, eg. OK', 1);
 AddPullItem(Menu.PullInf^[02], 'Busy  ', 2202, #00, 'Modem''s response to going off-hook, eg. OK', 1);
 AddPullItem(Menu.PullInf^[03], 'Ring  ', 2203, #00, 'Modem''s response when an incoming call is detected, eg. RING', 1);
 AddPullItem(Menu.PullInf^[04], 'Secure', 2204, #00, 'Modem''s response to indicate an error-free connection, eg. /ARQ', 1);
 AddPullItem(Menu.PullInf^[05], '300   ', 2205, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[06], '1200  ', 2206, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[07], '2400  ', 2207, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[08], '4800  ', 2208, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[09], '7200  ', 2209, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[10], '9600  ', 2210, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[11], '12000 ', 2211, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[12], '14400 ', 2212, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[13], '16800 ', 2213, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[14], '19200 ', 2214, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[15], '21600 ', 2215, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[16], '24000 ', 2216, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[17], '26400 ', 2217, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[18], '28800 ', 2218, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[19], '31200 ', 2219, #00, 'Modem''s response to indicate a successful connect', 1);
 AddPullItem(Menu.PullInf^[20], '33600 ', 2220, #00, 'Modem''s response to indicate a successful connect', 1);

 AddPullItem(Menu.PullInf^[21], '38400 ', 2221, #00, 'Modem''s response to indicate a successful connect', 2);
 AddPullItem(Menu.PullInf^[22], '57600 ', 2222, #00, 'Modem''s response to indicate a successful connect', 2);
 AddPullItem(Menu.PullInf^[23], '64000 ', 2223, #00, 'Modem''s response to indicate a successful connect', 2);
 AddPullItem(Menu.PullInf^[24], '115200', 2224, #00, 'Modem''s response to indicate a successful connect', 2);
 AddPullItem(Menu.PullInf^[25], 'Fax   ', 2225, #00, 'Modem''s response to indicate a successful connect', 2);

 Menu.Items   := 25;
 Menu.Width   := 79;
 Menu.Length  := 20;
 Menu.X       := 1;
 Menu.Y       := 1;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Modem responses ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;
 Menu.PosArray[2].XInc := 38;
 Menu.PosArray[2].YDec := 20;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(10, 03, mnuNormColor, LineCfg^.Modem^.InitResp);
  WriteAT(10, 04, mnuNormColor, LineCfg^.Modem^.BusyResp);
  WriteAT(10, 05, mnuNormColor, LineCfg^.Modem^.RingStr);
  WriteAT(10, 06, mnuNormColor, LineCfg^.Modem^.ErrorFreeString);
  WriteAT(10, 07, mnuNormColor, LineCfg^.Modem^.Connect300);
  WriteAT(10, 08, mnuNormColor, LineCfg^.Modem^.Connect1200);
  WriteAT(10, 09, mnuNormColor, LineCfg^.Modem^.Connect2400);
  WriteAT(10, 10, mnuNormColor, LineCfg^.Modem^.Connect4800);
  WriteAT(10, 11, mnuNormColor, LineCfg^.Modem^.Connect7200);
  WriteAT(10, 12, mnuNormColor, LineCfg^.Modem^.Connect9600);
  WriteAT(10, 13, mnuNormColor, LineCfg^.Modem^.Connect12k);
  WriteAT(10, 14, mnuNormColor, LineCfg^.Modem^.Connect14k);
  WriteAT(10, 15, mnuNormColor, LineCfg^.Modem^.Connect16k);
  WriteAT(10, 16, mnuNormColor, LineCfg^.Modem^.Connect19k);
  WriteAT(10, 17, mnuNormColor, LineCfg^.Modem^.Connect21k);
  WriteAT(10, 18, mnuNormColor, LineCfg^.Modem^.Connect24k);
  WriteAT(10, 19, mnuNormColor, LineCfg^.Modem^.Connect26k);
  WriteAT(10, 20, mnuNormColor, LineCfg^.Modem^.Connect28k);
  WriteAT(10, 21, mnuNormColor, LineCfg^.Modem^.Connect31k);
  WriteAT(10, 22, mnuNormColor, LineCfg^.Modem^.Connect33k);

  WriteAT(48, 03, mnuNormColor, LineCfg^.Modem^.Connect38k);
  WriteAT(48, 04, mnuNormColor, LineCfg^.Modem^.Connect57k);
  WriteAT(48, 05, mnuNormColor, LineCfg^.Modem^.Connect64k);
  WriteAT(48, 06, mnuNormColor, LineCfg^.Modem^.Connect115k);
  WriteAT(48, 07, mnuNormColor, LineCfg^.Modem^.ConnectFax);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    2201 : LineCfg^.Modem^.InitResp := Edit(LineCfg^.Modem^.InitResp, 10, 03, mnuEditColor, 16, False, False, True);
    2202 : LineCfg^.Modem^.BusyResp := Edit(LineCfg^.Modem^.BusyResp, 10, 04, mnuEditColor, 16, False, False, True);
    2203 : LineCfg^.Modem^.RingStr := Edit(LineCfg^.Modem^.RingStr, 10, 05, mnuEditColor, 20, False, False, True);
    2204 : LineCfg^.Modem^.ErrorFreeString := Edit(LineCfg^.Modem^.ErrorFreeString, 10, 06, mnuEditColor, 15,False,False,True);
    2205 : LineCfg^.Modem^.Connect300 := Edit(LineCfg^.Modem^.Connect300, 10, 07, mnuEditColor, 30, False, False, True);
    2206 : LineCfg^.Modem^.Connect1200 := Edit(LineCfg^.Modem^.Connect1200, 10, 08, mnuEditColor, 30, False, False, True);
    2207 : LineCfg^.Modem^.Connect2400 := Edit(LineCfg^.Modem^.Connect2400, 10, 09, mnuEditColor, 30, False, False, True);
    2208 : LineCfg^.Modem^.Connect4800 := Edit(LineCfg^.Modem^.Connect4800, 10, 10, mnuEditColor, 30, False, False, True);
    2209 : LineCfg^.Modem^.Connect7200 := Edit(LineCfg^.Modem^.Connect7200, 10, 11, mnuEditColor, 30, False, False, True);
    2210 : LineCfg^.Modem^.Connect9600 := Edit(LineCfg^.Modem^.Connect9600, 10, 12, mnuEditColor, 30, False, False, True);
    2211 : LineCfg^.Modem^.Connect12k := Edit(LineCfg^.Modem^.Connect12k, 10, 13, mnuEditColor, 30, False, False, True);
    2212 : LineCfg^.Modem^.Connect14k := Edit(LineCfg^.Modem^.Connect14k, 10, 14, mnuEditColor, 30, False, False, True);
    2213 : LineCfg^.Modem^.Connect16k := Edit(LineCfg^.Modem^.Connect16k, 10, 15, mnuEditColor, 30, False, False, True);
    2214 : LineCfg^.Modem^.Connect19k := Edit(LineCfg^.Modem^.Connect19k, 10, 16, mnuEditColor, 30, False, False, True);
    2215 : LineCfg^.Modem^.Connect21k := Edit(LineCfg^.Modem^.Connect21k, 10, 17, mnuEditColor, 30, False, False, True);
    2216 : LineCfg^.Modem^.Connect24k := Edit(LineCfg^.Modem^.Connect24k, 10, 18, mnuEditColor, 30, False, False, True);
    2217 : LineCfg^.Modem^.Connect26k := Edit(LineCfg^.Modem^.Connect26k, 10, 19, mnuEditColor, 30, False, False, True);
    2218 : LineCfg^.Modem^.Connect28k := Edit(LineCfg^.Modem^.Connect28k, 10, 20, mnuEditColor, 30, False, False, True);
    2219 : LineCfg^.Modem^.Connect31k := Edit(LineCfg^.Modem^.Connect31k, 10, 21, mnuEditColor, 30, False, False, True);
    2220 : LineCfg^.Modem^.Connect33k := Edit(LineCfg^.Modem^.Connect33k, 10, 22, mnuEditColor, 30, False, False, True);
    2221 : LineCfg^.Modem^.Connect38k := Edit(LineCfg^.Modem^.Connect38k, 48, 03, mnuEditColor, 30, False, False, True);
    2222 : LineCfg^.Modem^.Connect57k := Edit(LineCfg^.Modem^.Connect57k, 48, 04, mnuEditColor, 30, False, False, True);
    2223 : LineCfg^.Modem^.Connect64k := Edit(LineCfg^.Modem^.Connect64k, 48, 05, mnuEditColor, 30, False, False, True);
    2224 : LineCfg^.Modem^.Connect115k := Edit(LineCfg^.Modem^.Connect115k, 48, 06, mnuEditColor, 30, False, False, True);
    2225 : LineCfg^.Modem^.ConnectFax := Edit(LineCfg^.Modem^.ConnectFax, 48, 07, mnuEditColor, 30, False, False, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoModemResponses }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoModemTelnet;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    Temp      : SmallWord;
    SaveDirect: Boolean;
    Tmpbyte   : Byte;
    TmpWord   : SmallWord;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoModemTelnet');
  InitPullMenu(Menu);
  AddPullItem(Menu.PullInf^[01], 'Max. Sessions  ', 2001, #00, 'Maximum number of simultaneous sessions allowed', 1);
  AddPullItem(Menu.PullInf^[02], 'Server port    ', 2002, #00, 'Port that EleBBS will respond to when called', 1);
  AddPullItem(Menu.PullInf^[03], 'Start node#    ', 2003, #00, 'EleBBS will start counting from this nodenumber '+
                                   'with new telnet nodes', 1);
  AddPullItem(Menu.PullInf^[04], 'No telnet.DL   ', 2004, #00, 'Disallow downloads for telnet nodes?', 1);
  AddPullItem(Menu.PullInf^[05], 'Show type      ', 2005, #00, 'How to show new nodes, (normal, minimized, hidden)', 1);

  Menu.Items   := 5;
  Menu.Width   := 29;
  Menu.Length  := 5;
  Menu.X       := 2;
  Menu.Y       := 4;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Telnet options ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectscrnUpdate := false;
    WriteAT(20, 06, mnuNormColor, FStr(LineCfg^.Telnet^.MaxSessions));
    WriteAT(20, 07, mnuNormColor, FStr(LineCfg^.Telnet^.ServerPort));
    WriteAT(20, 08, mnuNormColor, FStr(LineCfg^.Telnet^.StartNodeWith));
    WriteAT(20, 09, mnuNormColor, Bit2Str(LineCfg^.Telnet^.Attrib, 0));

    if ReadBit(LineCfg^.Telnet^.Attrib, 1) then
      begin
        WriteAT(20, 10, mnuNormColor, 'Minimized')
      end
    else
      begin
        if ReadBit(LineCfg^.Telnet^.Attrib, 2) then
          begin
            WriteAT(20, 10, mnuNormColor, 'Hidden    ')
          end { if }
            else WriteAT(20, 10, mnuNormColor, 'Normal    ');
      end;

    DirectScrnUpdate := SaveDirect;

    Choice := DoPullMenu(Menu, CH, False, False);

    case Choice of
        2001 : begin
                 TmpByte := LineCfg^.Telnet^.MaxSessions;
                 EditByte(TmpByte, 20, 06, mnuEditColor, True);
                 LineCfg^.Telnet^.MaxSessions := TmpByte;
               end;
        2002 : begin
                 TmpWord := LineCfg^.Telnet^.ServerPort;
                 EditWord(TmpWord, 20, 07, mnuEditColor, True);
                 LineCfg^.Telnet^.ServerPort := TmpWord;
               end;
        2003 : begin
                 TmpByte := LineCfg^.Telnet^.StartNodeWith;
                 EditByte(TmpByte, 20, 08, mnuEditColor, True);
                 LineCfg^.Telnet^.StartNodeWith := TmpByte;
               end;
        2004 : LineCfg^.Telnet^.Attrib := Edit_Bit(LineCfg^.Telnet^.Attrib, 00, 20, 09, mnuNormColor);
        2005 : begin
                 TmpByte := LineCfg^.Telnet^.Attrib;

                 if ReadBit(LineCfg^.Telnet^.Attrib, 1) then
                   begin
                     ClearBit(TmpByte, 1);
                     SetBit(TmpByte, 2);
                   end { if }
                     else begin
                            if ReadBit(LineCfg^.Telnet^.Attrib, 2) then
                              begin
                                ClearBit(TmpByte, 2);
                              end
                                else begin
                                       SetBit(TmpByte, 1);
                                     end; { else }
                          end; { else }

                 LineCfg^.Telnet^.Attrib := TmpByte;
               end; { if }
    end; { Case }

  until Choice=0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoModemTelnet }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
