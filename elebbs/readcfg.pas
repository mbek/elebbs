unit ReadCfg;
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
** ReadCONFIG routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 01-May-1998
** Last update : 01-May-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec;

procedure ReadModemRA(var Modem: ModemRecord);     { Opens and reads MODEM }
procedure ReadConfigRA;                           { Opens and reads CONFIG.RA }
procedure ReadTelnetELE;                         { Opens and reads TELNET.ELE }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, ElLog_U, Cases, Global, LongStr, GenFile, CfgFile,
     StrPath, FileObj, ObjDec;


procedure ReadConfigRA;                           { Opens and reads CONFIG.RA }
var Config_F: pFileObj;
    TempPath: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'ReadConfigRa (begin)');
  {$ENDIF}

  if FileExist('config.ra') then TempPath := ''
    else Temppath := ForceBack(GetSysEnv);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'ReadConfigRa - Reading '+ForceBack(TempPath)+'config.ra');
  {$ENDIF}

  Config_OpenFile(Config_F, ForceBack(TempPath)+'config.ra', 01, ReadMode + DenyWrite, False, True);
  Config_ReadFile(Config_F, GlobalCfg^.RaConfig^, SizeOf(ConfigRecord));
  Config_DoneFile(Config_F);

  Config_OpenFile(Config_F, ForceBack(TempPath)+'config.ele', 01, ReadMode + DenyWrite, False, True);
  Config_ReadFile(Config_F, GlobalCfg^.ElConfig^, SizeOf(EleConfigRecord));
  Config_DoneFile(Config_F);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'ReadConfigRa ( end )');
  {$ENDIF}
end; { proc. ReadConfigRA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadModemRA(var Modem: ModemRecord);     { Opens and reads MODEM }
var Modem_F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ReadModemRa (begin)');
     DebugObj.DebugLog(logMain, 'modem.ra Filename = '+ModemFileName);
  {$ENDIF}

  Config_OpenFile(Modem_F, ModemFileName, 01, ReadMode + DenyWrite, False, True);
  Config_ReadFile(Modem_F, Modem, SizeOf(ModemRecord));
  Config_DoneFile(Modem_F);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ReadModemRA ( end )');
  {$ENDIF}
end; { proc. ReadModemRA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadTelnetELE;                         { Opens and reads TELNET.ELE }
var Telnet_F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ReadTelnetEle (begin)');
     DebugObj.DebugLog(logMain, 'TELNET.ELE Filename = '+TelnetFileName);
  {$ENDIF}

  Config_OpenFile(Telnet_F, TelnetFileName, 01, ReadMode + DenyWrite, False, True);
  Config_ReadFile(Telnet_F, LineCfg^.Telnet^, SizeOf(TelnetRecord));
  Config_DoneFile(Telnet_F);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ReadTelnetELE ( end )');
  {$ENDIF}
end; { proc. ReadTelnetELE }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { unit READCFG }
