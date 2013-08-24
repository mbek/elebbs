unit Cmdline;
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
** Commandline parser routines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 07-Mar-1999
** Last update : 07-Mar-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine(Full: Boolean);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, FileRout, ElLog_U, Exitprog, HelpUnit, Debug_U, Fastscrn,
     RemScrn, MultiLn, CfgRec, StrPath, WordStr, LongStr, User_U, ObjDec
       {$IFDEF WINGUI}
         ,Win_Main
       {$ENDIF};

{ Note: This function is called twice, once for the ovrerlay exceptions and }
{ stuff, and the 2nd time for the modem parameters and other info }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FVal (S : String) : LongInt;            { Convert string to Integer }
{$IFNDEF WIN32}
var Code  : Integer;
{$ELSE}
var Code  : LongInt;
{$ENDIF}
    Number: LongInt;
begin
  While (Length(S) > 0) AND (S[1] = #32) do Delete(S, 1, 1);
  While (Length(S) > 0) AND (S[1] = #32) do Delete(S, Length(S), 1);

  Val (S,Number,Code);
  If Code<>0 then
    Number:=0;
  FVal:=Number;
end; { func. FVal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  SUpCase (S : String) : String;                { UpCase for strings }
var Teller: Byte;
begin
  For Teller:=01 to Length(S) do
   S[Teller] := Upcase(S[Teller]);
  SUpcase := S;
end; { func. SUpcase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine(Full: Boolean);
var Counter : Byte;
    TempStr : String;
    TempPath: String;
begin
  For Counter := 01 to ParamCount do
   begin
      TempStr := System.ParamStr(Counter);

      {$IFDEF WITH_DEBUG}
        if Full then
         DebugObj.DebugLog(logString, 'ParseCommandLine: ' +System.ParamStr(Counter));
      {$ENDIF}

      if TempStr[1] in ['?'] then
       if Full then
        ShowHelpScreen;

      if NOT Full then
       if (NOT (System.UpCase(TempStr[2]) in ['?', 'H', 'N', 'M', 'O', 'S', 'X'])) then
         TempStr[2] := #00;                          { Invalidate switches }

      If TempStr[1] in ['/', '-'] then
        Case System.UpCase(TempStr[2]) of
          '?' : if Full then ShowHelpScreen;

          'A' : ; { Lan Adapter }

          'B' : begin
                    TempStr := Copy(TempStr, 3, 255);

                    LineCfg^.ConnectStr := TempStr;

                    If Pos('/', TempStr)>00 then
                      begin
                       if Copy(TempStr, 1, Pos('/', TempStr)-1) = '115200' then
                          Delete(TempStr, Pos('/', TempStr)-2, 1);

                       LineCfg^.Exitinfo^.Baud := SmallWord(FVal(Copy(TempStr, 1, Pos('/', TempStr)-1)));
                       LineCfg^.Exitinfo^.ErrorFreeConnect := Pos(LineCfg^.Modem^.ErrorFreeString,
                                                SUpcase(LineCfg^.ConnectStr))>00;
                      end
                         else begin
                                if TempStr = '115200' then
                                  Delete(TempStr, Length(TempStr) - 1, 1);
                                LineCfg^.Exitinfo^.Baud := SmallWord(FVal(TempStr));
                              end; { else }

                    if LineCfg^.Exitinfo^.Baud=0 then
                      LineCfg^.LocalLogon := true;
                end; { Logon at current rate }

          'C' : begin
                  LineCfg^.Modem^.Comport := FVal(Copy(TempStr, 3, 255));
                end; { C }

          'D' : begin
                 DefStatusLine := 10;

                 {$IFDEF WINGUI}
                   Form1.StatusBarBox.Checked := false;
                 {$ENDIF}
                end; { D }

          'E' : begin
                  DefExitCode := FVal(Copy(Tempstr, 3, 255));
                end;

          'F' : begin
                  if SUpCase(Copy(TempStr, 2, 4)) = 'FEND' then
                    LineCfg^.BatchAtExit := Copy(TempStr, Length('FEND') + 2, 255);
                end; { FrontEnd }

          'G' : LineCfg^.RelogMenu := True;                { Set return menu }

          {$IFDEF VirtualPascal}
          'H' : begin
                  if Full then
                    LineCfg^.InheritedHandle := FVal(Copy(TempStr, 3, 255));
                end;
          {$ENDIF}

          {$IFDEF WinGUI}
          'H' : begin
                  LineCfg^.InheritedHandle := FVal(Copy(TempStr, 3, 255));
                end;
          {$ENDIF}

          {$IFNDEF ELEUNIX}
          'L' : begin
                  LineCfg^.Exitinfo^.Baud := 00;
                  LineCfg^.LocalLogon := true;
                end; { Local }
          {$ENDIF}

          'M' : MailerCmdLine := Copy(TempStr, 3, 255); { Shell To Mailer }


          'N' : begin
                  If SUpcase(Copy(TempStr, 2, 255))='NOEMS' then
                      OvrUseEMS := False else

                   If SUpcase(Copy(TempStr, 2, 255))='NOXMS' then
                       OvrUseXMS := False else

                   if FVal(Copy(TempStr, 3, 255)) = -1 then
                     begin
                       if Full then
                         begin
                           LineCfg^.RaNodeNr := MultiLnObj^.EmptyNodeNr(false);
                           LogFileName := GetLogFileName;

                           TempPath := ForceBack(LineCfg^.Telnet^.NodeDirectories);
                           While Pos('*N', SUpcase(TempPath)) > 00 do
                              Replace('*N', FStr(LineCfg^.RaNodeNr), TempPath);

                           GetDir(0, OldProgramDir);
                           {$i-}
                             MkDir(NoBackSlash(TempPath));
                             if IoResult > 0 then ;
                             ChDir(NoBackSlash(TempPath));
                           {$i+}
                           if IOResult > 00 then ;

                           {-- because "local" files, all of a sudden --------}
                           {-- can be non-local, we force re-init of system --}
                           {-- files locations -------------------------------}
                           InitSystemNames;
                         end; { if }
                     end
                       else begin
                              if Full then
                                begin
                                  LineCfg^.RaNodenr := FVal(Copy(TempStr, 3, 255));
                                  LogFileName := GetLogFileName;
                                end; { if }
                            end; { else }
                end; { N }

{$IFDEF OVERLAY}
          'O' : begin
                  if SUpCase(Copy(TempStr, 2, Length('OVRSIZE'))) = 'OVRSIZE' then
                    ExtraOvrSize := FVal(Copy(TempStr, Length('-OVRSIZE')+1, 255));
                end; { O }
{$ENDIF}

          'P' : LineCfg^.PrinterLogging := True; { Log activity to printer }

          'R' : begin
                  LineCfg^.ReLogOnBBS := true;                  { Log user back online }

                  LocalScreenLn(SystemMsgPrefix + 'Logging user back on-line');
                  LineCfg^.Exitinfo^.Userinfo.Name := '';

                  if LineCfg^.Exitinfo<>nil then
                    begin
                      ReadExitinfo(LineCfg^.Exitinfo^);
                      ReadSysInfoBBS(LineCfg^.Exitinfo^.Sysinfo);
                      LineCfg^.Exitinfo^.UserRecord := SearchUser(LineCfg^.Exitinfo^.Userinfo.Name);

                      If LineCfg^.Exitinfo^.Baud = 00 then LineCfg^.LocalLogon := true;
                      if LineCfg^.Exitinfo^.Userinfo.Name='' then
                        begin
                          RaLog('!', 'EXITINFO.BBS missing!');
                          HangUp;
                        end; { if }
                    end; { if }
                end; { if ReLogOn }

          'S' : begin
                  If SUpcase(Copy(TempStr, 2, 255))='SMALLOVRBUF' then
                    OvrExtraMemory := False else
                      LineCfg^.Snooping := False;
                end; { S }
          'T' : begin                                { Set time to next event }
                  CmdEventSet := True;
                  NextEventMins := FVal(Copy(TempStr, 3, 255));
                end; { if }
          'X' : begin
                  Case System.UpCase(TempStr[3]) of
                      'D' : OpenComDelay := FVal(Copy(TempStr, 4, 255));
                      'C' : if Full then
                              LineCfg^.ComNoClose := true;
                      'M' : if Full then
                              LineCfg^.DoMonitor := true;
                      'P' : if RemScrnObj <> nil then
                              RemScrnObj^.remStartPort := FVal(Copy(TempStr, 4, 255));
                      'I' : if Full then
                              LineCfg^.TelnetFromIp := Copy(TempStr, 4, 255);
                      'T' : if Full Then
                            begin
                              TelnetServ := true;
                              LineCfg^.Modem^.Comport := 01;
                            end;
                  end; { case }
                end; { if 'X' }
        End; { case }
   end; { Grab Param }
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit CMDLINE }
