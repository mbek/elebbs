unit Helpunit;
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
** HelpScreen routines for EleBBS
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

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

procedure ShowHelpScreen;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, Crt{$IFDEF WINGUI},  Forms, HelpScreenUnit{$ENDIF};

procedure ShowHelpScreen;
var CH: Char;
begin
  {$IFNDEF WINGUI}
    Crt.TextAttr := 7;
    Crt.ClrScr;
    WriteLn(CrtOutput, SystemMsgPrefix, PidName, ' (', MaxNodes, ' node version) - Command line parameters');
    WriteLn(CrtOutput);
    WriteLn(CrtOutput, '-Bxxxx                 - Log on to EleBBS using ''''xxxx'''' bps (eg: /B2400)');
    WriteLn(CrtOutput, '-Cxx                   - Set the comport to use with EleBBS');
    WriteLn(CrtOutput, '-E                     - Specifies the default exit error-level');
    WriteLn(CrtOutput, '-G                     - Re-Logon to the last menu instead of the TOP');
    WriteLn(CrtOutput, '-L                     - Local Logon, same as /B0');
    WriteLn(CrtOutput, '-M<....>               - Specifies mailer-frontend.');
    WriteLn(CrtOutput, '-Nxxx                  - Nodenumber to logon to (eg: /N250)');
    WriteLn(CrtOutput, '-R                     - Re-Logon the user.');
    WriteLn(CrtOutput, '-S                     - Disables screen-output (disables snooping)');
    WriteLn(CrtOutput, '-T<....>               - Set time till the next event.');
    WriteLn(CrtOutput, '-XC                    - Don''t close the communications port upon exit of');
    WriteLn(CrtOutput, '                         EleBBS');
    WriteLn(CrtOutput, '-XM                    - Make EleBBS available for EleMON (see docu)');
    WriteLn(CrtOutput);

    CH := UpCase(ReadKey);
    if CH = 'Q' then Halt(255);
    if CH = #00 then ReadKey;

    WriteLn(CrtOutput);
    WriteLn(CrtOutput, 'DOS specific command lines');
    WriteLn(CrtOutput, '==========================');
    WriteLn(CrtOutput, '-D                     - Disables the statusline');
    WriteLn(CrtOutput, '-NOEMS                 - Instructs EleBBS not to use EMS for overlay');
    WriteLn(CrtOutput, '-NOXMS                 - Instructs EleBBS not to use XMS for overlay');
    WriteLn(CrtOutput, '-Oxxx                  - Specifies the overlay size which is extra');
    WriteLn(CrtOutput, '-SMALLOVRBUF           - Uses a small overlay buffer (less memory)');
    WriteLn(CrtOutput);

    CH := UpCase(ReadKey);
    if CH = 'Q' then Halt(255);
    if CH = #00 then ReadKey;

    WriteLn(CrtOutput);
    WriteLn(CrtOutput);
    WriteLn(CrtOutput, 'OS/2 specific command lines');
    WriteLn(CrtOutput, '===========================');
    WriteLn(CrtOutput, '-Hxxxx                 - Retrieves handle passed from front-end');
    WriteLn(CrtOutput, '-NORAW                 - Doesn''t set the keyboard mode to binary for ELCONFIG.');
    WriteLn(CrtOutput);
    WriteLn(CrtOutput);

    CH := UpCase(ReadKey);
    if CH = 'Q' then Halt(255);
    if CH = #00 then ReadKey;

    WriteLn(CrtOutput);
    WriteLn(CrtOutput, 'Windows9x/NT-based specific command lines');
    WriteLn(CrtOutput, '======================================');
    WriteLn(CrtOutput, '-Hxxxx                 - Retrieves handle passed from front-end');
    WriteLn(CrtOutput, '-FEND                  - Specifies a batch-file to execute after the session');
    WriteLn(CrtOutput, '                         has been terminated. This can be used for executing');
    WriteLn(CrtOutput, '                         your front-end mailer again.');
    WriteLn(CrtOutput);

    CH := UpCase(ReadKey);
    if CH = 'Q' then Halt(255);
    if CH = #00 then ReadKey;
    Halt(255);
  {$ELSE}
    With HelpScreenForm do
      begin
        PageControl1.ActivePage := TabSheet1;

        {--------------------------- General part -----------------------------}
        Label2.Caption  := '-Bxxxx';
        Label3.Caption  := 'Log on to EleBBS using ''''xxxx'''' bps (eg: /B2400)';

        Label4.Caption  := '-Cxx';
        Label5.Caption  := 'Set the comport to use with EleBBS';

        Label6.Caption  := '-E';
        Label7.Caption  := 'Specifies the default exit error-level';

        Label8.Caption  := '-G';
        Label9.Caption  := 'Re-Logon to the last menu instead of the TOP';

        Label10.Caption  := '-L';
        Label11.Caption := 'LocalLogon, same as /B0';

        Label12.Caption := '-M<....>';
        Label13.Caption := 'Specifies mailer-frontend.';

        Label14.Caption := '-Nxxx';
        Label15.Caption := 'Nodenumber to logon to (eg: /N250)';

        Label16.Caption := '-R';
        Label17.Caption := 'Re-Logon the user.';

        Label18.Caption := '-S';
        Label19.Caption := 'Disables screen-output (disables snooping)';

        Label20.Caption := '-T<....>';
        Label21.Caption := 'Set time till the next event.';

        Label22.Caption := '-XC';
        Label23.Caption := 'Don''t close the communications port upon exit of EleBBS';


        {--------------------------- DOS specific -----------------------------}
        Label24.Caption := '-D';
        Label25.Caption := 'Disables the statusline';

        Label26.Caption := '-NOEMS';
        Label27.Caption := 'Instructs EleBBS not to use EMS for overlay';

        Label28.Caption := '-NOXMS';
        Label29.Caption := 'Instructs EleBBS not to use XMS for overlay';

        Label30.Caption := '-Oxxx';
        Label31.Caption := 'Specifies the overlay size which is extra';

        Label32.Caption := '-SMALLOVRBUF';
        Label33.Caption := 'Uses a small overlay buffer (less memory)';

        {-------------------------- OS/2 specific -----------------------------}
        Label34.Caption := '-H';
        Label35.Caption := 'Retrieves handle passed from front-end';

        Label36.Caption := '-NORAW';
        Label37.Caption := 'Doesn''t set the keyboard mode to binary for ELCONFIG';

        {-------------------------- Win32 specific ----------------------------}
        Label38.Caption := '-H';
        Label39.Caption := 'Retrieves handle passed from front-end';

        Label40.Caption := '-FEND';
        Label41.Caption := 'Specifies a batch file to execute after the session';
        Label42.Caption := 'has been terminated. This can be used for executing';
        Label43.Caption := 'your front-end mailer again.';
     end; { with }

    HelpScreenForm.ShowModal;

    Application.Terminate;
    ProgTerminated := true;
  {$ENDIF}
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit HELPUNIT }
