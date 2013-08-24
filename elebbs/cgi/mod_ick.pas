library mod_ick;
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

{$IFNDEF ISCGI}
  Wont compile!!
{$ENDIF}
{$R+,S+,W+}
{&LocInfo+}

(*
**
** EleWEB Apache main executable
**
** Copyright (c) 2001-2002 by Maarten Bekers
**
** Created: 21-Apr-2001
** Last update: 21-Jul-2002
**
**
*)
uses
     dos,                                    { environment variable handling }
     web_apache1,                                   { CGI handling routines }
      apache1_base,
       ap1_base,
        web_gen,
       web_glob,                                               { global data }
        web_sup,                               { Support routines for EleWEB }
         web_scr,                                       { Scripting routines }
          longStr,                     { Converting strings to ints and v.v. }
           user_u,                          { User search routines and alike }
            filerout,                                { EleBBS-files routines }
             global,                                      { Global variables }
              crc_Unit,                                     { CRC32 routines }
               multiln,                                { Free nodenumber etc }
                jdates,                              { Date and age routines }
                 terminal,                             { System / user codes }
                  CfgRec,                            { Data type definitions }
                   objDec;                            { several object decls }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const oo = 12;

{$IFDEF ELEUNIX}
procedure pascalmain; cdecl;
begin
  writeln('wheeee');
end;
{$ENDIF}

exports
  apache_init{ index 3},
  apache_module{ index 1} name 'ick_module';


begin
  {-- let the system know were running EleWEB ------------------------------}
  RunningWEB := TRUE;

  {-- First initialize some variables --------------------------------------}
  web_Access := FALSE;

  {-- and skip along -------------------------------------------------------}
  web_InitMemory;                         { Allocate memory for the globals }
  web_RunLocal := false;

  web_Initted := web_InitSystemFiles(NOT web_RunLocal); { read all the system files }
end. { EleWEB }
