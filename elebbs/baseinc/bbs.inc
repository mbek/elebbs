{-------------------------- Set the program options ---------------------------}
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

 (*
 **
 ** Please note that some options below are exclusive. For example, you can
 ** never build an build that uses both USINGMGR and WITH_FULL as the screen
 ** output routines will conflict. Most programs have built-in checks that
 ** prevent this.
 **
 ** If you define RELEASE it will disable debug logging later on in this define
 ** file
 **
 ** Below is a chart that tells you how to set this for different programs:
 **
 **                 USINGMGR        WITH_FULL     ISCGI
 **   ELEBBS           -                x           -
 **   ELEXER           -                -           -
 **   ELCONFIG         x                -           -
 **   ELEFILE          x                -           -
 **   ELEUSER          x                -           -
 **   ELEMGR           x                -           -
 **   ELENODE          x                -           -
 **   ELEMON           x                -           -
 **   ELEWEB           -                -           x
 **
 *)

 {.$DEFINE USINGMGR}             { <-- building utility, eg ELCONFIG or ELEMGR }
 {$DEFINE WITH_FULL}                { <-- Define when building a full EleBBS }
 {.$DEFINE ISCGI}                           { <-- Define when building EleWEB }
 {.$DEFINE RELEASE}               { <-- Define when building a release version }


 {$DEFINE WITH_DEBUG}                      { <-- should include debuglogging? }
 {$DEFINE ELEBBS}                                    { A full EleBBS compile? }
 {$DEFINE LOCALSCRN}
 {.$DEFINE NEWPROT}   { Build without AsyncPro and with a new protocol driver }
{------------------------- End of program options ----------------------------}

{-- ELEXER options -----------------------------------------------------------}
 {$IFDEF FPC}
  {.$IFDEF Win32}
    {.$DEFINE elx_MYSQL}
  {.$ENDIF}
 {$ENDIF}
{-- ELEXER options (end) -----------------------------------------------------}

{-------------------------- Set the memory options ----------------------------}
 {$IFDEF WIN32}
   {$IFNDEF VirtualPascal}
    {$M 1024, 8900000}
   {$ELSE}
    {$M 205499, 755000}
   {$ENDIF}
 {$ENDIF}

 {$IFDEF MSDOS}
    {$M 60000, 0, 455000}
 {$ENDIF}

 {$IFDEF OS2}
    {$M 205499, 755000}
 {$ENDIF}

 {$IFDEF LINUX}
    {$M 205499, 755000}
 {$ENDIF}

 {$IFDEF FreeBSD}
    {$M 205499, 755000}
 {$ENDIF}

 {$IFDEF GO32V2}
    {$M 105499, 755000}
 {$ENDIF}
{-------------------------- End of the memory options -------------------------}


{----------------------- Will this be a 32-bit program? -----------------------}
  {$IFNDEF MSDOS}
    {$DEFINE USE32BIT}
  {$ELSE}
    {$UNDEF USE32BIT}
  {$ENDIF}
{--------------------- End of will this be 32-bit program? --------------------}




{------------- Determine wether we use DELPHI or VirtualPascal/FPC etc --------}
 {$IFDEF VER140}
   {$DEFINE DELPHI}
   {$DEFINE WINGUI}
   {$REALCOMPATIBILITY ON}
 {$ENDIF}
{-------------------------- End of compiler determination ---------------------}



{------------------------ Set communications settings -------------------------}
 {$DEFINE DOSCOM}
{------------------------ End of communications settings ----------------------}


{----------------------------- Set internet options ---------------------------}
{$DEFINE TCPIP}

{$IFDEF MSDOS}
  {$UNDEF TCPIP}
{$ENDIF}

{$IFDEF GO32V2}
  {$UNDEF TCPIP}
{$ENDIF}

{$IFDEF FREEBSD}
  {$UNDEF TCPIP}
  {$UNDEF DOLIBC}

  {$DEFINE TCPIP}
  {$DEFINE DOLIBC}
{$ENDIF}

{$IFDEF LINUX}
  {$UNDEF TCPIP}
  {$UNDEF DOLIBC}

  {$DEFINE TCPIP}
  {$DEFINE DOLIBC}
{$ENDIF}

{--------------------------- Set the MKMSG options ----------------------------}
 {$DEFINE WITH_HUDSON}
 {$DEFINE WITH_JAM}
 {$DEFINE WITH_SQUISH}
 {$UNDEF WITH_EZYCOM}
 {$UNDEF WITH_FIDO}
{------------------------- End of the MKMSG options ---------------------------}



{---------------------------- Set the compiler options ------------------------}
 {$IFDEF LINUX}
   {$DEFINE ELEUNIX}
 {$ENDIF}

 {$IFDEF FreeBSD}
   {$DEFINE ELEUNIX}
 {$ENDIF}

 {$IFDEF UNIX}
   {$UNDEF LOCALSCRN}
   {$DEFINE NOLINLOCAL}
 {$ENDIF}

 {$IFDEF VirtualPascal}
   {&Use32+}
   {$IFDEF WITH_DEBUG}
     {$LocInfo+}
   {$ENDIF}
 {$ENDIF}

 {$IFNDEF MSDOS}
   {$UNDEF OVERLAY}
 {$ELSE}
   {$DEFINE OVERLAY}
 {$ENDIF}

 {$IFDEF FPC}
   {$PACKRECORDS 1}
 {$ENDIF}

 {$I+}                                           { Turn on I/O error checking }
 {$Q-}                                           { Turn off overflow checking }
 {$R+}                                               { Turn on range checking }
 {$S+}                                               { Turn on stack checking }
 {$Y+}                              { Turn on symbol information (units only) }
 {$T-}                                            { Generate untyped pointers }
 {$V-}                                    { Turn off strict variable checking }
 {$X+}                                          { Turn on the extended syntax }

 {$IFNDEF VER0_99_11}
  {$IFNDEF FPC}
   {$L+}                                          { Turn on local-debug symbols }
  {$ENDIF}
 {$ENDIF}

 {$IFNDEF FPC}
   {$A+}                                             { Turn on word alignment }
   {$B-}                       { Turn on full boolean evaluation (important!) }
   {$F+}                              { Enable FAR calls (needed for overlay) }
   {$O+}                                   { Turn on overlaying for this unit }
 {$ENDIF}
 {$D+}                                            { Turn on debug information }
 {$IFNDEF VER0_99_11}
   {$P+}                           { All strings are treated as "OpenStrings" }
 {$ENDIF}

 {$IFDEF RELEASE}
   {$UNDEF WITH_DEBUG}
   {$D-}                                         { Turn debugging symbols off }
   {$L-}                                   { Turn local debugging symbols off }
   {$Q-}                                               { No overflow checking }
   {$S-}                                            { Turn Stack checking off }
   {$R-}                                            { Turn Range checking off }
 {$ENDIF}

 {$IFDEF DELPHI}
   {$A-}            { Turn off aligned record fields (creates incomatibility) }
   {$H-}                                              { Turn off long-strings }
   {$J+}                                    { Turn on writable const declares }

   {$DEFINE DELCRT}
 {$ENDIF}

 {$IFDEF FPC}
   {$H-}
   {$Y-}                                              { else causes 200306063 }
   {$LongStrings-}

	{$IFDEF VER1_9_5}
	 	{ seems like a compiler bug with regards to range checking in this beta release }
	 	{ we disable range checking for now }
	 	{$R-}
	{$ENDIF}
 {$ENDIF}

{-------------------------- End of the compiler options -----------------------}



(*
  TurboPascal v7.01, compiler switches
  A+ Word alignment       I+ I/O error checking   R- Range checking
  B- Full boolean eval    L+ Local debug symbols  S+ Stack checking
  D+ Debug information    N- 80x87 instructions   T- Typed pointers
  E+ 80x87 emulation      O- Overlays allowed     V+ Strict var-strings
  F- Force FAR calls      P- Open string params   X+ Extended syntax
  G- 80286 instructions   Q- Overflow checking
*)

(*
  DELPHI v1.00, compiler switches
  A+ Word alignment       L+ Local debug symbols  T- Typed pointers
  B- Full boolean eval    M- Runtime type info    U+ Pentium safe FDIV
  D+ Debug information    N+ 80x87 instructions   V+ Strict var-strings
  E+ 80x87 emulation      O- Overlays allowed     W- Windows stack frames
  F- Force FAR calls      P+ Open string params   X+ Extended syntax
  G+ 80286 instructions   Q- Overflow checking    Y+ Symbol reference info
 .I+ I/O error checking   R- Range checking       Z- Word-sized enums
  K+ Smart callbacks      S+ Stack checking
*)
