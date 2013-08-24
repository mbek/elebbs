unit objdec;
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
** Object declarations for EleBBS
**
** Copyright (c) 2001 by Maarten Bekers
**
** Created : 13-Oct-2001
** Last update : 13-Oct-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
      Terminal,                                               { pTerminalObj }
      MultiLn,                                                { pMultiLnObj }
      RAL,                                                   { pLanguageObj }
      TagObj,                                                 { pTaggingObj }
      RemScrn,                                             { pRemoteScrnObj }
      ReadMsg                                                 { pReadMsgObj }

{$IFDEF WITH_FULL},
       Avatar,                                               { pAvatarObject }
       Control,                                             { pControlObject }
       InOut_U,                                                 { pOutputObj }
       Input_U,                                                  { pInputObj }
       Outblock,                                              { pOutblockObj }
       Wfc,                                                        { pWfcObj }
       EditUser,                                              { pUserEditObj }
       Chat                                                    { pChatterObj }

       {$IFDEF TCPIP}
        ,IrcUnit,                                         { pIrcInterfaceObj }
        TelUnit                                           { pTelInterfaceObj }
       {$ENDIF}

       {$IFDEF ELEUNIX}
        ,SigBBS                                             { SIGHUP handler }
       {$ENDIF}

{$ENDIF};

var
  termObj    : pTerminalObj;                 { Terminal interpreter routines }
  LangObj    : pLanguageObj;                                  { RAL routines }
  MultiLnObj : pMultiLnObj;                            { Multi-line routines }
  TaggingObj : pTagFileObj;                   { Tagfiles (download) routines }
  RemScrnObj : pRemoteScrnObj;              { Remote screen (EleMON routines }
  ReadMsgObj : pReadMsgObj;                               { Reading messages }

  {$IFDEF WITH_FULL}
   avtObj     : pAvatarObj;                           { ANSi/Avatar routines }
   contrObj   : pControlObj;                          { Timing/DCD/etc check }
   OutputObj  : pOutputObj;                                { Output routines }
   InputObj   : pInputObj;                                  { Input routines }
   OutBlockObj: pOutblockObj;                      { Output (in blocks) unit }
   WfcObj     : pWfcObj;                                { Waiting For Caller }
   UserEditObj: pUserEditObj;                            { Usereditor object }
   ChatObj    : pChatterObj;                                { Chatter object }
   {$IFDEF TCPIP}
    IrcIntObj  : pIrcInterfaceObj;                      { IRC interface unit }
    TelIntObj  : pTelInterfaceObj;                   { TELNET interface unit }
   {$ENDIF}

   {$IFDEF ELEUNIX}
     sigObj     : pSignalObj;                        { SIGHUP handler (unix) }
   {$ENDIF}
  {$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
