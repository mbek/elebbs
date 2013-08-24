program NWSFILT;
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
** EleBBS EleXer source.
** Filter for EleNEWS.
**
** Created: 24-may-2001
** Last update: 24-may-2001
** Written by: Maarten Bekers
**
*)

type ArticleRecord = record
                       FromWho : String;
                       Subject : String;
                       MsgSize : Integer[4];
                       MsgLines: Integer[4];
                       Headers : Array[1..50] of String;
                       HdrLines: Integer[4];
                     end; { ArticleRecord }

var ArticleInf: ArticleRecord;
begin
  {-- now get the header as read by EleNEWS ----------------------------------}
  GetNewsArticleHeader(ArticleInf);

  {-- we can add checking here. if our return value is NO the message wont ---}
  {-- be downloaded any further ----------------------------------------------}
   {  ...  }

  {-- we accept all messages by default --------------------------------------}
  SetResultValue('YES');
end. { nwsfilt }
