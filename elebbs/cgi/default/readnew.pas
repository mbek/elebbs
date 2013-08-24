program readmsg;
(*
**
** EleWEB EleXer source
** EleBBS message reader and writer.
** It's recommended to only use JAM messagebases (as threading is something
** a Hudson message base doesn't know)
**
** Created: 24-may-2001
** Last update: 08-Aug-2002
** Written by: Maarten Bekers
**
** Todo:
**   - Downloading of attachments
**   - Uploading of attachment
**   - Read receipt (writing) (reason: CONFIG.RA is unknown!)
**   - Messages to "sysop" are not handled (reason: RACONFIG is unknown!)
**   - Replying to internet gateway messages using the FSC specs is not
**     working correctly.
**
*)

(*
**  Things that should be added to EleWEB some time:
**  - [url][img] codes in quotes should be removed
**  - there should be a [code] tag
**  - Reply only by topic starter and moderators
**  - Reply only, except to those with sysop access
**  - Voting booth using graphics
**  - Remove the mixed use of POST and GET in the HTML templates
**
**)

  {-- Include standard type definitions -------------------------------------}
  {$I stdrec.inc}

  {-- Include area records --------------------------------------------------}
  {$i arearec.inc}

  {-- Special index records for the forum style. ----------------------------}
  {$I msgidx.inc}

  {-- Special UBB code parser -----------------------------------------------}
  {$I ubbrec.inc}

  {-- User extension record -------------------------------------------------}
  {$I usrrec.inc}

  {-- Message thread sorting records ----------------------------------------}
  {$I srtrec.inc}

  {-- Web configuration record ----------------------------------------------}
  {$I cfgrec.inc}

const
  maxCacheFiles = 15;
  

const
  html_QrdData_Tpl   = 'qrddata.htm';
  html_QrdFlonl_Tpl  = 'qrdflonl.htm';
  html_QrdFlon_Tpl   = 'qrdflon.htm';
  html_QrdFlofl_Tpl  = 'qrdflofl.htm';
  html_QrdFloff_tpl  = 'qrdfloff.htm';
  html_qrdhead_tpl   = 'qrdhead.htm';
  html_qrdhd2_tpl    = 'qrdhd2.htm';
  html_qrdfood_tpl   = 'qrdfoot.htm';
  html_qrdend_tpl    = 'qrdend.htm';
  html_qrdft2_tpl    = 'qrdft2.htm';
  html_qrdend2_tpl   = 'qrdend2.htm';
  html_stfl_cmn_tpl  = 'stfl_cmn.htm';
  html_trdhead_tpl   = 'trdhead.htm';
  html_trdfoot_tpl   = 'trdfoot.htm';
  html_trdend_tpl    = 'trdend.htm';
  html_stor_dt_tpl   = 'stor_dt.htm';
  html_stfl_dt_tpl   = 'stfl_dt.htm';
  html_stor_hd_tpl   = 'stor_hd.htm';
  html_stor_f2_tpl   = 'stor_f2.htm';
  html_stor_ft_tpl   = 'stor_ft.htm';
  html_rdhead1_tpl   = 'rdhead1.htm';
  html_rdhead2_tpl   = 'rdhead2.htm';
  html_rdhead3_tpl   = 'rdhead3.htm';
  html_rdhead4_tpl   = 'rdhead4.htm';
  html_rdhead5_tpl   = 'rdhead5.htm';
  html_rdhead6_tpl   = 'rdhead6.htm';
  html_rdhead7_tpl   = 'rdhead7.htm';
  html_rdhead99_tpl  = 'rdhead99.htm';
  html_rdtextn_tpl   = 'rdtextn.htm';
  html_rdtextq_tpl   = 'rdtextq.htm';
  html_rdtextk_tpl   = 'rdtextk.htm';
  html_rdtextt_tpl   = 'rdtextt.htm';
  html_rdfoot_tpl    = 'rdfoot.htm';
  html_rderror_tpl   = 'rderror.htm';
  html_rd_flhd_tpl   = 'rd_flhd.htm';
  html_rd_flhd1_tpl  = 'rd_flhd1.htm';
  html_rd_Flhd2_tpl  = 'rd_flhd2.htm';
  html_rdfltxth_tpl  = 'rdfltxth.htm';
  html_rdfltxta_tpl  = 'rdfltxta.htm';
  html_rdfltxt1_tpl  = 'rdfltxt1.htm';
  html_rd_flft1_tpl  = 'rd_flft1.htm';
  html_rdfltxt2_tpl  = 'rdfltxt2.htm';
  html_rd_flft2_tpl  = 'rd_flft2.htm';
  html_rd_flft_tpl   = 'rd_ftft.htm';
  html_stfl_hd_tpl   = 'stfl_hd.htm';
  html_stfl_f2_tpl   = 'stfl_f2.htm';
  html_stfl_ft_tpl   = 'stfl_ft.htm';
  html_delerr2_tpl   = 'delerr2.htm';
  html_delsucc_tpl   = 'delsucc.htm';
  html_delerror_tpl  = 'delerror.htm';
  html_wmsg_one_tpl  = 'wmsg_one.htm';
  html_wmsg_two_tpl  = 'wmsg_two.htm';
  html_wmsg_tre_tpl  = 'wmsg_tre.htm';
  html_wmsg_for_tpl  = 'wmsg_for.htm';
  html_wmsg_fiv_tpl  = 'wmsg_fiv.htm';
  html_wmsg_six_tpl  = 'wmsg_six.htm';
  html_werror1_tpl   = 'werror1.htm';
  html_werror2_tpl   = 'werror2.htm';
  html_werror3_tpl   = 'werror3.htm';
  html_werror4_tpl   = 'werror4.htm';
  html_werror5_tpl   = 'werror5.htm';
  html_werror6_tpl   = 'werror6.htm';
  html_werror7_tpl   = 'werror7.htm';
  html_werror8_tpl   = 'werror8.htm';
  html_werror9_tpl   = 'werror9.htm';
  html_w2succ_tpl    = 'w2succ.htm';
  html_wsucc_tpl     = 'wsucc.htm';
  
var MessageInf  : MessageRecord;
    StartMsgNum : Integer;
    SubAction   : Integer;
    MsgTxtLine  : String;        { Contains the latest string of message text }
    ShowKludges : Boolean;
    SysopAcc    : Boolean;      { Do we have SysOpAccess to this message area }
    ThrdCount   : Integer;    { Number of messages listed in thread list mode }
    AreaNum     : Integer;                               { Areanumber to read }
    IsForum     : Boolean;         { The area we are currently in, is a forum }
    IsFrontPage : Boolean;                           { Is this the frontpage? }
    GuestAccess : Boolean;                       { user we see now is a guest }
    DaysOld     : Integer;          { Dont show messages older than 'xx' days }

    webCfgInf   : webCfgRecord;

    TopicInf    : TopicIdxRecord;
    TopicHdr    : TopicHdrRecord;

    StoryShowAll: Boolean;   { Show the full story, or only the first lines? }
    usrExtra    : usrExtraInfo;
    LoginUnix   : Integer[4];       { Unix timestamp of last login time/date }

    MsgNumberWritten : Integer;                             { Message number }

    reply_ReplyAddr  : String;
    reply_ReplyTo    : String;
    reply_Address    : String;
    reply_ReplyKludge: String;

    timing_StartTime : Real;                             { Total module timer }
    timing_Finished  : Real;

    timing_ForumStart: Real;                            { Forum lister module }
    timing_ForumEnd  : Real;

    CodeTable        : Array[1..24] of String;
    ValueTable       : Array[1..24] of String;

    timing_TplTotal  : Real;                { Total time to display each item }
    timing_ReadTotal : Real;           { Total time spend in accessing the MB }
    timing_IdxTotal  : Real;                { Total time spend in the indexes }
    timing_McrTotal  : Real;           { Total time spend in macro processing }

    cache_Record     : array[1..maxCacheFiles] of
                                       record
                                         FName     : String;
                                         Data      : String;
                                       end; { record }

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}


procedure ShowError(TmpStr: String);
begin
  WriteLn;
  WriteLn;
  WriteLn('<HTML> <BODY> <B> <H1>');
  WriteLn('Unable to display file!<BR><BR><BR>');
  WriteLn(TmpStr, '<BR><BR><BR>');
  WriteLn('</H> </B> </BODY> </HTML>');
end; { proc. ShowError }

{-- Include some basic date/time routines ------------------------------------}
{$I datefnc.inc}

{-- Include some of the IDX support routines ---------------------------------}
{$I idxsup.inc}

{-- Include web configuration record functions -------------------------------}
{$i cfgfnc.inc}

{-- Include timing routines --------------------------------------------------}
{$I timefnc.inc}

{-- Include UBB parser codes -------------------------------------------------}
{$I ubbparse.inc}

{-- Include user extension routines ------------------------------------------}
{$i usrfunc.inc}

{-- Include thread sorting functions -----------------------------------------}
{$I srtfunc.inc}


function OpenMessageBase(AreaNum: Integer): Boolean;
var Success: Boolean;
    TmpFlag: record
               Flags: FlagType;
             end; { record }
begin
  {-- we default to success --------------------------------------------------}
  Success := TRUE;

  {-- First get the area number we want --------------------------------------}
  if NOT GetMessageRecord(MessageInf,
                          AreaNum,
                          FALSE) then ;                     { Check RDX files }

  {-- make sure we know wether we have sysop access --------------------------}
  SysopAcc := CheckMsgSysopAccess(MessageInf);
  if GuestAccess then
    SysopAcc := FALSE;

  {-- IsForum ----------------------------------------------------------------}
  IsForum := (MessageInf.Typ = 5);

  {-- IsFrontPage ------------------------------------------------------------}
  if IsForum then
    begin
      IsFrontPage := (MessageInf.AreaNum = webCfgInf.FrontPageNum);
    end
      else IsFrontPage := false;

  {-- Make sure we have access to this area ----------------------------------}
  if NOT CheckMsgAreaAccess(Messageinf,
                            FALSE,                            { Message group }
                            TRUE,                               { Read access }
                            0) then                    { Message group number }
     begin
       Success := FALSE;
     end; { if }

  {-- open the messagebase ---------------------------------------------------}
  if NOT mb_OpenMsgBase(MessageInf) then
    begin
      Success := FALSE;
    end; { if }

  {-- and feed back the result -----------------------------------------------}
  OpenMessageBase := Success;
end; { func. OpenMessageBase }


function GetAttributesStr: String;
var Tmpstr: String;
begin
  {-- Build the string -------------------------------------------------------}
  TmpStr := '&nbsp;';

  if mb_GetPrivate then
    TmpStr := TmpStr + ' (Private)';

  if mb_GetReturnReceipt then
    TmpStr := TmpStr + ' (RetRcpt)';

  if mb_GetMarkAsReceived then
    TmpStr := Tmpstr + ' (Rcvd)';

  {-- and set the result -----------------------------------------------------}
  GetAttributesStr := TmpStr;
end; { func. GetAttributesStr }

function ClassifyMsgText(TmpStr: String): Integer;
var MsgId  : Integer;
    TmpVal : Integer;
begin
  {-- We classify our message text in 4 sorts of text -----------------------}
  {-- 00 - Normal text ------------------------------------------------------}
  {-- 01 - Quoted text ------------------------------------------------------}
  {-- 02 - Kludges ----------------------------------------------------------}
  {-- 03 - Tearlines --------------------------------------------------------}
  {-- 04 - [code] -----------------------------------------------------------}
  MsgId := 0;                                       { Default to normal text }
  TmpVal := Pos('>', TmpStr);
  if (TmpVal > 0) AND (TmpVal < 15) then
    MsgId := 1;                                                { Quoted text }

  if Length(TmpStr) > 1 then
   if Copy(TmpStr, 1,1) = #1 then                                  { Kludges }
      MsgId := 2;

  if (Copy(TrimLeft(TmpStr), 1, 4) = '--- ') OR                   { Tearline }
      (Copy(TmpStr, 1, 10) = ' * Origin:') then
        MsgId := 3;

  {-- and return the message type -------------------------------------------}
  ClassifyMsgText := MsgId;
end; { func. ClassifyMsgText }


procedure DisplayMessageText(var EndStr: String);
begin
  {-- now get the text buffer -----------------------------------------------}
  EndStr := mb_GetMessageLine(-1);

  {-- parse for UBB codes and the likes -------------------------------------}
  EndStr := UbbParse(EndStr, mb_GetFromWho);
  FixSmileys(EndStr);
end; { proc. DisplayMessageText }


procedure DisplayMessageLine(var TmpStr: String);
begin
  {-- if the string is empty, at least show something -----------------------}
  if TmpStr = '' then
    TmpStr := #32;

  {-- and display the converted string --------------------------------------}
  TmpStr := UbbParse(TmpStr, mb_GetFromWho);

  {-- and save the time -----------------------------------------------------}
  FixSmileys(TmpStr);
end; { proc. DisplayMessageLine }


procedure DisplayReplyText(var EndStr: String);
var Count   : Integer;
    AddStr  : String;
    MsgTyp  : Integer;
    InQuote : Boolean;
    TmpPos  : Integer;
    MsgText : String;
begin
  {-- Create an add-string --------------------------------------------------}
  EndStr := '';
  AddStr := InitialString(mb_GetFromWho) + '> ';
  InQuote := false;

  {-- First write a nice header ---------------------------------------------}
  if NOT IsForum then
    begin
      Concatenate(EndStr, '* In a messge originally to ' + mb_GetToWho + ', ' + mb_GetFromWho + ' said: ');
      Concatenate(EndStr, #13 + #10 + #13 + #10);
    end
      else begin
             if NOT IsFrontPage then
               begin
                 Concatenate(EndStr, '[quote]');
                 Concatenate(EndStr, '[b]' + mb_GetFromWho + ' wrote at ' +
                    FormatDateString(mb_GetDateStr, 8, 3) + ', ' + mb_GetTimeStr + ': [/b]' + #13 + #10);
               end; { if }
           end; { else }

  {-- now loop through the text ---------------------------------------------}
  while (NOT mb_EndOfMessage) do
    begin
      {-- Get the next line of message text ---------------------------------}
      MsgText := mb_GetMessageLine(250);

      {-- make sure we dont quote kludges or tearlines e.d. -----------------}
      MsgTyp := ClassifyMsgText(MsgText);

      {-- see if we are not quoting -----------------------------------------}
      {-- we cannot quote in quote, it'd end up a mess ----------------------}
      if NOT InQuote then
        begin
          TmpPos := Pos('[quote]', MsgText);
          if TmpPos > 0 then
            begin
              {-- set the inquote marker ------------------------------------}
              InQuote := true;

              {-- add this text already to the quoted text ------------------}
              Concatenate(EndStr, Copy(MsgText, 1, TmpPos - 1));

              {-- and add some txt ------------------------------------------}
              Concatenate(EndStr, #13 + #10 + '[...]' + #13 + #10 + #13 + #10);

              {-- delete the rest of the line -------------------------------}
              Delete(MsgText, TmpPos, Length('[quote]'));
            end; { if }
        end; { if }

      if InQuote then
        begin
          {-- perhaps we can end the quote ----------------------------------}
          TmpPos := Pos('[/quote]', MsgText);

          if TmpPos > 0 then
            begin
              Delete(MsgText, 1, TmpPos + Length('[/quote]'));

              {-- disable the inquote marker --------------------------------}
              InQuote := false;
            end
              else MsgTyp := 12;
        end; { else }

      {-- if this a kludge we need to handle it -----------------------------}
      if MsgTyp = 2 then
        begin
          if SUpCase(Copy(MsgText, 2, Length('REPLYTO'))) = 'REPLYTO' then
            begin
              reply_ReplyTo := Trim(Copy(MsgText, Length('REPLYTO') + 02, 255));
            end; { replyto }

          if SUpCase(Copy(MsgText, 2, Length('REPLYADDR'))) = 'REPLYADDR' then
            begin
              reply_ReplyAddr := Trim(Copy(MsgText, Length('REPLYADDR') + 02, 255));
            end; { replyaddr }

         if SUpCase(Copy(MsgText, 2, Length('MSGID:'))) = 'MSGID:' then
           begin
             reply_ReplyKludge := Copy(MsgText, Length(#1 + 'MSGID:') + 2, 255);
           end; { replykludge }

          if Copy(MsgText, 1, 1) = #01 then
           if SUpCase(Copy(MsgText, 2, Length('MSGID:'))) = 'MSGID:' then
             begin
               reply_Address := Trim(Copy(MsgText, Length('MSGID:') + 02, 255));

               if Pos('@', reply_Address) > 00 then
                 Delete(reply_Address, Pos('@', reply_Address), 255);

               if Pos(#32, reply_Address) > 00 then
                 Delete(reply_Address, Pos(#32, reply_Address), 255);
             end; { if }

        end; { if }

      {-- and display it ----------------------------------------------------}
      if ((MsgTyp = 0) OR (MsgTyp = 1)) then
        begin
          if IsForum then
            Concatenate(EndStr, MsgText + #13 + #10)
              else Concatenate(EndStr, AddStr + MsgText + #13 + #10);
        end; { if }
    end; { while }

  if IsForum then
    Concatenate(EndStr, '[/quote]' + #13 + #10);
end; { proc. DisplayReplyText }


procedure DisplayStoryExcerpt(var EndStr: String);
var TmpPos: Integer;
begin
  {-- now get the text buffer -----------------------------------------------}
  EndStr := mb_GetMessageLine(-1);

  {-- now  we cut off after the first enter ---------------------------------}
  {-- enters get translated to <BR>, so find it, and delete the rest --------}
  TmpPos := Pos(#13 + #13, EndStr);
  if (TmpPos > 0) then
    begin
      Delete(EndStr, TmpPos, Length(EndStr));
    end; { if }

  {-- parse for UBB codes and the likes -------------------------------------}
  EndStr := UbbParse(EndStr, mb_GetFromWho);
end; { proc. DisplayStoryExcerpt }


procedure DisplayEditText(var EndStr: String);
var MsgTyp  : Integer;
    MsgText : String;
begin
  {-- now loop through the text ---------------------------------------------}
  while (NOT mb_EndOfMessage) do
    begin
      {-- Get the next line of message text ---------------------------------}
      MsgText := mb_GetMessageLine(250);

      {-- make sure we dont quote kludges or tearlines e.d. -----------------}
      MsgTyp := ClassifyMsgText(MsgText);

      {-- and display it ----------------------------------------------------}
      if (MsgTyp = 0) OR (MsgTyp = 1) then
        Concatenate(EndStr, MsgText + #13);
    end; { while }
end; { proc. DisplayEditText }


procedure DisplayCommentText(var MsgText: String);
begin
  {-- now get the text buffer -----------------------------------------------}
  MsgText := mb_GetMessageLine(-1);

  {-- parse for UBB codes and the likes -------------------------------------}
  MsgText := UbbParse(MsgText, mb_GetFromWho);
  FixSmileys(MsgText);
end; { proc. DisplayCommentText }


procedure ConvertMacros(var DataStr: String);
var Count     : Integer;
    TmpInt    : Integer;

    TmpChar   : Char;
    SaveMsg   : Integer;
    TmpStr    : String;
    NewStr    : String;
    Timing_Tmp: Real;
begin
  Count := 1;
  NewStr := '';

  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  if Pos('|', DataStr) > 0 then
    begin
      while DataStr <> ''do
        begin
          {-- find the first macro ------------------------------------------}
          TmpInt := Pos('|', DataStr);
          if TmpInt = 0 then
            Tmpint := Length(DataStr);

          {-- add everything that does not need concatenating ---------------}
          Concatenate(NewStr, Copy(DataStr, 1, TmpInt - 1));

          {-- and delete the non-macro code ---------------------------------}
          Delete(DataStr, 1, TmpInt);

          {-- now lets find the macro ---------------------------------------}
          if Length(DataStr) > 1 then
            begin
              TmpChar := DataStr[2];

              if DataStr[1] = 'Y' then
                begin
                   if TmpChar = 'A' then Concatenate(NewStr, MessageInf.Name)
                    else if TmpChar = 'B' then Concatenate(NewStr, FStr(MessageInf.Areanum))
                    else if TmpChar = 'C' then Concatenate(NewStr, GetAttributesStr)
                    else if TmpChar = 'D' then begin
                                                  DisplayMessageText(TmpStr);
                                                  Concatenate(NewStr, TmpStr);
                                               end { display messagetext }
                    else if TmpChar = 'E' then Concatenate(NewStr, FStr(mb_GetMsgNumber + 1))
                    else if TmpChar = 'F' then begin
                                                  {-- Previous message is a bit more difficult as ------}
                                                  {-- ReadMsg defaults to the next message so we have --}
                                                  {-- to calculate it ----------------------------------}
                                                  {-- Please note that calling this function resets ----}
                                                  {-- the current message text pointer -----------------}
                                                  SaveMsg := mb_GetMsgNumber;

                                                  {-- read the previous message ------------------------}
                                                  mb_GetPrevious;

                                                  {-- display the message ------------------------------}
                                                  if mb_GetMsgNumber <> SaveMsg then
                                                    Concatenate(NewStr, FStr(mb_GetMsgNumber))
                                                      else Concatenate(NewStr, '-1');

                                                  {-- and restore the message --------------------------}
                                                  mb_Read(SaveMsg);
                                                  mb_DoReadMessageText;
                                               end { F }
                    else if TmpChar = 'G' then begin
                                                 {-- its not always certain waht to use in the from ---}
                                                 {-- name. --------------------------------------------}
                                                 TmpStr := '';

                                                 {-- if forced handles, use one -----------------------}
                                                 if ReadBit(MessageInf.Attribute, 5) then
                                                  begin
                                                    TmpStr := EleCodeStr('|F3');
                                                  end; { if }

                                                 {-- if no forced handles or handle is emtpy, use -----}
                                                 {-- the users name -----------------------------------}
                                                 if TmpStr = '' then TmpStr := EleCodeStr('|FA');

                                                 {-- and convert the macro ----------------------------}
                                                 Concatenate(NewStr, TmpStr);
                                               end { G }
                    else if TmpChar = 'H' then begin
                                                 {-- now display the text -----------------------------}
                                                 if NOT (sLowCase(web_GetFormData('dontquotetext')) = 'true') then
                                                   DisplayReplyText(TmpStr)
                                                    else TmpStr := '';

                                                 {-- and add the text ---------------------------------}
                                                 Concatenate(NewStr, TmpStr);
                                               end { H }
                    else if TmpChar = 'I' then Concatenate(NewStr, reply_ReplyAddr)
                    else if TmpChar = 'J' then Concatenate(NewStr, reply_ReplyTo)
                    else if TmpChar = 'K' then Concatenate(NewStr, reply_ReplyKludge)
                    else if TmpChar = 'L' then Concatenate(NewStr, reply_Address)
                    else if TmpChar = 'M' then Concatenate(NewStr, FStr(mb_GetReplyFirst))
                    else if TmpChar = 'N' then Concatenate(NewStr, FStr(mb_GetReplyNext))
                    else if TmpChar = 'O' then Concatenate(NewStr, FStr(mb_GetReplyTo))
                    else if TmpChar = 'P' then Concatenate(NewStr, FStr(StartMsgNum))
                    else if TmpChar = 'Q' then begin
                                                 TmpStr := '';
                                                 DisplayEditText(TmpStr);
                                                 Concatenate(NewStr, TmpStr);
                                               end { Q }
                    else if TmpChar = 'R' then Concatenate(NewStr, web_ConvertLink(mb_GetFromWho))
                    else if TmpChar = 'S' then Concatenate(NewStr, web_ConvertLink(EleCodeStr('|FA')))
                    else if TmpChar = 'T' then begin
                                                 DisplayMessageLine(MsgTxtLine);
                                                 Concatenate(NewStr, MsgTxtLine);
                                               end { T }
                    else if TmpChar = 'U' then Concatenate(NewStr, web_GetFormData('towho'));
                end
              else if DataStr[1] = 'Z' then
                    begin
                     if TmpChar = 'A' then Concatenate(NewStr, mb_GetFromWho)
                      else if TmpChar = 'B' then Concatenate(NewStr, mb_GetToWho)
                      else if TmpChar = 'C' then Concatenate(NewStr, mb_GetSubject)
                      else if TmpChar = 'D' then Concatenate(NewStr, FStr(mb_GetMsgNumber))
                      else if TmpChar = 'E' then Concatenate(NewStr, mb_GetTimeStr)
                      else if TmpChar = 'F' then Concatenate(NewStr, FormatDateString(mb_GetDateStr, 8, 3))
                      else if TmpChar = 'G' then Concatenate(NewStr, FStr(TopicInf.TotalPosts))
                      else if TmpChar = 'H' then Concatenate(NewStr,
                                                    FormatDateString(Unix2Str(TopicInf.LastPostDate, 4), 8, 3) +
                                                    '&nbsp;&nbsp;' + Unix2Str(TopicInf.LastPostDate, 0))
                      else if TmpChar = 'I' then Concatenate(NewStr, FStr(TopicInf.MsgNum))
                      else if TmpChar = 'J' then begin
                                                   if FVal(web_GetFormData('threadreply')) = 0 then
                                                     begin
                                                       if mb_GetMsgNumber = 0 then
                                                         Concatenate(NewStr, FStr(MsgNumberWritten))
                                                           else Concatenate(NewStr, FStr(mb_GetMsgNumber));
                                                     end { else }
                                                       else Concatenate(NewStr,
                                                                          FStr(FVal(web_GetFormData('threadreply'))));
                                                 end { J }
                      else if TmpChar = 'K' then Concatenate(NewStr, FStr(TopicInf.TotalPosts -1))
                      else if TmpChar = 'L' then begin
                                                   if usrExtra.IcqNumber <> '' then
                                                     begin
                                                       Concatenate(NewStr, '<img src="http://web.icq.' +
                                                            'com/whitepages/online?icq='+
                                                            usrExtra.IcqNumber + '&img=5"' +
                                                            ' alt="ICQ online status"></img>');
                                                     end; { if }
                                                 end { L }
                      else if TmpChar = 'M' then begin
                                                   if usrExtra.Homepage <> '' then
                                                    begin
                                                      Concatenate(NewStr, '<a href="'+usrExtra.HomePage+
                                                         '" TARGET=_NEW"><img border=0 alt="Homepage" ' +
                                                         'src="/images/home.gif"></img></a>');
                                                    end; { if }
                                                 end { M }
                      else if TmpChar = 'N' then begin
                                                   if usrExtra.MemberType <> '' then
                                                     Concatenate(NewStr, '(' + usrExtra.MemberType + ')');
                                                 end { N }
                      else if TmpChar = 'O' then Concatenate(NewStr, TopicInf.LastPoster)
                      else if TmpChar = 'P' then begin
                                                   DisplayStoryExcerpt(TmpStr);
                                                   Concatenate(NewStr, TmpStr);
                                                 end
                      else if TmpChar = 'Q' then begin
                                                   DisplayCommentText(TmpStr);
                                                   Concatenate(NewStr, TmpStr);
                                                 end
                      else if TmpChar = 'R' then Concatenate(NewStr, web_GetFormData('fpreply'))
                      else if TmpChar = 'S' then begin
                                                   if usrExtra.Avatar <> '' then
                                                     Concatenate(NewStr, '<IMG SRC="'+ usrExtra.Avatar + '"></IMG>');
                                                 end { S }
                      else if TmpChar = 'T' then begin
                                                   if usrExtra.MemberType <> '' then
                                                     begin
                                                       Concatenate(NewStr, '(' + usrExtra.MemberType + ')');
                                                     end; { if }
                                                 end { T }
                      else if TmpChar = 'U' then begin
                                                   {-- now get the next 3 numbers ----------------}
                                                   {-- and if that number matches, we show -------}
                                                   {-- selected ----------------------------------}
                                                   if FVal(Copy(DataStr, 3, 3)) = FVal(
                                                      web_GetFormData('daysprune')) then
                                                        Concatenate(NewStr, 'SELECTED');

                                                   {-- and remove those --------------------------}
                                                   Delete(DataStr, 2, 3);
                                                 end; { U }
                   end; { else }

              {-- and remove those two characters too -----------------------}
              if (DataStr[1] = 'Y') OR (DataStr[1] = 'Z') then
                Delete(DataStr, 1, 2)
                  else Concatenate(NewStr, '|');
            end; { if }
        end; { while }
    end
      else NewStr := DataStr;

  {-- and save the time -----------------------------------------------------}
  timing_McrTotal  := timing_McrTotal + (GetMicroTimer - timing_Tmp);

  {-- Convert any EleBBS codes to user data ---------------------------------}
  DataStr := EleCodeStr(NewStr);
end; { proc. ConvertMacros }


procedure ShowHtmlFile(HtmlFile: String);
var HtmlHandle: Integer;
    Error     : Integer;
    DataStr   : String;
    Counter   : Integer;

    CachePtr  : Integer;
    CacheLne  : Integer;
    DidCache  : Boolean;
begin
  {-- Initialize variable ---------------------------------------------------}
  DidCache := false;

  {-- Is this file cached? --------------------------------------------------}
  for Counter := 1 to maxCacheFiles do
    if cache_Record[Counter].FName = HtmlFile then
      begin
        DidCache := true;

        DataStr := cache_Record[Counter].Data;
        ConvertMacros(DataStr);
        Write(DataStr);
      end; { if }

  {-- initialize variables --------------------------------------------------}
  CachePtr := -1;
  CacheLne := 0;

  if NOT DidCache then
    begin
      {-- find an empty cached entry ----------------------------------------}
      for Counter := 1 to maxCacheFiles do
        if cache_Record[Counter].fName = '' then CachePtr := Counter;

      {-- and assign the data -----------------------------------------------}
      if CachePtr > 0 then
        cache_Record[CachePtr].FName := HtmlFile;

      {-- Assign the file to the proper handle etc --------------------------}
      HtmlHandle := FileAssign(GetPath_HtmlPath + HtmlFile, 0, 3);
      Error := FileGetError(HtmlHandle);

      {-- Now open the file -------------------------------------------------}
      if Error = 0 then
        FileOpen(HtmlHandle);
      Error := FileGetError(HtmlHandle);

      {-- display an error if we cant find this html-file -------------------}
      if Error > 0 then
        begin
          ShowError('Unable to find ' + HtmlFile);
        end; { if }

      if (Error = 0) then
       begin
         {-- we try to read the whole string in once to prevent an expensive -}
         {-- loop. The FileReadString() routine maxes out at 64k which should-}
         {-- be more than enough ---------------------------------------------}
         FileReadString(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- Add it to the cache ---------------------------------------------}
         cache_Record[CachePtr].Data := DataStr;

         {-- now convert the macros ------------------------------------------}
         if Error = 0 then
           ConvertMacros(DataStr);

         {-- and display it to the user --------------------------------------}
         if Error = 0 then
           Write(DataStr);
       end; { if }

    {-- Close the file -------------------------------------------------------}
    FileClose(HtmlHandle);
    FileDone(HtmlHandle);
  end; { if }
end; { proc. ShowHtmlFile }




procedure DisplayQuickListItem(IncludeReplies: Boolean);
begin
  if IncludeReplies then
    ShowHtmlFile(html_QrdData_Tpl)
     else begin
            if HasNewMessages(TopicInf.LastPostDate) then
              begin
                if ReadBit(TopicInf.Attribute, 0) then { is this topic closed? }
                  ShowHtmlFile(Html_qrdflonl_tpl)
                    else ShowHtmlFile(html_qrdflon_tpl);
              end { if }
               else begin
                      if ReadBit(TopicInf.Attribute, 0) then { is this topic closed? }
                        ShowHtmlFile(html_qrdflofl_tpl)
                          else ShowHtmlFile(html_qrdfloff_tpl);
                    end; { else }
          end; { else }
end; { proc. DisplayQuickListItem }


procedure ListHeaders(StartMsgNum   : Integer;
                      CountToShow   : Integer;
                      IncludeReplies: Boolean);
var Count  : Integer;
    DoShow : Boolean;
begin
  {-- initialize some variables ----------------------------------------------}
  if CountToShow <= -1 then
    Count := -2
      else Count := 0;

  {-- Display the "quick scan" header ----------------------------------------}
  if IncludeReplies then
    ShowHtmlFile(html_qrdhead_tpl)
     else ShowHtmlFile(html_qrdhd2_tpl);

  {-- Begin at the beginning -------------------------------------------------}
  mb_Read(StartMsgNum);

  {-- Loop through all messages till we're at the end ------------------------}
  while (mb_MessageFound) AND (Count <= CountToShow) do
    begin
      {-- Make sure we can view this item ------------------------------------}
      DoShow := CheckReadMsgAccess(mb_GetFromWho,
                                   SysopAcc,
                                   mb_GetPrivate);

      {-- If this is a reply, check if we should actually display it ---------}
      if mb_GetReplyTo > 0 then
        if NOT IncludeReplies then DoShow := FALSE;

      {-- Open the item ------------------------------------------------------}
      if DoShow then
        begin
          {-- Get the forum info, if necessary -------------------------------}
          if IsForum then
           if NOT IncludeReplies then
             begin
               if FindMsgIdx(JustPath(MessageInf.JamBase), mb_GetMsgNumber, MessageInf.AreaNum) > 0 then ;
             end; { if }

          {-- Actually display the item --------------------------------------}
          DisplayQuickListItem(IncludeReplies);

          {-- Make sure we only show the number of items we should -----------}
          if CountToShow >= 0 then
            Count := Count + 1;
        end; { if }

      {-- and get the next item ----------------------------------------------}
      mb_GetNext;
    end; { while }

  {-- Display the "quick scan" footer ----------------------------------------}
  if IncludeReplies then
    begin
      if mb_MessageFound then
        ShowHtmlFile(html_qrdfood_tpl)
          else ShowHtmlFile(html_qrdend_tpl);
    end
      else begin
             if mb_MessageFound then
               ShowHtmlFile(html_qrdft2_tpl)
                 else ShowHtmlFile(html_qrdend2_tpl);
           end; { else }
end; { proc. ListHeaders }


procedure thread_ShowItem(var Deep: Integer);
var timing_Tmp: Real;
begin
  {-- this is basically the only output in this script that is hardcoded -----}
  {-- we do it like this to get the performance anything acceptable, also ----}
  {-- the listing of messages should be kept as simple as possible to keep ---}
  {-- the speed up for a bit -------------------------------------------------}

  if StoryShowAll then
    begin
      {-- initialize the timer -----------------------------------------------}
      timing_Initialize(timing_Tmp);

      {-- Read the message text ----------------------------------------------}
      mb_DoReadMessageText;

      {-- and save the time --------------------------------------------------}
      timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

      {-- initialize the timer -----------------------------------------------}
      timing_Initialize(timing_Tmp);

      {-- and show this item -------------------------------------------------}
      ShowHtmlFile(html_stfl_cmn_tpl);

      {-- and save the time --------------------------------------------------}
      timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);
    end
      else begin
             Write('<LI><A HREF="/cgi-bin/eleweb.exe?action=3&script=readmsg&sub-action=1&msgnum=');
             WriteLn(mb_getMsgNumber, '&areanum=', MessageInf.AreaNum, '">', mb_GetSubject, '</A> by ', mb_GetFromWho);
           end; { else }

  {-- increase the number of items we have showed ----------------------------}
  ThrdCount := ThrdCount + 1;
end; { proc. thread_ShowItem }


procedure thread_IncDepth(var HowDeep: Integer);
begin
  HowDeep := HowDeep + 1;                   { another reply to this reply }
  Write('<UL>');
end; { proc. thread_IncDepth }

procedure thread_DecDepth(var HowDeep: Integer);
begin
  HowDeep := HowDeep - 1;                       { we finished this thread }
  Write('</UL>');
end; { proc. thread_DecDepth }

procedure thread_WalkNested(MsgNum: integer; var HowDeep: Integer; ShowFirst: Boolean);
var SaveMsg   : Integer;
    timing_Tmp: Real;
begin
  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- save the current message number ----------------------------------------}
  SaveMsg := MsgNum;
  mb_Read(MsgNum);

  {-- and save the time -----------------------------------------------------}
  timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

  {-- and show this item -----------------------------------------------------}
  if ShowFirst then
   thread_ShowItem(HowDeep);

  {-- now handle nesting, if there is any ------------------------------------}
  if mb_GetReplyFirst > 0 then
    begin
      {-- Increase thread-depth ----------------------------------------------}
      thread_IncDepth(HowDeep);

      {-- now recursively call ourselves to walk this thread -----------------}
      thread_WalkNested(mb_GetReplyFirst, HowDeep, true);

      {-- and decrease the depth ---------------------------------------------}
      thread_DecDepth(HowDeep);

      {-- initialize the timer -----------------------------------------------}
      timing_Initialize(timing_Tmp);

      {-- and return to our original setup -----------------------------------}
      mb_Read(SaveMsg);

      {-- and save the time --------------------------------------------------}
      timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);
    end; { if reply_first }

  if mb_GetReplyNext > 0 then
    begin
      {-- now recursively call ourselves to walk this thread -----------------}
      thread_WalkNested(mb_GetReplyNext, HowDeep, true);

      {-- and return to our original setup -----------------------------------}
      mb_Read(SaveMsg);
    end; { if reply_second }
end; { proc. thread_WalkNested }


procedure thread_WalkChain(MsgNum: Integer;
                           var HowDeep: Integer;
                           var CountToShow: Integer;
                           var OldestToNew: Boolean);
var SaveMsg: Integer;
    DoShow : Boolean;
begin
  {-- initialize this first message ------------------------------------------}
  mb_Read(MsgNum);

  {-- now loop through all messages ------------------------------------------}
  while (mb_MessageFound) AND (ThrdCount <= CountToShow) do
    begin
      {-- if this isnt a reply to anything we handle it, else we rely on -----}
      {-- it that the original message is still around and linked to this ----}
      {-- reply --------------------------------------------------------------}
      if mb_GetReplyTo = 0 then
        begin
          {-- this is an original message, reset the deep count --------------}
          HowDeep := 0;

          {-- Make sure we can view this item --------------------------------}
          DoShow := CheckReadMsgAccess(mb_GetFromWho,
                                       SysopAcc,
                                       mb_GetPrivate);

          {-- and show the message -------------------------------------------}
          if DoShow then
            thread_ShowItem(HowDeep);

          {-- if this message has a ReplyFirst count, walk through it --------}
          if mb_GetReplyFirst > 0 then
           if DoShow then
            begin
              {-- increase the depth -----------------------------------------}
              thread_IncDepth(HowDeep);

              {-- save the current message so we can return to it ------------}
              SaveMsg := mb_GetMsgNumber;

              {-- now walk through the reply chain ---------------------------}
              thread_WalkNested(mb_GetReplyFirst, HowDeep, true);

              {-- now restore the original message because the reply could ---}
              {-- have an earlier message number than the original message ---}
              mb_Read(SaveMsg);

              {-- decrease the depth -----------------------------------------}
              thread_DecDepth(HowDeep);
            end; { if }
        end; { if }

      {-- and retrieve the next message --------------------------------------}
      if (OldestToNew) then
        begin
          mb_GetNext;
        end 
          else begin
                 mb_GetPrevious;
               end; { else }
    end; { while }
end; { proc. thread_WalkChain }


procedure ListHeadersThreaded(StartMsgNum: Integer;
                              CountToShow: Integer;
                              OldestToNew: Boolean);
var HowDeep   : Integer;
begin
  {-- If StartMsgNum == 0 and we are reading reverse, fix it up --------------}
  if (StartMsgNum = 0) then
    begin
      StartMsgNum := mb_GetHighMsgNum;
    end; { if }
    
  {-- the count to show is rather in-accurate as we only check the maximum ---}
  {-- for each "main" thread. This means that if a thread has 300 replies, ---}
  {-- and the maximum is set at 100, we still list 300 messages --------------}
  {-- we can further refine this but i dont think that is necessary as it ----}
  {-- would create a problem in where to start the next page -----------------}

  {-- initialize some variables ----------------------------------------------}
  HowDeep := 0;

  {-- Display the "quick scan" header ----------------------------------------}
  ShowHtmlFile(html_trdhead_tpl);

  {-- Begin at the beginning -------------------------------------------------}
  thread_WalkChain(StartMsgNum, HowDeep, CountToShow, OldestToNew);

  {-- Display the "quick scan" footer ----------------------------------------}
  if mb_MessageFound then
    ShowHtmlFile(html_trdfoot_tpl)
      else ShowHtmlFile(html_trdend_tpl);
end; { proc. ListHeadersThreaded }


procedure ShowStoryItem;
var timing_Tmp: Real;
begin
  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- initialize the message text -------------------------------------------}
  mb_DoReadMessageText;

  {-- and save the time -----------------------------------------------------}
  timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- and display the HTML file ---------------------------------------------}
  if NOT StoryShowAll then
    ShowHtmlFile(html_stor_dt_tpl)
      else ShowHtmlFile(html_stfl_dt_tpl);

  {-- and save the time -----------------------------------------------------}
  timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);
end; { proc. ShowStoryItem }


procedure ListForums(StartIdxNum   : Integer;
                     CountToShow   : Integer;
                     IsStoryPage   : Boolean);
var Count       : Integer;
    Error       : Integer;
    DoShow      : Boolean;
    IdxHandle   : Integer;
    ThreadInf   : ThreadSortRec;
    StopLoop    : Boolean;
    TmpSize     : Integer;
    TmpDir      : String;
    timing_Tmp  : Real;

    UnixHistDate: Longint;                  { now() - daysago in unix format }
begin
  {-- Calculate the history date of 'daysago' -------------------------------}
  if DaysOld >= 0 then
    begin
      UnixHistDate := Date2Unix(PackTimeStr(HistoryDate(DaysOld), ''));
    end; { if }

  {-- Initialize the timing cumulative --------------------------------------}
  timing_TplTotal := 0;
  timing_ReadTotal := 0;
  timing_IdxTotal := 0;

  {-- Save the current time -------------------------------------------------}
  timing_Initialize(timing_ForumStart);

  {-- initialize some variables ---------------------------------------------}
  TmpSize := GetRecSize(ThreadInf);

  StopLoop := false;
  if CountToShow <= -1 then
    Count := -2
      else Count := 0;

  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- Display the "quick scan" header ---------------------------------------}
  if NOT IsStoryPage then
    ShowHtmlFile(html_qrdhd2_tpl)
     else ShowHtmlFile(html_stor_hd_tpl);

  {-- and save the time -----------------------------------------------------}
  timing_TplTotal  := timing_TplTotal + (GetMicroTimer - timing_Tmp);

  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(JustPath(MessageInf.JamBase) + 'threads' + FStr(MessageInf.AreaNum) + '.idx', 0, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- Seek to the end of the file, and work our way back up -----------------}
  FileSeek(IdxHandle, FileGetSize(IdxHandle) - TmpSize);

  {-- loop through the file -------------------------------------------------}
  if FileGetSize(IdxHandle) > 0 then
    while (NOT StopLoop) do
      begin
        {-- initialize the timer --------------------------------------------}
        timing_Initialize(timing_Tmp);

        {-- Read the index --------------------------------------------------}
        FileRead(IdxHandle, ThreadInf);

        {-- if we are at the beginning, just stop at all --------------------}
        if FileGetPos(IdxHandle) <= TmpSize then
          StopLoop := true
            else FileSeek(IdxHandle, FileGetPos(IdxHandle) - (TmpSize * 2));

        {-- and save the time ---------------------------------------}
        timing_IdxTotal := timing_IdxTotal + (GetMicroTimer - timing_Tmp);

        {-- if this is a valid entry ----------------------------------------}
        if ThreadInf.ThreadNum >= 0 then
          begin
            {-- initialize the timer ------------------------------------}
            timing_Initialize(timing_Tmp);

            {-- read the actual message -------------------------------------}
            mb_Read(ThreadInf.ThreadNum);

            {-- Make sure we can view this item -----------------------------}
            DoShow := CheckReadMsgAccess(mb_GetFromWho,
                                         SysopAcc,
                                         mb_GetPrivate);

            {-- and save the time -------------------------------------------}
            timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

            {-- Open the item -----------------------------------------------}
            if DoShow then
              begin
                {-- initialize the timer ------------------------------------}
                timing_Initialize(timing_Tmp);

                {-- Get the forum info, if necessary ------------------------}
                if IsForum then
                  if FindMsgIdx(JustPath(MessageInf.JamBase), mb_GetMsgNumber, MessageInf.AreaNum) < 0 then
                    DoShow := false;  {  invalid message }

                {-- and save the time ---------------------------------------}
                timing_IdxTotal := timing_IdxTotal + (GetMicroTimer - timing_Tmp);

                {-- initialize the timer ------------------------------------}
                timing_Initialize(timing_Tmp);

                {-- Make sure this day is in range --------------------------}
                if DoShow then
                  if DaysOld >= 0 then
                    begin
                      DoShow := (UnixHistDate <= TopicInf.LastPostDate);
                    end; { if }

                {-- Actually display the item -------------------------------}
                if DoShow then
                  begin
                    if NOT IsStoryPage then
                      begin
                        {-- and actually show the item ----------------------}
                        DisplayQuickListItem(false);
                      end
                        else begin
                               if mb_GetReplyTo = 0 then
                                 ShowStoryItem;
                             end; { if }
                  end; { if }

                {-- and save the time ---------------------------------------}
                timing_TplTotal  := timing_TplTotal + (GetMicroTimer - timing_Tmp);

                {-- Make sure we only show the number of items we should ----}
                if DoShow then
                  if CountToShow >= 0 then
                    Count := Count + 1;
              end; { if }
          end; { if }

         {-- stop looping ---------------------------------------------------}
         if CountToShow >= 0 then
           if Count >= CountToShow then
             begin
               if IsFrontPage then
                 begin
                   {-- always show todays stories, no matter how many -------}
                   if TopicInf.OrigPostDate <= Date2Unix(PackTimeStr(HistoryDate(1), '')) then
                     Stoploop := true;
                 end
                   else StopLoop := true;
             end; { if }
      end; { while }

  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- show the footer -------------------------------------------------------}
  if NOT IsStoryPage then
    ShowHtmlFile(html_qrdft2_tpl)
     else begin
            {-- Get the home directory --------------------------------------}
            if web_IsLoggedOn then
              begin
                TmpDir := GetHomeDirectory(EleCodeStr('|FA'), true);
                ReadUserInfo(TmpDir);
              end; { if }

            {-- and show the appropriate file -------------------------------}
            if NOT web_IsLoggedOn then
              ShowHtmlFile(html_stor_f2_tpl)
                else ShowHtmlFile(html_stor_ft_tpl);
          end; { else }

  {-- and save the time -----------------------------------------------------}
  timing_TplTotal  := timing_TplTotal + (GetMicroTimer - timing_Tmp);

  {-- Close the files -------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);

  {-- and the forum is done -------------------------------------------------}
  timing_Done(timing_ForumEnd);
end; { proc. ListForums }



procedure ReadMessage(MsgNum: Integer);
var DoShow: Boolean;
    MsgId : Integer;
begin
  {-- Begin at the beginning -------------------------------------------------}
  mb_Read(MsgNum);

  {-- Make sure we can view this item ----------------------------------------}
  DoShow := CheckReadMsgAccess(mb_GetFromWho,
                               SysopAcc,
                               mb_GetPrivate);


  {-- Read the actual message ------------------------------------------------}
  if (mb_MessageFound) AND (DoShow) then
    begin
      {-- initialize the message text ----------------------------------------}
      mb_DoReadMessageText;

      {-- Display the header for this message --------------------------------}
      ShowHtmlFile(html_rdhead1_tpl);

      {-- now determine what sort of reply-chaining we have to support -------}
      {-- if this message isn't a reply to anything, display that ------------}
      if mb_GetReplyTo = 0 then
        ShowHtmlFile(html_rdhead2_tpl)
          else ShowHtmlFile(html_rdhead3_tpl);

      {-- if this message hasn't got any (other) replies, show it ------------}
      if mb_GetReplyFirst = 0 then
        ShowHtmlFile(html_rdhead4_tpl)
          else ShowHtmlFile(html_rdhead5_tpl);

      {-- if this message hasn't got any threads linked to it, show it -------}
      if mb_GetReplyNext = 0 then
        ShowHtmlFile(html_rdhead6_tpl)
          else ShowHtmlFile(html_rdhead7_tpl);

      {-- Display the header for this message --------------------------------}
      ShowHtmlFile(html_rdhead99_tpl);

      {-- Display the text for this message ----------------------------------}
      while (NOT mb_EndOfMessage) do
        begin
          {-- Get the next line of message text ------------------------------}
          MsgTxtLine := mb_GetMessageLine(250);

          {-- now classify the message text ----------------------------------}
          MsgId := ClassifyMsgText(MsgTxtLine);

          {-- and display the appopriate message line ------------------------}
          Case MsgId of
            { normal } 0 : ShowHtmlFile(html_rdtextn_tpl);
            { quote  } 1 : ShowHtmlFile(html_rdtextq_tpl);
            { kludge } 2 : if ShowKludges then
                             ShowHtmlFile(html_rdtextk_tpl);
            { tear   } 3 : ShowHtmlFile(html_rdtextt_tpl);
              else ;
          end; { case }
        end; { while }

      {-- Display the footer for this message --------------------------------}
      ShowHtmlFile(html_rdfoot_tpl);
      
      {-- set the received bit to true etc -----------------------------------}
      mb_HandleMessageRead(FALSE,              { Do not set lastread pointer? }
                           MessageInf);                         { Area record }
    end
      else begin
             {-- unable to find this message ---------------------------------}
             ShowHtmlFile(html_rderror_tpl);
           end; { else }
end; { proc. ReadMessage }

procedure ListMessageAndThread(MsgNum: Integer);
begin
  ReadMessage(MsgNum);
  ListHeadersThreaded(StartMsgNum, 100, True);
end; { proc. ListMessageAndThread }


procedure ShowFlatMessageThread(MsgNum    : Integer);
var MsgId     : Integer;
    EndOfList : Boolean;
    DoShow    : Boolean;
    ShowOdd   : Boolean;
    DidKludge : Boolean;
    TmpDir    : String;
    timing_Tmp: Real;
begin
  {-- Initialize the timing cumulative --------------------------------------}
  timing_TplTotal := 0;
  timing_ReadTotal := 0;
  timing_IdxTotal := 0;

  {-- Save the current time -------------------------------------------------}
  timing_Initialize(timing_ForumStart);

  {-- Initialize some variables ----------------------------------------------}
  EndOfList := FALSE;
  DoShow := TRUE;
  ShowOdd := false;

  {-- initialize the timer ---------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- read the actual message ------------------------------------------------}
  mb_Read(MsgNum);

  {-- and save the time ------------------------------------------------------}
  timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

  {-- initialize the timer ---------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- read the index file ----------------------------------------------------}
  if IsForum then
    if FindMsgIdx(JustPath(MessageInf.JamBase), MsgNum, MessageInf.AreaNum) > 0 then ;

  {-- and save the time ------------------------------------------------------}
  timing_IdxTotal := timing_IdxTotal + (GetMicroTimer - timing_Tmp);

  {-- initialize the timer ---------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- Show the "large" header ------------------------------------------------}
  ShowHtmlFile(html_rd_flhd_tpl);

  {-- and save the time ------------------------------------------------------}
  timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);

  {-- Read the actual message ------------------------------------------------}
  While (mb_MessageFound) AND (NOT EndOfList) do
    begin
      {-- Toggle colours in the flat view ------------------------------------}
      ShowOdd := NOT ShowOdd;

      {-- show a seperator line for tearlines per message --------------------}
      DidKludge := false;

      {-- initialize the timer -----------------------------------------------}
      timing_Initialize(timing_Tmp);

      {-- Make sure we can view this item ------------------------------------}
      DoShow := CheckReadMsgAccess(mb_GetFromWho,
                               SysopAcc,
                               mb_GetPrivate);

      {-- initialize the message text ----------------------------------------}
      mb_DoReadMessageText;

      {-- Get the home directory ---------------------------------------------}
      TmpDir := GetHomeDirectory(mb_GetFromWho, true);
      ReadUserInfo(TmpDir);

      {-- and save the time ------------------------------------------------------}
      timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

      {-- initialize the timer -----------------------------------------------}
      timing_Initialize(timing_Tmp);

      {-- Display the header for this message --------------------------------}
      if ShowOdd then
        begin
          ShowHtmlFile(html_rd_flhd1_tpl);
        end
          else begin
                 ShowHtmlFile(html_rd_flhd2_tpl);
               end; { else }

      {-- Now show the message header in the text box (..) -------------------}
      if NOT SysOpAcc then
        ShowHtmlFile(html_rdfltxth_tpl)
          else ShowHtmlFile(html_rdfltxta_tpl);

      {-- Display the text for this message ----------------------------------}
      if ShowOdd then
        begin
          {-- show the actual message text -----------------------------------}
          ShowHtmlFile(html_rdfltxt1_tpl);

          {-- Display the footer for this message ----------------------------}
          ShowHtmlFile(html_rd_flft1_tpl);
        end
          else begin
                 {-- show the message text -----------------------------------}
                 ShowHtmlFile(html_rdfltxt2_tpl);

                 {-- Display the footer for this message ---------------------}
                 ShowHtmlFile(html_rd_flft2_tpl);
               end; { else }

      {-- and save the time --------------------------------------------------}
      timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);

      {-- initialize the timer -----------------------------------------------}
      timing_Initialize(timing_Tmp);

      {-- set the received bit to true etc -----------------------------------}
      mb_HandleMessageRead(FALSE,              { Do not set lastread pointer? }
                           MessageInf);                         { Area record }

      {-- surf to the next message -------------------------------------------}
      if mb_GetReplyFirst > 0 then
        begin
          mb_Read(mb_GetReplyFirst);
        end
          else EndOfList := TRUE;

      {-- and save the time --------------------------------------------------}
      timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);
    end; { while }

  {-- initialize the timer ---------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- Show the "large" footer ------------------------------------------------}
  ShowHtmlFile(html_rd_flft_tpl);

  {-- and save the time ------------------------------------------------------}
  timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);

  {-- and the forum is done -------------------------------------------------}
  timing_Done(timing_ForumEnd);
end; { proc. ShowFlatMessageThread }


procedure ShowStoryExpanded(StartMsgNum: Integer);
var HowDeep   : Integer;
    timing_Tmp: Real;
begin
  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- Start showing the templates ------------------------------------------}
  ShowHtmlFile(html_stfl_hd_tpl);

  {-- and save the time -----------------------------------------------------}
  timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);

  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- and save the time -----------------------------------------------------}
  timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

  {-- Read the actual message ----------------------------------------------}
  mb_Read(StartMsgNum);

  {-- and save the time -----------------------------------------------------}
  timing_ReadTotal := timing_ReadTotal + (GetMicroTimer - timing_Tmp);

  {-- Set the global variable to pass this on -------------------------------}
  StoryShowAll := true;

  {-- Actually display the message ------------------------------------------}
  ShowStoryItem;

  {-- Get the home directory ------------------------------------------------}
  if web_IsLoggedOn then
    begin
      ReadUserInfo( GetHomeDirectory(EleCodeStr('|FA'), true) );
    end; { if }

  {-- initialize the timer --------------------------------------------------}
  timing_Initialize(timing_Tmp);

  {-- and show the footer ---------------------------------------------------}
  if NOT web_IsLoggedOn then
    ShowHtmlFile(html_stfl_f2_tpl)
      else ShowHtmlFile(html_stfl_ft_tpl);

  {-- and save the time -----------------------------------------------------}
  timing_TplTotal := timing_TplTotal + (GetMicroTimer - timing_Tmp);

  {-- now list all replies to this message below the mains tory -------------}
  HowDeep := 0;
  thread_WalkNested(StartMsgNum, HowDeep, false);
end; { proc. ShowStoryExpanded }


procedure DeleteMsgNum(MsgNum: Integer);
var DoKill        : Boolean;
    MsgId         : Integer;

    SaveReplyTo   : Integer;
    SaveReplyFirst: Integer;
    UpdateThreads : Boolean;

    TmpReply      : Integer;
    TmpIdx        : Integer;
    TmpSortIdx    : Integer;
    KillError     : Integer;
begin
  {-- Initialize variables ---------------------------------------------------}
  UpdateThreads := false;
  KillError := 1;

  {-- Begin at the beginning -------------------------------------------------}
  mb_Read(MsgNum);

  {-- Make sure we can view this item ----------------------------------------}
  DoKill := CheckReadMsgAccess(mb_GetFromWho,
                               SysOpAcc,
                               mb_GetPrivate);


  {-- Make sure this topic is not locked -------------------------------------}
  if DoKill then
   if IsForum then
    if FindMsgIdx(JustPath(MessageInf.JamBase), MsgNum, MessageInf.AreaNum) > 0 then
      if ReadBit(TopicInf.Attribute, 0) then
        begin
          KillError := 2;
          DoKill := false;
        end; { if }

  {-- if we have access to this message area, can we also delete this msg? ---}
  if DoKill then
    DoKill := CheckMsgDeleteAccess(MessageInf,
                                   mb_GetToWho,
                                   mb_GetFromWho,
                                   mb_GetMarkAsSent);

  {-- Loop through all messages till we're at the end ------------------------}
  if (mb_MessageFound) AND (DoKill) then
    begin
      {-- If this is on a forum, lets try to fix the reply thread ------------}
      if IsForum then
        begin
          if (mb_GetReplyTo > 0) OR (SysopAcc) then
            begin
              {-- Because the topic way of replying is one large thread, -----}
              {-- deleting one of the messages in the thread would cause -----}
              {-- all messages below it to suddenly disappear. We set the ----}
              {-- next message to link to the previous message, so we can ----}
              {-- safely delete this one -------------------------------------}
              UpdateThreads := TRUE;

              {-- Save the pointer to then next message ----------------------}
              SaveReplyTo := mb_GetReplyTo;
              SaveReplyFirst := mb_GetReplyFirst;
            end { if }
              else begin
                     ShowHtmlFile(html_delerr2_tpl);
                     DoKill := false;
                   end;  { else }
        end; { if }

      {-- now silently delete the message ------------------------------------}
      if DoKill then
        mb_SilentDeleteMsg(Ra250MsgArea(MessageInf.AreaNum),   { Board number }
                           mb_GetMsgNumber,                   { Messagenumber }
                           false);        { Do not check access (we just did) }

      {-- and update our reply threads ---------------------------------------}
      if UpdateThreads then
       if DoKill then
        begin
          {-- we set the previous replyfirst pointer to this -----------------}
          mb_Read(SaveReplyTo);
          mb_SetReplyFirst(SaveReplyFirst);
          mb_Write(SaveReplyTo);

          {-- and update the next message ------------------------------------}
          if SaveReplyFirst > 0 then
            begin
              mb_Read(SaveReplyFirst);
              mb_SetReplyTo(SaveReplyTo);
              mb_Write(SaveReplyFirst);
            end; { if }
        end; { if }


      {-- And now update the index -------------------------------------------}
      if IsForum then
       if DoKill then
        begin
          {-- get the thread index -------------------------------------------}
          TmpReply := FVal(web_GetFormData('threadreply'));

          {-- find the index record ------------------------------------------}
          TmpIdx := FindMsgIdx(JustPath(MessageInf.JamBase), TmpReply, MessageInf.AreaNum);
          if TmpIdx > 0 then
            begin
              {-- update the info --------------------------------------------}
              TopicInf.TotalPosts := TopicInf.TotalPosts - 1;
              TopicInf.LastPostDate := NowAsUnixDate;

              {-- if the last message was deleted, update the lastpostnum ----}
              if TopicInf.LastPostNum = MsgNum then
                begin
                  if SaveReplyTo > 0 then
                    TopicInf.LastPostNum := SaveReplyTo
                      else TopicInf.LastPostNum := MsgNum;
                 end; { if }

              {-- if we delete the main thread, delete everything ------------}
              if TopicInf.MsgNum = MsgNum then
                begin
                  TopicInf.MsgNum := -1;

                  {-- Now if it exists, delete the sort index for this msg ---}
                  TmpSortIdx := FindSortIndex(JustPath(MessageInf.JamBase), TmpReply, MessageInf.AreaNum);
                  if TmpSortIdx >= 0 then
                    begin
                      DisableSortIndex(JustPath(MessageInf.JamBase), TmpSortIdx, MessageInf.AreaNum);
                    end; { if }
                end; { if }

              {-- and update it on disk --------------------------------------}
              UpdateMsgIdx(JustPath(MessageInf.Jambase), TmpIdx, true, false,
                           MessageInf.AreaNum);
            end; { if }
        end; { if }

      {-- and notify the user that the message was deleted -------------------}
      if DoKill then
        ShowHtmlFile(html_delsucc_tpl);
    end { if }
      else begin
             {-- unable to find this message ---------------------------------}
             if KillError = 1 then
               ShowHtmlFile(html_delerror_tpl);

             if KillError = 2 then
               ShowHtmlFile(html_werror9_tpl);
           end; { else }
end; { proc. DeleteMsgNum }

procedure WriteMessage_Startup(ReplyingTo: Integer; IsEdit: Boolean);
var Success : Boolean;
    ErrorNr : Integer;
begin
  ErrorNr := 1;

  {-- Make sure we have access to this area ----------------------------------}
  if NOT CheckMsgAreaAccess(Messageinf,
                            FALSE,                            { Message group }
                            FALSE,      { We dont want READ access, but WRITE }
                            0) then                    { Message group number }
     begin
       Success := FALSE;
     end { if }
       else Success := TRUE;

  {-- Make sure this topic is not locked -------------------------------------}
  if ReplyingTo >= 0 then
   if IsForum then
    begin
      if FindMsgIdx(JustPath(MessageInf.JamBase), ReplyingTo, MessageInf.AreaNum) > 0 then
        if ReadBit(TopicInf.Attribute, 0) then
          begin
            ErrorNr := 9;
            Success := false;
          end; { if }
    end; { if }

  {-- if this message is a reply to someone, act upon it ---------------------}
  if Success then
    if ReplyingTo >= 0 then
      begin
        {-- read the message header ------------------------------------------}
        mb_Read(ReplyingTo);

        {-- initialize the message text --------------------------------------}
        mb_DoReadMessageText;
      end; { if }

  {-- are we allowed to edit this message? -----------------------------------}
  if IsEdit then
   if Success then
    if NOT mb_AllowEdit(MessageInf, mb_GetFromWho) then
      begin            
        Success := false;
        ErrorNr := 8;
      end; { if }

  {-- If we can succeed, display the script ----------------------------------}
  if Success then
    begin
      if ReplyingTo >= 0 then
        begin
          if IsEdit then
            begin
              ShowHtmlFile(html_wmsg_tre_tpl);
            end
              else begin
                     if IsForum then
                       begin
                         if IsFrontPage then
                           ShowHtmlFile(html_wmsg_six_tpl)
                             else ShowHtmlFile(html_wmsg_fiv_tpl)
                       end
                         else ShowHtmlfile(html_wmsg_two_tpl);
                   end; { else }
        end
          else begin
                 if IsForum then
                   ShowHtmlFile(html_wmsg_for_tpl)
                     else ShowHtmlFile(html_wmsg_one_tpl);
               end; { else }
    end
      else begin                     
      	     Case ErrorNr of
      	       01 : ShowHtmlFile(html_werror1_tpl);
      	       02 : ShowHtmlFile(html_werror2_tpl);
      	       03 : ShowHtmlFile(html_werror3_tpl);
      	       04 : ShowHtmlFile(html_werror4_tpl);
      	       05 : ShowHtmlFile(html_werror5_tpl);
      	       06 : ShowHtmlFile(html_werror6_tpl);
      	       07 : ShowHtmlFile(html_werror7_tpl);
      	       08 : ShowHtmlFile(html_werror8_tpl);
      	       09 : ShowHtmlFile(html_werror9_tpl);
      	         else ;
      	     end; { case }
           end; { else }
end; { proc. Writemessage_Startup }


procedure EditMessage_Process(ReplyingTo: Integer);
var Error: Integer;
    Tmp  : Integer;
begin
  {-- default to success -----------------------------------------------------}
  Error := 0;

  {-- Make sure this topic is not locked -------------------------------------}
  if ReplyingTo >= 0 then
   if IsForum then
    begin
      if FindMsgIdx(JustPath(MessageInf.JamBase), ReplyingTo, MessageInf.AreaNum) > 0 then
        if ReadBit(TopicInf.Attribute, 0) then
          begin
            Error := 9;
          end; { if }
    end; { if }

  {-- Make sure we have access to this area ----------------------------------}
  if NOT CheckMsgAreaAccess(Messageinf,
                            FALSE,                            { Message group }
                            FALSE,      { We dont want READ access, but WRITE }
                            0) then                    { Message group number }
     begin
       Error := 1;
     end; { if }

  {-- read the message header ------------------------------------------------}
  mb_Read(ReplyingTo);

  {-- are we allowed to edit this message? -----------------------------------}
  if NOT mb_AllowEdit(MessageInf, mb_GetFromWho) then
    begin
      Error := 8; { edit failed }
    end; { if }

  {-- we only support editting of message text in EleWEB ---------------------}
  {-- Now convert the block of data to an usable string ----------------------}
  if Error = 0 then
    begin
      mb_StrBuf_Init;                    { Intialize special "writing" buffer }
    end; { if }

  {-- if we did succeed, write the error -------------------------------------}
  if Error = 0 then
    begin
      {-- initialize the message text ----------------------------------------}
      mb_DoReadMessageText;

      {-- clear the actual message text --------------------------------------}
      mb_ResetMessagePtr;

      {-- update the message text --------------------------------------------}
      mb_Form_To_StrBuf('post_msgtext',           { Convert to special buffer }
                        false);                             { dont allow hTML }

      {-- add two lines to show it's been editted ----------------------------}
(*
      mb_StrBuf_Add('');
      mb_StrBuf_Add('[hr]');
      mb_StrBuf_Add(EleCodeStr('Edited by |FA on |KI |KJ'));
*)

      {-- actually update the messagebase ------------------------------------}
      Tmp := mb_UpdateMsgText;

      {-- write the new header -----------------------------------------------}
      mb_Write(ReplyingTo);
    end; { if }

  {-- Dispose of the data block ----------------------------------------------}
  if Error = 0 then
    begin
      mb_StrBuf_Done;                      { dispose special "writing" buffer }
    end; { if }

  {-- If we didn't succeed, display the error --------------------------------}
  if Error > 0 then
    begin
      Case Error of
      	01 : ShowHtmlFile(html_werror1_tpl);
      	02 : ShowHtmlFile(html_werror2_tpl);
      	03 : ShowHtmlFile(html_werror3_tpl);
      	04 : ShowHtmlFile(html_werror4_tpl);
      	05 : ShowHtmlFile(html_werror5_tpl);
      	06 : ShowHtmlFile(html_werror6_tpl);
      	07 : ShowHtmlFile(html_werror7_tpl);
       	08 : ShowHtmlFile(html_werror8_tpl);
        09 : ShowHtmlFile(html_werror9_tpl);
          else ;
      end; { case }
    end
       else ShowHtmlFile(html_w2succ_tpl);
end; { proc. EditMessage_Process }


procedure IncreasePostCount;
var UserInf   : UsersRecord;
    UserExt   : usrEleRecord;
begin
  {-- first get the current Userinfo fields ----------------------------------}
  GetUserRecord(UserInf, UserExt);

  {-- Update the post count --------------------------------------------------}
  UserInf.MsgsPosted := UserInf.MsgsPosted + 1;

  {-- now update the new values ----------------------------------------------}
  SetUserRecord(UserInf, UserExt);

  {-- and write them back to disk --------------------------------------------}
  UpdateUserRecord;
end; { proc. IncreasePostcount }


procedure WriteMessage_Process(ReplyingTo: Integer);
var FromWho         : String;
    ToWho           : String;
    Subject         : String;
    IsPrivate       : Boolean;
    IsReqRct        : Boolean;
    Error           : Integer;

    ReplyToStr      : String;
    ReplyAddr       : String;
    ReplyKludge     : String;
    Reply_Address   : String;

    TmpIdx          : Integer;
    TmpReply        : Integer;
{ Error 00 - no error }
{ Error 01 - No write access to this area }
{ Error 02 - FromWho is empty }
{ Error 03 - ToWho is empty }
{ Error 04 - Subject is empty }
{ Error 05 - Message to sysop in echomail }
{ Error 06 - We do not support netmails from EleWEB }
{ Error 07 - Invalid e-mail address entered }
begin
  {-- default to success -----------------------------------------------------}
  Error := 0;

  {-- Make sure this topic is not locked -------------------------------------}
  if ReplyingTo >= 0 then
   if IsForum then
    begin
      if FindMsgIdx(JustPath(MessageInf.JamBase), ReplyingTo, MessageInf.AreaNum) > 0 then
        if ReadBit(TopicInf.Attribute, 0) then
          begin
            Error := 9;
          end; { if }
    end; { if }


  {-- Make sure we have access to this area ----------------------------------}
  if NOT CheckMsgAreaAccess(Messageinf,
                            FALSE,                            { Message group }
                            FALSE,      { We dont want READ access, but WRITE }
                            0) then                    { Message group number }
     begin
       Error := 1;
     end; { if }

  {-- now get all the data fields --------------------------------------------}
  FromWho := Trim(web_GetFormData('post_fromwho'));
  ToWho := Trim(web_GetFormData('post_towho'));
  Subject := Trim(web_GetFormData('post_subject'));
  ReplyToStr := web_GetFormData('post_reply_replyto');
  ReplyAddr := web_GetFormData('post_reply_replyaddr');
  ReplyKludge := web_GetFormData('post_reply_replykludge');
  Reply_Address := web_GetFormData('post_reply_address');
  IsPrivate := SUpCase(web_GetFormData('post_private')) = 'TRUE';
  IsReqRct := SUpCase(web_GetformData('post_reqrct')) = 'TRUE';

  {-- is this area is an newsgroup, default to "All" -------------------------}
  if MessageInf.Typ = 4 then
    ToWho := 'All';

  {-- if this is an netmail area, abort --------------------------------------}
  if MessageInf.Typ = 1 then
    Error := 6;

  {-- and validate those fields ----------------------------------------------}
  if SUPCase(ToWho) = 'SYSOP' then
    if MessageInf.Typ = 2 then Error := 5;

  if SUpCase(ToWho) = 'SYSOP' then
    ToWho := GetSysopName;

  if Subject = '' then Error := 4;
  if ToWho = '' then Error := 3;
  if FromWho = '' then Error := 2;

  {-- if this area is forced private, this message is private ----------------}
  if (MessageInf.MsgKinds = 1) then IsPrivate := TRUE;       { private }
  if (MessageInf.MsgKinds = 2) then IsPrivate := FALSE;      { public }

  {-- if this message is to everyone, disable private and return receipt -----}
  if SUpCase(ToWho) = 'ALL' then
    begin
      IsPrivate := FALSE;
      IsReqRct := FALSE;
    end; { if }

  {-- if this is an internet area, make sure a valid email addy is entered ---}
  if MessageInf.Typ = 3 then
    begin
      if (Pos('.', ToWho) = 0) OR (Pos('@', ToWho) = 0) then
        Error := 7;
    end
      else begin
             ToWho := FixUserName(NoDoubleSpace(ToWho));
           end; { else }

  {-- check the validity of the FromWho field --------------------------------}
  if ReadBit(MessageInf.Attribute, 5) then                    { Force handles }
    begin
      FromWho := EleCodeStr('|F3');
      if FromWho = '' then
        FromWho := EleCodeStr('|FA');
    end; { Force Handles }

  if NOT ReadBit(MessageInf.Attribute, 3) then                { Pick an alias }
    if NOT ReadBit(MessageInf.Attribute, 5) then              { Force handles }
      FromWho := EleCodeStr('|FA');

  {-- Now convert the block of data to an usable string ----------------------}
  if Error = 0 then
    begin
      mb_StrBuf_Init;                    { Intialize special "writing" buffer }
      mb_StrBuf_Add(#01 + 'IP: ' + GetEnvironment('REMOTE_ADDR'));
      mb_Form_To_StrBuf('post_msgtext',           { Convert to special buffer }
                        false);                             { Dont allow HTML }
    end; { if }

  {-- if we did succeed, write the error -------------------------------------}
  if Error = 0 then
    mb_PostMessage(Ra250MsgArea(MessageInf.AreaNum),           { Board number }
                   ToWho,
                   FromWho,
                   Subject,
                   Reply_Address,                       { Destination address }
                   '',                                  { Originating address }
                   ReplyKludge,                                { Reply kludge }
                   IsReqRct,                                 { Return receipt }
                   IsPrivate,                                       { Private }
                   FALSE,                                         { Kill/Sent }
                   FALSE,                                         { Crashmail }
                   FALSE,                                       { Attachments }
                   FALSE,                         { Mark this message as sent }
                   MsgNumberWritten);       { Message number that was written }

  {-- if message written successfully, update post count ---------------------}
  if MsgNumberWritten > 0 then
    IncreasePostCount;

  {-- if this an reply, set the reply number ---------------------------------}
  if ReplyingTo >= 0 then
   if Error = 0 then
    if MsgNumberWritten > 0 then
      begin
        TmpReply := FVal(web_GetFormData('threadreply'));
        if TmpReply = 0 then
          TmpReply := ReplyingTo;

        if IsForum then
          begin
            {-- Update the index field for this one --------------------------}
            TmpIdx := FindMsgIdx(JustPath(MessageInf.JamBase), TmpReply, MessageInf.AreaNum);

            {-- If this is frontpage feedback, lets use the hidden thread value --}
            if IsFrontPage then
              begin
                if FVal(web_GetFormData('fpreply')) <> 0 then
                  begin
                    TmpIdx := FindMsgIdx(JustPath(MessageInf.JamBase),
                                         fVal(web_GetFormData('fpreply')),
                                         MessageInf.AreaNum);
                  end; { if }
              end; { if }

            if IsFrontPage then
              begin
                {-- update the actual reply chain ----------------------------}
                mb_SetReplyNr(Ra250MsgArea(MessageInf.AreaNum),
                              TmpReply,
                              MsgNumberWritten);
              end; { if }


            if TmpIdx > 0 then
              begin
                {-- update the actual reply chain --------------------------------}
                if NOT IsFrontPage then
                  begin
                    mb_SetReplyNr(Ra250MsgArea(MessageInf.AreaNum),
                                  TopicInf.LastPostNum,
                                  MsgNumberWritten);
                  end; { if }

                {-- update the info ----------------------------------------------}
                TopicInf.TotalPosts := TopicInf.TotalPosts + 1;
                TopicInf.LastPoster := FromWho;
                TopicInf.LastPostDate := NowAsUnixDate;
                TopicInf.LastPostNum := MsgNumberWritten;

                {-- and update it on disk ----------------------------------------}
                UpdateMsgIdx(JustPath(MessageInf.Jambase), TmpIdx, false, true,
                             MessageInf.AreaNum);
              end { if }
          end { if isforum }
            else begin
                   {-- update the actual reply chain ------------------------------------}
                   mb_SetReplyNr(Ra250MsgArea(MessageInf.AreaNum),
                                 TmpReply,
                                 MsgNumberWritten);
                 end; { else }

      end; { if }

  {-- if this is not a reply, add an index -----------------------------------}
  if ReplyingTo < 0 then
   if Error = 0 then
    if MsgNumberWritten > 0 then
     if IsForum then
      begin
        {-- Create the messagebase index if it doesnt exist yet --------------}
        if NOT FileExist(JustPath(MessageInf.JamBase) + 'forum' + FStr(MessageInf.AreaNum) + '.idx') then
          CreateNewIdx(JustPath(MessageInf.JamBase), MessageInf.AreaNum);

        {-- Add index for this thread ----------------------------------------}
        TopicInf.MsgNum := MsgNumberWritten;
        TopicInf.TotalPosts := 1;
        TopicInf.TopicStarter := FromWho;
        TopicInf.LastPoster := FromWho;
        TopicInf.LastPostDate := NowAsUnixDate;
        TopicInf.LastPostNum := MsgNumberWritten;
        TopicInf.OrigPostDate := NowAsUnixDate;

        AddMsgIdx(JustPath(MessageInf.JamBase), MessageInf.AreaNum);
      end; { if }

  {-- Create an message entry index (to sort on last post) -------------------}
  if IsForum then
   if Error = 0 then
    if MsgNumberWritten > 0 then
     if ((IsFrontPage) AND (TmpReply = 0)) OR (NOT (IsFrontPage)) then
      begin
        {-- Make sure we always update the correct number --------------------}
        TmpReply := FVal(web_GetFormData('threadreply'));
        if TmpReply = 0 then
          TmpReply := ReplyingTo;

        if TmpReply <= 0 then
          TmpReply := MsgNumberWritten;

        {-- Create the messagebase index if it doesnt exist yet --------------}
        if NOT FileExist(JustPath(MessageInf.JamBase) + 'threads' + FStr(MessageInf.AreaNum)+'.idx') then
          CreateSortIndex(JustPath(MessageInf.JamBase), MessageInf.AreaNum);

        {-- Now if it exists, delete the previous pointer --------------------}
        TmpIdx := FindSortIndex(JustPath(MessageInf.JamBase), TmpReply, MessageInf.AreaNum);
        if TmpIdx >= 0 then
          begin
            DisableSortIndex(JustPath(MessageInf.JamBase), TmpIdx, MessageInf.AreaNum);
          end; { if }

        {-- and now add a new index ------------------------------------------}
        AddSortIndex(JustPath(MessageInf.JamBase), TmpReply, false, MessageInf.AreaNum);
      end; { if }


  {-- Dispose of the data block ----------------------------------------------}
  if Error = 0 then
    begin
      mb_StrBuf_Done;                    { Intialize special "writing" buffer }
    end; { if }

  {-- If we didn't succeed, display the error --------------------------------}
  if Error > 0 then
    begin
      Case Error of
      	01 : ShowHtmlFile(html_werror1_tpl);
      	02 : ShowHtmlFile(html_werror2_tpl);
      	03 : ShowHtmlFile(html_werror3_tpl);
      	04 : ShowHtmlFile(html_werror4_tpl);
      	05 : ShowHtmlFile(html_werror5_tpl);
      	06 : ShowHtmlFile(html_werror6_tpl);
      	07 : ShowHtmlFile(html_werror7_tpl);
       	08 : ShowHtmlFile(html_werror8_tpl);
        09 : ShowHtmlFile(html_werror9_tpl);
          else ;
      end; { case }
    end
     else ShowHtmlFile(html_wsucc_tpl);
end; { proc. Writemessage_Process }


procedure LockTopic(MsgNum: Integer; DoLock: Boolean);
var TmpIdx: Longint;
begin
  if  SysOpAcc then
    begin
      {-- find the index record ------------------------------------------}
      TmpIdx := FindMsgIdx(JustPath(MessageInf.JamBase), MsgNum, MessageInf.AreaNum);
      if TmpIdx > 0 then
        begin
          {-- update the info --------------------------------------------}
          if DoLock then
            SetBit(TopicInf.Attribute, 0)
              else ClearBit(TopicInf.Attribute, 0);

          {-- and update it on disk --------------------------------------}
          UpdateMsgIdx(JustPath(MessageInf.Jambase), TmpIdx, false, false,
                       MessageInf.AreaNum);
        end { if }
          else WriteLn('Unable to find message');
    end
     else ShowHtmlFile(html_werror1_tpl); { edit failed }
end; { proc. LockTopic }


procedure ShowTotalTimings;
begin
  if timing_TplTotal = 0 then                  { no advanced timings are done }
    begin
      timing_ShowTimeFooter(timing_StartTime, timing_Finished);
    end
      else begin
             WriteLn('<BR><BR><CENTER><font color="white" size="1">',
                     'Processing time: ',
                      timing_ShowDifference(timing_StartTime, timing_Finished),
                      ' (Total: ', timing_ShowMSecs(timing_ForumEnd - timing_ForumStart),
                      ', tpl: ', timing_ShowMSecs(timing_TplTotal),
                      ', msg: ', timing_ShowMSecs(timing_ReadTotal),
                      ', idx: ', timing_ShowMSecs(timing_IdxTotal),
                      ', mcr: ', timing_ShowMSecs(timing_McrTotal),
                      ').</FONT></CENTER>');
           end; { else }

  WriteLn('<CENTER><font color="white" size="1">', EleWebVersion, '</font></center>');
end; { proc. ShowTotalTimings }

procedure SanityCheckSubAction(var SubAction: Integer);
begin
  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 1 then
   if IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 01');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 7 then
   if IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 03');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 8 then
   if IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 04');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- make sure they dont try to access the frontpage --------------------}
  if IsFrontPage then
   if NOT SysOpAcc then
    if (SubAction = 3) OR (SubAction = 4) OR (SubAction = 2) then
    begin
      web_RunErrorScript(4, 'Wrong area type 05');        { Access denied }
      SubAction := 99;                            { Invalidate sub-action }
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 9 then
   if NOT IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 06');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 10 then
   if IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 07');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 13 then
   if NOT IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 08');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if SubAction = 14 then
   if NOT IsForum then
    begin
      web_RunErrorScript(4, 'Wrong area type 09');        { Access denied }
      SubAction := 99;
    end; { if }

  {-- Make sure you dont flat read a forum -------------------------------}
  if (SubAction = 15) OR (SubAction = 16) then
   if NOT IsFrontPage then
    begin
      web_RunErrorScript(4, 'Wrong area type 10');        { Access denied }
      SubAction := 99;
    end; { if }
end; { proc. SanityCheckSubAction }

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;

  if web_GetFormData('newsubaction') = '' then
    begin
      ShowHeader;
    end; { if }

  {-- Save the current time --------------------------------------------------}
  timing_Initialize(timing_StartTime);

  {-- initialize variables ---------------------------------------------------}
  ShowKludges := FALSE;
  GuestAccess := FALSE;
  DaysOld := -1;

  {-- Init smiley table ------------------------------------------------------}
  InitTable;

  {-- Initialize the UBB parser ----------------------------------------------}
  InitUbbParser;

  {-- Load configuration -----------------------------------------------------}
  LoadWebConfig;

  {-- we dont allow color codes ----------------------------------------------}
  ubb_AllowColors := true;

  {-- we do allow HTML as the "mb_FormToStrBuf" defines wether the messages --}
  {-- allow HTML or not ------------------------------------------------------}
  ubb_AllowHtml := true;

  {-- First get some variables -----------------------------------------------}
  StartMsgNum := FVal(web_GetFormData('msgnum'));

  if web_GetFormData('daysprune') <> '' then
    DaysOld := FVal(web_GetFormData('daysprune'));

  if web_GetFormData('newsubaction') = '' then
    SubAction := FVal(web_GetFormData('sub-action'))
     else SubAction := FVal(web_GetFormData('newsubaction'));

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      GuestAccess := true;

      if SubAction <> 15 then              { we allow access to the frontpage }
       if SubAction <> 16 then        { we allow access to frontpage messages }
        if SubAction <> 14 then              { we allow access to forum reads }
         if SubAction <> 9 then                      { read the actual forums }
           begin                                  { for unauthenticated users }
            web_RunErrorScript(4, '');        { Show an "Access denied" error }
            Halt;                                     { Terminate this script }
           end; { if }
    end; { if }

  {-- Make sure the last login date/time is available in unix format ---------}
  SetLoginUnixTimeStamp;

  {-- First get some variables -----------------------------------------------}
  StartMsgNum := FVal(web_GetFormData('msgnum'));

  if web_GetFormData('newsubaction') = '' then
    SubAction := FVal(web_GetFormData('sub-action'))
     else SubAction := FVal(web_GetFormData('newsubaction'));

  if web_GetFormData('areanum') = '' then
    AreaNum := FVal(EleCodeStr('|K1'))
      else AreaNum := Ra250MsgArea(FVal(web_GetFormData('areanum')));

  {-- open the message base --------------------------------------------------}
  if OpenMessageBase(AreaNum) then
    begin
      {-- Sanity check the access type ---------------------------------------}
      SanityCheckSubAction(SubAction);

      {-- and run our function -----------------------------------------------}
      Case SubAction of
        00 : ListHeaders(StartMsgNum, 10, TRUE);    { not flat }
        01 : ReadMessage(StartMsgNum);
        02 : DeleteMsgNum(StartMsgNum);
        03 : WriteMessage_Startup(-1, false);
        04 : WriteMessage_Process(-1);
        05 : WriteMessage_StartUp(StartMsgNum, false);
        06 : WriteMessage_Process(StartMsgNum);
        07 : ListHeadersThreaded(StartMsgNum, 100, True);  { oldest to newest }
        08 : ListHeaders(StartMsgNum, -1, FALSE);     { flat }
        09 : ShowFlatMessageThread(StartMsgNum);
        10 : ListMessageAndThread(StartMsgNum);
        11 : WriteMessage_StartUp(StartMsgNum, true);
        12 : EditMessage_Process(StartMsgNum);
        13 : LockTopic(StartMsgNum, (web_GetFormData('unlock') <> 'true'));
        14 : ListForums(0, -1, false);     { false = is not story page }
        15 : ListForums(0, 15, true);
        16 : ShowStoryExpanded(StartMsgNum);
        17 : ListHeadersThreaded(StartMsgNum, 100, False); { newest to oldest }
        99 : ; { invalidated by sanity check }
          else ShowError('Unknown sub-action code');
      end; { case }

      {-- close the mesasgebase again ----------------------------------------}
      mb_CloseMessageBase;
    end { if }
      else ShowError('Unable to open messagebase');

  {-- and stop the timer -----------------------------------------------------}
  timing_Done(timing_Finished);

  {-- show the footer --------------------------------------------------------}
  ShowTotalTimings;
end. { readmsg }
