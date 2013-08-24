program FixIdx;
(*
**
** EleWEB EleXer source
** Rebuild the indexes
**
** Created: 25-Aug-2002
** Last update: 25-Aug-2002
** Written by: Maarten Bekers
**
*)

{-- Include standard type definitions ----------------------------------------}
{$I stdrec.inc}

{-- Include area records -----------------------------------------------------}
{$i arearec.inc}

{-- Include some of the IDX support routines ---------------------------------}
{$I msgidx.inc}

{-- Message thread sorting records -------------------------------------------}
{$I srtrec.inc}

{-- Web configuration record -------------------------------------------------}
{$I cfgrec.inc}


var TopicInf    : TopicIdxRecord;
    TopicHdr    : TopicHdrRecord;
    MessageInf  : MessageRecord;
    AreaNum     : Integer;
    ThreadHead  : Integer;


procedure ShowError(TmpStr: String);
begin
  WriteLn;
  WriteLn('ERROR: ', TmpStr);
end; { proc. ShowError }


{-- Include some of the IDX support routines ---------------------------------}
{$I idxsup.inc}

{-- Include thread sorting functions -----------------------------------------}
{$I srtfunc.inc}


procedure ShowHelp;
begin
  WriteLn('Usage:');
  WriteLn;
  WriteLn('eleweb.exe fixidx "areanum=<area num>"');
  WriteLn;
  WriteLn;
end; { proc. ShowHelp }


procedure thread_IncDepth(var HowDeep: Integer);
begin
  HowDeep := HowDeep + 1;                   { another reply to this reply }
end; { proc. thread_IncDepth }

procedure thread_DecDepth(var HowDeep: Integer);
begin
  HowDeep := HowDeep - 1;                       { we finished this thread }
end; { proc. thread_DecDepth }

procedure thread_ShowItem(HowDeep: Integer);
var TmpIdx: Integer;
begin
  WriteLn(Dup('-', HowDeep), ': ', mb_GetMsgNumber, ' (', mb_GetSubject, ')');

  if HowDeep = 0 then
    begin
      {-- Add index for this thread ----------------------------------------}
      TopicInf.MsgNum := mb_GetMsgNumber;
      TopicInf.TotalPosts := 1;
      TopicInf.TopicStarter := mb_GetFromWho;
      TopicInf.LastPoster := mb_GetFromWho;
      TopicInf.LastPostDate := Date2Unix(PackTimeStr(mb_GetDateStr, mb_GetTimeStr)); {was: NowAsUnixDate; }
      TopicInf.LastPostNum := mb_GetMsgNumber;
      TopicInf.OrigPostDate := Date2Unix(PackTimeStr(mb_GetDateStr, mb_GetTimeStr)); {was:NowAsUnixDate;}

      AddMsgIdx(JustPath(MessageInf.JamBase), MessageInf.AreaNum);
      AddSortIndex(JustPath(MessageInf.JamBase), mb_GetMsgNumber, false, MessageInf.AreaNum);

      ThreadHead := mb_GetMsgNumber;
    end { if }
      else begin
             {-- Now if it exists, delete the previous pointer --------------}
             TmpIdx := FindSortIndex(JustPath(MessageInf.JamBase), ThreadHead, MessageInf.AreaNum);
             if TmpIdx >= 0 then
               begin
                 DisableSortIndex(JustPath(MessageInf.JamBase), TmpIdx, MessageInf.AreaNum);
               end; { if }

             {-- and now add a new index ------------------------------------}
             AddSortIndex(JustPath(MessageInf.JamBase), ThreadHead, false, MessageInf.AreaNum);

             {-- find the index record ------------------------------------------}
             TmpIdx := FindMsgIdx(JustPath(MessageInf.JamBase), ThreadHead, MessageInf.AreaNum);
             if TmpIdx > 0 then
               begin
                 {-- update the info --------------------------------------------}
                 TopicInf.TotalPosts := TopicInf.TotalPosts + 1;
                 TopicInf.LastPostDate := Date2Unix(PackTimeStr(mb_GetDateStr, mb_GetTimeStr)); {was:NowAsUnixDate;}
                 TopicInf.LastPoster := mb_GetFromWho;
                 TopicInf.LastPostNum := mb_GetMsgNumber;

                 {-- and update it on disk --------------------------------------}
                 UpdateMsgIdx(JustPath(MessageInf.Jambase), TmpIdx, false, true,
                              MessageInf.AreaNum);
               end { if }

           end; { if }
end; { proc. thread_ShowItem }


procedure thread_WalkNested(MsgNum, HowDeep: integer);
var SaveMsg: Integer;
begin
  {-- save the current message number ----------------------------------------}
  SaveMsg := MsgNum;
  mb_Read(MsgNum);

  {-- and show this item -----------------------------------------------------}
  thread_ShowItem(HowDeep);

  {-- now handle nesting, if there is any ------------------------------------}
  if mb_GetReplyFirst > 0 then
    begin
      {-- Increase thread-depth ----------------------------------------------}
      thread_IncDepth(HowDeep);

      {-- now recursively call ourselves to walk this thread -----------------}
      thread_WalkNested(mb_GetReplyFirst, HowDeep);

      {-- and decrease the depth ---------------------------------------------}
      thread_DecDepth(HowDeep);

      {-- and return to our original setup -----------------------------------}
      mb_Read(SaveMsg);
    end; { if reply_first }

  if mb_GetReplyNext > 0 then
    begin
      {-- now recursively call ourselves to walk this thread -----------------}
      thread_WalkNested(mb_GetReplyNext, HowDeep);

      {-- and return to our original setup -----------------------------------}
      mb_Read(SaveMsg);
    end; { if reply_second }
end; { proc. thread_WalkNested }


procedure thread_WalkChain(MsgNum: Integer;
                           var HowDeep: Integer);
var SaveMsg: Integer;
begin
  {-- initialize this first message ------------------------------------------}
  mb_Read(MsgNum);

  {-- now loop through all messages ------------------------------------------}
  while (mb_MessageFound) do
    begin
      {-- if this isnt a reply to anything we handle it, else we rely on -----}
      {-- it that the original message is still around and linked to this ----}
      {-- reply --------------------------------------------------------------}
      if mb_GetReplyTo = 0 then
        begin
          {-- this is an original message, reset the deep count --------------}
          HowDeep := 0;

          {-- and show the message -------------------------------------------}
          thread_ShowItem(HowDeep);

          {-- if this message has a ReplyFirst count, walk through it --------}
          if mb_GetReplyFirst > 0 then
            begin
              {-- increase the depth -----------------------------------------}
              thread_IncDepth(HowDeep);

              {-- save the current message so we can return to it ------------}
              SaveMsg := mb_GetMsgNumber;

              {-- now walk through the reply chain ---------------------------}
              thread_WalkNested(mb_GetReplyFirst, HowDeep);

              {-- now restore the original message because the reply could ---}
              {-- have an earlier message number than the original message ---}
              mb_Read(SaveMsg);

              {-- decrease the depth -----------------------------------------}
              thread_DecDepth(HowDeep);
            end; { if }
        end; { if }

      {-- and retrieve the next message --------------------------------------}
      mb_GetNext;
    end; { while }
end; { proc. thread_WalkChain }


procedure FixIdx;
var HowDeep: Integer;
begin
  {-- and lets start at the top ----------------------------------------------}
  thread_WalkChain(1, HowDeep);
end; { proc. FixIdx }


function OpenMessageBase(AreaNum: Integer): Boolean;
var Success: Boolean;
begin
  {-- we default to success --------------------------------------------------}
  Success := TRUE;

  {-- First get the area number we want --------------------------------------}
  if NOT GetMessageRecord(MessageInf,
                          AreaNum,
                          FALSE) then ;                     { Check RDX files }

  {-- open the messagebase ---------------------------------------------------}
  if NOT mb_OpenMsgBase(MessageInf) then
    begin
      Success := FALSE;
    end; { if }

  {-- and feed back the result -----------------------------------------------}
  OpenMessageBase := Success;
end; { func. OpenMessageBase }

begin
  {-- SHow some info ---------------------------------------------------------}
  WriteLn('FIXIDX (c)2002 by Maarten Bekers');
  WriteLn;

  {-- get the areanummer -----------------------------------------------------}
  AreaNum := FVal(web_GetFormData('areanum'));
  if AreaNum = 0 then
    begin
      ShowHelp;
    end; { if }

  {-- try to open the message base -------------------------------------------}
  if AreaNum <> 0 then
   if OpenMessageBase(Ra250MsgArea(AreaNum)) then
    begin
      {-- create the new index file ------------------------------------------}
      CreateNewIdx(JustPath(MessageInf.Jambase), Areanum);
      CreateSortIndex(JustPath(MessageInf.JamBase), AreaNum);

      {-- and fix the idx reply chains ---------------------------------------}
      FixIdx;

      {-- and remove unused sort indexes -------------------------------------}
      CompressSortIndex(JustPath(MessageInf.JamBase), AreaNum);
    end
      else ShowError('Unable to open area ' + FStr(AreaNum));
end.
