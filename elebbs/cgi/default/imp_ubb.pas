program imp_ubb;
(*
**
** EleWEB - UBB importer
**
** Created: 09-Sep-2002
** Last update: 09-Sep-2002
** Written by: Maarten Bekers
**
** Example script written to import the old UBB style messages into
** an existing EleWEB. Written for Mike Ehlert of PCMICRO.
**
*)
  
  {-- Include standard type definitions -------------------------------------}
  {$I stdrec.inc}

  {-- Include area records --------------------------------------------------}
  {$i arearec.inc}

  {-- Special index records for the forum style. ----------------------------}
  {$I msgidx.inc}

  {-- Message thread sorting records ----------------------------------------}
  {$I srtrec.inc}

var
  SubjectStr : String;                              { Subject of this message }
  AreaNum    : Integer;
  Poster     : String;
  Date_Str   : String;
  Time_Str   : String;
  ReplyingTo : Integer;
  TmpIdx     : Integer;
  MessageInf : MessageRecord;


  TopicInf    : TopicIdxRecord;
  TopicHdr    : TopicHdrRecord;


procedure ShowError(S:String);
begin
  WriteLn('FatalError: ', S);
end; { proc. ShowError }

{-- Include some of the IDX support routines ---------------------------------}
{$I idxsup.inc}

{-- Include thread sorting functions -----------------------------------------}
{$I srtfunc.inc}



procedure DoActualPost;
var MsgNumberWritten: Integer;
begin
  {-- if we did succeed, write the error -------------------------------------}
  mb_PostMessage(Ra250MsgArea(MessageInf.AreaNum),             { Board number }
                   'All',
                   Poster,
                   SubjectStr,
                   '',                                  { Destination address }
                   '',                                  { Originating address }
                   false,                                      { Reply kludge }
                   false,                                    { Return receipt }
                   false,                                           { Private }
                   FALSE,                                         { Kill/Sent }
                   FALSE,                                         { Crashmail }
                   FALSE,                                       { Attachments }
                   FALSE,                         { Mark this message as sent }
                   MsgNumberWritten);       { Message number that was written }

  {-- if message written successfully, update post count ---------------------}
  if MsgNumberWritten > 0 then
    begin
      WriteLn('Posted message from "', Poster, '" in area ', AreaNum, ' message #',
             MsgNumberWritten, ' (', SUbjectStr, ')');
    end
      else WriteLn('Error posting ?!');

  {-- Update the actual date and time ----------------------------------------}
  if MsgNumberWritten > 0 then
    begin
      mb_Read(MsgNumberWritten);
      mb_SetDateStr(Date_Str);
      mb_SetTimeStr(Time_Str);
      mb_Write(MsgNumberWritten);
    end; { if }

  {-- if this an reply, set the reply number ---------------------------------}
  if ReplyingTo >= 0 then
    begin
      {-- Update the index field for this one --------------------------}
      TmpIdx := FindMsgIdx(JustPath(MessageInf.JamBase), ReplyingTo, MessageInf.AreaNum);

      if TmpIdx > 0 then
        begin
          {-- update the actual reply chain --------------------------------}
          mb_SetReplyNr(Ra250MsgArea(MessageInf.AreaNum),
                        TopicInf.LastPostNum,
                        MsgNumberWritten);

          {-- update the info ----------------------------------------------}
          TopicInf.TotalPosts := TopicInf.TotalPosts + 1;
          TopicInf.LastPoster := Poster;
          TopicInf.LastPostDate := Date2Unix(PackTimeStr(Date_Str, Time_Str));
          TopicInf.LastPostNum := MsgNumberWritten;
          TopicInf.OrigPostDate := Date2Unix(PackTimeStr(Date_Str, Time_Str));

          {-- and update it on disk ----------------------------------------}
          UpdateMsgIdx(JustPath(MessageInf.Jambase), TmpIdx, false, true,
                             MessageInf.AreaNum);
        end { if }
      end; { if }

  {-- if this is not a reply, add an index -----------------------------------}
  if ReplyingTo < 0 then
      begin
        WriteLn('Adding new head index');
        ReplyingTo := MsgNumberWritten;

        {-- Create the messagebase index if it doesnt exist yet --------------}
        if NOT FileExist(JustPath(MessageInf.JamBase) + 'forum' + FStr(MessageInf.AreaNum) + '.idx') then
          CreateNewIdx(JustPath(MessageInf.JamBase), MessageInf.AreaNum);

        {-- Add index for this thread ----------------------------------------}
        TopicInf.MsgNum := MsgNumberWritten;
        TopicInf.TotalPosts := 1;
        TopicInf.TopicStarter := Poster;
        TopicInf.LastPoster := Poster;
        TopicInf.LastPostDate := Date2Unix(PackTimeStr(Date_Str, Time_Str));
        TopicInf.LastPostNum := MsgNumberWritten;

        AddMsgIdx(JustPath(MessageInf.JamBase), MessageInf.AreaNum);
      end; { if }

  {-- Create an message entry index (to sort on last post) -------------------}
  if MsgNumberWritten > 0 then
      begin
        {-- Create the messagebase index if it doesnt exist yet --------------}
        if NOT FileExist(JustPath(MessageInf.JamBase) + 'threads' + FStr(MessageInf.AreaNum)+'.idx') then
          CreateSortIndex(JustPath(MessageInf.JamBase), MessageInf.AreaNum);

        {-- Now if it exists, delete the previous pointer --------------------}
        TmpIdx := FindSortIndex(JustPath(MessageInf.JamBase), ReplyingTo, MessageInf.AreaNum);
        if TmpIdx >= 0 then
          begin
            DisableSortIndex(JustPath(MessageInf.JamBase), TmpIdx, MessageInf.AreaNum);
          end; { if }

        {-- and now add a new index ------------------------------------------}
        AddSortIndex(JustPath(MessageInf.JamBase), ReplyingTo, false, MessageInf.AreaNum);
      end; { if }
end; { DoActualPost }

procedure ParseThisMessage(ThisMsg: string);
var PosCnt    : Integer;
    PipeCnt   : Integer;

    ImgStr    : String;

    Hour,
    Min       : Integer;
    MsgText   : String;
begin
  MsgText := '';

  {-- Extract the poster ----------------------------------------------------}
  Poster := ExtractWord(ThisMsg, 3, '|', false, false);

  {-- Extract the data ------------------------------------------------------}
  Date_Str := ExtractWord(ThisMsg, 4, '|', false, false);

  if Length(Date_Str) = 8 then { .. }
    Date_Str := Copy(Date_Str, 1, 6) + Copy(Date_Str, 7, 2)
      else Date_Str := Copy(Date_Str, 1, 6) + Copy(Date_Str, 9, 2);

  {-- Extract the time ------------------------------------------------------}
  Time_Str := ExtractWord(ThisMsg, 5, '|', false, false);
  Hour := FVal(Copy(Time_Str, 1, 2));
  Min := FVal(Copy(Time_Str, 4, 2));

  if Copy(Time_Str, 7, 2) = 'PM' then
    begin
      Hour := Hour + 12;
    end; { if }

  Time_Str := LeadingZero(Hour, 2) + ':' + LeadingZero(Min, 2);

  {-- now parse the message text --------------------------------------------}
  PipeCnt := 0;
  Poscnt := 1;

  while PipeCnt < 12 do
    begin
      if ThisMsg[PosCnt] = '|' then
        PipeCnt := PipeCnt + 1;

      PosCnt := PosCnt + 1;
    end; { while }

  {-- ok, now we have the message text at the position we want, just --------}
  {-- display it ------------------------------------------------------------}
  while (PipeCnt = 12) AND
         (PosCnt <= Length(ThisMsg)) do
    begin
      if ThisMsg[PosCnt] = '|' then
        PipeCnt := PipeCnt + 1;

      if PipeCnt = 12 then
        begin
          if ThisMsg[PosCnt] = '<' then
            begin
              PosCnt := PosCnt + 1;

              if UpCase(ThisMsg[PosCnt]) = 'B' then
                begin
                  if UpCase(ThisMsg[PosCnt + 1]) = 'R' then
                    begin
                      { we expect <br> just assume it is }
                      PosCnt := PosCnt + 3;

                      mb_StrBuf_Add(MsgText);
                      MsgText := '';
                    end
                      else begin
                             {- blockquote }
                             PosCnt := PosCnt + 11;

                             MsgText := MsgText + '[quote]';
                           end; { else }
                end; { if }

              if ThisMsg[PosCnt - 1] = '<' then
               if UpCase(ThisMsg[PosCnt]) = 'P' then
                 begin
                   { we expect <p> just assume it is }
                   PosCnt := PosCnt + 2;

                   mb_StrBuf_Add(MsgText);
                   mb_StrBuf_Add('');  { add an extra white line }
                   MsgText := '';
                 end; { if }

              if ThisMsg[PosCnt - 1] = '<' then
              if upCase(ThisMsg[PosCnt]) = 'A' then
                begin
                  ImgStr := '';

                  { we expect <a href=> just assume it is }
                  PosCnt := PosCnt + 6;

                  {-- get the actual url }
                  while ThisMsg[PosCnt] <> '>' do
                    begin
                      PosCnt := PosCnt + 1;

                      if ThisMsg[PosCnt] <> '>' then
                        ImgStr := ImgStr + ThisMsg[PosCnt];
                    end; { if }

                  {-- create the url --}
                  Replace('"', '', ImgStr, false);
                  MsgText := '[url=' + ImgStr + ']';
                  ImgStr := '';

                  {-- sometimes, an <BR> is placed in the url desc, workaround }
                  if ThisMsg[PosCnt + 1] = '<' then
                    begin
                      PosCnt := PosCnt + 1;

                      while ThisMsg[PosCnt] <> '>' do
                        PosCnt := PosCnt + 1;
                    end; { if }

                  {-- get the url description }
                  while ThisMsg[PosCnt] <> '<' do
                    begin
                      PosCnt := PosCnt + 1;

                      if ThisMsg[PosCnt] <> '<' then
                        ImgStr := ImgStr + ThisMsg[PosCnt];
                    end; { if }

                  {-- create the url --}
                  MsgText := MsgText + ImgStr + '[/url]';

                  {-- get the url description }
                  while ThisMsg[PosCnt] <> '>' do
                    begin
                      PosCnt := PosCnt + 1;
                    end; { if }
                  Poscnt := PosCnt + 1;

                  mb_StrBuf_Add(MsgText);
                  MsgText := '';
                end; { if }

              if ThisMsg[PosCnt - 1] = '<' then
              if (upCase(ThisMsg[PosCnt]) = 'F') Or
                  (ThisMsg[PosCnt] = '/') OR
                   (ThisMsg[PosCnt] = 'H') then
                begin
                  if UpCAse(ThisMsg[PosCnt + 1]) = 'B' then
                    MsgText := MsgText + '[/quote]';

                  while ThisMsg[PosCnt] <> '>' do
                    begin
                      PosCnt := PosCnt + 1;
                    end; { if }

                  PosCnt := PosCnt + 1;
                end; { if }

              if ThisMsg[PosCnt - 1] = '<' then
              if ThisMsg[PosCnt] = 'I' then
                begin
                  ImgStr := '';

                  while ThisMsg[PosCnt] <> '>' do
                    begin
                      PosCnt := PosCnt + 1;
                      ImgStr := ImgStr + ThisMsg[PosCnt];
                    end; { if }

                  {-- try to fix some smileys -------------------------------}
                  if ImgStr = 'MG SRC="http://www.pcmicro.com/forums/biggrin.gif">' then
                    MsgText := MsgText + ':D';

                  if ImgStr = 'MG SRC="http://www.pcmicro.com/forums/smile.gif">' then
                    MsgText := MsgText + ':)';

                  if ImgStr = 'MG SRC="http://www.pcmicro.com/forums/frown.gif">' then
                    MsgText := MsgText + ':(';

                  if ImgStr = 'MG SRC="http://www.pcmicro.com/forums/redface.gif">' then
                    MsgText := MsgText + ':o';

                  if ImgStr = 'MG SRC="http://www.pcmicro.com/forums/wink.gif">' then
                    MsgText := MsgText + ';)';


                  PosCnt := PosCnt + 1;
                end; { if }
            end; { HTML code }


          if PosCnt < Length(ThisMsg) then
           if ThisMsg[PosCnt] <> '<' then
            if ThisMsg[PosCnt] <> '|' then
              MsgText := MsgText + ThisMsg[PosCnt];
        end; { if }

      if ThisMsg[PosCnt] <> '<' then
        PosCnt := PosCnt + 1;

      if Length(MsgText) > 230 then
       if PosCnt < Length(ThisMsg) then
        begin
          if (Pos(ThisMsg[PosCnt - 1], ' .,![]()?') > 0) OR
              (Length(MsgText) > 250) then
            begin
              if Pos('quote:origi', MsgText) > 0 then
                Replace('quote:origi', 'quote: origi', MsgText, false);

              mb_StrBuf_Add(MsgText);
              MsgText := '';
            end; { if }
        end; { if }
     end; { while }

  {-- and show a footer or something ----------------------------------------}
  mb_StrBuf_Add(MsgText);
  MsgText := '';
end; { proc. ParseThisMessage }



procedure ImportUbb(UbbName: String);
var UbbHandle : Integer;
    Error     : Integer;
    DataStr   : String;
    MsgIsGood : Boolean;
    TmpStr    : String;
begin
  {-- Initialize the global variables ---------------------------------------}
  ReplyingTo := -1;

  {-- Assign the file to the proper handle etc ------------------------------}
  UbbHandle := FileAssign(UbbName, 0, 3);
  Error := FileGetError(UbbHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(UbbHandle);
  Error := FileGetError(UbbHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      WriteLn('Fatal error reading UBB file: ', Error);
      Halt;
    end; { if }

  {-- Read the header -------------------------------------------------------}
  FileReadStringLn(UbbHandle, DataStr);

  {-- we fix invalid topics -------------------------------------------------}
  if DataStr[4] = '|' then
   Insert('N', DataStr, 4);

  {-- extract the 5th word --------------------------------------------------}
  SubjectStr := ExtractWord(DataStr, 5, '|', false, false);

  {-- and loop through all messages -----------------------------------------}
  if (Error = 0) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         MsgIsGood := false;
         DataStr := '';

         {-- Read the next line ---------------------------------------------}
         while (Error = 0) AND (NOT MsgIsGood) do
           begin
             FileReadStringLn(UbbHandle, TmpStr);
             Error := FileGetError(UbbHandle);

             DataStr := DataStr + TmpStr + ' ';
             if (Pos('|unreg', DataStr) <> 0) OR
                 (Pos('|reg', DataStr) <> 0) then
                   MsgIsGood := true;
           end; { while }

         {-- Parse this specific message ------------------------------------}
         if Error = 0 then
           begin
             mb_StrBuf_Init;                    { Intialize special "writing" buffer }

             ParseThisMessage(DataStr);
             DoActualPost;

             mb_StrBuf_Done;
           end; { if }

       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(UbbHandle);
  FileDone(UbbHandle);
end; { proc. ImportUbb }


begin
  WriteLn('imp_ubb - (c)2002 by Maarten Bekers');
  WriteLn;

  if (web_Getformdata('file') = '') OR (FVal(web_GetFormData('areanum'))=0) then
    begin
      WriteLn('Usage:');
      WriteLn('eleweb.exe imp_ubb "file=000010.txt&areanum=1006"');
      WriteLn;
      Halt;
    end; { if }

  AreaNum := fVal(web_GetFormdata('areanum'));  { Operators Test Forum }
  if NOT GetMessageRecord(MessageInf,
                          AreaNum,
                          true) then ;                     { Check RDX files }

  {-- open the messagebase ---------------------------------------------------}
  if NOT mb_OpenMsgBase(MessageInf) then
    begin
      writeln('ohmy');
    end; { if }
  WriteLn('Opened: ', MessageInf.Name);

  {-- now import the file ---------------------------------------------------}
  ImportUbb(web_GetformData('file'));
end.

