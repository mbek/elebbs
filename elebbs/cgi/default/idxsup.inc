procedure CreateNewIdx(BasePath: String; AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'forum' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileCreate(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      ShowError('Unable to create forum index file');
    end; { if }

  if (Error = 0) then
   begin
     {-- Create the header --------------------------------------------------}
     TopicHdr.NumMsgs := 0;
     TopicHdr.NumThreads := 0;
     TopicHdr.LastPoster := 'n/a';
     TopicHdr.LastPostDate := NowAsUnixDate;

     {-- write the data to the file -----------------------------------------}
     FileWrite(IdxHandle, TopicHdr);
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { proc. CreateNewIdx }


function FindMsgIdx(BasePath: String; MsgNum, AreaNum: Integer): Integer;
var IdxHandle : Integer;
    Error     : Integer;
    DataStr   : String;
    DidFind   : Boolean;
begin
  {-- initialize the result to not found ------------------------------------}
  FindMsgIdx := -1;
  DidFind := false;

  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'forum' + FStr(AreaNum) +'.idx', 0, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- look for this message's index -----------------------------------------}
  if (Error = 0) then
   begin
     {-- read the data ------------------------------------------------------}
     FileRead(IdxHandle, TopicHdr);
     Error := FileGetError(IdxHandle);

     {-- now go through all records -----------------------------------------}
     while (Error = 0) AND (NOT DidFind) do
       begin
         {-- read the idx ---------------------------------------------------}
         FileRead(IdxHandle, TopicInf);

         {-- is this our message? -------------------------------------------}
         if TopicInf.MsgNum = MsgNum then
           begin
             DidFind := true;
             FindMsgIdx := FileGetPos(IdxHandle) - GetRecSize(TopicInf);
           end; { if }

         {-- query the result -----------------------------------------------}
         Error := FileGetError(IdxHandle);
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { func. FindMsgIdx }


procedure ReadIdxHdr(BasePath: String; AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'forum' + FStr(AreaNum) + '.idx', 0, 3);
  Error := FileGetError(IdxHandle);

  {-- Initialize all info ---------------------------------------------------}
  TopicHdr.NumMsgs := 0;
  TopicHdr.NumThreads := 0;
  TopicHdr.LastPoster := 'n/a';
  TopicHdr.LastPostDate := 0;
  
  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- look for this message's index -----------------------------------------}
  if (Error = 0) then
   begin
     {-- read the data ------------------------------------------------------}
     FileRead(IdxHandle, TopicHdr);
     Error := FileGetError(IdxHandle);
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { func. ReadIdxHdr }


procedure AddMsgIdx(BasePath: String; AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'forum' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- look for this message's index -----------------------------------------}
  if (Error = 0) then
   begin
     {-- Read the header ----------------------------------------------------}
     FileRead(IdxHandle, TopicHdr);

     {-- update the data in it ----------------------------------------------}
     TopicHdr.NumMsgs := TopicHdr.NumMsgs + 1;
     TopicHdr.NumThreads := TopicHdr.NumThreads + 1;
     TopicHdr.LastPoster := TopicInf.LastPoster;
     TopicHdr.LastPostDate := TopicInf.LastPostDate;

     {-- and update the file itself -----------------------------------------}
     FileSeek(IdxHandle, 0);
     FileWrite(IdxHandle, TopicHdr);
     Error := FileGetError(IdxHandle);

     {-- read the data ------------------------------------------------------}
     FileSeek(IdxHandle, FileGetSize(IdxHandle));
     Error := FileGetError(IdxHandle);

     {-- now add the record -------------------------------------------------}
     FileWrite(IdxHandle, TopicInf);
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { proc. AddMsgIdx }


procedure UpdateMsgIdx(BasePath: String; IdxPos: Longint; IsDelete, IsPost: Boolean; AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'forum' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- look for this message's index -----------------------------------------}
  if (Error = 0) then
   begin
     {-- Read the header ----------------------------------------------------}
     FileRead(IdxHandle, TopicHdr);

     {-- update the data in it ----------------------------------------------}
     if IsDelete then
       TopicHdr.NumMsgs := TopicHdr.NumMsgs - 1;

     if IsPost then
        TopicHdr.NumMsgs := TopicHdr.NumMsgs + 1;

     TopicHdr.LastPoster := TopicInf.LastPoster;
     TopicHdr.LastPostDate := TopicInf.LastPostDate;

     {-- and update the file itself -----------------------------------------}
     FileSeek(IdxHandle, 0);
     FileWrite(IdxHandle, TopicHdr);
     Error := FileGetError(IdxHandle);

     {-- read the data ------------------------------------------------------}
     FileSeek(IdxHandle, IdxPos);
     Error := FileGetError(IdxHandle);

     {-- now add the record -------------------------------------------------}
     FileWrite(IdxHandle, TopicInf);
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { proc. UpdateMsgIdx }

