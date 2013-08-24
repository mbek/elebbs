(*
** Thread sorter records
**
** Written: 22-Jul-2002
**
**)

function FindSortIndex(BasePath: String; ThreadNum, AreaNum: Integer): Integer;
var IdxHandle : Integer;
    Error     : Integer;
    DidFind   : Boolean;
    ThreadInf : ThreadSortRec;
begin
  {-- initialize the result to not found ------------------------------------}
  FindSortIndex := -1;
  DidFind := false;

  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'threads' + FStr(AreaNum) + '.idx', 0, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- Never start a search for -1 -------------------------------------------}
  if ThreadNum < 0 then
    Error := 1;

  {-- look for this message's index -----------------------------------------}
  if (Error = 0) then
   begin
     {-- now go through all records -----------------------------------------}
     while (Error = 0) AND (NOT DidFind) do
       begin
         {-- read the idx ---------------------------------------------------}
         FileRead(IdxHandle, ThreadInf);

         {-- is this our message? -------------------------------------------}
         if ThreadInf.ThreadNum = ThreadNum then
           begin
             DidFind := true;
             FindSortIndex := FileGetPos(IdxHandle) - GetRecSize(ThreadInf);
           end; { if }

         {-- query the result -----------------------------------------------}
         Error := FileGetError(IdxHandle);
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { func. FindSortIndex }

procedure DisableSortIndex(BasePath: String; SortIdx, AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
    ThreadInf : ThreadSortRec;
begin
  {-- Set up thread inf -----------------------------------------------------}
  ThreadInf.ThreadNum := -1;

  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'threads' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- Go to the end of the index --------------------------------------------}
  FileSeek(IdxHandle, SortIdx);
  FileWrite(IdxHandle, ThreadInf);

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { proc. DisableSortIndex }


procedure AddSortIndex(BasePath: String; ThreadNum: Integer; IsLocked: Boolean; AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
    ThreadInf : ThreadSortRec;
begin
  {-- Set up thread inf -----------------------------------------------------}
  ThreadInf.ThreadNum := ThreadNum;
  if IsLocked then
    SetBit(ThreadInf.Attribute, 0);                       { Thread is locked }

  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'threads' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- Go to the end of the index --------------------------------------------}
  FileSeek(IdxHandle, FileGetSize(IdxHandle));
  FileWrite(IdxHandle, ThreadInf);

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { proc. AddSortIndex }

procedure CreateSortIndex(BasePath: String; AreaNum: Integer);
var IdxHandle : Integer;
    Error     : Integer;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  IdxHandle := FileAssign(BasePath + 'threads' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileCreate(IdxHandle);
  Error := FileGetError(IdxHandle);

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle);
  FileDone(IdxHandle);
end; { proc. CreateSortIndex }

procedure CompressSortIndex(BasePath: String; AreaNum: Integer);
var IdxHandle_Src : Integer;
    IdxHandle_Dest: Integer;
    Error         : Integer;
    ThreadInf     : ThreadSortRec;
begin
  {-- Rename the original source file ---------------------------------------}
  if RenameFile(BasePath + 'threads' + FStr(AreaNum) + '.idx',
                BasePath + 'threads$' + FStr(AreaNum) + '.$idx$') then ;

  {-- Open the source file --------------------------------------------------}
  IdxHandle_Src := FileAssign(BasePath + 'threads$' + FStr(AreaNum) + '.$idx$', 2, 3);
  Error := FileGetError(IdxHandle_Src);
  FileOpen(IdxHandle_Src);
  Error := FileGetError(IdxHandle_Src);

  {-- Create the destination file -------------------------------------------}
  IdxHandle_Dest := FileAssign(BasePath + 'threads' + FStr(AreaNum) + '.idx', 2, 3);
  Error := FileGetError(IdxHandle_Dest);
  FileCreate(IdxHandle_Dest);
  Error := FileGetError(IdxHandle_Dest);

  {-- Go to the end of the index --------------------------------------------}
  while Error = 0 do
    begin
      FileRead(IdxHandle_Src, ThreadInf);
      Error := FileGetError(IdxHandle_Src);

      if ThreadInf.ThreadNum >= 0 then
       if Error = 0 then
         FileWrite(IdxHandle_Dest, ThreadInf);
    end; { while }

  {-- Close the file --------------------------------------------------------}
  FileClose(IdxHandle_Dest);
  FileClose(IdxHandle_Src);
  FileDone(IdxHandle_Dest);
  FileDelete(IdxHandle_Src);
  FileDone(IdxHandle_Src);
end; { proc. AddSortIndex }

